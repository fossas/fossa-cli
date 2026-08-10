{-# LANGUAGE QuasiQuotes #-}

module Python.ReqTxtSpec (
  spec,
) where

import Control.Monad (void)
import Data.Foldable (find)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import DepTypes
import Effect.Grapher
import Graphing (Graphing)
import Graphing qualified
import Strategy.Python.Pip (PythonPackage (..))
import Strategy.Python.Util
import Text.URI.QQ (uri)

import Test.Hspec

newtype ExpectedDependency = ExpectedDependency (Dependency, [ExpectedDependency])

setupPyInput :: [Req]
setupPyInput =
  [ NameReq
      "pkgOne"
      Nothing
      ( Just
          [ Version OpGtEq "1.0.0"
          , Version OpLt "2.0.0"
          ]
      )
      Nothing
  , NameReq "pkgTwo" Nothing Nothing Nothing
  , UrlReq "pkgThree" Nothing [uri|https://example.com|] Nothing
  ]

installedPackages :: [PythonPackage]
installedPackages =
  [ PythonPackage "foo" "2" []
  , PythonPackage "pkgOne" "1.0.0" [PythonPackage "pkgOne_One" "3.0" []]
  , PythonPackage "pkgTwo" "1" [PythonPackage "pkgTwo_One" "1" []]
  , PythonPackage "pkgNotThree" "https://example-not.com" [PythonPackage "ignored_me" "1" []]
  ]

expectedDeps :: [ExpectedDependency]
expectedDeps =
  [ ExpectedDependency
      ( Dependency
          { dependencyType = PipType
          , dependencyName = "pkgOne"
          , dependencyVersion =
              Just
                ( CAnd
                    (CGreaterOrEq "1.0.0")
                    (CLess "2.0.0")
                )
          , dependencyLocations = []
          , dependencyEnvironments = mempty
          , dependencyTags = Map.empty
          }
      ,
        [ ExpectedDependency
            ( Dependency
                { dependencyType = PipType
                , dependencyName = "pkgOne_One"
                , dependencyVersion = Just (CEq "3.0")
                , dependencyLocations = []
                , dependencyEnvironments = mempty
                , dependencyTags = Map.empty
                }
            , []
            )
        ]
      )
  , ExpectedDependency
      ( Dependency
          { dependencyType = PipType
          , dependencyName = "pkgTwo"
          , dependencyVersion = Nothing
          , dependencyLocations = []
          , dependencyEnvironments = mempty
          , dependencyTags = Map.empty
          }
      ,
        [ ExpectedDependency
            ( Dependency
                { dependencyType = PipType
                , dependencyName = "pkgTwo_One"
                , dependencyVersion = Just (CEq "1")
                , dependencyLocations = []
                , dependencyEnvironments = mempty
                , dependencyTags = Map.empty
                }
            , []
            )
        ]
      )
  , ExpectedDependency
      ( Dependency
          { dependencyType = PipType
          , dependencyName = "pkgThree"
          , dependencyVersion = Just (CURI "https://example.com")
          , dependencyLocations = []
          , dependencyEnvironments = mempty
          , dependencyTags = Map.empty
          }
      , []
      )
  ]

-- | What the graph looks like when pip reports the installed packages: the
-- declared constraints on pkgOne and pkgTwo give way to the versions actually
-- present, and their transitive dependencies appear.
expectedInstalledDeps :: [ExpectedDependency]
expectedInstalledDeps = map resolve expectedDeps
  where
    resolve (ExpectedDependency (dep, children)) =
      ExpectedDependency (withInstalled dep, children)

    withInstalled dep = case dependencyName dep of
      "pkgOne" -> dep{dependencyVersion = Just (CEq "1.0.0")}
      "pkgTwo" -> dep{dependencyVersion = Just (CEq "1")}
      _ -> dep

traverseDirect :: [ExpectedDependency] -> Graphing Dependency
traverseDirect deps = run . evalGrapher $ do
  traverse
    ( \(ExpectedDependency (dep, _)) -> do
        direct dep
    )
    deps

traverseDirectAndDeep :: [ExpectedDependency] -> Graphing Dependency
traverseDirectAndDeep deps = run . evalGrapher $ do
  traverse
    ( \(ExpectedDependency (dep, deepDeps)) -> do
        direct dep
        traverseDeepDeps dep deepDeps
    )
    deps
  where
    traverseDeepDeps parent children = do
      traverse addDeps children
      where
        addDeps (ExpectedDependency (child, deeperDeps)) = do
          deep child
          edge parent child
          void $ traverseDeepDeps child deeperDeps

spec :: Spec
spec =
  describe "analyze" $ do
    it "should produce expected output" $ do
      let result = buildGraph Nothing setupPyInput

      result `shouldBe` traverseDirect expectedDeps

    it "should only report transitive dependencies for packages found in req.txt" $ do
      let result = buildGraph (Just installedPackages) setupPyInput

      result `shouldBe` traverseDirectAndDeep expectedInstalledDeps

    it "should report the installed version rather than a declared range" $ do
      -- pkgOne is declared as ">=1.0.0, <2.0.0" but installed at 1.0.0. A
      -- locator can only carry one revision, and the environment knows which
      -- one is actually there.
      let result = buildGraph (Just installedPackages) setupPyInput

      versionOf "pkgOne" result `shouldBe` Just (Just (CEq "1.0.0"))

    it "should report the installed version regardless of bound order" $ do
      -- The declared range says the same thing either way round, so the
      -- reported version must not depend on which bound was written first.
      let lowerFirst = [NameReq "pkgOne" Nothing (Just [Version OpGtEq "1.0.0", Version OpLt "2.0.0"]) Nothing]
          upperFirst = [NameReq "pkgOne" Nothing (Just [Version OpLt "2.0.0", Version OpGtEq "1.0.0"]) Nothing]

      versionOf "pkgOne" (buildGraph (Just installedPackages) upperFirst)
        `shouldBe` versionOf "pkgOne" (buildGraph (Just installedPackages) lowerFirst)

    it "should fill in a version for a requirement that declares none" $ do
      -- A bare package name on a requirements.txt line otherwise produces a
      -- locator with no revision at all.
      let result = buildGraph (Just installedPackages) setupPyInput

      versionOf "pkgTwo" result `shouldBe` Just (Just (CEq "1"))

    it "should match installed packages by their canonical name" $ do
      -- PEP 503: "Zope.Interface", "zope_interface" and "zope-interface" all
      -- name the same package.
      let reqs = [NameReq "Zope.Interface" Nothing (Just [Version OpGtEq "5.0"]) Nothing]
          installed = [PythonPackage "zope-interface" "5.4.0" []]

      versionOf "Zope.Interface" (buildGraph (Just installed) reqs)
        `shouldBe` Just (Just (CEq "5.4.0"))

    it "should preserve environment markers when substituting a version" $ do
      -- The marker becomes the dependency's tags, and describes the
      -- requirement rather than the version it resolved to.
      let marker = MarkerExpr "sys_platform" (MarkerOperator OpEq) "linux"
          reqs = [NameReq "pkgOne" Nothing (Just [Version OpGtEq "1.0.0"]) (Just marker)]
          result = buildGraph (Just installedPackages) reqs

      tagsOf "pkgOne" result `shouldBe` Just (Map.fromList [("sys_platform", ["linux"])])

    it "should leave a URL requirement alone" $ do
      let reqs = [UrlReq "pkgOne" Nothing [uri|https://example.com|] Nothing]

      versionOf "pkgOne" (buildGraph (Just installedPackages) reqs)
        `shouldBe` Just (Just (CURI "https://example.com"))

    it "should keep the declared constraint when pip reports nothing" $ do
      let result = buildGraph Nothing setupPyInput

      versionOf "pkgOne" result `shouldBe` Just (Just (CAnd (CGreaterOrEq "1.0.0") (CLess "2.0.0")))

-- | Look up a dependency by name and return its version, or 'Nothing' if no
-- dependency by that name is in the graph.
versionOf :: Text -> Graphing Dependency -> Maybe (Maybe VerConstraint)
versionOf name = fmap dependencyVersion . findDep name

-- | Look up a dependency by name and return its tags.
tagsOf :: Text -> Graphing Dependency -> Maybe (Map.Map Text [Text])
tagsOf name = fmap dependencyTags . findDep name

findDep :: Text -> Graphing Dependency -> Maybe Dependency
findDep name = find ((== name) . dependencyName) . Graphing.vertexList
