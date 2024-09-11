module App.Fossa.VSI.IAT.ResolveSpec (spec) where

import App.Fossa.VSI.IAT.Resolve (resolveGraph, resolveRevision, resolveUserDefined)
import App.Fossa.VSI.IAT.Types (UserDefinedAssertionMeta (..), UserDep (..))
import App.Fossa.VSI.Types (Locator (..), SkipResolution (SkipResolution))
import Control.Effect.FossaApiClient (FossaApiClientF (ResolveProjectDependencies, ResolveUserDefinedBinary))
import Data.Set qualified as Set
import Data.Text.Extra (showT)
import DepTypes (DepType (CustomType))
import Graphing qualified
import Srclib.Converter (depTypeToFetcher)
import Srclib.Types (SourceUserDefDep (..))
import Test.Effect (it', shouldBe')
import Test.Hspec (Spec, describe)
import Test.MockApi (fails, returnsOnce)

spec :: Spec
spec = do
  describe "resolveUserDefined" $ do
    it' "returns Nothing if there are no deps" $ do
      userDeps <- resolveUserDefined []
      userDeps `shouldBe'` Nothing
    it' "returns all user defined dependencies that are found" $ do
      let userDep1 = testUserDep 1
          userDep2 = testUserDep 2
          assertion1 = testUserDefinedAssertionMeta 1
          assertion2 = testUserDefinedAssertionMeta 2
      ResolveUserDefinedBinary userDep1 `returnsOnce` assertion1
      ResolveUserDefinedBinary userDep2 `returnsOnce` assertion2
      userDeps <- resolveUserDefined [userDep1, userDep2]
      userDeps `shouldBe'` Just [testUserDefinedDep 1, testUserDefinedDep 2]
  describe "resolveRevision" $ do
    it' "ignores non-top-level locators" $ do
      let package = testLocator 1
      maybeDeps <- resolveRevision package
      maybeDeps `shouldBe'` Just []
    it' "returns Nothing if there is an error" $ do
      let package = testTopLevelLocator 1
      ResolveProjectDependencies package `fails` "Mock error"
      maybeDeps <- resolveRevision package
      maybeDeps `shouldBe'` Nothing
    it' "resolves dependencies for top-level locators" $ do
      let package = testTopLevelLocator 1
          dep1 = testLocator 11
          dep2 = testLocator 12
      ResolveProjectDependencies package `returnsOnce` [dep1, dep2]
      maybeDeps <- resolveRevision package
      maybeDeps `shouldBe'` Just [dep1, dep2]
  describe "resolveGraph" $ do
    it' "resolves available subgraphs" $ do
      let package1 = testTopLevelLocator 1
          package2 = testTopLevelLocator 2
          dep1 = testLocator 11
          dep2 = testLocator 12
          skips = SkipResolution (Set.empty)
          expectedGraph =
            Graphing.directs [package1, package2]
              <> Graphing.edges [(package1, dep1), (package1, dep2)]
      -- Package1 is fine
      ResolveProjectDependencies package1 `returnsOnce` [dep1, dep2]
      -- Package2 has an error
      ResolveProjectDependencies package2 `fails` "Mock error"
      -- The graph should resolve with the successful results.
      graph <- resolveGraph [package1, package2] skips
      graph `shouldBe'` expectedGraph
    it' "resolves dependencies not in the skip list" $ do
      let package1 = testTopLevelLocator 1
          package2 = testTopLevelLocator 2
          skips = SkipResolution (Set.empty)
          dep1 = testLocator 11
          dep2 = testLocator 12
          commonDep = testLocator 10
          expectedGraph =
            Graphing.directs [package1, package2]
              <> Graphing.edges [(package1, dep1), (package1, commonDep), (package2, commonDep), (package2, dep2)]
      ResolveProjectDependencies package1 `returnsOnce` [dep1, commonDep]
      ResolveProjectDependencies package2 `returnsOnce` [dep2, commonDep]
      graph <- resolveGraph [package1, package2] skips
      graph `shouldBe'` expectedGraph
    it' "does not resolve dependencies in the skip list" $ do
      let package1 = testTopLevelLocator 1
          package2 = testTopLevelLocator 2
          skips = SkipResolution (Set.fromList [package1])
          dep1 = testLocator 11
          dep2 = testLocator 12
          expectedGraph =
            Graphing.directs [package1, package2]
              <> Graphing.edges [(package2, dep1), (package2, dep2)]
      ResolveProjectDependencies package2 `returnsOnce` [dep1, dep2]
      graph <- resolveGraph [package1, package2] skips
      graph `shouldBe'` expectedGraph

testUserDep :: Int -> UserDep
testUserDep i =
  UserDep
    { userDepName = "TestUserDepName" <> showT i
    , userDepVersion = "TestUserDepVersion" <> showT i
    }

testUserDefinedAssertionMeta :: Int -> UserDefinedAssertionMeta
testUserDefinedAssertionMeta i =
  UserDefinedAssertionMeta
    { assertedName = "TestDepName" <> showT i
    , assertedVersion = "TestDepVersion" <> showT i
    , assertedLicense = "TestDepLicense" <> showT i
    , assertedDescription = Just $ "TestDepDescription" <> showT i
    , assertedUrl = Just $ "TestDepUrl" <> showT i
    }

testUserDefinedDep :: Int -> SourceUserDefDep
testUserDefinedDep i =
  SourceUserDefDep
    { srcUserDepName = "TestDepName" <> showT i
    , srcUserDepVersion = "TestDepVersion" <> showT i
    , srcUserDepLicense = "TestDepLicense" <> showT i
    , srcUserDepDescription = Just $ "TestDepDescription" <> showT i
    , srcUserDepHomepage = Just $ "TestDepUrl" <> showT i
    , srcUserDepOrigin = Nothing
    }

testTopLevelLocator :: Int -> Locator
testTopLevelLocator i =
  Locator
    { locatorFetcher = depTypeToFetcher CustomType
    , locatorProject = "TestProject" <> showT i
    , locatorRevision = "TestRevision" <> showT i
    }

testLocator :: Int -> Locator
testLocator i =
  Locator
    { locatorFetcher = "TestFetcher" <> showT i
    , locatorProject = "TestProject" <> showT i
    , locatorRevision = "TestRevision" <> showT i
    }
