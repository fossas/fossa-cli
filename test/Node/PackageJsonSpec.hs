{-# LANGUAGE TemplateHaskell #-}

module Node.PackageJsonSpec (
  spec,
) where

import Algebra.Graph.AdjacencyMap qualified as AM
import App.Fossa.Analyze.LicenseAnalyze (LicenseAnalyzeProject (licenseAnalyzeProject))
import Data.Aeson (decode, encode)
import Data.Foldable (for_)
import Data.Glob (toGlob)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import DepTypes (
  DepEnvironment (EnvDevelopment, EnvProduction),
  DepType (NodeJSType),
  Dependency (..),
  VerConstraint (CCompatible),
 )
import GraphUtil (expectDeps, expectDirect, expectEdges)
import Hedgehog (Gen, forAll, tripping)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Path (mkRelFile, parseRelDir, toFilePath, (</>))
import Path.IO (getCurrentDir)
import Strategy.Node (NodeProject (NPM, NPMLock, Yarn))
import Strategy.Node.PackageJson (
  Manifest (Manifest, unManifest),
  PackageJson (..),
  PkgJsonGraph (PkgJsonGraph, jsonGraph, jsonLookup),
  PkgJsonLicense (LicenseObj, LicenseText),
  PkgJsonLicenseObj (PkgJsonLicenseObj, licenseType, licenseUrl),
  PkgJsonWorkspaces (PkgJsonWorkspaces),
  buildGraph,
 )
import Test.Effect (it', shouldBe', shouldMatchList')
import Test.Hspec (Spec, describe, it, runIO)
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)
import Types (
  License (License),
  LicenseResult (LicenseResult, licenseFile, licensesFound),
  LicenseType (LicenseURL, UnknownType),
 )

pkgJsonMock :: Maybe PkgJsonLicense -> Maybe [PkgJsonLicenseObj] -> PackageJson
pkgJsonMock license licenses =
  PackageJson
    { packageName = Nothing
    , packageVersion = Nothing
    , packageWorkspaces = PkgJsonWorkspaces []
    , packageDeps = Map.fromList [("packageOne", "^1.0.0")]
    , packageDevDeps = Map.fromList [("packageTwo", "^2.0.0")]
    , packageLicense = license
    , packageLicenses = licenses
    , packagePeerDeps = Map.empty
    }

mockInput :: PackageJson
mockInput = pkgJsonMock Nothing Nothing

packageOne :: Dependency
packageOne =
  Dependency
    { dependencyType = NodeJSType
    , dependencyName = "packageOne"
    , dependencyVersion = Just (CCompatible "^1.0.0")
    , dependencyLocations = []
    , dependencyEnvironments = Set.singleton EnvProduction
    , dependencyTags = Map.empty
    }

packageTwo :: Dependency
packageTwo =
  Dependency
    { dependencyType = NodeJSType
    , dependencyName = "packageTwo"
    , dependencyVersion = Just (CCompatible "^2.0.0")
    , dependencyLocations = []
    , dependencyEnvironments = Set.singleton EnvDevelopment
    , dependencyTags = Map.empty
    }

graphSpec :: Spec
graphSpec =
  describe "buildGraph" $
    it "should produce expected output" $ do
      let graph = buildGraph mockInput
      expectDeps [packageOne, packageTwo] graph
      expectDirect [packageOne, packageTwo] graph
      expectEdges [] graph

-- License Testing

mockUrl1 :: Text
mockUrl1 = "https://foo.com"

mockUrl2 :: Text
mockUrl2 = "https://bar.com"

mockLicensesObjs :: [PkgJsonLicenseObj]
mockLicensesObjs =
  [ PkgJsonLicenseObj{licenseType = "MIT", licenseUrl = mockUrl1}
  , PkgJsonLicenseObj{licenseType = "GPL", licenseUrl = mockUrl2}
  ]

mkTestLicenseResult :: FilePath -> [License] -> Types.LicenseResult
mkTestLicenseResult manifest ls =
  LicenseResult{licenseFile = manifest, licensesFound = ls}

singleLicenseResult :: FilePath -> LicenseResult
singleLicenseResult mockManifestFilePath =
  mkTestLicenseResult mockManifestFilePath [License Types.UnknownType "MIT"]

singleLicenseObjResult :: FilePath -> Text -> LicenseResult
singleLicenseObjResult mockManifestFilePath url =
  mkTestLicenseResult mockManifestFilePath [License LicenseURL url]

multiLicenseObjResult :: FilePath -> [Text] -> LicenseResult
multiLicenseObjResult mockManifestFilePath urls =
  mkTestLicenseResult mockManifestFilePath $ License LicenseURL <$> urls

mkMockPkgJsonGraph :: [(Manifest, PackageJson)] -> PkgJsonGraph
mkMockPkgJsonGraph packagePairs =
  PkgJsonGraph
    { -- jsonGraph isn't relevant to license detection
      jsonGraph = AM.empty
    , jsonLookup = Map.fromList packagePairs
    }

mkNodeProject :: (PkgJsonGraph -> NodeProject) -> [(Manifest, PackageJson)] -> NodeProject
mkNodeProject con = con . mkMockPkgJsonGraph

licenseSpec :: Spec
licenseSpec = do
  -- The actual paths here don't matter much, but this is a cross-platform way
  -- to get absolute filepaths.
  currDir <- runIO getCurrentDir
  let manifest = Manifest $ currDir </> $(mkRelFile "foo/package.json")
      manifestFilePath = toFilePath . unManifest $ manifest

  for_
    [ ("NPM", NPM)
    , ("Yarn", Yarn manifest)
    , ("NPMLock", NPMLock manifest)
    ]
    $ \(name, nodeConstr) ->
      describe ("license field detection from a " <> name <> " PackageJson") $ do
        let mockMIT = Just $ LicenseText "MIT"
        it' "It discovers a string license field" $ do
          let mockPackageJson = pkgJsonMock mockMIT Nothing
          foundLicenses <- licenseAnalyzeProject (mkNodeProject nodeConstr [(manifest, mockPackageJson)])
          foundLicenses `shouldBe'` [singleLicenseResult manifestFilePath]

        it' "It discovers an object license field" $ do
          let mockLicense = LicenseObj PkgJsonLicenseObj{licenseType = "MIT", licenseUrl = mockUrl1}
              mockPackageJson = pkgJsonMock (Just mockLicense) Nothing
          foundLicenses <-
            licenseAnalyzeProject
              . mkNodeProject nodeConstr
              $ [(manifest, mockPackageJson)]
          foundLicenses `shouldMatchList'` [singleLicenseObjResult manifestFilePath mockUrl1]

        it' "It discovers a licenses field with multiple license objects" $ do
          let mockPackageJson = pkgJsonMock Nothing (Just mockLicensesObjs)
          foundLicenses <-
            licenseAnalyzeProject
              . mkNodeProject nodeConstr
              $ [(manifest, mockPackageJson)]
          foundLicenses `shouldMatchList'` [multiLicenseObjResult manifestFilePath [mockUrl1, mockUrl2]]

        it' "It discovers licenses when both 'license' and 'licenses' are set." $ do
          let mockPackageJson = pkgJsonMock mockMIT (Just mockLicensesObjs)
              licenses =
                mkTestLicenseResult manifestFilePath $
                  License UnknownType "MIT" : map (License LicenseURL) [mockUrl1, mockUrl2]

          foundLicenses <- licenseAnalyzeProject . mkNodeProject nodeConstr $ [(manifest, mockPackageJson)]
          foundLicenses `shouldMatchList'` [licenses]

        it' "It discovers licenses when there are > 1 PackageJson's in a Node project" $ do
          let packageJson1 = pkgJsonMock Nothing (Just mockLicensesObjs)
              result1 =
                mkTestLicenseResult manifestFilePath
                  . map (License LicenseURL)
                  $ [mockUrl1, mockUrl2]
              manifest2 = Manifest $ currDir </> $(mkRelFile "bar/package.json")
              manifestFilePath2 = toFilePath . unManifest $ manifest2

              packageJson2 = pkgJsonMock (Just $ LicenseText "MIT") Nothing
              result2 = mkTestLicenseResult manifestFilePath2 [License UnknownType "MIT"]
              nodeProjects =
                NPM $
                  mkMockPkgJsonGraph
                    [ (manifest, packageJson1)
                    , (manifest2, packageJson2)
                    ]
          foundLicenses <- licenseAnalyzeProject nodeProjects
          foundLicenses `shouldMatchList'` [result1, result2]

-- PackageJson parsing Spec

alphaTextGen :: Int -> Gen Text
alphaTextGen n = Gen.text (Range.linear 1 n) Gen.alpha

alphaNumGen :: Gen Text
alphaNumGen = Gen.text (Range.linear 5 50) Gen.alphaNum

pkgJsonWorkSpaceGen :: Gen PkgJsonWorkspaces
pkgJsonWorkSpaceGen = PkgJsonWorkspaces <$> Gen.list (Range.linear 0 5) globGen
  where
    alphaString = Gen.string (Range.linear 1 5) Gen.alpha
    relDir = Gen.just (parseRelDir <$> alphaString)
    globGen = toGlob <$> relDir

licenseObjGen :: Gen PkgJsonLicenseObj
licenseObjGen = PkgJsonLicenseObj <$> alphaNumGen <*> alphaNumGen

licenseGen :: Gen PkgJsonLicense
licenseGen = Gen.choice [textGen, objGen]
  where
    textGen = LicenseText <$> alphaNumGen
    objGen = LicenseObj <$> licenseObjGen

packageJsonGen :: Gen PackageJson
packageJsonGen = do
  let keyValGen = Gen.list (Range.linear 0 10) ((,) <$> alphaTextGen 5 <*> alphaTextGen 5)
      keyValMap = Map.fromList <$> keyValGen
  name <- Gen.maybe $ alphaTextGen 20
  version <- Gen.maybe $ alphaTextGen 4
  workspaces <- pkgJsonWorkSpaceGen
  pkgDeps <- keyValMap
  devDeps <- keyValMap
  license <- Gen.maybe licenseGen
  licenses <- Gen.maybe $ Gen.list (Range.linear 0 5) licenseObjGen
  pkgPeerDeps <- keyValMap
  pure
    PackageJson
      { packageName = name
      , packageVersion = version
      , packageWorkspaces = workspaces
      , packageDeps = pkgDeps
      , packageDevDeps = devDeps
      , packageLicense = license
      , packageLicenses = licenses
      , packagePeerDeps = pkgPeerDeps
      }

pkgJsonParseSpec :: Spec
pkgJsonParseSpec =
  describe "Test parsing package.json files" $
    modifyMaxSuccess (const 20) $
      it "round-trips package.json structures" $
        hedgehog $
          do
            pkgJson <- forAll packageJsonGen
            tripping pkgJson encode decode

spec :: Spec
spec = do
  graphSpec
  licenseSpec
  pkgJsonParseSpec
