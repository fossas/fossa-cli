{-# LANGUAGE TemplateHaskell #-}

module Node.PackageJsonSpec (
  spec,
) where

import Algebra.Graph.AdjacencyMap qualified as AM
import App.Pathfinder.Types (LicenseAnalyzeProject (licenseAnalyzeProject))
import Data.Foldable (for_)
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
import Path (mkRelFile, toFilePath, (</>))
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
import Types (License (License), LicenseResult (LicenseResult, licenseFile, licensesFound), LicenseType (LicenseURL, UnknownType))

mockInput :: PackageJson
mockInput =
  PackageJson
    { packageName = Nothing
    , packageVersion = Nothing
    , packageWorkspaces = PkgJsonWorkspaces []
    , packageDeps = Map.fromList [("packageOne", "^1.0.0")]
    , packageDevDeps = Map.fromList [("packageTwo", "^2.0.0")]
    , packageLicense = Nothing
    , packageLicenses = Nothing
    }

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
  describe "buildGraph" $ do
    it "should produce expected output" $ do
      let graph = buildGraph mockInput
      expectDeps [packageOne, packageTwo] graph
      expectDirect [packageOne, packageTwo] graph
      expectEdges [] graph

-- License Testing

licenseMock :: Maybe PkgJsonLicense -> Maybe [PkgJsonLicenseObj] -> PackageJson
licenseMock license licenses =
  PackageJson
    { packageName = Nothing
    , packageVersion = Nothing
    , packageWorkspaces = PkgJsonWorkspaces []
    , packageDeps = Map.fromList [("packageOne", "^1.0.0")]
    , packageDevDeps = Map.fromList [("packageTwo", "^2.0.0")]
    , packageLicense = license
    , packageLicenses = licenses
    }

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
mkTestLicenseResult manifest ls = LicenseResult{licenseFile = manifest, licensesFound = ls}

singleLicenseResult :: FilePath -> LicenseResult
singleLicenseResult mockManifestFilePath = mkTestLicenseResult mockManifestFilePath [License Types.UnknownType "MIT"]

singleLicenseObjResult :: FilePath -> Text -> LicenseResult
singleLicenseObjResult mockManifestFilePath url = mkTestLicenseResult mockManifestFilePath [License LicenseURL url]

multiLicenseObjResult :: FilePath -> [Text] -> LicenseResult
multiLicenseObjResult mockManifestFilePath urls = mkTestLicenseResult mockManifestFilePath $ License LicenseURL <$> urls

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
  let mockManifest = Manifest $ currDir </> $(mkRelFile "foo/package.json")
      mockManifestFilePath = toFilePath . unManifest $ mockManifest

  for_ [("NPM", NPM), ("Yarn", Yarn mockManifest), ("NPMLock", NPMLock mockManifest)] $ \(name, nodeConstr) ->
    describe ("license field detection from a " <> name <> " PackageJson") $ do
      let mockMIT = Just $ LicenseText "MIT"
      it' "It discovers a string license field" $ do
        let mockPackageJson = licenseMock mockMIT Nothing
        foundLicenses <- licenseAnalyzeProject (mkNodeProject nodeConstr [(mockManifest, mockPackageJson)])
        foundLicenses `shouldBe'` [singleLicenseResult mockManifestFilePath]

      it' "It discovers an object license field" $ do
        let mockLicense = LicenseObj PkgJsonLicenseObj{licenseType = "MIT", licenseUrl = mockUrl1}
            mockPackageJson = licenseMock (Just mockLicense) Nothing
        foundLicenses <- licenseAnalyzeProject . mkNodeProject nodeConstr $ [(mockManifest, mockPackageJson)]
        foundLicenses `shouldMatchList'` [singleLicenseObjResult mockManifestFilePath mockUrl1]

      it' "It discovers a licenses field with multiple license objects" $ do
        let mockPackageJson = licenseMock Nothing (Just mockLicensesObjs)
        foundLicenses <- licenseAnalyzeProject . mkNodeProject nodeConstr $ [(mockManifest, mockPackageJson)]
        foundLicenses `shouldMatchList'` [multiLicenseObjResult mockManifestFilePath [mockUrl1, mockUrl2]]

      it' "It discovers licenses when both 'license' and 'licenses' are set." $ do
        let mockPackageJson = licenseMock mockMIT (Just mockLicensesObjs)
            licenses = mkTestLicenseResult mockManifestFilePath $ (License UnknownType "MIT") : map (License LicenseURL) [mockUrl1, mockUrl2]
        foundLicenses <- licenseAnalyzeProject . mkNodeProject nodeConstr $ [(mockManifest, mockPackageJson)]
        foundLicenses `shouldMatchList'` [licenses]

      it' "It discovers licenses when there are > 1 PackageJson's in a Node project" $ do
        let mockPackageJson1 = licenseMock Nothing (Just mockLicensesObjs)
            mockResult1 = mkTestLicenseResult mockManifestFilePath . map (License LicenseURL) $ [mockUrl1, mockUrl2]
            mockManifest2 = Manifest $ currDir </> $(mkRelFile "bar/package.json")
            mockManifestFilePath2 = toFilePath . unManifest $ mockManifest2

            mockPackageJson2 = licenseMock (Just $ LicenseText "MIT") Nothing
            mockResult2 = mkTestLicenseResult mockManifestFilePath2 [(License UnknownType "MIT")]
            nodeProjects =
              NPM $
                mkMockPkgJsonGraph
                  [ (mockManifest, mockPackageJson1)
                  , (mockManifest2, mockPackageJson2)
                  ]
        foundLicenses <- licenseAnalyzeProject nodeProjects
        foundLicenses `shouldMatchList'` [mockResult1, mockResult2]

spec :: Spec
spec = do
  graphSpec
  licenseSpec
