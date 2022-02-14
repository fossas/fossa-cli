{-# LANGUAGE TemplateHaskell #-}

module Node.PackageJsonSpec (
  spec,
) where

import Algebra.Graph.AdjacencyMap qualified as AM
import App.Pathfinder.Types (LicenseAnalyzeProject (licenseAnalyzeProject))
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
import Path (mkAbsFile, toFilePath)
import Strategy.Node (NodeProject (NPM))
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
import Test.Hspec (Spec, describe, fdescribe, it)
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

mockManifest :: Manifest
mockManifest = Manifest $(mkAbsFile "/usr/local/foo/package.json")

mockManifestFilePath :: FilePath
mockManifestFilePath = toFilePath . unManifest $ mockManifest

mkTestLicenseResult :: [License] -> Types.LicenseResult
mkTestLicenseResult ls = LicenseResult{licenseFile = mockManifestFilePath, licensesFound = ls}

singleLicenseResult :: LicenseResult
singleLicenseResult = mkTestLicenseResult [License Types.UnknownType "MIT"]

singleLicenseObjResult :: Text -> LicenseResult
singleLicenseObjResult url = mkTestLicenseResult [License LicenseURL url]

multiLicenseObjResult :: [Text] -> LicenseResult
multiLicenseObjResult urls = mkTestLicenseResult $ License LicenseURL <$> urls

mkMockPkgJsonGraph :: [(Manifest, PackageJson)] -> PkgJsonGraph
mkMockPkgJsonGraph packagePairs =
  PkgJsonGraph
    { -- jsonGraph isn't relevant to license detection
      jsonGraph = AM.empty
    , jsonLookup = Map.fromList packagePairs
    }

-- make an initial helper function to generate test NodeProjects
-- begin by doing a test of just the cases for the different types of node projects with
-- a fixed license. Then add the next different dimension (e.g. license text or license obj)
-- keep adding dimensions until you get to a comprehensive test.

-- remember that most of the fields in a NodeProject/PackageJson are not of interest to us.
licenseSpec :: Spec
licenseSpec =
  fdescribe "license field detection from a single PackageJson" $ do
    it' "It discovers a string license field" $ do
      let mockPackageJson = licenseMock (Just $ LicenseText "MIT") Nothing
      foundLicenses <- licenseAnalyzeProject (NPM $ mkMockPkgJsonGraph [(mockManifest, mockPackageJson)])
      foundLicenses `shouldBe'` [singleLicenseResult]

    it' "It discovers an object license field" $ do
      let url = "http://foo.com"
      let mockLicense = LicenseObj PkgJsonLicenseObj{licenseType = "MIT", licenseUrl = url}
      let mockPackageJson = licenseMock (Just mockLicense) Nothing
      foundLicenses <- licenseAnalyzeProject . NPM . mkMockPkgJsonGraph $ [(mockManifest, mockPackageJson)]
      foundLicenses `shouldMatchList'` [singleLicenseObjResult url]

    it' "It discovers a licenses field with multiple license objects" $ do
      let url1 = "https://foo.com"
      let url2 = "https://bar.com"
      let mockLicenses =
            [ PkgJsonLicenseObj{licenseType = "MIT", licenseUrl = url1}
            , PkgJsonLicenseObj{licenseType = "GPL", licenseUrl = url2}
            ]
      let mockPackageJson = licenseMock Nothing (Just mockLicenses)
      foundLicenses <- licenseAnalyzeProject . NPM . mkMockPkgJsonGraph $ [(mockManifest, mockPackageJson)]
      foundLicenses `shouldMatchList'` [multiLicenseObjResult [url1, url2]]

    it' "It discovers licenses when both 'license' and 'licenses' are set." $ do
      let url1 = "https://foo.com"
      let url2 = "https://bar.com"
      let mockLicenses =
            [ PkgJsonLicenseObj{licenseType = "MIT", licenseUrl = url1}
            , PkgJsonLicenseObj{licenseType = "GPL", licenseUrl = url2}
            ]
      let mockPackageJson = licenseMock (Just $ LicenseText "MIT") (Just mockLicenses)
      let licenses = mkTestLicenseResult $ (License UnknownType "MIT") : map (License LicenseURL) [url1, url2]
      foundLicenses <- licenseAnalyzeProject . NPM . mkMockPkgJsonGraph $ [(mockManifest, mockPackageJson)]
      foundLicenses `shouldMatchList'` [licenses]

spec :: Spec
spec = do
  graphSpec
  licenseSpec
