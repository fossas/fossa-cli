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
import Test.Effect (it', shouldBe')
import Test.Hspec (Spec, describe, fdescribe, it)
import Types (License (License, licenseValue), LicenseResult (LicenseResult, licenseFile, licensesFound), LicenseType (LicenseURL, UnknownType))

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

licenseMock :: PackageJson
licenseMock =
  PackageJson
    { packageName = Nothing
    , packageVersion = Nothing
    , packageWorkspaces = PkgJsonWorkspaces []
    , packageDeps = Map.fromList [("packageOne", "^1.0.0")]
    , packageDevDeps = Map.fromList [("packageTwo", "^2.0.0")]
    , packageLicense = Nothing
    , packageLicenses = Nothing
    }

mockManifest :: Manifest
mockManifest = Manifest $(mkAbsFile "/usr/local/foo/package.json")

mockManifestFilePath :: FilePath
mockManifestFilePath = toFilePath . unManifest $ mockManifest

-- 3-tuple of test name, the package json to analyze and a LicenseResult
type LicenseTestTriple = (String, PackageJson, [LicenseResult])

mkTestLicenseResult :: [License] -> Types.LicenseResult
mkTestLicenseResult ls = LicenseResult{licenseFile = mockManifestFilePath, licensesFound = ls}

singleLicense :: LicenseTestTriple
singleLicense =
  ("Single License", licenseMock{packageLicense = Just (LicenseText "MIT")}, [mkTestLicenseResult [License Types.UnknownType "MIT"]])

singleLicenseResult :: [LicenseResult]
singleLicenseResult = [mkTestLicenseResult [License Types.UnknownType "MIT"]]

singleLicenseObjResult :: Text -> [LicenseResult]
singleLicenseObjResult url = [mkTestLicenseResult [License LicenseURL url]]

mkMockPkgJsonGraph :: PkgJsonLicense -> PkgJsonGraph
mkMockPkgJsonGraph license =
  PkgJsonGraph
    { -- jsonGraph isn't relevant to license detection
      jsonGraph = AM.empty
    , jsonLookup = Map.fromList [(mockManifest, licenseMock{packageLicense = Just license})]
    }

-- make an initial helper function to generate test NodeProjects
-- begin by doing a test of just the cases for the different types of node projects with
-- a fixed license. Then add the next different dimension (e.g. license text or license obj)
-- keep adding dimensions until you get to a comprehensive test.

-- remember that most of the fields in a NodeProject/PackageJson are not of interest to us.
licenseSpec :: Spec
licenseSpec =
  fdescribe "license field generation from PackageJson" $ do
    it' "It discovers a string license field" $ do
      foundLicenses <- licenseAnalyzeProject (NPM . mkMockPkgJsonGraph $ LicenseText "MIT")
      foundLicenses `shouldBe'` singleLicenseResult

    it' "It discovers an object license field" $ do
      let url = "http://foo.com"
      foundLicenses <-
        licenseAnalyzeProject
          ( NPM . mkMockPkgJsonGraph $
              LicenseObj $
                PkgJsonLicenseObj
                  { licenseType = "MIT"
                  , licenseUrl = url
                  }
          )
      foundLicenses `shouldBe'` singleLicenseObjResult url

-- do license <- [Just (LicenseText "MIT")]
-- [it' ("It correctly detects a license field of " <> show license) $ do
--     foundLicenses <- licenseAnalyzeProject (licenseMock {packageLicense = license})
--     pure ()]
--foundLicenses `shouldBe` mkTestLicenseResult [(catMaybes [License UnknownType <$> license])]

-- forM_ licenseTests $
--   \(testCaseName, mock, result) ->
--     it' ("It generates licenses in the " <> testCaseName <> "case") $ do
--     foundLicenses <- licenseAnalyzeProject mock
--     foundLicenses `shouldBe'` result

spec :: Spec
spec = do
  graphSpec
  licenseSpec
