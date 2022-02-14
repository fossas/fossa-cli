{-# LANGUAGE TemplateHaskell #-}

module Node.PackageJsonSpec (
  spec,
) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import DepTypes (
  DepEnvironment (EnvDevelopment, EnvProduction),
  DepType (NodeJSType),
  Dependency (..),
  VerConstraint (CCompatible),
 )
import GraphUtil (expectDeps, expectDirect, expectEdges)
import Path (mkAbsFile, toFilePath)
import Strategy.Node.PackageJson (
  Manifest (Manifest, unManifest),
  PackageJson (..),
  PkgJsonLicense (LicenseText),
  PkgJsonLicenseObj (licenseType),
  PkgJsonWorkspaces (PkgJsonWorkspaces),
  buildGraph, PkgJsonGraph (PkgJsonGraph, jsonGraph, jsonLookup)
 )
import Test.Hspec (Spec, describe, it, shouldBe, fdescribe)
import Types (License (License, licenseValue), LicenseResult (LicenseResult, licenseFile, licensesFound), LicenseType (UnknownType))
import Control.Monad (forM_)
import App.Pathfinder.Types (LicenseAnalyzeProject(licenseAnalyzeProject))
import Test.Effect (shouldBe', it')
import Data.Maybe (catMaybes)
import qualified Algebra.Graph.AdjacencyMap as AM
import Strategy.Node (NodeProject(NPM))

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

mkTestLicenseResult :: [License] -> LicenseResult
mkTestLicenseResult ls = LicenseResult{licenseFile = mockManifestFilePath, licensesFound = ls}

singleLicense :: LicenseTestTriple
singleLicense =
  ("Single License", licenseMock{packageLicense = Just (LicenseText "MIT")}, [mkTestLicenseResult [License UnknownType "MIT"]])

licenseTests :: [LicenseTestTriple]
licenseTests =
  [singleLicense]

mkMockPkgJsonGraph :: PackageJson -> PkgJsonGraph
mkMockPkgJsonGraph pkg =
  PkgJsonGraph {
  -- jsonGraph isn't relevant to license detection
  jsonGraph = AM.empty
  , jsonLookup = Map.fromList [(mockManifest, pkg)]
  }

-- make an initial helper function to generate test NodeProjects
-- begin by doing a test of just the cases for the different types of node projects with
-- a fixed license. Then add the next different dimension (e.g. license text or license obj)
-- keep adding dimensions until you get to a comprehensive test.

-- remember that most of the fields in a NodeProject/PackageJson are not of interest to us.
licenseSpec :: Spec
licenseSpec =
  fdescribe "license field generation from PackageJson" $ do
  it' "It discovers a license field" $ do
    foundLicenses <- licenseAnalyzeProject (NPM $ mkMockPkgJsonGraph licenseMock {packageLicense = Just (LicenseText "MIT") })
    foundLicenses `shouldBe'` [mkTestLicenseResult [License UnknownType "MIT"]]

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
