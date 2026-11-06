{-# LANGUAGE TemplateHaskell #-}

module Swift.SwiftPMSpec (
  spec,
) where

import App.Types (Mode (NonStrict, Strict))
import Control.Carrier.Diagnostics (runDiagnostics)
import Control.Carrier.Reader (runReader)
import Control.Carrier.Stack (runStack)
import Data.Function ((&))
import Data.Text (Text)
import DepTypes (DepType (GitType), Dependency (..), VerConstraint (CEq))
import Diag.Result (Result)
import Effect.ReadFS (runReadFSIO)
import Graphing (directList, vertexList)
import Path (Abs, Dir, File, Path, mkRelDir, mkRelFile)
import Path.IO (makeAbsolute)
import ResultUtil (assertOnSuccess, expectFailure)
import Strategy.SwiftPM (SwiftPackageProject (..), SwiftProject (PackageProject), getDepsStatic)
import Test.Hspec (Spec, describe, it, runIO, shouldBe, shouldMatchList, shouldSatisfy)
import Types (DependencyResults (..), GraphBreadth (Partial))

-- | A package whose manifest the static parser rejects, next to a Package.resolved.
testdataDir :: Path Abs Dir -> Path Abs File -> Maybe (Path Abs File) -> SwiftProject
testdataDir dir manifest resolved = PackageProject $ SwiftPackageProject manifest dir resolved

analyze :: Mode -> SwiftProject -> IO (Result DependencyResults)
analyze mode project =
  getDepsStatic project
    & runReader mode
    & runReadFSIO
    & runDiagnostics
    & runStack

gitDep :: Text -> Text -> Dependency
gitDep name revision =
  Dependency
    { dependencyType = GitType
    , dependencyName = name
    , dependencyVersion = Just (CEq revision)
    , dependencyLocations = []
    , dependencyEnvironments = mempty
    , dependencyTags = mempty
    }

spec :: Spec
spec = do
  dir <- runIO $ makeAbsolute $(mkRelDir "test/Swift/testdata/lockfile-only")
  manifest <- runIO $ makeAbsolute $(mkRelFile "test/Swift/testdata/lockfile-only/Package.swift")
  resolved <- runIO $ makeAbsolute $(mkRelFile "test/Swift/testdata/lockfile-only/Package.resolved")

  describe "static analysis of a Swift package whose manifest cannot be parsed" $ do
    it "falls back to Package.resolved and reports every pin as a direct dependency" $ do
      result <- analyze NonStrict (testdataDir dir manifest (Just resolved))
      assertOnSuccess result $ \warnings depResults -> do
        warnings `shouldSatisfy` (not . null)
        dependencyGraphBreadth depResults `shouldBe` Partial
        dependencyManifestFiles depResults `shouldBe` [manifest]
        let pins =
              [ gitDep "https://github.com/apple/swift-algorithms.git" "5b7143f8e291dee0e14c118fd0212487f0b37af5"
              , gitDep "https://github.com/apple/swift-log.git" "e3da04078777e6a1b72ce19981fbd6539b0847d8"
              ]
        directList (dependencyGraph depResults) `shouldMatchList` pins
        vertexList (dependencyGraph depResults) `shouldMatchList` pins

    it "fails when there is no Package.resolved to fall back to" $ do
      result <- analyze NonStrict (testdataDir dir manifest Nothing)
      expectFailure result

    it "does not fall back in strict mode" $ do
      result <- analyze Strict (testdataDir dir manifest (Just resolved))
      expectFailure result
