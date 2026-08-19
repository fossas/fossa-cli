{-# LANGUAGE TemplateHaskell #-}

module Python.SetuptoolsSpec (
  spec,
) where

import Control.Carrier.Diagnostics (runDiagnostics)
import Control.Carrier.Stack (runStack)
import Data.Function ((&))
import Effect.ReadFS (runReadFSIO)
import Graphing (directList)
import Path (File, Path, Rel, mkRelDir, mkRelFile)
import Path.IO (makeAbsolute)
import ResultUtil (assertOnSuccess)
import Strategy.Python.Setuptools (SetuptoolsProject (..), getDepsStatically)
import Test.Hspec (Spec, describe, it, runIO, shouldBe, shouldSatisfy)
import Types (DependencyResults (..))

reqTxtFile :: Path Rel File
reqTxtFile = $(mkRelFile "test/Python/testdata/req.txt")

spec :: Spec
spec = do
  reqTxt <- runIO $ makeAbsolute reqTxtFile
  dir <- runIO $ makeAbsolute $(mkRelDir "test/Python/testdata")
  let project =
        SetuptoolsProject
          { setuptoolsReqTxt = [reqTxt]
          , setuptoolsSetupPy = Nothing
          , setuptoolsSetupCfg = Nothing
          , setuptoolsDir = dir
          }
  result <-
    runIO $
      getDepsStatically project
        & runReadFSIO
        & runDiagnostics
        & runStack

  describe "Setuptools analysis of a requirements.txt-only project" $ do
    it "should not warn when setup.py is absent" $
      assertOnSuccess result $ \warnings _ -> length warnings `shouldBe` 0

    it "should report dependencies from requirements.txt" $
      assertOnSuccess result $ \_ depResults -> do
        dependencyManifestFiles depResults `shouldBe` [reqTxt]
        directList (dependencyGraph depResults) `shouldSatisfy` (not . null)
