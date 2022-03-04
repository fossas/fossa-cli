{-# LANGUAGE TemplateHaskell #-}

module Discovery.WalkSpec (
  spec,
) where

import Control.Carrier.Diagnostics (runDiagnostics)
import Control.Carrier.Stack
import Control.Carrier.State.Strict (runState)
import Control.Carrier.Writer.Strict (runWriter, tell)
import Control.Effect.State (get, put)
import Data.Foldable (traverse_)
import Diag.Result
import Discovery.Walk
import Effect.ReadFS (runReadFSIO)
import Path
import Path.IO
import Test.Hspec

spec :: Spec
spec =
  describe "walk" $ do
    it "does a pre-order depth-first traversal" . withSystemTempDir "test-Discovery-Walk-XXX" $ \tmpDir -> do
      let dirs =
            map
              (tmpDir </>)
              [ $(mkRelDir "a")
              , $(mkRelDir "a/b")
              , $(mkRelDir "c")
              , $(mkRelDir "c/d")
              ]
      traverse_ createDir dirs

      paths <- runWalk tmpDir
      paths `shouldBe` map toFilePath (tmpDir : dirs)

    it "handles symlink loops" . withSystemTempDir "test-Discovery-Walk-XXX" $ \tmpDir -> do
      let dirs =
            map
              (tmpDir </>)
              [ $(mkRelDir "lib")
              , $(mkRelDir "lib/sqlite")
              , $(mkRelDir "lib/pg")
              ]
      traverse_ createDir dirs

      -- Each of these symlinks forms a loop.
      createDirLink (tmpDir </> $(mkRelDir "lib")) (tmpDir </> $(mkRelDir "lib/pg/lib"))
      createDirLink (tmpDir </> $(mkRelDir "lib")) (tmpDir </> $(mkRelDir "lib/sqlite/lib"))

      paths <- runWalkWithCircuitBreaker 10 tmpDir
      paths `shouldBe` map toFilePath (tmpDir : dirs)

runWalk :: Path Abs Dir -> IO [FilePath]
runWalk = runWalkWithCircuitBreaker 100

runWalkWithCircuitBreaker :: Int -> Path Abs Dir -> IO [FilePath]
runWalkWithCircuitBreaker maxIters startDir = do
  output <-
    runStack
      . runDiagnostics
      . runReadFSIO
      . fmap fst
      . runWriter
      . fmap snd
      . runState (0 :: Int)
      $ walk
        ( \dir _ _ -> do
            iterations :: Int <- get
            if iterations < maxIters
              then do
                put (iterations + 1)
                tell [toFilePath dir]
                pure WalkContinue
              else do
                pure WalkStop
        )
        startDir
  case output of
    f@(Failure _ _) ->
      fail $ "Walk failed: " ++ show f
    Success _ paths ->
      return paths
