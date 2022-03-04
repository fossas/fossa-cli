{-# LANGUAGE TemplateHaskell #-}

module Discovery.WalkSpec (
  spec,
) where

import Control.Carrier.State.Strict (runState)
import Control.Carrier.Writer.Strict (runWriter, tell)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift
import Control.Effect.State (get, put)
import Data.Foldable (traverse_)
import Discovery.Walk
import Effect.ReadFS
import Path
import Path.IO (createDir, createDirLink)
import Test.Effect
import Test.Hspec

spec :: Spec
spec =
  describe "walk" $ do
    it' "does a pre-order depth-first traversal" . withTempDir "test-Discovery-Walk" $ \tmpDir -> do
      let dirs =
            map
              (tmpDir </>)
              [ $(mkRelDir "a")
              , $(mkRelDir "a/b")
              , $(mkRelDir "c")
              , $(mkRelDir "c/d")
              ]
      sendIO $ traverse_ createDir dirs

      paths <- runWalk tmpDir
      paths `shouldBe'` map toFilePath (tmpDir : dirs)

    it' "handles symlink loops" . withTempDir "test-Discovery-Walk" $ \tmpDir -> do
      let dirs =
            map
              (tmpDir </>)
              [ $(mkRelDir "lib")
              , $(mkRelDir "lib/sqlite")
              , $(mkRelDir "lib/pg")
              ]
      sendIO $ do
        traverse_ createDir dirs
        -- Each of these symlinks forms a loop.
        createDirLink (tmpDir </> $(mkRelDir "lib")) (tmpDir </> $(mkRelDir "lib/pg/lib"))
        createDirLink (tmpDir </> $(mkRelDir "lib")) (tmpDir </> $(mkRelDir "lib/sqlite/lib"))

      paths <- runWalkWithCircuitBreaker 10 tmpDir
      paths `shouldBe'` map toFilePath (tmpDir : dirs)

runWalk ::
  (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [FilePath]
runWalk = runWalkWithCircuitBreaker 100

runWalkWithCircuitBreaker ::
  (Has ReadFS sig m, Has Diagnostics sig m) => Int -> Path Abs Dir -> m [FilePath]
runWalkWithCircuitBreaker maxIters startDir =
  do
    fmap fst
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
