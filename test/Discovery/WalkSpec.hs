{-# LANGUAGE TemplateHaskell #-}

module Discovery.WalkSpec (
  spec,
) where

import Discovery.Walk
import Path
import Path.IO
import Test.Hspec
import Control.Carrier.Writer.Strict (runWriter, tell)
import Control.Carrier.Diagnostics (runDiagnostics)
import Control.Carrier.State.Strict (runState)
import Effect.ReadFS (runReadFSIO)
import Diag.Result
import Control.Carrier.Stack
import Control.Effect.State (get, put)
import Data.Foldable (traverse_)

spec :: Spec
spec = 
  describe "walk" $ do
    it "does a depth-first traversal" $ do
      withSystemTempDir "testXXX" $ \tmpDir -> do
        let dirs = map (tmpDir </>) [
              $(mkRelDir "a"),
              $(mkRelDir "a/b"),
              $(mkRelDir "c"),
              $(mkRelDir "c/d") ]
        traverse_ createDir dirs

        output :: Result [FilePath] <- (
          runStack
          . runDiagnostics
          . runReadFSIO
          . fmap fst
          . runWriter
          $ walk (\dir _ _ -> tell [toFilePath dir] >> pure WalkContinue) tmpDir)
        case output of
          f@(Failure _ _) ->
            fail $ "Walk failed: " ++ show f
          Success _ paths ->
            paths `shouldBe` map toFilePath (tmpDir : dirs)

    it "handles symlink loops" $ do
      withSystemTempDir "testXXX" $ \tmpDir -> do
        let dirs = map (tmpDir </>) [
              $(mkRelDir "lib"),
              $(mkRelDir "lib/sqlite"),
              $(mkRelDir "lib/pg")
              ]
        traverse_ createDir dirs
        createDirLink (tmpDir </> $(mkRelDir "lib")) (tmpDir </> $(mkRelDir "lib/pg/lib"))
        createDirLink (tmpDir </> $(mkRelDir "lib")) (tmpDir </> $(mkRelDir "lib/sqlite/lib"))

        output :: Result [FilePath] <- (
          runStack
          . runDiagnostics
          . runReadFSIO
          . fmap fst
          . runWriter
          . fmap snd
          . runState (0 :: Int)
          $ walk
            (\dir _ _ -> do
              iterations :: Int <- get
              if iterations < 10
                then do
                  put (iterations + 1)
                  tell [toFilePath dir]
                  pure WalkContinue
                else do
                  -- Message?
                  pure WalkStop)
            tmpDir)
        case output of
          f@(Failure _ _) ->
            fail $ "Walk failed: " ++ show f
          Success _ paths ->
            paths `shouldBe` map toFilePath (tmpDir : dirs)
  
