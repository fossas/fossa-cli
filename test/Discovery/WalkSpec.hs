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
import Data.List (isPrefixOf, stripPrefix)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
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
      pathsToTree paths
        `shouldBe'` dirTree
          [
            ( toFilePath tmpDir
            , dirTree
                [
                  ( "a/"
                  , dirTree
                      [ ("b/", dirTree [])
                      ]
                  )
                ,
                  ( "c/"
                  , dirTree
                      [ ("d/", dirTree [])
                      ]
                  )
                ]
            )
          ]

    it' "handles symlink loops" . withTempDir "test-Discovery-Walk" $ \tmpDir -> do
      -- This example comes from the `shards` dependency manager.
      let dirs =
            map
              (tmpDir </>)
              [ $(mkRelDir "lib")
              , $(mkRelDir "lib/pg")
              , $(mkRelDir "lib/sqlite")
              ]
      sendIO $ do
        traverse_ createDir dirs
        -- Each of these symlinks forms a loop.
        createDirLink (tmpDir </> $(mkRelDir "lib")) (tmpDir </> $(mkRelDir "lib/pg/lib"))
        createDirLink (tmpDir </> $(mkRelDir "lib")) (tmpDir </> $(mkRelDir "lib/sqlite/lib"))

      paths <- runWalkWithCircuitBreaker 10 tmpDir
      pathsToTree paths
        `shouldBe'` dirTree
          [
            ( toFilePath tmpDir
            , dirTree
                [
                  ( "lib/"
                  , dirTree
                      [ ("pg/", dirTree [])
                      , ("sqlite/", dirTree [])
                      ]
                  )
                ]
            )
          ]

newtype DirTree = DirTree (Map FilePath DirTree) deriving (Show, Eq)

dirTree :: [(FilePath, DirTree)] -> DirTree
dirTree = DirTree . Map.fromList

-- | Creates a tree from a list of paths.
-- Files are walked in an arbitrary order within the same level, e.g. by inode
-- number.  We can make the test deterministic and by recreating the tree.
pathsToTree :: [FilePath] -> DirTree
pathsToTree [] = DirTree (Map.empty)
pathsToTree (path : paths) =
  let (subdirs, rest) = span (path `isPrefixOf`) paths
      subtree = pathsToTree $ mapMaybe (stripPrefix path) subdirs
      DirTree siblingTrees = pathsToTree rest
   in DirTree $ Map.insert path subtree siblingTrees

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
