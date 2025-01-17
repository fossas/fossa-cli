{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Discovery.WalkSpec (
  spec,
) where

import Control.Carrier.Reader (runReader)
import Control.Carrier.State.Strict (runState)
import Control.Carrier.Writer.Strict (runWriter, tell)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift
import Control.Effect.State (get, put)
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Map qualified as Map
import Discovery.Filters (AllFilters)
import Discovery.Walk
import Effect.ReadFS
import Path
import Path.IO (createDir, createDirLink, emptyPermissions, getPermissions, setPermissions)
import Test.Effect
import Test.Fixtures (excludePath)
import Test.Hspec

walkWithFilters'Spec :: Spec
walkWithFilters'Spec =
  describe "walkWithFilters'" $ do
    it' "ignores excluded paths" . withTempDir "test-Discovery-Walk-walkWithFilters'" $ \tmpDir -> do
      let dirs@[foo, bar, baz] =
            map
              (tmpDir </>)
              [ $(mkRelDir "foo")
              , $(mkRelDir "foo/bar")
              , $(mkRelDir "foo/baz")
              ]
      sendIO $ do
        traverse_ createDir dirs
        setPermissions bar emptyPermissions

      case stripProperPrefix tmpDir bar of
        Nothing -> expectationFailure' "Failed to get a relative path of foo/bar"
        Just relBar -> do
          let filters = excludePath relBar
          paths <- runWalkWithFilters' 100 filters tmpDir
          pathsToTree paths
            `shouldBe'` dirTree
              [
                ( tmpDir
                , dirTree
                    [
                      ( foo
                      , dirTree
                          [ (baz, dirTree [])
                          ]
                      )
                    ]
                )
              ]
      sendIO $ do
        fooPermissions <- getPermissions foo
        setPermissions bar fooPermissions

walkSpec :: Spec
walkSpec =
  describe "walk" $ do
    it' "does a pre-order depth-first traversal" . withTempDir "test-Discovery-Walk" $ \tmpDir -> do
      let dirs@[a, ab, c, cd] =
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
            ( tmpDir
            , dirTree
                [
                  ( a
                  , dirTree
                      [ (ab, dirTree [])
                      ]
                  )
                ,
                  ( c
                  , dirTree
                      [ (cd, dirTree [])
                      ]
                  )
                ]
            )
          ]

    it' "handles symlink loops" . withTempDir "test-Discovery-Walk" $ \tmpDir -> do
      -- This example comes from the `shards` dependency manager.
      let dirs@[lib, pg, sqlite] =
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
            ( tmpDir
            , dirTree
                [
                  ( lib
                  , dirTree
                      [ (pg, dirTree [])
                      , (sqlite, dirTree [])
                      ]
                  )
                ]
            )
          ]

spec :: Spec
spec = do
  walkSpec
  walkWithFilters'Spec

newtype DirTree = DirTree (Map (Path Abs Dir) DirTree) deriving (Show, Eq)

dirTree :: [(Path Abs Dir, DirTree)] -> DirTree
dirTree = DirTree . Map.fromList

-- | Creates a tree from a list of paths.
-- Files are walked in an arbitrary order within the same level, e.g. by inode
-- number.  We can make the test deterministic and by recreating the tree.
pathsToTree :: [Path Abs Dir] -> DirTree
pathsToTree [] = DirTree (Map.empty)
pathsToTree (path : paths) =
  let (subdirs, rest) = span (path `isProperPrefixOf`) paths
      subtree = pathsToTree subdirs
      DirTree siblingTrees = pathsToTree rest
   in DirTree $ Map.insert path subtree siblingTrees

runWalk ::
  (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [Path Abs Dir]
runWalk = runWalkWithCircuitBreaker 100

runWalkWithFilters' ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  ) =>
  Int ->
  AllFilters ->
  Path Abs Dir ->
  m [Path Abs Dir]
runWalkWithFilters' maxIters filters startDir =
  do
    fmap fst
    . runWriter
    . fmap snd
    . runState (0 :: Int)
    . runReader filters
    $ walkWithFilters'
      ( \dir _ _ -> do
          iterations :: Int <- get
          if iterations < maxIters
            then do
              put (iterations + 1)
              tell [dir]
              pure ((), WalkContinue)
            else do
              pure ((), WalkStop)
      )
      startDir

runWalkWithCircuitBreaker ::
  (Has ReadFS sig m, Has Diagnostics sig m) => Int -> Path Abs Dir -> m [Path Abs Dir]
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
              tell [dir]
              pure WalkContinue
            else do
              pure WalkStop
      )
      startDir
