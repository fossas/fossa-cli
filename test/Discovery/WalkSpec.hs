{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Discovery.WalkSpec (
  spec,
) where

import Control.Carrier.State.Strict (runState)
import Control.Carrier.Writer.Strict (runWriter, tell)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift
import Control.Effect.State (get, put)
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Map qualified as Map
import Discovery.Walk
import Effect.ReadFS
import Path
import Path.IO (createDir, createDirLink, getPermissions, setPermissions, emptyPermissions)
import Test.Effect
import Test.Hspec
import Discovery.Filters (AllFilters (AllFilters, excludeFilters), comboExclude)
import App.Fossa.Config.Analyze (AnalyzeCliOpts(analyzeExcludePaths))
import Control.Exception (finally)

spec :: Spec
spec =
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

      paths <- runWalkWithCircuitBreaker Nothing 10 tmpDir
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

    it' "handles excluded files without permission" . withTempDir "test-Discovery-Walk" $ \tmpDir -> do
      -- This example comes from the `shards` dependency manager.
      let dirs@[lib, foo, bar] =
            map
              (tmpDir </>)
              [ $(mkRelDir "lib")
              , $(mkRelDir "lib/foo")
              , $(mkRelDir "lib/bar")
              ]
      sendIO $ do
        traverse_ createDir dirs
        setPermissions bar emptyPermissions

      finally $ do
        fooPermissions <- liftIO $ getPermissions foo
        setPermissions bar fooPermissions

      let filters = AllFilters mempty $ comboExclude mempty [relativeTo tmpDir bar]

      paths <- runWalkWithFilters filters tmpDir
      pathsToTree paths
        `shouldBe'` dirTree
          [
            ( tmpDir
            , dirTree
                [
                  ( lib
                  , dirTree
                      [ (foo, dirTree []) ]
                  )
                ]
            )
          ]
        

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
runWalk = runWalkWithCircuitBreaker Nothing 100

runWalkWithFilters ::
  (Has ReadFS sig m, Has Diagnostics sig m) => AllFilters -> Path Abs Dir -> m [Path Abs Dir]
runWalkWithFilters filters = runWalkWithCircuitBreaker (Just filters) 100

runWalkWithCircuitBreaker ::
  (Has ReadFS sig m, Has Diagnostics sig m) => Maybe AllFilters -> Int -> Path Abs Dir -> m [Path Abs Dir]
runWalkWithCircuitBreaker filters maxIters startDir =
  do
    fmap fst
    . runWriter
    . fmap snd
    . runState (0 :: Int)
    $ walk filters 
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
