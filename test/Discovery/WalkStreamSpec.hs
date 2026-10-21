{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Discovery.WalkStreamSpec (
  spec,
) where

import Control.Carrier.Reader (runReader)
import Control.Effect.Lift (sendIO)
import Data.Foldable (traverse_)
import Data.List (sort)
import Data.Set (Set)
import Data.Set qualified as Set
import Discovery.Filters (AllFilters)
import Discovery.Walk
import Discovery.WalkStream
import Path
import Path.IO (createDir, createDirLink)
import Streaming (Of, Stream)
import Streaming.Prelude qualified as S
import Test.Effect
import Test.Fixtures (excludePath)
import Test.Hspec

spec :: Spec
spec =
  describe "Discovery.WalkStream" $ do
    it' "emits a listing for every directory the plain walk visits" $
      withTempDir "test-Discovery-WalkStream-basic" $ \tmpDir -> do
        let dirs@[a, ab, b, bc] =
              map
                (tmpDir </>)
                [ $(mkRelDir "a")
                , $(mkRelDir "a/b")
                , $(mkRelDir "b")
                , $(mkRelDir "b/c")
                ]
        sendIO $ do
          traverse_ createDir dirs
          writeFile (toFilePath (tmpDir </> $(mkRelFile "top.txt"))) "top"
          writeFile (toFilePath (a </> $(mkRelFile "a.txt"))) "a"
          writeFile (toFilePath (ab </> $(mkRelFile "ab.txt"))) "ab"
          writeFile (toFilePath (b </> $(mkRelFile "b.txt"))) "b"
          writeFile (toFilePath (bc </> $(mkRelFile "bc.txt"))) "bc"

        emitted <- collect (walkStreamUnfiltered tmpDir)
        reference <- collectWalk tmpDir

        normalizeSet emitted `shouldBe'` normalizeSet (map toListing reference)
        sendIO $ traverse_ (const (pure ())) [a, ab, b, bc]

    it' "prunes directories excluded by path filters" $
      withTempDir "test-Discovery-WalkStream-filter" $ \tmpDir -> do
        let dirs@[a, ab, abc, b] =
              map
                (tmpDir </>)
                [ $(mkRelDir "a")
                , $(mkRelDir "a/b")
                , $(mkRelDir "a/b/c")
                , $(mkRelDir "b")
                ]
        sendIO $ traverse_ createDir dirs

        let filters = excludePath $(mkRelDir "a/b")

        emitted <- collect (walkStream filters tmpDir)
        reference <- collectWalkFiltered filters tmpDir

        normalizeSet emitted `shouldBe'` normalizeSet (map toListing reference)
        let visited = Set.map dlDir (Set.fromList emitted)
        Set.member ab visited `shouldBe'` False
        -- The allowed ancestor 'a' and sibling 'b' must still be visited.
        Set.member a visited `shouldBe'` True
        Set.member b visited `shouldBe'` True
        sendIO $ traverse_ (const (pure ())) [ab, abc]

    it' "terminates and matches the walk under symlink loops" $
      withTempDir "test-Discovery-WalkStream-loops" $ \tmpDir -> do
        let dirs@[lib, _, _] =
              map
                (tmpDir </>)
                [ $(mkRelDir "lib")
                , $(mkRelDir "lib/pg")
                , $(mkRelDir "lib/sqlite")
                ]
        sendIO $ do
          traverse_ createDir dirs
          -- Each of these symlinks forms a loop.
          createDirLink lib (lib </> $(mkRelDir "pg/lib"))
          createDirLink lib (lib </> $(mkRelDir "sqlite/lib"))

        emitted <- collect (walkStreamUnfiltered tmpDir)
        reference <- collectWalk tmpDir

        normalizeSet emitted `shouldBe'` normalizeSet (map toListing reference)
        let visited = Set.map dlDir (Set.fromList emitted)
        -- Loop-back symlinks must never be descended into.
        Set.member (tmpDir </> $(mkRelDir "lib/pg/lib")) visited `shouldBe'` False
        Set.member (tmpDir </> $(mkRelDir "lib/sqlite/lib")) visited `shouldBe'` False

-- | Drain a producer into a strict list.  Uses @uncons@ so we do not need to
-- import the @Of@/@Nil@ constructors.
collect :: Stream (Of DirListing) EffectStack () -> EffectStack [DirListing]
collect str = do
  m <- S.uncons str
  case m of
    Just (a, rest) -> (a :) <$> collect rest
    Nothing -> pure []

-- | Reference: the list of @(dir, subdirs, files)@ visits performed by the
-- existing 'walk' with a consumer that always 'WalkContinue's.
collectWalk :: Path Abs Dir -> EffectStack [(Path Abs Dir, [Path Abs Dir], [Path Abs File])]
collectWalk =
  walk' (\dir subdirs files -> pure ([(dir, subdirs, files)], WalkContinue))

-- | Reference for the filtered case, running 'walkWithFilters'' under the
-- given filters.
collectWalkFiltered :: AllFilters -> Path Abs Dir -> EffectStack [(Path Abs Dir, [Path Abs Dir], [Path Abs File])]
collectWalkFiltered filters root =
  runReader filters (walkWithFilters' (\dir subdirs files -> pure ([(dir, subdirs, files)], WalkContinue)) root)

toListing :: (Path Abs Dir, [Path Abs Dir], [Path Abs File]) -> DirListing
toListing (d, s, f) = DirListing d s f

-- | Sort the (non-deterministically ordered) subdirs/files of every listing so
-- the comparison is order-independent, exactly like 'Discovery.WalkSpec'.
normalizeListing :: DirListing -> DirListing
normalizeListing dl = dl{dlSubdirs = sort (dlSubdirs dl), dlFiles = sort (dlFiles dl)}

normalizeSet :: [DirListing] -> Set DirListing
normalizeSet = Set.fromList . map normalizeListing
