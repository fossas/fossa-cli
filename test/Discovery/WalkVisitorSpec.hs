{-# LANGUAGE TemplateHaskell #-}

module Discovery.WalkVisitorSpec (
  spec,
) where

import Control.Effect.Lift (sendIO)
import Data.Foldable (traverse_)
import Data.IORef qualified as IOR (modifyIORef, newIORef, readIORef)
import Data.List (sort)
import Data.Maybe (catMaybes, isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Discovery.Walk (WalkStep (..), findFileNamed, walk')
import Discovery.WalkStream (walkStreamUnfiltered)
import Discovery.WalkVisitor (runVisitor, runVisitors)
import Path
import Path.IO (createDir)
import Streaming (Of, Stream)
import Streaming.Prelude qualified as S
import Test.Effect
import Test.Hspec

spec :: Spec
spec =
  describe "Discovery.WalkVisitor" $ do
    it' "does not let one visitor's WalkSkipAll starve another on a shared walk" $
      withTempDir "test-Discovery-WalkVisitor-shared" $ \tmpDir -> do
        let web = tmpDir </> $(mkRelDir "web")
            webSrc = web </> $(mkRelDir "src")
        sendIO $ do
          traverse_ createDir [web, webSrc]
          writeFile (toFilePath (tmpDir </> $(mkRelFile "Cargo.toml"))) "[package]"
          writeFile (toFilePath (tmpDir </> $(mkRelFile "top.txt"))) "top"
          writeFile (toFilePath (web </> $(mkRelFile "package.json"))) "{}"
          writeFile (toFilePath (webSrc </> $(mkRelFile "app.js"))) ""

        -- Cargo-like: matches at the directory holding Cargo.toml and, like
        -- the old cargo walk, skips its entire subtree.  Node-like: matches
        -- at directories holding package.json and continues.
        entries <- collectPairs (runVisitors [cargoLike, nodeLike] (walkStreamUnfiltered tmpDir))
        let hits = concatMap (catMaybes . snd) entries
            cargoHits = filter ((== "cargo") . hitBy) hits
            nodeHits = filter ((== "node") . hitBy) hits
        -- Cargo-like collected exactly at the root (and its own WalkSkipAll
        -- kept that visit's result).
        cargoHits `shouldBe'` [Hit tmpDir "cargo"]
        -- Node-like still reached web/ although the other visitor skipped
        -- all below the root: per-visitor state must not swallow it.
        nodeHits `shouldBe'` [Hit web "node"]

    it' "still visits a pruned directory's siblings after WalkSkipAll" $
      withTempDir "test-Discovery-WalkVisitor-siblings" $ \tmpDir -> do
        let a = tmpDir </> $(mkRelDir "a")
            aSub = a </> $(mkRelDir "sub")
            b = tmpDir </> $(mkRelDir "b")
            bSub2 = b </> $(mkRelDir "sub2")
            c = tmpDir </> $(mkRelDir "c")
        sendIO $ do
          traverse_ createDir [a, aSub, b, bSub2, c]
          writeFile (toFilePath (a </> $(mkRelFile "marker.txt"))) ""
          writeFile (toFilePath (aSub </> $(mkRelFile "deep.txt"))) ""
          writeFile (toFilePath (b </> $(mkRelFile "note.txt"))) ""
          writeFile (toFilePath (bSub2 </> $(mkRelFile "deep.txt"))) ""
          writeFile (toFilePath (c </> $(mkRelFile "plain.txt"))) ""

        visited <- visitedDirs skipAllVisitor tmpDir
        let dirSet = Set.fromList visited
        -- The visitor ran at 'a' and kept that visit, then swallowed a/sub.
        Set.member tmpDir dirSet `shouldBe'` True
        Set.member a dirSet `shouldBe'` True
        Set.member aSub dirSet `shouldBe'` False
        -- Siblings of the pruned directory (and their subtrees) are visited.
        Set.member b dirSet `shouldBe'` True
        Set.member bSub2 dirSet `shouldBe'` True
        Set.member c dirSet `shouldBe'` True

    it' "WalkSkipSome swallows only the named subdirectories" $
      withTempDir "test-Discovery-WalkVisitor-skipSome" $ \tmpDir -> do
        let d = tmpDir </> $(mkRelDir "d")
            dSub = d </> $(mkRelDir "sub")
            dOther = d </> $(mkRelDir "other")
            dTwo = d </> $(mkRelDir "two")
        sendIO $ do
          traverse_ createDir [d, dSub, dOther, dTwo]
          writeFile (toFilePath (d </> $(mkRelFile "marker.txt"))) ""
          writeFile (toFilePath (dSub </> $(mkRelFile "inner.txt"))) ""
          writeFile (toFilePath (dOther </> $(mkRelFile "inner.txt"))) ""
          writeFile (toFilePath (dTwo </> $(mkRelFile "inner.txt"))) ""

        -- Skip 'sub'; the empty name is unparseable and must be dropped
        -- silently without affecting anything else.
        visited <- visitedDirs (skipSomeVisitor ["sub", ""]) tmpDir
        let dirSet = Set.fromList visited
        Set.member d dirSet `shouldBe'` True
        Set.member dSub dirSet `shouldBe'` False
        Set.member dOther dirSet `shouldBe'` True
        Set.member dTwo dirSet `shouldBe'` True

    it' "mirrors Discovery.Walk's WalkSkipSome dirname-parsing rule" $
      withTempDir "test-Discovery-WalkVisitor-skipSomeRef" $ \tmpDir -> do
        let d = tmpDir </> $(mkRelDir "d")
            dSub = d </> $(mkRelDir "sub")
            dOther = d </> $(mkRelDir "other")
        sendIO $ do
          traverse_ createDir [d, dSub, dOther]
          writeFile (toFilePath (d </> $(mkRelFile "marker.txt"))) ""
          writeFile (toFilePath (dSub </> $(mkRelFile "inner.txt"))) ""
          writeFile (toFilePath (dOther </> $(mkRelFile "inner.txt"))) ""

        -- A leading './' (like a name a strategy might emit) plus an
        -- unparseable empty name.
        let skipNames :: [Text] = ["./sub", ""]
            visit dir _subdirs files =
              if isJust (findFileNamed "marker.txt" files)
                then pure ([dir], WalkSkipSome skipNames)
                else pure ([dir], WalkContinue)
        reference <- walk' visit tmpDir
        actual <- visitedDirs (skipSomeVisitor skipNames) tmpDir
        -- The visitor state machine must visit exactly the directories the
        -- old per-strategy walk would have, whatever path's parsing rule
        -- does with these names.
        Set.fromList actual `shouldBe'` Set.fromList reference

    it' "WalkStop stops the visitor everywhere, later siblings included" $
      withTempDir "test-Discovery-WalkVisitor-stop" $ \tmpDir -> do
        let a = tmpDir </> $(mkRelDir "a")
            b = tmpDir </> $(mkRelDir "b")
            c = tmpDir </> $(mkRelDir "c")
        sendIO $ do
          traverse_ createDir [a, b, c]
          -- Every sibling holds the stop marker: whichever one the
          -- filesystem reports first stops the visitor, so the assertions
          -- below hold regardless of readdir order.
          writeFile (toFilePath (a </> $(mkRelFile "stop.txt"))) ""
          writeFile (toFilePath (b </> $(mkRelFile "stop.txt"))) ""
          writeFile (toFilePath (c </> $(mkRelFile "stop.txt"))) ""

        counterRef <- sendIO $ IOR.newIORef 0
        let visit dir _subdirs files = do
              sendIO $ IOR.modifyIORef counterRef (+ 1)
              if isJust (findFileNamed "stop.txt" files)
                then pure (Just (Hit dir "v"), WalkStop)
                else pure (Just (Hit dir "v"), WalkContinue)
        visited <- visitedDirs visit tmpDir

        count <- sendIO $ IOR.readIORef counterRef
        let dirSet = Set.fromList visited
        -- The walk visits the root, then whichever sibling is reported first
        -- stops the visitor: exactly two invocations, and at most one of
        -- a/b/c is ever visited (the later siblings are never reached).
        count `shouldBe'` 2
        length visited `shouldBe'` 2
        Set.member tmpDir dirSet `shouldBe'` True
        length (filter (`elem` visited) [a, b, c]) `shouldBe'` 1

    it' "swallowing means the visitor is never invoked (and yields one output per invocation)" $
      withTempDir "test-Discovery-WalkVisitor-notInvoked" $ \tmpDir -> do
        let a = tmpDir </> $(mkRelDir "a")
            aSub = a </> $(mkRelDir "sub")
        sendIO $ do
          traverse_ createDir [a, aSub]
          writeFile (toFilePath (a </> $(mkRelFile "marker.txt"))) ""
          writeFile (toFilePath (aSub </> $(mkRelFile "deep.txt"))) ""

        counterRef <- sendIO $ IOR.newIORef 0
        let visit dir _subdirs files = do
              sendIO $ IOR.modifyIORef counterRef (+ 1)
              if isJust (findFileNamed "marker.txt" files)
                then pure (Just (Hit dir "v"), WalkSkipAll)
                else pure (Just (Hit dir "v"), WalkContinue)
        entries <- collectPairs (runVisitor visit (walkStreamUnfiltered tmpDir))

        count <- sendIO $ IOR.readIORef counterRef
        let visited = map fst entries
        -- The visitor was invoked at the root and at 'a' only; its own
        -- WalkSkipAll swallowed a/sub.  One (dir, o) yield per invocation.
        count `shouldBe'` 2
        length entries `shouldBe'` count
        sort visited `shouldBe'` sort [tmpDir, a]

-- | A visitor that records every invocation and skips the whole subtree of
-- any directory holding @marker.txt@.
skipAllVisitor :: Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> EffectStack (Maybe Hit, WalkStep)
skipAllVisitor dir _subdirs files =
  if isJust (findFileNamed "marker.txt" files)
    then pure (Just (Hit dir "v"), WalkSkipAll)
    else pure (Just (Hit dir "v"), WalkContinue)

-- | A visitor that records every invocation; directories holding
-- @marker.txt@ skip the sub-directories named by @names@.
skipSomeVisitor :: [Text] -> Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> EffectStack (Maybe Hit, WalkStep)
skipSomeVisitor names dir _subdirs files =
  if isJust (findFileNamed "marker.txt" files)
    then pure (Just (Hit dir "v"), WalkSkipSome names)
    else pure (Just (Hit dir "v"), WalkContinue)

-- | Cargo-like: collect at directories holding Cargo.toml, then skip all.
cargoLike :: Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> EffectStack (Maybe Hit, WalkStep)
cargoLike dir _subdirs files =
  if isJust (findFileNamed "Cargo.toml" files)
    then pure (Just (Hit dir "cargo"), WalkSkipAll)
    else pure (Nothing, WalkContinue)

-- | Node-like: collect at directories holding package.json and continue.
nodeLike :: Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> EffectStack (Maybe Hit, WalkStep)
nodeLike dir _subdirs files =
  if isJust (findFileNamed "package.json" files)
    then pure (Just (Hit dir "node"), WalkContinue)
    else pure (Nothing, WalkContinue)

-- | A directory hit recorded by a test visitor.
data Hit = Hit
  { hitWhere :: Path Abs Dir
  -- ^ The directory the visitor matched at.
  , hitBy :: Text
  -- ^ Label of the visitor that produced the hit.
  }
  deriving (Eq, Show)

-- | Collect which directories a collecting visitor was actually invoked at.
visitedDirs ::
  (Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> EffectStack (Maybe Hit, WalkStep)) ->
  Path Abs Dir ->
  EffectStack [Path Abs Dir]
visitedDirs visit root = do
  pairs <- collectPairs (runVisitor visit (walkStreamUnfiltered root))
  pure $ map fst pairs

-- | Drain a producer of @(dir, o)@ into a strict list.  Uses @uncons@ so we
-- do not need to import the @Of@/@Nil@ constructors.
collectPairs :: Stream (Of (Path Abs Dir, o)) EffectStack () -> EffectStack [(Path Abs Dir, o)]
collectPairs str = do
  m <- S.uncons str
  case m of
    Just (a, rest) -> (a :) <$> collectPairs rest
    Nothing -> pure []
