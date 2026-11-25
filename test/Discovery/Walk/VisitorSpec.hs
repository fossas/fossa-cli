{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Discovery.Walk.VisitorSpec (
  spec,
) where

import Control.Effect.Lift (sendIO)
import Data.Foldable (traverse_)
import Data.IORef qualified as IOR (modifyIORef, newIORef, readIORef)
import Data.List (sort)
import Data.Maybe (catMaybes, isJust)
import Data.Set qualified as Set
import Data.Text (Text)
import Discovery.Walk (WalkStep (..), findFileNamed, walk')
import Discovery.Walk.Stream (walkStreamUnfiltered)
import Discovery.Walk.Visitor (
  VisitorState (..),
  applyVisitorStep,
  initialVisitorState,
  isSwallowed,
  runVisitor,
  runVisitors,
 )
import Path
import Path.IO (createDir)
import Path.Internal (Path (..))
import Streaming (Of, Stream)
import Streaming.Prelude qualified as S
import Test.Effect
import Test.Fixtures (absDir)
import Test.Hspec

spec :: Spec
spec =
  describe "Discovery.Walk.Visitor" $ do
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

        counterRef <- sendIO $ IOR.newIORef (0 :: Int)
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

        counterRef <- sendIO $ IOR.newIORef (0 :: Int)
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

    -- Pure state-machine tests: the drivers only invoke visitors on
    -- non-swallowed listings, so the state machine itself is what decides
    -- which listings are swallowed.
    describe "visitor state machine" $ do
      let root = absDir
          a = root </> $(mkRelDir "a")
          aSub = a </> $(mkRelDir "sub")
          aDeeper = aSub </> $(mkRelDir "deeper")
          b = root </> $(mkRelDir "b")
          c = root </> $(mkRelDir "c")
          -- Every walk's first listing is the root: it is what tells the
          -- state machine the walk root's path form (see 'vsRoot').
          rootListed = applyVisitorStep WalkContinue root [a, b, c]

      it' "stops swallowing once the DFS escapes the skipped scope" $
        let st = applyVisitorStep WalkSkipAll a [aSub] (rootListed initialVisitorState)
         in do
              isSwallowed st aSub `shouldBe'` True
              isSwallowed st aDeeper `shouldBe'` True
              isSwallowed st b `shouldBe'` False
              -- At the sibling, a's decision is dead forever: dropped from
              -- the state, so the live frontier shrinks instead of growing.
              let st' = applyVisitorStep WalkContinue b [] st
              vsPruned st' `shouldBe'` []
              isSwallowed st' aSub `shouldBe'` False

      it' "WalkSkipSome swallows each named sibling regardless of descent order" $
        -- The visitor names ["b", "a"] at root, but the walk descends into
        -- "a" first: both subtrees must be swallowed, siblings outside both
        -- must not be.  This rules out any single-root design.
        let st = applyVisitorStep (WalkSkipSome ["b", "a"]) root [a, b, c] initialVisitorState
         in do
              isSwallowed st a `shouldBe'` True
              isSwallowed st aDeeper `shouldBe'` True
              isSwallowed st b `shouldBe'` True
              isSwallowed st c `shouldBe'` False

      it' "matches root-level skips in container walks, whose listings are root-relative" $
        -- The container walk roots at the raw sentinel Path "vfs-root", but
        -- IndexFileTree.lookupDir strips that prefix from its listings: the
        -- root's children arrive as "node_modules/", not
        -- "vfs-root/node_modules/".  Skip roots must therefore come from the
        -- listing itself, not from joining the names onto the scope — the
        -- joined form matches no listed child and silently disables
        -- root-level skips (Node's node_modules/, Maven's target/) for
        -- container layers.
        let st =
              applyVisitorStep
                (WalkSkipSome ["node_modules/"])
                (Path ("vfs-root" :: FilePath))
                [Path ("node_modules/" :: FilePath), Path ("app/" :: FilePath)]
                initialVisitorState
         in do
              isSwallowed st (Path ("node_modules/" :: FilePath)) `shouldBe'` True
              isSwallowed st (Path ("node_modules/checkouts/" :: FilePath)) `shouldBe'` True
              isSwallowed st (Path ("app/" :: FilePath)) `shouldBe'` False
              -- The decision stays live below the root: its scope's form need
              -- not relate to the listings'.
              let st' = applyVisitorStep WalkContinue (Path ("node_modules/checkouts/" :: FilePath)) [] st
              isSwallowed st' (Path ("node_modules/checkouts/pkg/" :: FilePath)) `shouldBe'` True

      it' "cannot act inside a swallowed subtree" $
        -- A visitor is never invoked inside a swallowed subtree (pinned by
        -- the driver tests above), so no new decision can be made there;
        -- every depth below the skipped root stays swallowed.  Applying a
        -- step deeper in the scope keeps the outer scope alive (no nesting
        -- can occur, but the state must still compare sensibly).
        let st = applyVisitorStep WalkSkipAll a [aSub] (rootListed initialVisitorState)
         in do
              isSwallowed st aDeeper `shouldBe'` True
              let st' = applyVisitorStep WalkSkipAll aSub [aDeeper] st
              vsPruned st' `shouldBe'` [(aSub, [aDeeper]), (a, [aSub])]
              isSwallowed st' (aDeeper </> $(mkRelDir "x")) `shouldBe'` True

      it' "a skip inside an unskipped child coexists with the ancestor's skip and dies at its sibling" $
        -- root/ skips node_modules; the walk descends into the unskipped
        -- root/vendor/ and skips vendor/cache there.  Both live decisions
        -- coexist: the deeper one swallows its subtree, the ancestor's keeps
        -- swallowing node_modules, and the unskipped sibling of cache stays
        -- visible.  Once the DFS reaches vendor/other, the cache decision is
        -- dead (dropped) but the ancestor's is not.
        let vendor = root </> $(mkRelDir "vendor")
            vCache = vendor </> $(mkRelDir "cache")
            vCacheInner = vCache </> $(mkRelDir "inner")
            vOther = vendor </> $(mkRelDir "other")
            nodeModules = root </> $(mkRelDir "node_modules")
            nodeModulesDeep = nodeModules </> $(mkRelDir "pkg")
         in do
              let st0 = applyVisitorStep (WalkSkipSome ["node_modules"]) root [nodeModules, vendor] initialVisitorState
                  st1 = applyVisitorStep (WalkSkipSome ["cache"]) vendor [vCache, vOther] st0
              vsPruned st1 `shouldBe'` [(vendor, [vCache]), (root, [nodeModules])]
              isSwallowed st1 vCacheInner `shouldBe'` True
              isSwallowed st1 nodeModulesDeep `shouldBe'` True
              isSwallowed st1 vOther `shouldBe'` False
              let st2 = applyVisitorStep WalkContinue vOther [] st1
              -- Still inside vendor's scope: the cache decision stays live
              -- (DFS may still descend into cache after other), so it is
              -- neither dropped nor forgettable.
              vsPruned st2 `shouldBe'` [(vendor, [vCache]), (root, [nodeModules])]
              isSwallowed st2 vCacheInner `shouldBe'` True
              isSwallowed st2 nodeModulesDeep `shouldBe'` True
              -- Once the DFS backtracks above vendor entirely, the cache
              -- decision is dead (dropped), but the ancestor's is not.
              let st3 = applyVisitorStep WalkContinue (root </> $(mkRelDir "src")) [] st2
              vsPruned st3 `shouldBe'` [(root, [nodeModules])]
              isSwallowed st3 vCacheInner `shouldBe'` False
              isSwallowed st3 nodeModulesDeep `shouldBe'` True

      it' "drops unparseable skip names silently" $
        let st = applyVisitorStep (WalkSkipSome ["", "sub"]) a [aSub] (rootListed initialVisitorState)
         in vsPruned st `shouldBe'` [(a, [aSub])]

      it' "WalkStop marks the visitor done" $
        let st = applyVisitorStep WalkStop a [] (rootListed initialVisitorState)
         in do
              vsDone st `shouldBe'` True
              vsPruned st `shouldBe'` []

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
