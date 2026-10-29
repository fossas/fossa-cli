{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Integration test that materializes the checked-in multi-project fixture
-- tree ("Analysis.Fixtures.MultiProjectTree") under a temp dir and drives the
-- real per-strategy discovery visitors through the shared-walk wiring
-- ("Discovery.WalkStream" + "Discovery.WalkVisitor" +
-- "Discovery.Simple.cachedDiscover"), mirroring "App.Fossa.Analyze.runAnalyzers".
--
-- Only strategies that export a per-directory 'discoverVisitor' can be driven
-- this way.  Of the fixture's ten ecosystems, Maven ("Strategy.Maven", via
-- 'Discovery.Simple.simpleDiscover') and Npm ("Strategy.Node", via 'discover')
-- have no visitor, so they are not part of the shared walk; the eight
-- drivable strategies (Mix, Fpm, Leiningen, Gomodules, Composer, Cocoapods,
-- Conda, Cabal) are exercised here.  Each visitor accumulates its own raw
-- projects into a private sink and is finalized exactly like runAnalyzers.
--
-- A second describe covers the two self-discovering strategies, Maven and Npm,
-- which have no 'discoverVisitor': they are run directly through their own
-- 'Strategy.Maven.discover' / 'Strategy.Node.discover' entry points on the same
-- materialized tree under the analysis carrier stack, and a final top-level
-- case asserts all eleven projects across both mechanisms.
module Analysis.SharedWalkDiscoverySpec (
  spec,
) where

import Analysis.FixtureUtils (FixtureEnvironment (LocalEnvironment), testRunner, withResult)
import Analysis.Fixtures.MultiProjectTree (multiProjectTree)
import Control.Carrier.Diagnostics (DiagnosticsC, runDiagnostics)
import Control.Carrier.Reader (ReaderC, ask, runReader)
import Control.Carrier.Stack (StackC, runStack)
import Control.Effect.Diagnostics (Diagnostics, warnThenRecover)
import Control.Effect.Lift (Has, Lift, sendIO)
import Data.Foldable (traverse_)
import Data.Functor (void, ($>))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Set qualified as Set
import Data.String.Conversion (toString)
import Data.Text (Text)
import Diag.Result (Result (Failure, Success), renderFailure)
import Discovery.Filters (AllFilters)
import Discovery.Simple (cachedDiscover)
import Discovery.Walk (WalkStep (..))
import Discovery.WalkStream (walkStream)
import Discovery.WalkVisitor (runVisitors)
import Effect.ReadFS (ReadFSIOC, runReadFSIO)
import Path
import Path.IO qualified as PIO
import Strategy.Cocoapods qualified as Cocoapods
import Strategy.Composer qualified as Composer
import Strategy.Conda qualified as Conda
import Strategy.Fpm qualified as Fpm
import Strategy.Gomodules qualified as Gomodules
import Strategy.Haskell.Cabal qualified as Cabal
import Strategy.Leiningen qualified as Leiningen
import Strategy.Maven qualified as Maven
import Strategy.Mix qualified as Mix
import Strategy.Node qualified as Node
import Streaming.Prelude qualified as S
import System.Directory (getTemporaryDirectory)
import System.Random (randomIO)
import Test.Hspec
import Text.Printf (printf)
import Type.Operator (type ($))
import Types (DiscoveredProject (projectPath, projectType), DiscoveredProjectType (..), projectTypeToText)

-- | The effect stack the shared walk and the cached finalizers run in: the
-- ReadFS backend, the Diagnostics carrier (so 'warnThenRecover' in
-- 'accumulate' works), and the AllFilters reader.  The base monad is IO, which
-- satisfies 'Lift IO' automatically.
type SharedWalkC m =
  ReadFSIOC
    $ DiagnosticsC
    $ ReaderC AllFilters
    $ StackC m

-- | Interpret a "SharedWalkC" computation down to IO, extracting the success
-- value and turning any diagnostic failure into an IO error.
runSharedWalk :: SharedWalkC IO a -> IO a
runSharedWalk act =
  runStack (runReader (mempty :: AllFilters) (runDiagnostics (runReadFSIO act))) >>= extractResult

extractResult :: Result a -> IO a
extractResult res =
  case res of
    Success _ a -> pure a
    Failure ws eg -> fail (show (renderFailure ws eg "shared-walk discovery failed"))

-- | One project as the walk reports it: the strategy's text label plus the
-- absolute directory containing its manifest.
type Entry = (Text, Path Abs Dir)

toEntries :: DiscoveredProject a -> [Entry]
toEntries dp = [(projectTypeToText (projectType dp), projectPath dp)]

-- | Accumulate a strategy's per-directory visitor output into its own sink,
-- swallowing any error so one failing visitor contributes nothing (as in
-- runAnalyzers).
accumulate ::
  (Has Diagnostics sig m, Has (Lift IO) sig m) =>
  IORef [a] ->
  m ([a], WalkStep) ->
  m ((), WalkStep)
accumulate sink action = do
  found <- warnThenRecover @Text "Error discovering projects for strategy" action
  case found of
    Nothing -> pure ((), WalkContinue)
    Just (projects, step) -> sendIO (modifyIORef' sink (<> projects)) $> ((), step)

-- | Drive every drivable strategy's visitor over the shared walk rooted at
-- @tmpDir@, then finalize each sink with 'cachedDiscover'.  Returns the
-- discovered entries across all eight strategies.
collectWalk :: Path Abs Dir -> SharedWalkC IO [Entry]
collectWalk tmpDir = do
  filters <- ask

  mixSink <- sendIO (newIORef [])
  fpmSink <- sendIO (newIORef [])
  leinSink <- sendIO (newIORef [])
  goSink <- sendIO (newIORef [])
  composerSink <- sendIO (newIORef [])
  cocoapodsSink <- sendIO (newIORef [])
  condaSink <- sendIO (newIORef [])
  cabalSink <- sendIO (newIORef [])

  let visitMix dir sub files = accumulate mixSink (Mix.discoverVisitor dir sub files)
      visitFpm dir sub files = accumulate fpmSink (Fpm.discoverVisitor dir sub files)
      visitLein dir sub files = accumulate leinSink (Leiningen.discoverVisitor dir sub files)
      visitGo dir sub files = accumulate goSink (Gomodules.discoverVisitor dir sub files)
      visitComposer dir sub files = accumulate composerSink (Composer.discoverVisitor dir sub files)
      visitCocoapods dir sub files = accumulate cocoapodsSink (Cocoapods.discoverVisitor dir sub files)
      visitConda dir sub files = accumulate condaSink (Conda.discoverVisitor dir sub files)
      visitCabal dir sub files = accumulate cabalSink (Cabal.discoverVisitor dir sub files)

  -- One shared walk feeds every visitor; each accumulates into its own sink.
  -- The per-visitor WalkStep decisions stay independent, so Conda's or Cabal's
  -- 'WalkSkipAll' can never starve another strategy.
  void $
    S.effects $
      runVisitors
        [ visitMix
        , visitFpm
        , visitLein
        , visitGo
        , visitComposer
        , visitCocoapods
        , visitConda
        , visitCabal
        ]
        (walkStream filters tmpDir)

  do
    mixP <- cachedDiscover MixProjectType Mix.mkProject (sendIO $ readIORef mixSink)
    fpmP <- cachedDiscover FpmProjectType Fpm.mkProject (sendIO $ readIORef fpmSink)
    leinP <- cachedDiscover LeiningenProjectType Leiningen.mkProject (sendIO $ readIORef leinSink)
    goP <- cachedDiscover GomodProjectType Gomodules.mkProject (sendIO $ readIORef goSink)
    composerP <- cachedDiscover ComposerProjectType Composer.mkProject (sendIO $ readIORef composerSink)
    cocoapodsP <- cachedDiscover CocoapodsProjectType Cocoapods.mkProject (sendIO $ readIORef cocoapodsSink)
    condaP <- cachedDiscover CondaProjectType Conda.mkProject (sendIO $ readIORef condaSink)
    cabalP <- cachedDiscover CabalProjectType Cabal.mkProject (sendIO $ readIORef cabalSink)
    pure $
      concatMap toEntries mixP
        <> concatMap toEntries fpmP
        <> concatMap toEntries leinP
        <> concatMap toEntries goP
        <> concatMap toEntries composerP
        <> concatMap toEntries cocoapodsP
        <> concatMap toEntries condaP
        <> concatMap toEntries cabalP

-- | The exact set of entries the eight drivable strategies must discover, each
-- pinned to the absolute directory containing its manifest.  Computed relative
-- to @tmpDir@ so the assertion is independent of the (non-deterministic) temp
-- path.
expectedEntries :: Path Abs Dir -> [Entry]
expectedEntries tmp =
  [ ("mix", tmp </> [reldir|apps|])
  , ("fpm", tmp </> [reldir|apps|])
  , ("leiningen", tmp </> [reldir|deep/l1/l2/l3/leaf|])
  , ("gomod", tmp </> [reldir|libs/go-app|])
  , ("gomod", tmp </> [reldir|services/internal/worker|])
  , ("composer", tmp </> [reldir|composer|])
  , ("cocoapods", tmp </> [reldir|cocoapods|])
  , ("conda", tmp </> [reldir|conda|])
  , ("cabal", tmp </> [reldir|cabal|])
  ]

-- | Materialize the checked-in fixture tree under @tmp@, creating every parent
-- directory and writing each file's contents.
materialize :: Path Abs Dir -> IO ()
materialize tmp = traverse_ writeEntry multiProjectTree
  where
    writeEntry :: (Path Rel File, Text) -> IO ()
    writeEntry (rel, contents) = do
      let absPath = tmp </> rel
      PIO.createDirIfMissing True (parent absPath)
      writeFile (toFilePath absPath) (toString contents)

-- | Run discovery over a freshly materialized tree.
runDiscovery :: Path Abs Dir -> IO [Entry]
runDiscovery tmp = do
  materialize tmp
  runSharedWalk (collectWalk tmp)

-- | Drive the self-discovering strategies ("Strategy.Maven" and
-- "Strategy.Node") directly through their own 'discover' entry points rather
-- than through the shared-walk visitor wiring they do not export.  These
-- strategies walk the tree themselves, so each is run against the same
-- materialized @tmp@ root under the analysis carrier stack
-- ("Analysis.FixtureUtils.testRunner"), which supplies the ReadFS backend,
-- Diagnostics, Logger, and AllFilters reader each 'discover' requires.
runMavenDiscover :: Path Abs Dir -> IO [Entry]
runMavenDiscover tmp = do
  materialize tmp
  res <- testRunner (Maven.discover tmp) LocalEnvironment
  withResult res $ \_ projects -> pure (concatMap toEntries projects)

runNodeDiscover :: Path Abs Dir -> IO [Entry]
runNodeDiscover tmp = do
  materialize tmp
  res <- testRunner (Node.discover tmp) LocalEnvironment
  withResult res $ \_ projects -> pure (concatMap toEntries projects)

-- | The entries the two self-discovering strategies must return, pinned to the
-- absolute directory containing each manifest.
expectedSelfDiscoverEntries :: Path Abs Dir -> [Entry]
expectedSelfDiscoverEntries tmp =
  [ ("maven", tmp)
  , ("npm", tmp </> [reldir|apps|])
  ]

-- | The full set of projects across both mechanisms: the shared-walk visitors
-- plus the two self-discovering strategies, each pinned to its manifest dir.
expectedFullEntries :: Path Abs Dir -> [Entry]
expectedFullEntries tmp = expectedEntries tmp <> expectedSelfDiscoverEntries tmp

-- | Create (and clean up) a uniquely named temp dir in the system temp dir.
withTempDir :: String -> (Path Abs Dir -> IO a) -> IO a
withTempDir label act = do
  sysTemp <- getTemporaryDirectory >>= parseAbsDir
  rand <- randomIO :: IO Word
  rel <- parseRelDir (label ++ printf "-%016x" rand)
  let tmp = sysTemp </> rel
  PIO.createDirIfMissing True tmp
  r <- act tmp
  PIO.removeDirRecur tmp
  pure r

spec :: Spec
spec =
  describe "Analysis.SharedWalkDiscovery" $ do
    it "discovers every shared-walk-discoverable fixture project at its manifest directory" $
      withTempDir "integration-SharedWalkDiscovery" $ \tmp -> do
        entries <- runDiscovery tmp
        Set.fromList entries `shouldBe` Set.fromList (expectedEntries tmp)

    it "finds all sibling projects sharing one subdirectory" $
      withTempDir "integration-SharedWalkDiscovery" $ \tmp -> do
        entries <- runDiscovery tmp
        let apps = tmp </> [reldir|apps|]
            inApps = Set.fromList (filter ((== apps) . snd) entries)
        inApps `shouldBe` Set.fromList [("mix", apps), ("fpm", apps)]

    it "finds a project nested behind empty directories" $
      withTempDir "integration-SharedWalkDiscovery" $ \tmp -> do
        entries <- runDiscovery tmp
        let leaf = tmp </> [reldir|deep/l1/l2/l3/leaf|]
        Set.member ("leiningen", leaf) (Set.fromList entries) `shouldBe` True

    it "finds same-ecosystem projects at different depths" $
      withTempDir "integration-SharedWalkDiscovery" $ \tmp -> do
        entries <- runDiscovery tmp
        let goSet = Set.fromList entries
            go1 = tmp </> [reldir|libs/go-app|]
            go2 = tmp </> [reldir|services/internal/worker|]
        Set.member ("gomod", go1) goSet `shouldBe` True
        Set.member ("gomod", go2) goSet `shouldBe` True

    describe "self-discovering strategies (Maven, Node)" $ do
      it "Maven.discover finds the root pom.xml project at the tree root" $
        withTempDir "integration-SelfDiscovering-Maven" $ \tmp -> do
          entries <- runMavenDiscover tmp
          Set.fromList entries `shouldBe` Set.fromList [("maven", tmp)]

      it "Node.discover finds the npm project under apps/" $
        withTempDir "integration-SelfDiscovering-Node" $ \tmp -> do
          entries <- runNodeDiscover tmp
          Set.fromList entries `shouldBe` Set.fromList [("npm", tmp </> [reldir|apps|])]

    it "discovers all 11 projects across both shared-walk and self-discovering mechanisms" $
      withTempDir "integration-FullSet" $ \tmp -> do
        walkEntries <- runDiscovery tmp
        mavenEntries <- runMavenDiscover tmp
        nodeEntries <- runNodeDiscover tmp
        Set.fromList (walkEntries <> mavenEntries <> nodeEntries)
          `shouldBe` Set.fromList (expectedFullEntries tmp)
