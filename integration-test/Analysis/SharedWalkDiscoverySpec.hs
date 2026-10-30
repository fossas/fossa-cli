{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Materializes the checked-in multi-project fixture tree
-- ("Analysis.Fixtures.MultiProjectTree") under a temp dir and drives discovery
-- over it, mirroring "App.Fossa.Analyze.runAnalyzers".
--
-- The shared-walk cases go through the production broadcast path: each
-- strategy's 'discoverVisitor' is wrapped in a
-- "Discovery.WalkBroadcast.accumulateConsumer" sink, all consumers are fanned
-- out through "Discovery.WalkBroadcast.withWalkBroadcast", and each sink is
-- finalized with "Discovery.Simple.cachedDiscover".  Only strategies that
-- export a 'discoverVisitor' can be driven this way: of the fixture's ten
-- ecosystems, Maven ("Strategy.Maven") and Npm ("Strategy.Node") have no
-- visitor, so the eight drivable strategies are exercised here.
--
-- A second describe runs the two self-discovering strategies directly through
-- their own 'discover' entry points on the same tree, under
-- "Analysis.FixtureUtils.testRunner".
module Analysis.SharedWalkDiscoverySpec (
  spec,
) where

import Analysis.FixtureUtils (FixtureEnvironment (LocalEnvironment), testRunner, withResult)
import Analysis.Fixtures.MultiProjectTree (multiProjectTree)
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, fatalText)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Reader (Reader, ask)
import Control.Exception (bracket)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.IORef (IORef, newIORef, readIORef)
import Data.Set qualified as Set
import Data.String.Conversion (toString)
import Data.Text (Text)
import Discovery.Filters (AllFilters)
import Discovery.Simple (cachedDiscover)
import Discovery.WalkBroadcast (accumulateConsumer, withWalkBroadcast)
import Effect.ReadFS (ReadFS)
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
import System.Directory (getTemporaryDirectory)
import System.Random (randomIO)
import Test.Hspec
import Text.Printf (printf)
import Types (DiscoveredProject (projectPath, projectType), DiscoveredProjectType (..), projectTypeToText)

-- | One project as the walk reports it: the strategy's text label plus the
-- absolute directory containing its manifest.
type Entry = (Text, Path Abs Dir)

toEntries :: DiscoveredProject a -> [Entry]
toEntries dp = [(projectTypeToText (projectType dp), projectPath dp)]

-- | Tool-attributed failure message recorded by accumulateConsumer on any
-- directory failure, mirroring 'App.Fossa.Analyze.prepare'.
msgFor :: DiscoveredProjectType -> Text
msgFor t = "Error discovering projects for " <> projectTypeToText t

-- | Finalize one accumulateConsumer sink like runAnalyzers: a recorded error
-- ('Left') fails the whole strategy via 'fatalText'; a healthy collection
-- ('Right') is handed to the strategy's finalizer.
finalizeSink :: (Has Diagnostics sig m, Has (Lift IO) sig m) => IORef (Either Text [a]) -> m [a]
finalizeSink sink = do
  state <- sendIO (readIORef sink)
  case state of
    Left e -> fatalText e
    Right projects -> pure projects

-- | Drive every drivable strategy's visitor over the production broadcast path
-- rooted at @tmpDir@, then finalize each sink with 'cachedDiscover'.  Returns
-- the discovered entries across all eight strategies.
collectWalk :: (Has (Lift IO) sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m, Has ReadFS sig m) => Path Abs Dir -> m [Entry]
collectWalk tmpDir = do
  filters <- ask

  -- Each accumulateConsumer sink starts healthy ('Right []'), matching the
  -- 'newIORef (Right ([] :: [raw]))' that 'App.Fossa.Analyze.prepare' builds
  -- per strategy.  The element type is left for inference: the wrapped visitor
  -- fixes it (most raw project types are not exported from their Strategy).
  mixSink <- sendIO (newIORef (Right ([])))
  fpmSink <- sendIO (newIORef (Right ([])))
  leinSink <- sendIO (newIORef (Right ([])))
  goSink <- sendIO (newIORef (Right ([])))
  composerSink <- sendIO (newIORef (Right ([])))
  cocoapodsSink <- sendIO (newIORef (Right ([])))
  condaSink <- sendIO (newIORef (Right ([])))
  cabalSink <- sendIO (newIORef (Right ([])))

  let -- One broadcast consumer per strategy, built exactly like production.
      consumers =
        [ accumulateConsumer MixProjectType (msgFor MixProjectType) mixSink Mix.discoverVisitor
        , accumulateConsumer FpmProjectType (msgFor FpmProjectType) fpmSink Fpm.discoverVisitor
        , accumulateConsumer LeiningenProjectType (msgFor LeiningenProjectType) leinSink Leiningen.discoverVisitor
        , accumulateConsumer GomodProjectType (msgFor GomodProjectType) goSink Gomodules.discoverVisitor
        , accumulateConsumer ComposerProjectType (msgFor ComposerProjectType) composerSink Composer.discoverVisitor
        , accumulateConsumer CocoapodsProjectType (msgFor CocoapodsProjectType) cocoapodsSink Cocoapods.discoverVisitor
        , accumulateConsumer CondaProjectType (msgFor CondaProjectType) condaSink Conda.discoverVisitor
        , accumulateConsumer CabalProjectType (msgFor CabalProjectType) cabalSink Cabal.discoverVisitor
        ]

  -- filters is mempty (supplied by testRunner); its gating/interplay is
  -- covered by WalkBroadcastSpec and WalkStreamSpec, so we do not re-pin it.
  withWalkBroadcast filters tmpDir consumers

  do
    mixP <- cachedDiscover MixProjectType Mix.mkProject (finalizeSink mixSink)
    fpmP <- cachedDiscover FpmProjectType Fpm.mkProject (finalizeSink fpmSink)
    leinP <- cachedDiscover LeiningenProjectType Leiningen.mkProject (finalizeSink leinSink)
    goP <- cachedDiscover GomodProjectType Gomodules.mkProject (finalizeSink goSink)
    composerP <- cachedDiscover ComposerProjectType Composer.mkProject (finalizeSink composerSink)
    cocoapodsP <- cachedDiscover CocoapodsProjectType Cocoapods.mkProject (finalizeSink cocoapodsSink)
    condaP <- cachedDiscover CondaProjectType Conda.mkProject (finalizeSink condaSink)
    cabalP <- cachedDiscover CabalProjectType Cabal.mkProject (finalizeSink cabalSink)
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
  , ("mix", tmp </> [reldir|apps/build|])
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
  res <- testRunner (collectWalk tmp) LocalEnvironment
  withResult res $ \_ projects -> pure projects

-- | Drive Maven and Node through their own 'discover' entry points (they have
-- no 'discoverVisitor') over the same materialized tree.
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

-- | Create a uniquely named temp dir and run @act@ inside it.  'bracket'
-- guarantees cleanup even when @act@ throws (e.g. a failing assertion).
withTempDir :: String -> (Path Abs Dir -> IO a) -> IO a
withTempDir label act = do
  sysTemp <- getTemporaryDirectory >>= parseAbsDir
  rand <- randomIO :: IO Word
  rel <- parseRelDir (label ++ printf "-%016x" rand)
  let tmp = sysTemp </> rel
  bracket (PIO.createDirIfMissing True tmp $> tmp) PIO.removeDirRecur act

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

    it "fpm's WalkSkipSome [\"build\"] swallows apps/build while mix still discovers it" $
      withTempDir "integration-SkipIsolation" $ \tmp -> do
        materialize tmp
        -- Fpm skips the "build" subtree; mix does not (it skips
        -- "deps"/"_build"), so apps/build/ (which holds both a fpm.toml and a
        -- mix.exs) is found by mix but swallowed by fpm's own skip decision.
        -- Only these two consumers run, isolating the pin to the skipping vs
        -- non-skipping strategies.
        fpmSink <- newIORef (Right ([]))
        mixSink <- newIORef (Right ([]))
        res <-
          testRunner
            ( do
                let fpmConsumer = accumulateConsumer FpmProjectType (msgFor FpmProjectType) fpmSink Fpm.discoverVisitor
                    mixConsumer = accumulateConsumer MixProjectType (msgFor MixProjectType) mixSink Mix.discoverVisitor
                -- filters mempty: gating is covered elsewhere (WalkBroadcastSpec).
                withWalkBroadcast (mempty :: AllFilters) tmp [fpmConsumer, mixConsumer]
                fpmFinal <- cachedDiscover FpmProjectType Fpm.mkProject (finalizeSink fpmSink)
                mixFinal <- cachedDiscover MixProjectType Mix.mkProject (finalizeSink mixSink)
                pure (fpmFinal, mixFinal)
            )
            LocalEnvironment
        (fpmFinal, mixFinal) <- withResult res $ \_ pair -> pure pair
        let fpmDirs = Set.fromList (map projectPath fpmFinal)
            mixDirs = Set.fromList (map projectPath mixFinal)
            apps = tmp </> [reldir|apps|]
            build = tmp </> [reldir|apps/build|]
        -- fpm finds apps/ but its WalkSkipSome ["build"] swallowed apps/build.
        Set.member apps fpmDirs `shouldBe` True
        Set.member build fpmDirs `shouldBe` False
        -- mix (which does not skip "build") finds both apps/ and apps/build.
        Set.member apps mixDirs `shouldBe` True
        Set.member build mixDirs `shouldBe` True

    it "discovers all 11 projects across both shared-walk and self-discovering mechanisms" $
      withTempDir "integration-FullSet" $ \tmp -> do
        walkEntries <- runDiscovery tmp
        mavenEntries <- runMavenDiscover tmp
        nodeEntries <- runNodeDiscover tmp
        let full = walkEntries <> mavenEntries <> nodeEntries
        Set.fromList full `shouldBe` Set.fromList (expectedFullEntries tmp)
        -- No visitor fired twice: the raw (pre-dedup) list has no duplicate
        -- entries, so Set.fromList above is not hiding a doubled project.
        length full `shouldBe` Set.size (Set.fromList full)
