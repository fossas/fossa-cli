{-# LANGUAGE RecordWildCards #-}

module App.Pathfinder.Scan (
  scanMain,
) where

import App.Pathfinder.Types (LicenseAnalyzeProject (licenseAnalyzeProject))
import Control.Carrier.AtomicCounter (AtomicCounter, runAtomicCounter)
import Control.Carrier.Debug (Debug, ignoreDebug)
import Control.Carrier.Diagnostics qualified as Diag
import Control.Carrier.Error.Either (Has)
import Control.Carrier.Finally (runFinally)
import Control.Carrier.Output.IO (Output, output, runOutput)
import Control.Carrier.Reader (runReader)
import Control.Carrier.Stack (runStack)
import Control.Carrier.StickyLogger (StickyLogger, logSticky', runStickyLogger)
import Control.Carrier.TaskPool (
  Progress (..),
  TaskPool,
  withTaskPool,
 )
import Control.Concurrent (getNumCapabilities)
import Control.Effect.Exception as Exc (Lift)
import Control.Effect.Lift (sendIO)
import Control.Effect.Reader (Reader)
import Control.Effect.Stack (Stack)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (
  KeyValue ((.=)),
  ToJSON (toJSON),
  encode,
  object,
 )
import Data.Bool (bool)
import Data.ByteString.Lazy qualified as BL
import Data.String.Conversion (ToText (toText))
import Data.Text (Text)
import Discovery.Filters (AllFilters, MavenScopeFilters)
import Discovery.Projects (withDiscoveredProjects)
import Effect.Exec (Exec, runExecIO)
import Effect.Logger (
  Color (Cyan, Green, Yellow),
  Logger,
  Pretty (pretty),
  Severity (SevDebug, SevInfo, SevWarn),
  annotate,
  color,
  withDefaultLogger,
 )
import Effect.ReadFS (ReadFS, runReadFSIO)
import Path (Abs, Dir, Path)
import Path.IO qualified as PIO
import Strategy.Bundler qualified as Bundler
import Strategy.Cargo qualified as Cargo
import Strategy.Cocoapods qualified as Cocaopods
import Strategy.Composer qualified as Composer
import Strategy.Maven qualified as Maven
import Strategy.Node qualified as Node
import Strategy.NuGet.Nuspec qualified as Nuspec
import System.Exit (die)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stderr, stdout)
import Types (
  DiscoveredProject (projectData, projectType),
  LicenseResult,
 )

scanMain :: Path Abs Dir -> Bool -> IO ()
scanMain basedir debug = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  exists <- PIO.doesDirExist basedir
  unless exists (die $ "ERROR: " <> show basedir <> " does not exist")

  runStack . withDefaultLogger (bool SevInfo SevDebug debug) $ scan basedir

runAll ::
  ( Has ReadFS sig m
  , Has Exec sig m
  , Has (Output ProjectLicenseScan) sig m
  , Has Logger sig m
  , Has TaskPool sig m
  , Has (Lift IO) sig m
  , Has AtomicCounter sig m
  , Has Debug sig m
  , Has Stack sig m
  , Has (Reader AllFilters) sig m
  , MonadIO m
  ) =>
  Path Abs Dir ->
  m ()
runAll basedir = do
  single Maven.discover
  single Nuspec.discover
  single Composer.discover
  single Cargo.discover
  single Node.discover
  single Bundler.discover
  single Cocaopods.discover
  where
    single f = withDiscoveredProjects f basedir runSingle

runSingle ::
  ( LicenseAnalyzeProject a
  , Has ReadFS sig m
  , Has Exec sig m
  , Has (Output ProjectLicenseScan) sig m
  , Has Logger sig m
  , Has (Lift IO) sig m
  , Has Stack sig m
  , MonadIO m
  ) =>
  DiscoveredProject a ->
  m ()
runSingle project = do
  licenseResult <- Diag.runDiagnosticsIO $ licenseAnalyzeProject (projectData project)
  Diag.withResult SevWarn SevWarn licenseResult (output . mkLicenseScan project)

scan ::
  ( Has (Lift IO) sig m
  , Has Logger sig m
  , Has Stack sig m
  , MonadIO m
  ) =>
  Path Abs Dir ->
  m ()
scan basedir = runFinally $ do
  sendIO $ PIO.setCurrentDir basedir
  capabilities <- sendIO getNumCapabilities

  (projectResults, ()) <-
    ignoreDebug
      . runOutput @ProjectLicenseScan
      . runStickyLogger SevInfo
      . runReadFSIO
      . runExecIO
      . runFinally
      . withTaskPool capabilities updateProgress
      . runAtomicCounter
      -- Pathfinder does not support filters.  When `fossa analyze` supports declared license scanning,
      -- we will need to support filters as well.
      . runReader (mempty :: AllFilters)
      . runReader (mempty :: MavenScopeFilters)
      $ runAll basedir

  sendIO (BL.putStr (encode projectResults))

data ProjectLicenseScan = ProjectLicenseScan
  { licenseStrategyType :: Text
  , licenseStrategyName :: Text
  , discoveredLicenses :: [LicenseResult]
  }
  deriving (Eq, Ord, Show)

instance ToJSON ProjectLicenseScan where
  toJSON ProjectLicenseScan{..} =
    object
      [ "type" .= licenseStrategyType
      , "name" .= licenseStrategyName
      , "files" .= discoveredLicenses
      ]

data CompletedLicenseScan = CompletedLicenseScan
  { completedLicenseName :: Text
  , completedLicenses :: [LicenseResult]
  }
  deriving (Eq, Ord, Show)

instance ToJSON CompletedLicenseScan where
  toJSON CompletedLicenseScan{..} =
    object
      [ "name" .= completedLicenseName
      , "licenseResults" .= completedLicenses
      ]

mkLicenseScan :: DiscoveredProject n -> [LicenseResult] -> ProjectLicenseScan
mkLicenseScan project licenses =
  ProjectLicenseScan
    { licenseStrategyType = toText (projectType project)
    , licenseStrategyName = toText (projectType project)
    , discoveredLicenses = licenses
    }

updateProgress :: Has StickyLogger sig m => Progress -> m ()
updateProgress Progress{..} =
  logSticky'
    ( "[ "
        <> annotate (color Cyan) (pretty pQueued)
        <> " Waiting / "
        <> annotate (color Yellow) (pretty pRunning)
        <> " Running / "
        <> annotate (color Green) (pretty pCompleted)
        <> " Completed"
        <> " ]"
    )
