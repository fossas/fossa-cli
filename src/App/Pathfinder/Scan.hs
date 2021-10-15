{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}

module App.Pathfinder.Scan (
  scanMain,
) where

import App.Pathfinder.Types (LicenseAnalyzeProject (licenseAnalyzeProject))
import Control.Carrier.AtomicCounter (AtomicCounter, runAtomicCounter)
import Control.Carrier.Debug (Debug, ignoreDebug)
import Control.Carrier.Diagnostics qualified as Diag
import Control.Carrier.Error.Either
import Control.Carrier.Finally
import Control.Carrier.Output.IO
import Control.Carrier.StickyLogger (StickyLogger, logSticky', runStickyLogger)
import Control.Carrier.TaskPool
import Control.Concurrent
import Control.Effect.Exception as Exc
import Control.Effect.Lift (sendIO)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Bool (bool)
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Discovery.Projects (withDiscoveredProjects)
import Effect.Exec
import Effect.Logger
import Effect.ReadFS
import Path
import Path.IO qualified as PIO
import Strategy.Maven qualified as Maven
import Strategy.NuGet.Nuspec qualified as Nuspec
import System.Exit (die)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stderr, stdout)
import Types

scanMain :: Path Abs Dir -> Bool -> IO ()
scanMain basedir debug = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  exists <- PIO.doesDirExist basedir
  unless exists (die $ "ERROR: " <> show basedir <> " does not exist")

  withDefaultLogger (bool SevInfo SevDebug debug) $ scan basedir

runAll ::
  ( Has ReadFS sig m
  , Has Exec sig m
  , Has (Output ProjectLicenseScan) sig m
  , Has Logger sig m
  , Has TaskPool sig m
  , Has (Lift IO) sig m
  , Has AtomicCounter sig m
  , Has Debug sig m
  , MonadIO m
  ) =>
  Path Abs Dir ->
  m ()
runAll basedir = do
  single Maven.discover
  single Nuspec.discover
  where
    single f = withDiscoveredProjects f basedir runSingle

runSingle ::
  ( LicenseAnalyzeProject a
  , Has ReadFS sig m
  , Has Exec sig m
  , Has (Output ProjectLicenseScan) sig m
  , Has Logger sig m
  , Has (Lift IO) sig m
  , MonadIO m
  ) =>
  DiscoveredProject a ->
  m ()
runSingle project = do
  licenseResult <- Diag.runDiagnosticsIO $ licenseAnalyzeProject (projectData project)
  Diag.withResult SevWarn licenseResult (output . mkLicenseScan project)

scan ::
  ( Has (Lift IO) sig m
  , Has Logger sig m
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
    { licenseStrategyType = projectType project
    , licenseStrategyName = projectType project
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
