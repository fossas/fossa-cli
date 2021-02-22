{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module App.Pathfinder.Scan
  ( scanMain
  ) where

import Control.Carrier.Error.Either
import Control.Carrier.Finally
import Control.Carrier.Output.IO
import Control.Carrier.TaskPool
import Control.Concurrent
import qualified Control.Carrier.Diagnostics as Diag
import Control.Effect.Exception as Exc
import Control.Effect.Lift (sendIO)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Bool (bool)
import qualified Data.ByteString.Lazy as BL
import Data.Function ((&))
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Discovery.Projects (withDiscoveredProjects)
import Effect.Logger
import Effect.ReadFS
import Path
import qualified Path.IO as PIO
import qualified Strategy.Maven as Maven
import qualified Strategy.NuGet.Nuspec as Nuspec
import System.Exit (die)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stderr, stdout)
import Types
import Effect.Exec

scanMain :: Path Abs Dir -> Bool -> IO ()
scanMain basedir debug = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  exists <- PIO.doesDirExist basedir
  unless exists (die $ "ERROR: " <> show basedir <> " does not exist")

  scan basedir
    & withLogger (bool SevInfo SevDebug debug)

runLicenseAnalysis ::
  (Has (Lift IO) sig m, Has Logger sig m, Has (Output ProjectLicenseScan) sig m) =>
  DiscoveredProject (ReadFSIOC (ExecIOC (Diag.DiagnosticsC IO))) ->
  m ()
runLicenseAnalysis project = do
  licenseResult <- sendIO . Diag.runDiagnosticsIO . runExecIO . runReadFSIO $ projectLicenses project
  Diag.withResult SevWarn licenseResult (output . mkLicenseScan project)

scan ::
  ( Has (Lift IO) sig m
  , Has Logger sig m
  , MonadIO m
  )
  => Path Abs Dir -> m ()
scan basedir = runFinally $ do
  sendIO $ PIO.setCurrentDir basedir
  capabilities <- sendIO getNumCapabilities

  (projectResults, ()) <-
    runOutput @ProjectLicenseScan
      . runReadFSIO
      . runFinally
      . withTaskPool capabilities updateProgress
      $ withDiscoveredProjects discoverFuncs False basedir runLicenseAnalysis

  logSticky "[ Combining Analyses ]"

  sendIO (BL.putStr (encode projectResults))

  logSticky ""


discoverFuncs ::
  ( Has Diag.Diagnostics sig m,
    Has (Lift IO) sig m,
    MonadIO m,
    Has ReadFS sig m,
    Has ReadFS rsig run,
    Has Exec rsig run,
    Has Diag.Diagnostics rsig run,
    Has (Lift IO) rsig run
  ) =>
  -- | Discover functions
  [Path Abs Dir -> m [DiscoveredProject run]]
discoverFuncs = [Maven.discover, Nuspec.discover]

data ProjectLicenseScan = ProjectLicenseScan
  { licenseStrategyType :: Text
  , licenseStrategyName :: Text
  , discoveredLicenses  :: [LicenseResult]
  } deriving (Eq, Ord, Show)

instance ToJSON ProjectLicenseScan where
  toJSON ProjectLicenseScan{..} = object
    [ "type"    .= licenseStrategyType
    , "name"    .= licenseStrategyName
    , "files"   .= discoveredLicenses
    ]

data CompletedLicenseScan = CompletedLicenseScan
  { completedLicenseName     :: Text
  , completedLicenses        :: [LicenseResult]
  } deriving (Eq, Ord, Show)

instance ToJSON CompletedLicenseScan where
    toJSON CompletedLicenseScan{..} = object
      [ "name"            .=  completedLicenseName
      , "licenseResults"  .=  completedLicenses
      ]

mkLicenseScan :: DiscoveredProject n -> [LicenseResult] -> ProjectLicenseScan
mkLicenseScan project licenses =
  ProjectLicenseScan
    { licenseStrategyType = projectType project,
      licenseStrategyName = projectType project,
      discoveredLicenses = licenses
    }

updateProgress :: Has Logger sig m => Progress -> m ()
updateProgress Progress{..} =
  logSticky ( "[ "
            <> annotate (color Cyan) (pretty pQueued)
            <> " Waiting / "
            <> annotate (color Yellow) (pretty pRunning)
            <> " Running / "
            <> annotate (color Green) (pretty pCompleted)
            <> " Completed"
            <> " ]" )
