
module App.Pathfinder.Scan
  ( scanMain
  ) where

import Prologue

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Control.Carrier.Error.Either
import Control.Effect.Exception as Exc
import Control.Carrier.Output.IO
import Control.Concurrent
import Path.IO
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout, stderr)
import System.Exit (die)

import Control.Carrier.TaskPool
import Control.Carrier.Threaded
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Effect.Logger
import qualified Strategy.Maven.Pom as MavenPom
import qualified Strategy.NuGet.Nuspec as Nuspec
import Types

scanMain :: Path Abs Dir -> Bool -> IO ()
scanMain basedir debug = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  exists <- doesDirExist basedir
  unless exists (die $ "ERROR: " <> show basedir <> " does not exist")

  scan basedir
    & withLogger (bool SevInfo SevDebug debug)
    & runThreaded

scan ::
  ( Has (Lift IO) sig m
  , Has Logger sig m
  , Has Threaded sig m
  , MonadIO m
  , Effect sig
  )
  => Path Abs Dir -> m ()
scan basedir = do
  setCurrentDir basedir
  capabilities <- liftIO getNumCapabilities

  (closures,(_,())) <- runOutput @ProjectClosure $ runOutput @ProjectFailure $
    withTaskPool capabilities updateProgress (traverse_ ($ basedir) discoverFuncs)

  logSticky "[ Combining Analyses ]"

  let projects = mkLicenseScans closures
  liftIO (BL.putStr (encode projects))

  logSticky ""

discoverFuncs :: HasDiscover sig m => [Path Abs Dir -> m ()]
discoverFuncs =
  [ Nuspec.discover
  , MavenPom.discover
  ]

data ProjectLicenseScan = ProjectLicenseScan
  { licenseStrategyType :: Text
  , licenseStrategyName :: Text
  , discoveredLicenses  :: [LicenseResult]
  } deriving (Eq, Ord, Show, Generic)

instance ToJSON ProjectLicenseScan where
  toJSON ProjectLicenseScan{..} = object
    [ "type"    .= licenseStrategyType
    , "name"    .= licenseStrategyName
    , "files"   .= discoveredLicenses
    ]

data CompletedLicenseScan = CompletedLicenseScan
  { completedLicenseName     :: Text
  , completedLicenses        :: [LicenseResult]
  } deriving (Eq, Ord, Show, Generic)

instance ToJSON CompletedLicenseScan where
    toJSON CompletedLicenseScan{..} = object
      [ "name"            .=  completedLicenseName
      , "licenseResults"  .=  completedLicenses
      ]

mkLicenseScans :: [ProjectClosure] -> [ProjectLicenseScan]
mkLicenseScans seqScans = toProjectScan <$> seqScans
  where
    toProjectScan :: ProjectClosure -> ProjectLicenseScan
    toProjectScan closure =
      ProjectLicenseScan { licenseStrategyType = T.pack (show (closureStrategyGroup closure))
                         , licenseStrategyName = closureStrategyName closure
                         , discoveredLicenses  = closureLicenses closure
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
