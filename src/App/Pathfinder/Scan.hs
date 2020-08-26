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
import Control.Effect.Exception as Exc
import Control.Effect.Lift (sendIO)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Bool (bool)
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Effect.Exec
import Effect.Logger
import Effect.ReadFS
import Path
import qualified Path.IO as PIO
import qualified Strategy.Maven.Pom as MavenPom
import qualified Strategy.NuGet.Nuspec as Nuspec
import System.Exit (die)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stderr, stdout)
import Types

scanMain :: Path Abs Dir -> Bool -> IO ()
scanMain basedir debug = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  exists <- PIO.doesDirExist basedir
  unless exists (die $ "ERROR: " <> show basedir <> " does not exist")

  scan basedir
    & withLogger (bool SevInfo SevDebug debug)

scan ::
  ( Has (Lift IO) sig m
  , Has Logger sig m
  , MonadIO m
  )
  => Path Abs Dir -> m ()
scan basedir = runFinally $ do
  sendIO $ PIO.setCurrentDir basedir
  capabilities <- sendIO getNumCapabilities

  (closures,(_,())) <- runOutput @ProjectClosure . runOutput @ProjectFailure . runExecIO . runReadFSIO $
    withTaskPool capabilities updateProgress (traverse_ (forkTask . apply basedir) discoverFuncs)

  logSticky "[ Combining Analyses ]"

  let projects = mkLicenseScans closures
  sendIO (BL.putStr (encode projects))

  logSticky ""

apply :: a -> (a -> b) -> b
apply x f = f x

discoverFuncs :: HasDiscover sig m => [Path Abs Dir -> m ()]
discoverFuncs =
  [ Nuspec.discover
  , MavenPom.discover
  ]

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
