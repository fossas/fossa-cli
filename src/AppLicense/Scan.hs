
module AppLicense.Scan
  ( scanMain
  ) where

import Prologue

import Control.Concurrent
import Control.Exception (SomeException)
import Data.Bool (bool)
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Sequence as S
import Path.IO
import Polysemy
import Polysemy.Async
import Polysemy.Error
import Polysemy.Output
import Polysemy.Resource
import System.Exit (die)

import Control.Parallel
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Diagnostics
import Discovery
import Effect.Error
import Effect.Exec
import Effect.Logger
import Effect.ReadFS hiding (doesDirExist)
import Types

scanMain :: Path Abs Dir -> Bool -> IO ()
scanMain basedir debug = do
  exists <- doesDirExist basedir
  unless exists (die $ "ERROR: " <> show basedir <> " does not exist")

  scan basedir
    & bool ignoreLogger (loggerToIO Debug) debug
    & asyncToIOFinal
    & resourceToIOFinal
    & embedToFinal @IO
    & runFinal

scan :: Members '[Final IO, Embed IO, Resource, Async, Logger] r => Path Abs Dir -> Sem r ()
scan basedir = do
  setCurrentDir basedir
  capabilities <- embed getNumCapabilities

  (results :: S.Seq CompletedLicenseScan, ()) <- runActions capabilities (map ADiscover licenseDiscoverFuncs) (runAction basedir) updateProgress
    & outputToIOMonoid S.singleton

  logSticky "[ Combining Analyses ]"

  let projects = mkLicenseScans strategyGroups results
  embed $ BL.putStrLn $ encode $ projects

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

type StrategyGroupName = Text

mkLicenseScans :: [StrategyGroup] -> S.Seq CompletedLicenseScan -> [ProjectLicenseScan]
mkLicenseScans groups seqScans = toProjectScan <$> toList seqScans
  where
    toProjectScan :: CompletedLicenseScan -> ProjectLicenseScan
    toProjectScan completedScan =
      ProjectLicenseScan { licenseStrategyType = completedToGroup completedScan
                         , licenseStrategyName = completedLicenseName completedScan
                         , discoveredLicenses = completedLicenses completedScan
                         }

    completedToGroup :: CompletedLicenseScan -> StrategyGroupName
    completedToGroup CompletedLicenseScan{completedLicenseName} =
      -- use the strategy name as a group name if a group doesn't exist
      fromMaybe completedLicenseName (M.lookup completedLicenseName groupsByStrategy)

    groupsByStrategy :: Map StrategyGroupName StrategyGroupName
    groupsByStrategy = M.fromList
      [(stratName, groupName) | StrategyGroup groupName strategies <- groups
                              , SomeStrategy strat <- strategies
                              , let stratName = strategyName strat
                              ]

runAction :: Members '[Final IO, Embed IO, Resource, Logger, Output CompletedLicenseScan] r => Path Abs Dir -> (Action -> Sem r ()) -> Action -> Sem r ()
runAction basedir enqueue = \case
  ADiscover Discover{..} -> do
    let prettyName = fill 20 (annotate (colorDull Cyan) (pretty discoverName <> " "))

    result <- discoverFunc basedir
      & readFSToIO
      & readFSErrToCLIErr
      & execToIO
      & execErrToCLIErr
      & errorToIOFinal @CLIErr
      & fromExceptionSem @SomeException
      & errorToIOFinal @SomeException
      & runOutputSem @ConfiguredStrategy (enqueue . AStrategy)

    case result of
      Left someException -> do
        logWarn $ prettyName <> annotate (color Red) "Discovery failed with uncaught SomeException"
        logWarn $ pretty (show someException) <> line
      Right (Left err) -> do
        logWarn $ prettyName <> annotate (color Red) "Discovery failed"
        logDebug $ pretty (show err) <> line
      Right (Right ()) -> logDebug $ prettyName <> annotate (color Green) "Finished discovery"

  AStrategy (ConfiguredStrategy Strategy{..} opts) -> do
    let prettyName = annotate (color Cyan) (pretty strategyName)
        prettyPath = pretty (toFilePath (strategyModule opts))

    result <- strategyLicense opts
      & readFSToIO
      & readFSErrToCLIErr
      & execToIO
      & execErrToCLIErr
      & errorToIOFinal @CLIErr
      & fromExceptionSem @SomeException
      & errorToIOFinal @SomeException

    case result of
      Left someException -> do
        logWarn $ prettyPath <> " " <> prettyName <> " " <> annotate (color Yellow) "Analysis failed with uncaught SomeException"
        logDebug $ pretty (show someException) <> line
      Right (Left err) -> do
        logWarn $ prettyPath <> " " <> prettyName <> " " <> annotate (color Yellow) "Analysis failed"
        logDebug $ pretty (show err) <> line
      Right (Right licenses) -> do
        logInfo $ prettyPath <> " " <> prettyName <> " " <> annotate (color Green) "Analyzed"
        logDebug (pretty (show licenses))
        output (CompletedLicenseScan strategyName licenses)

updateProgress :: Member Logger r => Progress -> Sem r ()
updateProgress Progress{..} =
  logSticky ( "[ "
            <> annotate (color Cyan) (pretty pQueued)
            <> " Waiting / "
            <> annotate (color Yellow) (pretty pRunning)
            <> " Running / "
            <> annotate (color Green) (pretty pCompleted)
            <> " Completed"
            <> " ]" )

data Action =
    ADiscover Discover
  | AStrategy ConfiguredStrategy
