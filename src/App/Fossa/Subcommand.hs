{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Subcommand (
  runSubCommand,
  EffStack,
  GetSeverity (..),
  GetCommonOpts (..),
  SubCommand (..),
  updateCommandName,
) where

import App.Fossa.Config.Common (CommonOpts, collectTelemetrySink)
import App.Fossa.Config.ConfigFile (ConfigFile)
import App.Fossa.Config.EnvironmentVars (EnvVars (envConfigDebug), getEnvVars)
import App.Fossa.DebugDir (DebugDirRef, newDebugDirRef, writeDebugDir)
import Control.Carrier.Diagnostics (DiagnosticsC, context, logWithExit_)
import Control.Carrier.Git (GitC, runGit)
import Control.Carrier.Reader (ReaderC, runReader)
import Control.Carrier.Stack (StackC, runStack)
import Control.Carrier.Telemetry (TelemetryC, withTelemetry)
import Control.Effect.Telemetry (setSink, trackConfig)
import Control.Exception (finally)
import Data.Aeson (ToJSON)
import Data.Foldable (traverse_)
import Data.String.Conversion (ToText (toText), toStrict)
import Data.UUID qualified as UUID (toString)
import Data.UUID.V4 qualified as UUID (nextRandom)
import Discovery.Archive qualified as Archive
import Effect.Exec (ExecIOC, runExecIO)
import Effect.Logger (
  LoggerC,
  Severity (SevDebug, SevInfo),
  logInfo,
  logStdout,
  withDefaultLogger,
 )
import Effect.ReadFS (ReadFSIOC, runReadFSIO)
import Options.Applicative (InfoMod, Parser)
import Path qualified as P
import System.Directory qualified as Dir
import Text.Pretty.Simple (pShowNoColor)

debugBundleZipPath :: FilePath
debugBundleZipPath = "fossa.debug.zip"

-- Function to finalize debug bundle after telemetry teardown
finalizeBundleWithTelemetry :: FilePath -> IO ()
finalizeBundleWithTelemetry debugDir = do
  putStrLn $ "[DEBUG] Finalizing debug bundle from: " <> debugDir

  -- Telemetry file should already be in debugDir (written there directly)
  -- Create zip file from the entire debug directory
  debugDirPath <- P.parseAbsDir debugDir
  zipPath <- P.parseAbsFile =<< Dir.makeAbsolute debugBundleZipPath
  Archive.mkZip debugDirPath zipPath

  putStrLn $ "Debug bundle created: " <> debugBundleZipPath

  -- Clean up the temporary debug directory
  Dir.removeDirectoryRecursive debugDir

data SubCommand cli cfg = SubCommand
  { commandName :: String
  , commandInfo :: InfoMod (IO ())
  , parser :: Parser cli
  , configLoader :: cli -> EffStack (Maybe ConfigFile) -- CLI args can control where we look for config file
  , optMerge :: Maybe ConfigFile -> EnvVars -> cli -> EffStack cfg
  , perform :: cfg -> EffStack ()
  }

type EffStack = ReaderC DebugDirRef (GitC (ExecIOC (ReadFSIOC (DiagnosticsC (LoggerC (StackC (TelemetryC IO)))))))

class GetSeverity a where
  getSeverity :: a -> Severity
  getSeverity = const SevInfo

class GetCommonOpts a where
  getCommonOpts :: a -> Maybe CommonOpts
  getCommonOpts = const Nothing

updateCommandName :: SubCommand cli cfg -> String -> SubCommand cli cfg
updateCommandName subCmd newName = subCmd{commandName = newName}

runSubCommand :: forall cli cfg. (GetCommonOpts cli, GetSeverity cli, Show cfg, ToJSON cfg) => SubCommand cli cfg -> Parser (IO ())
runSubCommand SubCommand{..} = (\cliOptions -> runWithDebugDir cliOptions) <$> parser
  where
    -- Create debug directory and run effects with cleanup
    runWithDebugDir :: cli -> IO ()
    runWithDebugDir cliOptions = do
      let sev = getSeverity cliOptions

      -- Create debug directory reference
      debugDirRef <- newDebugDirRef

      -- Create debug directory if in debug mode
      maybeDebugDir <-
        if sev == SevDebug
          then do
            tmpDir <- Dir.getTemporaryDirectory
            suffix <- UUID.toString <$> UUID.nextRandom
            let uniqueName = "fossa-debug-bundle-" <> suffix
            let dirName = tmpDir <> "/" <> uniqueName
            Dir.createDirectoryIfMissing True dirName
            writeDebugDir debugDirRef (Just dirName)
            pure (Just dirName)
          else do
            writeDebugDir debugDirRef Nothing
            pure Nothing

      let (_, action) = mergeAndRun debugDirRef cliOptions

      -- Run the command, ensuring finalization happens even on exceptions
      finally
        (runEffs sev debugDirRef action)
        (maybe (pure ()) finalizeBundleWithTelemetry maybeDebugDir)

    -- We have to extract the severity from the options, which is not straightforward
    -- since the CLI parser is Applicative, but not Monad.
    mergeAndRun :: DebugDirRef -> cli -> (Severity, EffStack ())
    mergeAndRun debugDirRef cliOptions = (getSeverity cliOptions,) $ do
      configFile <- context "Loading config file" $ configLoader cliOptions
      envvars <- context "Fetching environment variables" getEnvVars

      maybeTelSink <- collectTelemetrySink debugDirRef configFile envvars $ getCommonOpts cliOptions
      traverse_ setSink maybeTelSink

      cfg <- context "Validating configuration" $ optMerge configFile envvars cliOptions
      trackConfig (toText commandName) cfg

      if envConfigDebug envvars
        then do
          logInfo "Running in config-debug mode, no action will be performed"
          logStdout (toStrict $ pShowNoColor cfg)
        else perform cfg

runEffs :: Severity -> DebugDirRef -> EffStack () -> IO ()
runEffs sev debugDirRef = withTelemetry . runStack . withDefaultLogger sev . logWithExit_ . runReadFSIO . runExecIO . runGit . runReader debugDirRef
