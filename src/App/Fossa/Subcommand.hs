{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Subcommand (
  runSubCommand,
  EffStack,
  GetSeverity (..),
  SubCommand (..),
) where

import App.Fossa.Config.ConfigFile (ConfigFile)
import App.Fossa.Config.EnvironmentVars (EnvVars (envConfigDebug), getEnvVars)
import Control.Carrier.Diagnostics (DiagnosticsC, context, logWithExit_)
import Control.Carrier.Stack (StackC, runStack)
import Data.String.Conversion (toStrict)
import Effect.Exec (ExecIOC, runExecIO)
import Effect.Logger (
  LoggerC,
  Severity (SevInfo),
  logInfo,
  logStdout,
  withDefaultLogger,
 )
import Effect.ReadFS (ReadFSIOC, runReadFSIO)
import Options.Applicative (InfoMod, Parser)
import Text.Pretty.Simple (pShowNoColor)

data SubCommand cli cfg = SubCommand
  { commandName :: String
  , commandInfo :: InfoMod (IO ())
  , parser :: Parser cli
  , configLoader :: cli -> EffStack (Maybe ConfigFile) -- CLI args can control where we look for config file
  , optMerge :: Maybe ConfigFile -> EnvVars -> cli -> EffStack cfg
  , perform :: cfg -> EffStack ()
  }

type EffStack = ExecIOC (ReadFSIOC (DiagnosticsC (LoggerC (StackC IO))))

class GetSeverity a where
  getSeverity :: a -> Severity
  getSeverity = const SevInfo

runSubCommand :: forall cli cfg. (GetSeverity cli, Show cfg) => SubCommand cli cfg -> Parser (IO ())
runSubCommand SubCommand{..} = uncurry runEffs . mergeAndRun <$> parser
  where
    -- We have to extract the severity from the options, which is not straightforward
    -- since the CLI parser is Applicative, but not Monad.
    mergeAndRun :: cli -> (Severity, EffStack ())
    mergeAndRun cliOptions = (getSeverity cliOptions,) $ do
      configFile <- context "Loading config file" $ configLoader cliOptions
      envvars <- context "Fetching environment variables" getEnvVars
      cfg <- context "Validating configuration" $ optMerge configFile envvars cliOptions
      if envConfigDebug envvars
        then do
          logInfo "Running in config-debug mode, no action will be performed"
          logStdout (toStrict $ pShowNoColor cfg)
        else perform cfg

runEffs :: Severity -> EffStack () -> IO ()
runEffs sev = runStack . withDefaultLogger sev . logWithExit_ . runReadFSIO . runExecIO
