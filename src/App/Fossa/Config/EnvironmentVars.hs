module App.Fossa.Config.EnvironmentVars (
  EnvVars (..),
  getEnvVars,
) where

import App.Fossa.Config.ConfigFile (ConfigTelemetryScope (FullTelemetry, NoTelemetry))
import Control.Effect.Lift (Has, Lift, sendIO)
import Data.Functor.Extra ((<$$>))
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Effect.Logger (Logger, Pretty (pretty), logWarn)
import System.Environment (lookupEnv)

data EnvVars = EnvVars
  { envApiKey :: Maybe Text
  , envConfigDebug :: Bool
  , envTelemetryDebug :: Bool
  , envTelemetryScope :: Maybe ConfigTelemetryScope
  , envDockerHost :: Maybe Text
  }
  deriving (Eq, Ord, Show)

apiKeyName :: String
apiKeyName = "FOSSA_API_KEY"

configDebugName :: String
configDebugName = "FOSSA_CONFIG_DEBUG"

telemetryDebugName :: String
telemetryDebugName = "FOSSA_TELEMETRY_DEBUG"

telemetryScopeKeyName :: String
telemetryScopeKeyName = "FOSSA_TELEMETRY_SCOPE"

dockerHostName :: String
dockerHostName = "DOCKER_HOST"

getEnvVars :: (Has (Lift IO) sig m, Has Logger sig m) => m EnvVars
getEnvVars =
  EnvVars
    <$> lookupName apiKeyName
    <*> lookupBool configDebugName
    <*> lookupBool telemetryDebugName
    <*> lookUpTelemetryScope telemetryScopeKeyName
    <*> lookupName dockerHostName

lookupName :: Has (Lift IO) sig m => String -> m (Maybe Text)
lookupName name = toText <$$> sendIO (lookupEnv name)

lookupBool :: Has (Lift IO) sig m => String -> m Bool
lookupBool name = do
  value <- lookupName name
  pure $ case value of
    Nothing -> False
    Just txt -> not $ any' txt [Text.null, (== "0")]

lookUpTelemetryScope :: (Has (Lift IO) sig m, Has Logger sig m) => String -> m (Maybe ConfigTelemetryScope)
lookUpTelemetryScope varName = do
  telScope <- lookupName varName
  case telScope of
    Nothing -> pure Nothing
    Just scope ->
      case Text.toLower . Text.strip $ scope of
        "off" -> pure $ Just NoTelemetry
        "full" -> pure $ Just FullTelemetry
        other -> do
          logWarn $ pretty ("You provided telemetry scope of: " <> other <> " " <> "which is not valid. Please choose between \"full\" or \"off\".")
          pure Nothing

any' :: a -> [a -> Bool] -> Bool
any' _ [] = False
any' b (f : fs) = f b || any' b fs
