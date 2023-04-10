module App.Fossa.Config.EnvironmentVars (
  EnvVars (..),
  getEnvVars,
) where

import App.Fossa.Config.ConfigFile (ConfigTelemetryScope (FullTelemetry, NoTelemetry))
import Control.Effect.Lift (Has, Lift, sendIO)
import Data.Functor.Extra ((<$$>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Maybe.Extra (maybeTuple)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Text qualified as Text
import DepTypes (DepType (..))
import Effect.Logger (Logger, Pretty (pretty), logWarn)
import System.Environment (lookupEnv)

data EnvVars = EnvVars
  { envApiKey :: Maybe Text
  , envConfigDebug :: Bool
  , envTelemetryDebug :: Bool
  , envTelemetryScope :: Maybe ConfigTelemetryScope
  , envDockerHost :: Maybe Text
  , envCmdOverrides :: Map DepType Text
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

overridableCommands :: [DepType]
overridableCommands = [(minBound :: DepType) ..]

getEnvVars :: (Has (Lift IO) sig m, Has Logger sig m) => m EnvVars
getEnvVars =
  EnvVars
    <$> lookupName apiKeyName
    <*> lookupBool configDebugName
    <*> lookupBool telemetryDebugName
    <*> lookUpTelemetryScope telemetryScopeKeyName
    <*> lookupName dockerHostName
    <*> lookupNames mkCommandOverrideVar overridableCommands

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

-- | Given a list of @a@, and a function that may generate an environment variable for a given instance of @a@,
-- look up every environment variable and store its value next to the original instance in a map.
--
-- The "function that may generate an environment variable for a given instance of @a@" is done in order to
-- make it possible to not support certain instances of @a@ for lookup.
lookupNames :: (Has (Lift IO) sig m, Ord a) => (a -> Maybe String) -> [a] -> m (Map a Text)
lookupNames mkVar names = Map.fromList . catMaybes <$> traverse lookupPair (mapMaybe (toPair mkVar) names)
  where
    toPair :: (a -> Maybe String) -> a -> Maybe (a, String)
    toPair mkVar' a = maybeTuple a (mkVar' a)

    lookupPair :: Has (Lift IO) sig m => (a, String) -> m (Maybe (a, Text))
    lookupPair (a, name) = maybeTuple a <$> lookupName name

-- | Users need to be able to override the command used during dynamic analyis.
-- For example, users may have a custom Maven command at a specific path,
-- or a custom Go version installed they wish to use for a specific project.
--
-- In order to specify these in a user-facing manner, we use the @DepType@ to
-- generate an environment variable to use when overriding that command.
--
-- We don't use existing stringifications of @DepType@ (for example, @depTypeToFetcher@)
-- because we do not want to tie internal representations to user facing contracts.
mkCommandOverrideVar :: DepType -> Maybe String
mkCommandOverrideVar MavenType = Just $ commandOverrideVarForName "MAVEN"
mkCommandOverrideVar _ = Nothing

commandOverrideVarForName :: String -> String
commandOverrideVarForName name = "FOSSA_" <> name <> "_CMD"

any' :: a -> [a -> Bool] -> Bool
any' _ [] = False
any' b (f : fs) = f b || any' b fs
