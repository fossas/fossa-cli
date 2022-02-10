module App.Fossa.Config.EnvironmentVars (
  EnvVars (..),
  getEnvVars,
) where

import Control.Effect.Lift (Has, Lift, sendIO)
import Data.Functor.Extra ((<$$>))
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Text qualified as Text
import System.Environment (lookupEnv)

data EnvVars = EnvVars
  { envApiKey :: Maybe Text
  , envConfigDebug :: Bool
  }
  deriving (Eq, Ord, Show)

apiKeyName :: [Char]
apiKeyName = "FOSSA_API_KEY"

configDebugName :: [Char]
configDebugName = "FOSSA_CONFIG_DEBUG"

-- Currently, this is overkill, but useful if we add other environment vars
-- later, like the proposed FOSSA_BINARY_CMD in fossas/team-analysis#799
getEnvVars :: Has (Lift IO) sig m => m EnvVars
getEnvVars =
  EnvVars
    <$> lookupName apiKeyName
    <*> lookupBool configDebugName

lookupName :: Has (Lift IO) sig m => String -> m (Maybe Text)
lookupName name = toText <$$> sendIO (lookupEnv name)

lookupBool :: Has (Lift IO) sig m => String -> m Bool
lookupBool name = do
  value <- lookupName name
  pure $ case value of
    Nothing -> False
    Just txt -> not $ any' txt [Text.null, (== "0")]

any' :: a -> [a -> Bool] -> Bool
any' _ [] = False
any' b (f : fs) = f b || any' b fs
