module App.NewFossa.EnvironmentVars (
  EnvVars (..),
  getEnvVars,
) where

import Control.Effect.Lift (Has, Lift, sendIO)
import Data.Functor.Extra ((<$$>))
import Data.String.Conversion (toText)
import Data.Text (Text)
import System.Environment (lookupEnv)

newtype EnvVars = EnvVars
  { envApiKey :: Maybe Text
  }
  deriving (Eq, Ord, Show)

-- Currently, this is overkill, but useful if we add other environment vars
-- later, like the proposed FOSSA_BINARY_CMD in fossas/team-analysis#799
getEnvVars :: Has (Lift IO) sig m => m EnvVars
getEnvVars = do
  maybeApiKey <- toText <$$> sendIO (lookupEnv "FOSSA_API_KEY")
  pure $ EnvVars maybeApiKey
