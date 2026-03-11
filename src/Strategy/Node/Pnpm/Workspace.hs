module Strategy.Node.Pnpm.Workspace (PnpmWorkspace (..)) where

import Data.Aeson (FromJSON (..), withObject, (.!=), (.:?))
import Data.Glob (Glob)
import Debug.Trace (traceShow)
import Path (Rel)

newtype PnpmWorkspace = PnpmWorkspace {workspaceSpecs :: [Glob Rel]}
  deriving (Eq, Ord, Show)

instance FromJSON PnpmWorkspace where
  parseJSON = withObject "Pnpm Workspace" $
    \o -> traceShow o $ PnpmWorkspace <$> o .:? "packages" .!= []
