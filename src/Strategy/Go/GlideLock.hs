{-# LANGUAGE RecordWildCards #-}

module Strategy.Go.GlideLock (
  analyze',
  GlideLockfile (..),
  GlideDep (..),
  buildGraph,
) where

import Control.Applicative ((<|>))
import Control.Effect.Diagnostics
import Data.Aeson
import Data.Map.Strict qualified as Map
import Data.String.Conversion (toText)
import Data.Text (Text)
import DepTypes
import Effect.ReadFS
import Graphing (Graphing)
import Graphing qualified
import Path
import Types (DependencyResults (..), GraphBreadth (..))

analyze' :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m DependencyResults
analyze' file = do
  lockfile <- readContentsYaml @GlideLockfile file
  graph <- context "Building dependency graph" $ pure (buildGraph lockfile)
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Complete
      , dependencyManifestFiles = [file]
      }

buildGraph :: GlideLockfile -> Graphing Dependency
buildGraph lockfile = Graphing.fromList (map toDependency direct)
  where
    direct = imports lockfile
    toDependency GlideDep{..} =
      Dependency
        { dependencyType = GoType
        , dependencyName = depName
        , dependencyVersion = Just (CEq depVersion)
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = Map.empty
        }

data GlideLockfile = GlideLockfile
  { hash :: Text
  , updated :: Text
  , imports :: [GlideDep]
  }
  deriving (Eq, Ord, Show)

data GlideDep = GlideDep
  { depName :: Text
  , depVersion :: Text
  , depRepo :: Maybe Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON GlideLockfile where
  parseJSON = withObject "GlideLockfile" $ \obj ->
    GlideLockfile
      <$> obj .: "hash"
      <*> obj .: "updated"
      <*> obj .: "imports"

instance FromJSON GlideDep where
  parseJSON = withObject "GlideDep" $ \obj ->
    GlideDep
      <$> obj .: "name"
      -- version field can be text or an int (hexadecimal hash)
      <*> (obj .: "version" <|> (intToText <$> obj .: "version"))
      <*> obj .:? "repo"

intToText :: Int -> Text
intToText = toText . show
