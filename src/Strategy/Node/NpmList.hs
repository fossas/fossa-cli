module Strategy.Node.NpmList (
  analyze',
) where

import Control.Effect.Diagnostics
import Data.Aeson
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import DepTypes
import Effect.Exec
import Graphing (Graphing, unfold)
import Path

npmListCmd :: Command
npmListCmd =
  Command
    { cmdName = "npm"
    , cmdArgs = ["ls", "--json", "--production", "--depth", "1000"]
    , cmdAllowErr = NonEmptyStdout
    }

analyze' :: (Has Exec sig m, Has Diagnostics sig m) => Path Abs Dir -> m (Graphing Dependency)
analyze' dir = do
  npmOutput <- execJson @NpmOutput dir npmListCmd
  context "Building dependency graph" $ pure (buildGraph npmOutput)

buildGraph :: NpmOutput -> Graphing Dependency
buildGraph top = unfold direct getDeps toDependency
  where
    direct = Map.toList $ outputDependencies top
    getDeps (_, nodeOutput) = Map.toList $ outputDependencies nodeOutput
    toDependency (nodeName, nodeOutput) =
      Dependency
        { dependencyType = NodeJSType
        , dependencyName = nodeName
        , dependencyVersion = CEq <$> outputVersion nodeOutput
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = Map.empty
        }

data NpmOutput = NpmOutput
  { outputInvalid :: Maybe Bool
  , outputVersion :: Maybe Text
  , outputFrom :: Maybe Text
  , outputResolved :: Maybe Text
  , outputDependencies :: Map Text NpmOutput
  }
  deriving (Eq, Ord, Show)

instance FromJSON NpmOutput where
  parseJSON = withObject "NpmOutput" $ \obj ->
    NpmOutput <$> obj .:? "invalid"
      <*> obj .:? "version"
      <*> obj .:? "from"
      <*> obj .:? "resolved"
      <*> obj .:? "dependencies" .!= Map.empty
