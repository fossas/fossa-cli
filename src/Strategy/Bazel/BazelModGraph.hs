module Strategy.Bazel.BazelModGraph (
  BazelModGraphJson (..),
  BazelModGraphNode (..),
  buildModGraphDeps,
) where

import Data.Aeson (
  FromJSON (parseJSON),
  withObject,
  (.:),
  (.:?),
 )
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import DepTypes (
  DepType (BazelType),
  Dependency (..),
  VerConstraint (CEq),
 )
import Graphing (Graphing)
import Graphing qualified

-- | Top-level JSON output from `bazel mod graph --output json`.
data BazelModGraphJson = BazelModGraphJson
  { modGraphKey :: Text
  , modGraphVersion :: Text
  , modGraphDeps :: [BazelModGraphNode]
  }
  deriving (Eq, Ord, Show)

-- | A node in the module dependency graph.
data BazelModGraphNode = BazelModGraphNode
  { nodeKey :: Text
  , nodeVersion :: Text
  , nodeDeps :: [BazelModGraphNode]
  }
  deriving (Eq, Ord, Show)

instance FromJSON BazelModGraphJson where
  parseJSON = withObject "BazelModGraphJson" $ \obj ->
    BazelModGraphJson
      <$> obj .: "key"
      <*> (fromMaybe "" <$> obj .:? "version")
      <*> (fromMaybe [] <$> obj .:? "dependencies")

instance FromJSON BazelModGraphNode where
  parseJSON = withObject "BazelModGraphNode" $ \obj ->
    BazelModGraphNode
      <$> obj .: "key"
      <*> (fromMaybe "" <$> obj .:? "version")
      <*> (fromMaybe [] <$> obj .:? "dependencies")

-- | Build a dependency graph from `bazel mod graph` JSON output.
buildModGraphDeps :: BazelModGraphJson -> Graphing Dependency
buildModGraphDeps root =
  -- The root node represents the current module; its deps are direct dependencies.
  let directDeps = modGraphDeps root
   in mconcat (map (\node -> Graphing.direct (nodeToDep node) <> buildNodeEdges node) directDeps)

buildNodeEdges :: BazelModGraphNode -> Graphing Dependency
buildNodeEdges node =
  let parent = nodeToDep node
      children = nodeDeps node
      directEdges = mconcat [Graphing.edge parent (nodeToDep child) | child <- children]
      childEdges = mconcat (map buildNodeEdges children)
   in directEdges <> childEdges

nodeToDep :: BazelModGraphNode -> Dependency
nodeToDep node =
  Dependency
    { dependencyType = BazelType
    , dependencyName = nodeKey node
    , dependencyVersion =
        if Text.null (nodeVersion node)
          then Nothing
          else Just (CEq (nodeVersion node))
    , dependencyLocations = []
    , dependencyEnvironments = mempty
    , dependencyTags = Map.empty
    }
