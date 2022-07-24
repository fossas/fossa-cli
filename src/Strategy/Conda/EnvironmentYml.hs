{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Strategy.Conda.EnvironmentYml (
  analyze,
  buildGraph,
  EnvironmentYmlFile (..),
) where

import Control.Carrier.Diagnostics hiding (fromMaybe)
import Data.Aeson
import Data.List.Extra ((!?))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Effect.ReadFS
import Graphing (Graphing, fromList)
import Path
import Types

buildGraph :: EnvironmentYmlFile -> Graphing Dependency
buildGraph envYmlFile = Graphing.fromList (map toDependency allDeps)
  where
    allDeps = map getCondaDepFromText (dependencies envYmlFile)
    toDependency :: CondaDep -> Dependency
    toDependency CondaDep{..} =
      Dependency
        { dependencyType = CondaType
        , dependencyName = depName
        , dependencyVersion = CEq <$> depVersion -- todo - properly handle version constraints
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = Map.empty
        }

analyze ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  ) =>
  Path Abs File ->
  m (Graphing Dependency)
analyze envYml = buildGraph <$> readContentsYaml @EnvironmentYmlFile envYml

-- | an example Text: "biopython=1.78=py38haf1e3a3_0"
--   '=' is a delimeter for <name>=<version>=<build>
--   where <version> and <build> are optional
getCondaDepFromText :: Text -> CondaDep
getCondaDepFromText rcd =
  CondaDep
    { depName = name
    , depVersion = version
    , -- Note these aren't currently being used
      depBuild = build
    , depFullVersion = fullVersion
    }
  where
    depSplit = Text.split (== '=') rcd

    name = fromMaybe rcd (depSplit !? 0)
    version = depSplit !? 1 -- TODO: this may contain constraints that we need to parse
    build = depSplit !? 2
    fullVersion = getFullVersion version build

    -- if we have a version AND a build, we combine them to form a full version
    -- >>> getFullVersion (Just "1.2.3") (Just "build")
    -- Just "1.2.3=build"
    getFullVersion :: Maybe Text -> Maybe Text -> Maybe Text
    getFullVersion a b = do
      aVal <- a
      let bVal = case b of
            Just x -> "=" <> x
            Nothing -> ""
      pure (aVal <> bVal)

data EnvironmentYmlFile = EnvironmentYmlFile
  { name :: Text
  , dependencies :: [Text]
  }
  deriving (Eq, Ord, Show)

instance FromJSON EnvironmentYmlFile where
  parseJSON = withObject "EnvironmentYmlFile" $ \obj ->
    EnvironmentYmlFile
      <$> obj .: "name"
      <*> obj .: "dependencies"

data CondaDep = CondaDep
  { depName :: Text
  , depVersion :: Maybe Text
  , depBuild :: Maybe Text
  , depFullVersion :: Maybe Text
  }
  deriving (Eq, Ord, Show)
