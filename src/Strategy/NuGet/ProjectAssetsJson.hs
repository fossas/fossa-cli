{-# LANGUAGE RecordWildCards #-}

module Strategy.NuGet.ProjectAssetsJson (
  discover,
  findProjects,
  getDeps,
  mkProject,
  buildGraph,
  ProjectAssetsJson (..),

  -- * for testing
  approxEql,
) where

import App.Fossa.Analyze.Types (AnalyzeProject, analyzeProject)
import Control.Effect.Diagnostics hiding (fromMaybe)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.HashMap.Strict qualified as HM
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String.Conversion (toString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Traversable (for)
import DepTypes
import Discovery.Walk
import Effect.ReadFS
import GHC.Generics (Generic)
import Graphing (Graphing, empty, gmap, promoteToDirect, shrink, unfoldDeep)
import Path
import Text.Read (readMaybe)
import Types

discover :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [DiscoveredProject ProjectAssetsJsonProject]
discover dir = context "ProjectAssetsJson" $ do
  projects <- context "Finding projects" $ findProjects dir
  pure (map mkProject projects)

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [ProjectAssetsJsonProject]
findProjects = walk' $ \_ _ files -> do
  case findFileNamed "project.assets.json" files of
    Nothing -> pure ([], WalkContinue)
    Just file -> pure ([ProjectAssetsJsonProject file], WalkContinue)

newtype ProjectAssetsJsonProject = ProjectAssetsJsonProject
  { projectAssetsJsonFile :: Path Abs File
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ProjectAssetsJsonProject

instance AnalyzeProject ProjectAssetsJsonProject where
  analyzeProject _ = getDeps

mkProject :: ProjectAssetsJsonProject -> DiscoveredProject ProjectAssetsJsonProject
mkProject project =
  DiscoveredProject
    { projectType = "projectassetsjson"
    , projectBuildTargets = mempty
    , projectPath = parent $ projectAssetsJsonFile project
    , projectData = project
    }

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => ProjectAssetsJsonProject -> m DependencyResults
getDeps = context "ProjectAssetsJson" . context "Static analysis" . analyze' . projectAssetsJsonFile

analyze' :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m DependencyResults
analyze' file = do
  assetsJson <- readContentsJson @ProjectAssetsJson file
  graph <- context "Building dependency graph" $ pure (buildGraph assetsJson)
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Complete
      , dependencyManifestFiles = [file]
      }

data NuGetDep = NuGetDep
  { depName :: Text
  , depVersion :: Text
  , completeDepType :: Text
  , completeDeepDeps :: Map.Map Text Text
  }
  deriving (Show, Eq, Ord)

data ProjectAssetsJson = ProjectAssetsJson
  { targets :: Map.Map Text (Map.Map Text DependencyInfo)
  , projectFramework :: Map.Map Text (Set Text)
  }
  deriving (Show, Eq, Ord)

data DependencyInfo = DependencyInfo
  { depType :: Text
  , deepDeps :: Map.Map Text Text
  }
  deriving (Show, Eq, Ord)

instance FromJSON DependencyInfo where
  parseJSON = withObject "Dependency" $ \obj ->
    DependencyInfo <$> obj .: "type"
      <*> obj .:? "dependencies" .!= Map.empty

instance FromJSON ProjectAssetsJson where
  parseJSON = withObject "ProjectAssetsJson" $ \obj -> do
    targets <- obj .: "targets"
    projectFrameworks <- obj .: "project" |> "frameworks"
    deps <- parseFramework projectFrameworks
    pure $ ProjectAssetsJson targets deps
    where
      (|>) :: FromJSON a => Parser Object -> Text -> Parser a
      (|>) parser key = do
        obj <- parser
        obj .: key

      parseFrameworkDeps :: Value -> Parser (Set Text)
      parseFrameworkDeps = withObject "parseFrameworkDeps" $ \o -> do
        depsObj :: Maybe Object <- o .:? "dependencies"
        deps <- case depsObj of
          Nothing -> pure []
          Just hm -> pure (fst <$> HM.toList hm)
        pure $ Set.fromList deps

      parseFramework :: Value -> Parser (Map.Map Text (Set Text))
      parseFramework = withObject "parseFramework" $ \o -> do
        projectFrameworks <- for (HM.toList o) $ \(framework, kv) -> do
          frameworkDeps <- parseFrameworkDeps kv
          pure (framework, frameworkDeps)
        pure $ Map.fromList projectFrameworks

buildGraph :: ProjectAssetsJson -> Graphing Dependency
buildGraph project = foldr (<>) Graphing.empty graphsOfTargetFrameworks
  where
    graphsOfTargetFrameworks =
      map
        (graphOfFramework $ projectFramework project)
        (Map.toList $ targets project)

graphOfFramework :: Map.Map Text (Set Text) -> (Text, Map.Map Text DependencyInfo) -> Graphing Dependency
graphOfFramework projectFrameworkDeps (targetFramework, targetFrameworkDeps) =
  withoutTags
    . withoutProjects
    . promoteToDirect isDirectDep
    $ unfoldDeep allResolvedDeps getTransitiveDeps toDependency
  where
    withoutTags :: Graphing Dependency -> Graphing Dependency
    withoutTags = Graphing.gmap (\d -> d{dependencyTags = Map.empty})

    withoutProjects :: Graphing Dependency -> Graphing Dependency
    withoutProjects = Graphing.shrink (not . isProjectDep)

    isDirectDep :: Dependency -> Bool
    isDirectDep d = dependencyName d `elem` (getProjectDirectDepsByFramework)

    isProjectDep :: Dependency -> Bool
    isProjectDep Dependency{..} = case Map.lookup "type" dependencyTags of
      Nothing -> False
      Just tags -> "project" `elem` tags

    allResolvedDeps :: [NuGetDep]
    allResolvedDeps = mapMaybe toNugetDep $ Map.toList targetFrameworkDeps

    toNugetDep :: (Text, DependencyInfo) -> Maybe NuGetDep
    toNugetDep (depString, dep) = case Text.splitOn "/" depString of
      [name, ver] -> Just $ NuGetDep name ver (depType dep) (deepDeps dep)
      _ -> Nothing

    getTransitiveDeps :: NuGetDep -> [NuGetDep]
    getTransitiveDeps nugetDep = concat $ (\(name, _) -> filter (\d -> depName d == name) allResolvedDeps) <$> Map.toList (completeDeepDeps nugetDep)

    toDependency :: NuGetDep -> Dependency
    toDependency NuGetDep{..} =
      Dependency
        { dependencyType = NuGetType
        , dependencyName = depName
        , dependencyVersion = Just (CEq depVersion)
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = Map.fromList [("type", [completeDepType])] -- we provide tag, for shrinking project deps!
        }

    -- Note:
    --  Project's framework's identifier do not always match 1:1 with (resolved) target framework.
    --  This is because, target framework has different identifier scheme (long form, short form).
    --  This is very hard to parse, given changing scheme with nuget version, as well as .net identifiers.
    getProjectDirectDepsByFramework :: Set Text
    getProjectDirectDepsByFramework =
      -- There is only one framework or all frameworks have same direct deps
      if allEqual (Map.elems projectFrameworkDeps)
        then allProjectSpecDeps
        else case (Map.lookup targetFramework projectFrameworkDeps) of
          Just s -> s
          Nothing -> do
            -- We could not find exact framework, fallback to equivalent match.
            -- At last resort, report all direct deps from all framework.
            let simplifiedProjectFrameworks =
                  Map.elems $
                    Map.filterWithKey (\pF _ -> pF `approxEql` targetFramework) projectFrameworkDeps
            fromMaybe allProjectSpecDeps (listToMaybe simplifiedProjectFrameworks)

    allProjectSpecDeps :: Set Text
    allProjectSpecDeps = Set.unions $ Map.elems projectFrameworkDeps

    allEqual :: Eq a => [a] -> Bool
    allEqual [] = True
    allEqual (x : xs) = all (== x) xs

-- | Check if two framework identifier are equivalent.
-- FIXME: This hack, although works for 99+% targets, deterministic approach is needed (ideally we replicate nuget)
-- Reference: https://github.com/NuGet/NuGet.Client/blob/dev/src/NuGet.Core/NuGet.Frameworks/NuGetFrameworkFactory.cs
approxEql :: Text -> Text -> Bool
approxEql targetF projectF
  | targetF == projectF = True
  | simplified targetF == simplified projectF = True
  | otherwise = numbersOnly targetF == numbersOnly projectF
  where
    simplified :: Text -> Text
    simplified candidate =
      foldr
        (uncurry Text.replace)
        candidate
        [ (".NETFramework", "net")
        , (".NETPlatform", "dotnet")
        , (".NETStandard", "netstandard")
        , (".NETCoreApp", "netcoreapp")
        , ("netcoreapp5.0", "net5.0")
        , (",Version=", "")
        , (",Version=v", "")
        , (".NETPlatform,Version=v0.0", ".NETPlatform,Version=v5.0")
        ]

    withoutMetaAndProfile :: Text -> Text
    withoutMetaAndProfile t = foldr (\b v -> fst $ Text.breakOn b v) (simplified t) ["/", "+", "-", ","]

    removeTailingZeros :: [Char] -> [Char]
    removeTailingZeros cs = reverse $ dropWhile (== '0') $ reverse cs

    numbersOnly :: Text -> Int
    numbersOnly t =
      fromMaybe 0 $ readMaybe (removeTailingZeros $ filter (`elem` ['0' .. '9']) (toString . withoutMetaAndProfile $ t)) :: Int
