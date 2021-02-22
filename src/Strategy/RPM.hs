{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Strategy.RPM
  ( buildGraph,
    discover,
    getSpecDeps,
    getTypeFromLine,
    toDependency,
    RPMDependency (..),
    RequiresType (..),
    Dependencies (..),
  )
where

import Control.Effect.Diagnostics
import Control.Monad (when)
import Data.List (isSuffixOf)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe, catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Extra (splitOnceOn)
import DepTypes
import Discovery.Walk
import Effect.ReadFS
import Graphing (Graphing)
import qualified Graphing as G
import Path
import Types

newtype SpecFileLabel
  = RequiresType DepEnvironment
  deriving (Eq, Ord, Show)

data RPMDependency
  = RPMDependency
      { rpmDepName :: Text,
        rpmConstraint :: Maybe VerConstraint
      }
  deriving (Eq, Ord, Show)

data RequiresType
  = BuildRequires RPMDependency
  | RuntimeRequires RPMDependency
  deriving (Eq, Ord, Show)

data Dependencies
  = Dependencies
      { depBuildRequires :: [RPMDependency],
        depRuntimeRequires :: [RPMDependency]
      }
  deriving (Eq, Ord, Show)

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has ReadFS rsig run, Has Diagnostics rsig run) => Path Abs Dir -> m [DiscoveredProject run]
discover dir = map mkProject <$> findProjects dir

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [RpmProject]
findProjects = walk' $ \dir _ files -> do
  let specs = filter (\f -> ".spec" `isSuffixOf` fileName f) files

  case specs of
    [] -> pure ([], WalkContinue)
    _ -> pure ([RpmProject dir specs], WalkContinue)

data RpmProject = RpmProject
  { rpmDir :: Path Abs Dir
  , rpmFiles :: [Path Abs File]
  }
  deriving (Eq, Ord, Show)

mkProject :: (Has ReadFS sig n, Has Diagnostics sig n) => RpmProject -> DiscoveredProject n
mkProject project =
  DiscoveredProject
    { projectType = "rpm",
      projectBuildTargets = mempty,
      projectDependencyGraph = const $ getDeps project,
      projectPath = rpmDir project,
      projectLicenses = pure []
    }

getDeps :: (Has ReadFS sig m, Has Diagnostics sig m) => RpmProject -> m (Graphing Dependency)
getDeps = analyze . rpmFiles

analyze :: (Has ReadFS sig m, Has Diagnostics sig m) => [Path Abs File] -> m (Graphing Dependency)
analyze specFiles = do
  results <- traverse (recover . analyzeSingle) specFiles
  let successful = catMaybes results

  when (null successful) $ fatalText "Analysis failed for all discovered *.spec files"

  let graphing :: Graphing Dependency
      graphing = mconcat successful

  pure graphing

analyzeSingle :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m (Graphing Dependency)
analyzeSingle file = do 
  specFileText <- readContentsText file
  pure . buildGraph $ getSpecDeps specFileText

toDependency :: RPMDependency -> Dependency
toDependency pkg =
    Dependency
      { dependencyType = RPMType,
        dependencyName = rpmDepName pkg,
        dependencyVersion = rpmConstraint pkg,
        dependencyLocations = [],
        dependencyEnvironments = [],
        dependencyTags = M.empty
      }

buildGraph :: Dependencies -> Graphing Dependency
buildGraph Dependencies {..} = G.gmap toDependency $ G.fromList depRuntimeRequires

buildConstraint :: Text -> Maybe VerConstraint
buildConstraint raw = constraint
  where
    (comparatorStr, rawVersion) = splitOnceOn " " $ T.strip raw
    version = T.strip rawVersion
    constraint = case T.strip comparatorStr of
      "<=" -> Just $ CLessOrEq version
      "<" -> Just $ CLess version
      ">=" -> Just $ CGreaterOrEq version
      ">" -> Just $ CGreater version
      "=" -> Just $ CEq version
      "==" -> Just $ CEq version
      _ -> Nothing

getTypeFromLine :: Text -> Maybe RequiresType
getTypeFromLine line = safeReq
  where
    (header, value) = splitOnceOn ": " line
    (pkgName, rawConstraint) = splitOnceOn " " $ T.strip value
    --
    isSafeName :: Text -> Bool
    isSafeName name = not $ "%{" `T.isInfixOf` name
    -- TODO: temporarily ignore names with macros, until we support expansion
    safeReq :: Maybe RequiresType
    safeReq = if isSafeName pkgName then req else Nothing
    --
    req :: Maybe RequiresType
    req = case header of
      "BuildRequires" -> Just . BuildRequires . RPMDependency pkgName $ buildConstraint rawConstraint
      "Requires" -> Just . RuntimeRequires . RPMDependency pkgName $ buildConstraint rawConstraint
      _ -> Nothing

buildDependencies :: [RequiresType] -> Dependencies
buildDependencies = foldr addDep blankDeps
  where
    addDep :: RequiresType -> Dependencies -> Dependencies
    addDep req deps = case req of
      BuildRequires dep -> deps {depBuildRequires = dep : depBuildRequires deps}
      RuntimeRequires dep -> deps {depRuntimeRequires = dep : depRuntimeRequires deps}
    blankDeps = Dependencies [] []

getSpecDeps :: Text -> Dependencies
getSpecDeps = buildDependencies . mapMaybe getTypeFromLine . T.lines
