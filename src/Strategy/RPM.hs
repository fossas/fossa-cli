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
import Data.Foldable (traverse_)
import Data.List (isSuffixOf)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
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

discover :: HasDiscover sig m => Path Abs Dir -> m ()
discover = walk $ \dir _ files ->
  let specs = filter (\f -> ".spec" `isSuffixOf` fileName f) files
      analyzeFile specFile = runSimpleStrategy "rpm-spec" RPMGroup $ fmap (mkProjectClosure dir) (analyze specFile)
   in traverse_ analyzeFile specs >> pure WalkContinue

analyze :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m (Graphing Dependency)
analyze specFile = do
  specFileText <- readContentsText specFile
  pure . buildGraph $ getSpecDeps specFileText

mkProjectClosure :: Path Abs Dir -> Graphing Dependency -> ProjectClosureBody
mkProjectClosure dir graph =
  ProjectClosureBody
    { bodyModuleDir = dir,
      bodyDependencies = dependencies,
      bodyLicenses = []
    }
  where
    dependencies =
      ProjectDependencies
        { dependenciesGraph = graph,
          dependenciesOptimal = Optimal,
          dependenciesComplete = NotComplete
        }

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
