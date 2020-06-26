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

import Control.Effect.Error
import Control.Effect.Diagnostics
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import DepTypes
import Discovery.Walk
import Effect.ReadFS
import Graphing (Graphing)
import qualified Graphing as G
import Prologue
import Types

newtype SpecFileLabel
  = RequiresType DepEnvironment
  deriving (Eq, Ord, Show, Generic)

data RPMDependency
  = RPMDependency
      { rpmDepName :: Text,
        rpmConstraint :: Maybe VerConstraint
      }
  deriving (Eq, Ord, Show, Generic)

data RequiresType
  = BuildRequires RPMDependency
  | RuntimeRequires RPMDependency
  deriving (Eq, Ord, Show, Generic)

data Dependencies
  = Dependencies
      { depBuildRequires :: [RPMDependency],
        depRuntimeRequires :: [RPMDependency]
      }
  deriving (Eq, Ord, Show, Generic)

discover :: HasDiscover sig m => Path Abs Dir -> m ()
discover = walk $ \dir _ files ->
  let specs = find (\f -> ".spec" `isSuffixOf` fileName f) files
      analzyeFile specFile = runSimpleStrategy "rpm-spec" RPMGroup $ fmap (mkProjectClosure dir) (analyze specFile)
   in traverse_ analzyeFile specs >> pure WalkSkipAll

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
buildGraph Dependencies {..} = G.gmap toDependency $ G.fromList depBuildRequires

buildConstraint :: Text -> Maybe VerConstraint
buildConstraint tail = constraint
  where
    (comparatorStr, rawVersion) = splitOnceOn " " $ T.strip tail
    version = T.strip rawVersion
    constraint = case T.strip comparatorStr of
      "<=" -> Just $ CLessOrEq version
      "<" -> Just $ CLess version
      ">=" -> Just $ CGreaterOrEq version
      ">" -> Just $ CGreater version
      "=" -> Just $ CEq version
      "==" -> Just $ CEq version
      _ -> Nothing

splitOnceOn :: Text -> Text -> (Text, Text)
splitOnceOn needle haystack = (head, strippedTail)
  where
    len = T.length needle
    (head, tail) = T.breakOn needle haystack
    strippedTail = T.drop len tail

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
