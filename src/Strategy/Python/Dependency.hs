module Strategy.Python.Dependency (
  PythonDependency(..),
  PythonDependencySource(..),
  PythonDependencyType(..),
  PythonVersionConstraint(..),
  toDependency,
  fromPoetryDependency,
  fromPoetryDependencyPyProject,
  fromPDMDependency,
  fromPEP621Dependency,
  fromReq,
  versionConstraint,
  gitDependency,
  urlDependency,
  pathDependency,
  complexDependency,
  mapCategoryToEnvironment,
  determineEnvironmentFromDirect,
  fixHydratedEnvironments,
  parseDependencySpec,
) where

import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Text.URI qualified as URI
import Text.Megaparsec (ParseErrorBundle)
import Strategy.Python.DependencyParser (
  DependencySource(..),
  VersionConstraint(..),
  parseDependencySource,
  parseVersionConstraint
  )

import DepTypes (
  DepEnvironment(..),
  DepType (..),
  Dependency (..),
  VerConstraint (..),
 )
-- Import instances from DepTypes
import DepTypes()

import Strategy.Python.Util (
  Marker(..), 
  MarkerOp(..),
  Operator(..), 
  Req(..), 
  Version(..)
 )

import qualified Strategy.Python.PyProject.PyProjectToml as PyProject

data PythonDependencySource
  = FromPyProject
  | FromLockFile
  | FromBoth
  | FromSetupPy
  | FromRequirements
  deriving (Eq, Ord, Show)

data PythonDependencyType
  = SimpleVersion Text
  | VersionConstraint PythonVersionConstraint
  | GitDependency Text (Maybe Text) (Maybe Text) (Maybe Text)
  | PathDependency Text
  | URLDependency Text
  deriving (Eq, Ord, Show)

data PythonVersionConstraint
  = PythonVersionEq Text
  | PythonVersionGt Text
  | PythonVersionGtEq Text
  | PythonVersionLt Text
  | PythonVersionLtEq Text
  | PythonVersionComp Text
  | PythonVersionNot Text
  | PythonVersionArbitrary
  | PythonVersionAnd PythonVersionConstraint PythonVersionConstraint
  | PythonVersionOr PythonVersionConstraint PythonVersionConstraint
  deriving (Eq, Ord, Show)

data PythonDependency = PythonDependency
  { pyDepName :: Text
  , pyDepType :: PythonDependencyType
  , pyDepEnvironments :: Set.Set DepEnvironment
  , pyDepExtras :: [Text]
  , pyDepMarkers :: Maybe Marker
  , pyDepSource :: PythonDependencySource
  }
  deriving (Eq, Ord, Show)

toDependency :: PythonDependency -> Dependency
toDependency PythonDependency{pyDepName, pyDepType, pyDepEnvironments, pyDepMarkers, pyDepSource} =
  Dependency
    { dependencyType = depType
    , dependencyName = dependencyName
    , DepTypes.dependencyVersion = versionVal
    , dependencyLocations = locations
    , dependencyEnvironments = pyDepEnvironments
    , dependencyTags = maybe Map.empty toTags pyDepMarkers
    }
  where
    (depType, dependencyName, versionVal, locations) = case pyDepType of
      SimpleVersion v -> 
        -- Special case for the legacy source type from Poetry lock file
        -- where we need to store the URL in the locations
        case pyDepSource of
          FromLockFile -> 
            -- Check if this is a legacy source from Poetry
            if "https://" `Text.isPrefixOf` pyDepName || "http://" `Text.isPrefixOf` pyDepName
              then (PipType, pyDepName, convertVersionText v, [pyDepName]) 
              else (PipType, pyDepName, convertVersionText v, [])
          _ -> (PipType, pyDepName, convertVersionText v, [])
      
      VersionConstraint vc -> 
        (PipType, pyDepName, Just $ convertVersionConstraint vc, [])
      
      GitDependency url branch rev tag -> 
        let 
          baseUrl = url
          -- Add reference info if present (branch, tag, or rev)
          refInfo = case (branch, tag, rev) of
                      (Just b, _, _) -> "@" <> b
                      (_, Just t, _) -> "@" <> t
                      (_, _, Just r) -> "@" <> r
                      _ -> ""
          -- Get version from revision if available
          version = case rev of
                      Just r -> Just $ DepTypes.CEq r
                      Nothing -> Nothing
        in 
          (GitType, url, version, [baseUrl <> refInfo])
      
      PathDependency path -> 
        -- For path dependencies, preserve the path as both name and location
        -- Version value will come from a lock file if available
        -- Otherwise use the path as a version
        (UnresolvedPathType, path, Just (DepTypes.CEq path), [path])
      
      URLDependency url -> 
        -- For URL dependencies, the name has already been set correctly
        -- in fromReq to match the expected behavior (URL as the name)
        (URLType, pyDepName, Just (DepTypes.CURI url), [url])
    
    -- Convert simple version text to VerConstraint
    convertVersionText :: Text -> Maybe VerConstraint
    convertVersionText vt
      | vt == "*" = Nothing -- Wildcard matches anything
      | "^" `Text.isPrefixOf` vt = Just $ DepTypes.CCompatible (Text.drop 1 vt)
      | "~=" `Text.isPrefixOf` vt = Just $ DepTypes.CCompatible (Text.drop 2 vt)
      | ">=" `Text.isPrefixOf` vt = Just $ DepTypes.CGreaterOrEq (Text.drop 2 vt)
      | ">" `Text.isPrefixOf` vt = Just $ DepTypes.CGreater (Text.drop 1 vt)
      | "<=" `Text.isPrefixOf` vt = Just $ DepTypes.CLessOrEq (Text.drop 2 vt)
      | "<" `Text.isPrefixOf` vt = Just $ DepTypes.CLess (Text.drop 1 vt)
      | "==" `Text.isPrefixOf` vt = Just $ DepTypes.CEq (Text.drop 2 vt)
      | "!=" `Text.isPrefixOf` vt = Just $ DepTypes.CNot (Text.drop 2 vt)
      | Text.any (== ',') vt = -- parse comma-separated constraints
          let parts = Text.splitOn "," vt
              partConstraints = map convertVersionText parts
          in foldConstraints partConstraints
      | otherwise = Just $ DepTypes.CEq vt -- Default to equality
    
    -- Convert structured version constraint
    convertVersionConstraint :: PythonVersionConstraint -> VerConstraint
    convertVersionConstraint = \case
      PythonVersionEq v -> DepTypes.CEq v
      PythonVersionGt v -> DepTypes.CGreater v
      PythonVersionGtEq v -> DepTypes.CGreaterOrEq v
      PythonVersionLt v -> DepTypes.CLess v
      PythonVersionLtEq v -> DepTypes.CLessOrEq v
      PythonVersionComp v -> DepTypes.CCompatible v
      PythonVersionNot v -> DepTypes.CNot v
      PythonVersionArbitrary -> DepTypes.CEq "*"
      PythonVersionAnd c1 c2 -> DepTypes.CAnd (convertVersionConstraint c1) (convertVersionConstraint c2)
      PythonVersionOr c1 c2 -> DepTypes.COr (convertVersionConstraint c1) (convertVersionConstraint c2)
    
    -- Fold list of Maybe constraints into a single constraint
    foldConstraints :: [Maybe VerConstraint] -> Maybe VerConstraint
    foldConstraints [] = Nothing
    foldConstraints cs = 
      let validConstraints = [c | Just c <- cs]
      in if null validConstraints 
         then Nothing
         else Just $ foldr1 DepTypes.CAnd validConstraints
    
    -- Extract tags from marker
    toTags :: Marker -> Map.Map Text [Text]
    toTags = Map.fromListWith (++) . map (\(a, b) -> (a, [b])) . go
      where
        go (MarkerAnd a b) = go a ++ go b
        go (MarkerOr a b) = go a ++ go b
        go (MarkerExpr lhs op rhs) =
          case op of
            MarkerIn -> [(lhs, rhs)]
            MarkerNotIn -> [(lhs, "not (" <> rhs <> ")")]
            MarkerOperator _ -> [(lhs, rhs)]

fromPoetryDependency :: Text -> DepEnvironment -> PyProject.PoetryDependency -> PythonDependency
fromPoetryDependency name env = \case
  PyProject.PoetryTextVersion v -> 
    PythonDependency
      { pyDepName = name
      , pyDepType = SimpleVersion v
      , pyDepEnvironments = Set.singleton env
      , pyDepExtras = []
      , pyDepMarkers = Nothing
      , pyDepSource = FromPyProject
      }
  
  PyProject.PyProjectPoetryDetailedVersionDependencySpec d -> 
    PythonDependency
      { pyDepName = name
      , pyDepType = SimpleVersion (PyProject.poetryDependencyVersion d)
      , pyDepEnvironments = Set.singleton env
      , pyDepExtras = []
      , pyDepMarkers = Nothing
      , pyDepSource = FromPyProject
      }
  
  PyProject.PyProjectPoetryGitDependencySpec g -> 
    PythonDependency
      { pyDepName = name
      , pyDepType = GitDependency (PyProject.gitUrl g) (PyProject.gitBranch g) (PyProject.gitRev g) (PyProject.gitTag g)
      , pyDepEnvironments = Set.singleton env
      , pyDepExtras = []
      , pyDepMarkers = Nothing
      , pyDepSource = FromPyProject
      }
  
  PyProject.PyProjectPoetryPathDependencySpec p -> 
    PythonDependency
      { pyDepName = name
      , pyDepType = PathDependency (PyProject.sourcePath p)
      , pyDepEnvironments = Set.singleton env
      , pyDepExtras = []
      , pyDepMarkers = Nothing
      , pyDepSource = FromPyProject
      }
  
  PyProject.PyProjectPoetryUrlDependencySpec u -> 
    PythonDependency
      { pyDepName = name
      , pyDepType = URLDependency (PyProject.sourceUrl u)
      , pyDepEnvironments = Set.singleton env
      , pyDepExtras = []
      , pyDepMarkers = Nothing
      , pyDepSource = FromPyProject
      }

fromPoetryDependencyPyProject :: Text -> DepEnvironment -> PyProject.PoetryDependency -> PythonDependency
fromPoetryDependencyPyProject name env = \case
  PyProject.PoetryTextVersion v -> 
    PythonDependency
      { pyDepName = name
      , pyDepType = SimpleVersion v
      , pyDepEnvironments = Set.singleton env
      , pyDepExtras = []
      , pyDepMarkers = Nothing
      , pyDepSource = FromPyProject
      }
  
  PyProject.PyProjectPoetryDetailedVersionDependencySpec d -> 
    PythonDependency
      { pyDepName = name
      , pyDepType = SimpleVersion (PyProject.poetryDependencyVersion d)
      , pyDepEnvironments = Set.singleton env
      , pyDepExtras = []
      , pyDepMarkers = Nothing
      , pyDepSource = FromPyProject
      }
  
  PyProject.PyProjectPoetryGitDependencySpec g -> 
    PythonDependency
      { pyDepName = name
      , pyDepType = GitDependency (PyProject.gitUrl g) (PyProject.gitBranch g) (PyProject.gitRev g) (PyProject.gitTag g)
      , pyDepEnvironments = Set.singleton env
      , pyDepExtras = []
      , pyDepMarkers = Nothing
      , pyDepSource = FromPyProject
      }
  
  PyProject.PyProjectPoetryPathDependencySpec p -> 
    PythonDependency
      { pyDepName = name
      , pyDepType = PathDependency (PyProject.sourcePath p)
      , pyDepEnvironments = Set.singleton env
      , pyDepExtras = []
      , pyDepMarkers = Nothing
      , pyDepSource = FromPyProject
      }
  
  PyProject.PyProjectPoetryUrlDependencySpec u -> 
    PythonDependency
      { pyDepName = name
      , pyDepType = URLDependency (PyProject.sourceUrl u)
      , pyDepEnvironments = Set.singleton env
      , pyDepExtras = []
      , pyDepMarkers = Nothing
      , pyDepSource = FromPyProject
      }

fromPDMDependency :: DepEnvironment -> Req -> PythonDependency
fromPDMDependency env req = fromReq env req FromPyProject

fromPEP621Dependency :: DepEnvironment -> Req -> PythonDependency
fromPEP621Dependency env req = fromReq env req FromPyProject

fromReq :: DepEnvironment -> Req -> PythonDependencySource -> PythonDependency
fromReq env req source = 
  case req of
    NameReq nm extras versions marker ->
      PythonDependency 
        { pyDepName = nm
        , pyDepType = case versions of
            Nothing -> SimpleVersion "*" -- No version constraint
            Just vs -> VersionConstraint $ versionsToConstraint vs
        , pyDepEnvironments = Set.singleton env
        , pyDepExtras = fromMaybe [] extras
        , pyDepMarkers = marker
        , pyDepSource = source
        }
    
    UrlReq _ extras uri marker ->
      let url = URI.render uri
      in PythonDependency
        { pyDepName = url -- Always use the URL as the name for all URL dependencies to match master branch behavior
        , pyDepType = URLDependency url
        , pyDepEnvironments = Set.singleton env
        , pyDepExtras = fromMaybe [] extras
        , pyDepMarkers = marker
        , pyDepSource = source
        }

versionConstraint :: Text -> Maybe VerConstraint
versionConstraint vt = 
  case parseVersionConstraint vt of
    Right vc -> Just $ convertVersionConstraint vc
    Left _ -> 
      -- Fallback for backward compatibility in case parsing fails
      Just $ DepTypes.CEq vt
  where
    -- Convert our parsed version constraint to the DepTypes.VerConstraint type
    convertVersionConstraint :: VersionConstraint -> VerConstraint
    convertVersionConstraint = \case
      VersionEq v -> DepTypes.CEq v
      VersionGt v -> DepTypes.CGreater v
      VersionGtEq v -> DepTypes.CGreaterOrEq v
      VersionLt v -> DepTypes.CLess v
      VersionLtEq v -> DepTypes.CLessOrEq v
      VersionCompatible v -> DepTypes.CCompatible v
      VersionNot v -> DepTypes.CNot v
      VersionWildcard -> DepTypes.CEq "*" -- Match test expectations
      VersionAnd v1 v2 -> DepTypes.CAnd (convertVersionConstraint v1) (convertVersionConstraint v2)

gitDependency :: Text -> Maybe Dependency
gitDependency text = 
  case parseDependencySource text of
    Right (GitSource url reference) ->
      let
        -- Extract package name from the URL path
        packageName = case Text.splitOn "/" (Text.dropWhile (/= '/') url) of
                        [] -> "unknown"
                        parts -> maybe "unknown" (Text.replace ".git" "") (lastMaybe parts)
      in Just $ Dependency
           { dependencyType = GitType
           , dependencyName = packageName
           , DepTypes.dependencyVersion = Nothing
           , dependencyLocations = [url <> maybe "" (\ref -> "@" <> ref) reference]
           , dependencyEnvironments = mempty
           , dependencyTags = mempty
           }
    _ -> Nothing
  where
    lastMaybe :: [a] -> Maybe a
    lastMaybe [] = Nothing
    lastMaybe xs = Just $ last xs

urlDependency :: Text -> Maybe Dependency
urlDependency text =
  case parseDependencySource text of
    Right (HttpSource url) ->
      let
        -- Extract the filename from the URL to use as dependency name
        fileName = case Text.splitOn "/" url of
                     [] -> "unknown"
                     parts -> case lastMaybe parts of
                                Nothing -> "unknown"
                                Just fn -> case Text.splitOn "." fn of
                                             [] -> fn
                                             (firstPart:_) -> firstPart
      in Just $ Dependency
           { dependencyType = URLType
           , dependencyName = fileName
           , DepTypes.dependencyVersion = Just $ DepTypes.CURI url
           , dependencyLocations = [url]
           , dependencyEnvironments = mempty
           , dependencyTags = mempty
           }
    _ -> Nothing
  where
    lastMaybe :: [a] -> Maybe a
    lastMaybe [] = Nothing
    lastMaybe xs = Just $ last xs

pathDependency :: Text -> Maybe Dependency
pathDependency text =
  case parseDependencySource text of
    Right (FileSource path) ->
      let
        -- Extract the directory name to use as dependency name
        dirName = case Text.splitOn "/" path of
                    [] -> "unknown"
                    parts -> maybe "unknown" id (lastMaybe parts)
      in Just $ Dependency
           { dependencyType = UnresolvedPathType
           , dependencyName = dirName
           , DepTypes.dependencyVersion = Nothing
           , dependencyLocations = [path]
           , dependencyEnvironments = mempty
           , dependencyTags = mempty
           }
    _ -> Nothing
  where
    lastMaybe :: [a] -> Maybe a
    lastMaybe [] = Nothing
    lastMaybe xs = Just $ last xs

complexDependency :: Text -> Text -> Maybe Dependency
complexDependency name spec =
  case parseDependencySource spec of
    -- If it's a specialized dependency type, use the specific parser for it
    Right (GitSource _ _) -> gitDependency spec
    Right (HttpSource _) -> urlDependency spec
    Right (FileSource _) -> pathDependency spec
    Right (SimpleSource ver) -> 
      -- If it's a simple version specification, create a regular dependency
      Just $ Dependency
        { dependencyType = PipType
        , dependencyName = name
        , DepTypes.dependencyVersion = versionConstraint ver
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = mempty
        }
    -- If parsing fails, try to treat it as a simple version constraint
    Left _ -> 
      Just $ Dependency
        { dependencyType = PipType
        , dependencyName = name
        , DepTypes.dependencyVersion = versionConstraint spec
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = mempty
        }

versionsToConstraint :: [Version] -> PythonVersionConstraint
versionsToConstraint [] = PythonVersionArbitrary
versionsToConstraint [v] = versionToConstraint v
versionsToConstraint (v:vs) = PythonVersionAnd (versionToConstraint v) (versionsToConstraint vs)

versionToConstraint :: Version -> PythonVersionConstraint
versionToConstraint (Version op ver) = 
  case op of
    OpCompatible -> PythonVersionComp ver
    OpEq -> PythonVersionEq ver
    OpNot -> PythonVersionNot ver
    OpLtEq -> PythonVersionLtEq ver
    OpGtEq -> PythonVersionGtEq ver
    OpLt -> PythonVersionLt ver
    OpGt -> PythonVersionGt ver
    OpArbitrary -> PythonVersionArbitrary

-- This function provides consistent environment mapping across different package managers
mapCategoryToEnvironment :: Text -> DepEnvironment
mapCategoryToEnvironment category = case category of
  "dev" -> DepTypes.EnvDevelopment
  "development" -> DepTypes.EnvDevelopment
  "main" -> DepTypes.EnvProduction
  "prod" -> DepTypes.EnvProduction
  "production" -> DepTypes.EnvProduction
  "test" -> DepTypes.EnvTesting
  other -> DepTypes.EnvOther other

-- If the package is in the direct production dependencies, mark as production
-- Otherwise, consider it a development dependency
determineEnvironmentFromDirect :: Bool -> Set.Set DepEnvironment
determineEnvironmentFromDirect isProductionDirect =
  if isProductionDirect 
  then Set.singleton DepTypes.EnvProduction 
  else Set.singleton DepTypes.EnvDevelopment

-- When a dependency belongs to both production and development environments,
-- prioritize production over development.
--
-- After environment hydration, transitive dependencies may inherit environments from
-- multiple direct dependencies. This function implements the policy that production
-- dependencies take precedence over development dependencies.
--
-- Model assumptions:
-- 1. Environment hydration has already been performed
-- 2. A dependency is considered "production" if it's a direct production dependency OR
--    if it's a transitive dependency of a direct production dependency
-- 3. Each dependency should have exactly one environment (either production or development)
--
-- Example:
--   Direct dependencies:
--     requests (production)
--     pytest (development)
--   
--   Transitive dependencies:
--     urllib3 (used by both requests and pytest)
--     
--   After hydration, urllib3 would have both production and development environments.
--   This function will keep only the production environment for urllib3.
fixHydratedEnvironments :: Dependency -> Dependency
fixHydratedEnvironments d
  | Set.member DepTypes.EnvProduction envs && Set.member DepTypes.EnvDevelopment envs = 
      d{dependencyEnvironments = Set.singleton DepTypes.EnvProduction}
  | otherwise = d
  where
    envs = dependencyEnvironments d

-- This is a convenience function that wraps parseDependencySource
parseDependencySpec :: Text -> Either (ParseErrorBundle Text Void) DependencySource
parseDependencySpec = parseDependencySource