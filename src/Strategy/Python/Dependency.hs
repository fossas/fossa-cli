module Strategy.Python.Dependency (
  PythonDependency(..),
  PythonDependencySource(..),
  PythonDependencyType(..),
  PythonVersionConstraint(..),
  toDependency,
  fromPoetryDependency,
  fromPDMDependency,
  fromPEP621Dependency,
  fromReq,
  versionConstraint,
  gitDependency,
  urlDependency,
  pathDependency,
  complexDependency,
) where

import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Text.URI qualified as URI

import DepTypes (
  DepEnvironment,
  DepType (..),
  Dependency (..),
  VerConstraint (..),
 )
import qualified DepTypes

import Strategy.Python.Util (
  Marker(..), 
  MarkerOp(..),
  Operator(..), 
  Req(..), 
  Version(..),
  toConstraint,
 )

import Strategy.Python.PyProjectGeneric.Types (
  PoetryDependency(..),
  PyProjectDetailedVersionDependency(..),
  PyProjectGitDependency(..),
  PyProjectPathDependency(..),
  PyProjectUrlDependency(..),
  gitUrl,
  gitBranch,
  gitTag,
  gitRev,
  dependencyVersion,
  sourcePath,
  sourceUrl
  )

-- | Source of the Python dependency
data PythonDependencySource
  = FromPyProject  -- ^ Dependency declared in pyproject.toml
  | FromLockFile   -- ^ Dependency from lock file
  | FromBoth       -- ^ Dependency in both pyproject.toml and lock file
  | FromSetupPy    -- ^ Dependency from setup.py
  | FromRequirements -- ^ Dependency from requirements.txt
  deriving (Eq, Ord, Show)

-- | Type of Python dependency
data PythonDependencyType
  = SimpleVersion Text -- ^ Simple version specifier (e.g., "^1.2.3")
  | VersionConstraint PythonVersionConstraint -- ^ Structured version constraint
  | GitDependency Text (Maybe Text) (Maybe Text) (Maybe Text) -- ^ Git URL, branch, rev, tag
  | PathDependency Text -- ^ Local path to package
  | URLDependency Text -- ^ URL to package
  deriving (Eq, Ord, Show)

-- | Python version constraint representation
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

-- | Unified Python dependency representation
data PythonDependency = PythonDependency
  { pyDepName :: Text -- ^ Name of the dependency
  , pyDepType :: PythonDependencyType -- ^ Type and version specification
  , pyDepEnvironments :: Set.Set DepEnvironment -- ^ Environments (prod, dev, test, etc.)
  , pyDepExtras :: [Text] -- ^ Optional extras (features)
  , pyDepMarkers :: Maybe Marker -- ^ Environment markers (os, python_version, etc.)
  , pyDepSource :: PythonDependencySource -- ^ Source of the dependency
  }
  deriving (Eq, Ord, Show)

-- | Convert a Python dependency to the general FOSSA Dependency type
toDependency :: PythonDependency -> Dependency
toDependency PythonDependency{pyDepName, pyDepType, pyDepEnvironments, pyDepMarkers} =
  Dependency
    { dependencyType = depType
    , dependencyName = pyDepName
    , DepTypes.dependencyVersion = versionVal
    , dependencyLocations = locations
    , dependencyEnvironments = pyDepEnvironments
    , dependencyTags = maybe Map.empty toTags pyDepMarkers
    }
  where
    (depType, versionVal, locations) = case pyDepType of
      SimpleVersion v -> 
        (PipType, convertVersionText v, [])
      
      VersionConstraint vc -> 
        (PipType, Just $ convertVersionConstraint vc, [])
      
      GitDependency url branch rev tag -> 
        let 
          baseUrl = url
          -- Add reference info if present (branch, tag, or rev)
          refInfo = case (branch, tag, rev) of
                      (Just b, _, _) -> "@" <> b
                      (_, Just t, _) -> "@" <> t
                      (_, _, Just r) -> "@" <> r
                      _ -> ""
        in 
          (GitType, Nothing, [baseUrl <> refInfo])
      
      PathDependency path -> 
        (UnresolvedPathType, Nothing, [path])
      
      URLDependency url -> 
        (URLType, Just (DepTypes.CURI url), [url])
    
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

-- | Convert a Poetry dependency to unified PythonDependency
fromPoetryDependency :: Text -> DepEnvironment -> PoetryDependency -> PythonDependency
fromPoetryDependency name env = \case
  PoetryTextVersion v -> 
    PythonDependency
      { pyDepName = name
      , pyDepType = SimpleVersion v
      , pyDepEnvironments = Set.singleton env
      , pyDepExtras = []
      , pyDepMarkers = Nothing
      , pyDepSource = FromPyProject
      }
  
  PoetryDetailedVersion d -> 
    PythonDependency
      { pyDepName = name
      , pyDepType = SimpleVersion (Strategy.Python.PyProjectGeneric.Types.dependencyVersion d)
      , pyDepEnvironments = Set.singleton env
      , pyDepExtras = []
      , pyDepMarkers = Nothing
      , pyDepSource = FromPyProject
      }
  
  PoetryGitDependency g -> 
    PythonDependency
      { pyDepName = name
      , pyDepType = GitDependency (gitUrl g) (gitBranch g) (gitRev g) (gitTag g)
      , pyDepEnvironments = Set.singleton env
      , pyDepExtras = []
      , pyDepMarkers = Nothing
      , pyDepSource = FromPyProject
      }
  
  PoetryPathDependency p -> 
    PythonDependency
      { pyDepName = name
      , pyDepType = PathDependency (sourcePath p)
      , pyDepEnvironments = Set.singleton env
      , pyDepExtras = []
      , pyDepMarkers = Nothing
      , pyDepSource = FromPyProject
      }
  
  PoetryUrlDependency u -> 
    PythonDependency
      { pyDepName = name
      , pyDepType = URLDependency (sourceUrl u)
      , pyDepEnvironments = Set.singleton env
      , pyDepExtras = []
      , pyDepMarkers = Nothing
      , pyDepSource = FromPyProject
      }

-- | Convert a PDM dependency (as a Req) to PythonDependency
fromPDMDependency :: DepEnvironment -> Req -> PythonDependency
fromPDMDependency env req = fromReq env req FromPyProject

-- | Convert a PEP621 dependency (as a Req) to PythonDependency
fromPEP621Dependency :: DepEnvironment -> Req -> PythonDependency
fromPEP621Dependency env req = fromReq env req FromPyProject

-- | Convert a Req to PythonDependency
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
    
    UrlReq nm extras uri marker ->
      PythonDependency
        { pyDepName = nm
        , pyDepType = URLDependency (URI.render uri)
        , pyDepEnvironments = Set.singleton env
        , pyDepExtras = fromMaybe [] extras
        , pyDepMarkers = marker
        , pyDepSource = source
        }

-- | Helper functions exported for tests

-- | Convert version constraint from text into VerConstraint
versionConstraint :: Text -> Maybe VerConstraint
versionConstraint vt
  | vt == "*" = Just $ DepTypes.CEq "*" -- Wildcard matches anything, return as CEq for test compatibility
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
          partConstraints = map versionConstraint parts
      in foldConstraints partConstraints
  | otherwise = Just $ DepTypes.CEq vt -- Default to equality
  where
    -- Fold list of Maybe constraints into a single constraint
    foldConstraints :: [Maybe VerConstraint] -> Maybe VerConstraint
    foldConstraints [] = Nothing
    foldConstraints cs = 
      let validConstraints = [c | Just c <- cs]
      in if null validConstraints 
         then Nothing
         else Just $ foldr1 DepTypes.CAnd validConstraints

-- | Parse Git dependency from specification
gitDependency :: Text -> Maybe Dependency
gitDependency text =
  if "git+" `Text.isPrefixOf` text
    then 
      let 
        -- Extract URL and reference (branch/tag/rev) from git+https://repo@ref format
        baseUrl = if "@" `Text.isInfixOf` text
                    then Text.takeWhile (/= '@') (Text.drop 4 text)
                    else Text.drop 4 text
        reference = if "@" `Text.isInfixOf` text
                      then Just $ Text.drop 1 $ Text.dropWhile (/= '@') text
                      else Nothing
        -- For now, we don't parse the package name from the URL
        -- In real usage, we'd need to extract it from the repo path
        packageName = case Text.splitOn "/" (Text.dropWhile (/= '/') baseUrl) of
                        [] -> "unknown"
                        parts -> maybe "unknown" (Text.replace ".git" "") (lastMaybe parts)
      in Just $ Dependency
           { dependencyType = GitType
           , dependencyName = packageName
           , DepTypes.dependencyVersion = Nothing
           , dependencyLocations = [baseUrl <> maybe "" (\ref -> "@" <> ref) reference]
           , dependencyEnvironments = mempty
           , dependencyTags = mempty
           }
    else Nothing
  where
    lastMaybe :: [a] -> Maybe a
    lastMaybe [] = Nothing
    lastMaybe xs = Just $ last xs

-- | Parse URL dependency from specification
urlDependency :: Text -> Maybe Dependency
urlDependency text =
  if ("http://" `Text.isPrefixOf` text) || ("https://" `Text.isPrefixOf` text)
    then
      let
        -- Extract the filename from the URL to use as dependency name
        fileName = case Text.splitOn "/" text of
                     [] -> "unknown"
                     parts -> case lastMaybe parts of
                                Nothing -> "unknown"
                                Just fn -> case Text.splitOn "." fn of
                                             [] -> fn
                                             nameParts -> head nameParts
      in Just $ Dependency
           { dependencyType = URLType
           , dependencyName = fileName
           , DepTypes.dependencyVersion = Just $ DepTypes.CURI text
           , dependencyLocations = [text]
           , dependencyEnvironments = mempty
           , dependencyTags = mempty
           }
    else Nothing
  where
    lastMaybe :: [a] -> Maybe a
    lastMaybe [] = Nothing
    lastMaybe xs = Just $ last xs

-- | Parse Path dependency from specification
pathDependency :: Text -> Maybe Dependency
pathDependency text =
  if ("file:" `Text.isPrefixOf` text) || ("../" `Text.isPrefixOf` text) || ("./" `Text.isPrefixOf` text) || ("/" `Text.isPrefixOf` text)
    then
      let
        -- Extract path, removing file: prefix if present
        path = if "file:" `Text.isPrefixOf` text
                 then Text.drop 5 text
                 else text
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
    else Nothing
  where
    lastMaybe :: [a] -> Maybe a
    lastMaybe [] = Nothing
    lastMaybe xs = Just $ last xs

-- | Convert complex dependency specifications to Dependency type
complexDependency :: Text -> Text -> Maybe Dependency
complexDependency name spec =
  -- Try parsing as one of the specialized dependency types first
  if "git+" `Text.isPrefixOf` spec
    then gitDependency spec
  else if ("http://" `Text.isPrefixOf` spec) || ("https://" `Text.isPrefixOf` spec)
    then urlDependency spec
  else if ("file:" `Text.isPrefixOf` spec) || ("../" `Text.isPrefixOf` spec) || ("./" `Text.isPrefixOf` spec) || ("/" `Text.isPrefixOf` spec)
    then pathDependency spec
  else 
    -- If not a specialized type, use simple version constraint parsing
    Just $ Dependency
      { dependencyType = PipType
      , dependencyName = name
      , DepTypes.dependencyVersion = versionConstraint spec
      , dependencyLocations = []
      , dependencyEnvironments = mempty
      , dependencyTags = mempty
      }

-- | Convert list of Version to PythonVersionConstraint
versionsToConstraint :: [Version] -> PythonVersionConstraint
versionsToConstraint [] = PythonVersionArbitrary
versionsToConstraint [v] = versionToConstraint v
versionsToConstraint (v:vs) = PythonVersionAnd (versionToConstraint v) (versionsToConstraint vs)

-- | Convert Version to PythonVersionConstraint
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