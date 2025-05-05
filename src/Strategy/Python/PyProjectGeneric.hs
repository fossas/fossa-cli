module Strategy.Python.PyProjectGeneric
  ( PyProjectGeneric (..)
  , PyProjectType (..)
  , discover
  , analyze
  , parseGenericPyProject
  , extractDependencies
  , extractPoetryDependencies
  , extractPDMDependencies
  , extractPEP621Dependencies
  , parseVersionConstraint
  , parseGitDependency
  , parseUrlDependency
  , parsePathDependency
  , parseComplexDependency
  , PyProjectProject (..)
  ) where

import Control.Applicative ((<|>))
import Control.Effect.Diagnostics (Diagnostics, context, fatalText, recover)
import Control.Effect.Reader (Reader)
import Data.Aeson (ToJSON, Value)
import Data.Foldable (foldl')
import Data.Map qualified as Map
import Data.Maybe (isJust, fromMaybe, mapMaybe, maybeToList)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import DepTypes
  ( DepEnvironment (EnvDevelopment, EnvProduction)
  , DepType (PipType, URLType, GitType, UnresolvedPathType)
  , Dependency (..)
  , VerConstraint (..)
  )
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk
  ( WalkStep (WalkContinue, WalkSkipSome)
  , findFileNamed
  , walkWithFilters'
  )
import Effect.ReadFS (Has, ReadFS, readContentsToml)
import GHC.Generics (Generic)
import Graphing (Graphing, directs)
import Path (Abs, Dir, File, Path)
import Strategy.Python.PyProjectGeneric.Types
  ( PyProjectGeneric (..)
  , PyProjectMetadata (..)
  , PyProjectType (..)
  , PyProjectPoetry (..)
  , PyProjectPDM (..)
  , PoetryDependency (..)
  , PyProjectDetailedVersionDependency (..)
  , PyProjectGitDependency (..)
  , PyProjectPathDependency (..)
  , PyProjectUrlDependency (..)
  , detectProjectType
  , dependencyVersion
  , gitUrl
  , sourcePath
  , sourceUrl
  )
import Strategy.Python.Util (Req (..), reqToDependency, toConstraint, Version (..), Operator (..))
import Toml qualified
import Types
  ( DependencyResults (..)
  , DiscoveredProject (..)
  , DiscoveredProjectType (GenericPyProjectType)
  , GraphBreadth (Partial)
  )
import Text.URI qualified as URI

-- | Project discovery for PyProject.toml files
discover ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  , Has (Reader AllFilters) sig m
  ) =>
  Path Abs Dir ->
  m [DiscoveredProject PyProjectProject]
discover = simpleDiscover findProjects mkProject GenericPyProjectType

-- | Find PyProject.toml files
findProjects :: 
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  , Has (Reader AllFilters) sig m
  ) => 
  Path Abs Dir -> 
  m [PyProjectProject]
findProjects = walkWithFilters' $ \dir _ files -> do
  let pyprojectFile = findFileNamed "pyproject.toml" files
  case pyprojectFile of
    Just pyproject -> pure ([PyProjectProject pyproject dir], WalkSkipSome [".venv"])
    Nothing -> pure ([], WalkContinue)

-- | Data type representing a PyProject.toml project
data PyProjectProject = PyProjectProject
  { pyprojectFile :: Path Abs File
  , projectDir :: Path Abs Dir
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON PyProjectProject

-- | Create a DiscoveredProject from PyProjectProject
mkProject :: PyProjectProject -> DiscoveredProject PyProjectProject
mkProject project =
  DiscoveredProject
    { Types.projectType = GenericPyProjectType
    , projectBuildTargets = mempty
    , projectPath = projectDir project
    , projectData = project
    }

-- | Parse a PyProject.toml file into a PyProjectGeneric
parseGenericPyProject ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  ) =>
  Path Abs File ->
  m PyProjectGeneric
parseGenericPyProject = readContentsToml

-- | Analyze a PyProject.toml file and extract dependencies
analyze ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  ) =>
  Path Abs File ->
  m DependencyResults
analyze pyprojectToml = do
  pyproject <- parseGenericPyProject pyprojectToml
  
  -- Extract dependencies based on project type
  let dependencies = extractDependencies pyproject
  
  pure $
    DependencyResults
      { dependencyGraph = directs dependencies
      , dependencyGraphBreadth = Partial
      , dependencyManifestFiles = [pyprojectToml]
      }

-- | Extract dependencies from a PyProjectGeneric based on its project type
extractDependencies :: PyProjectGeneric -> [Dependency]
extractDependencies pyproject = 
  case Strategy.Python.PyProjectGeneric.Types.projectType pyproject of
    PoetryProject -> extractPoetryDependencies pyproject
    PDMProject -> extractPDMDependencies pyproject
    PEP621Project -> extractPEP621Dependencies pyproject
    UnknownProject -> [] -- Unknown project type, no dependencies

-- | Extract dependencies from a Poetry project
extractPoetryDependencies :: PyProjectGeneric -> [Dependency]
extractPoetryDependencies pyproject =
  case poetrySection pyproject of
    Nothing -> []
    Just poetry -> 
      let prodDeps = extractPoetryDeps EnvProduction (poetryDependencies poetry)
          devDeps = extractPoetryDeps EnvDevelopment (poetryDevDependencies poetry)
      in prodDeps ++ devDeps

-- | Extract Poetry dependencies from a dependency map
extractPoetryDeps :: DepEnvironment -> Map.Map Text PoetryDependency -> [Dependency]
extractPoetryDeps env = Map.foldrWithKey (\name dep acc -> poetryDepToDependency env name dep : acc) []

-- | Convert a Poetry dependency to a Dependency
poetryDepToDependency :: DepEnvironment -> Text -> PoetryDependency -> Dependency
poetryDepToDependency env name dep =
  let 
    (depType, version, locations) = case dep of
      PoetryTextVersion v -> 
        -- Handle different version constraint formats
        (PipType, parseVersionConstraint v, mempty)
      
      PoetryDetailedVersion d -> 
        -- Handle detailed version specification (version field in a table)
        (PipType, parseVersionConstraint (Strategy.Python.PyProjectGeneric.Types.dependencyVersion d), mempty)
      
      PoetryGitDependency g -> 
        -- Handle Git dependency with optional branch/tag/rev
        let 
          baseUrl = Strategy.Python.PyProjectGeneric.Types.gitUrl g
          -- Add reference info if present (branch, tag, or rev)
          refInfo = case (gitBranch g, gitTag g, gitRev g) of
                      (Just branch, _, _) -> "@" <> branch
                      (_, Just tag, _) -> "@" <> tag
                      (_, _, Just rev) -> "@" <> rev
                      _ -> ""
        in 
          (GitType, Nothing, [baseUrl <> refInfo])
      
      PoetryPathDependency p -> 
        -- Handle path dependency
        (UnresolvedPathType, Nothing, [Strategy.Python.PyProjectGeneric.Types.sourcePath p])
      
      PoetryUrlDependency u -> 
        -- Handle URL dependency
        (URLType, Just (CURI (Strategy.Python.PyProjectGeneric.Types.sourceUrl u)), [Strategy.Python.PyProjectGeneric.Types.sourceUrl u])
  in
    Dependency
      { dependencyType = depType
      , dependencyName = name
      , DepTypes.dependencyVersion = version
      , dependencyLocations = locations
      , dependencyEnvironments = Set.singleton env
      , dependencyTags = mempty
      }

-- | Extract dependencies from a PDM project
extractPDMDependencies :: PyProjectGeneric -> [Dependency]
extractPDMDependencies pyproject =
  -- Extract from PEP 621 metadata (project section)
  let pep621Deps = extractPEP621Dependencies pyproject
      -- Extract PDM-specific dev dependencies
      pdmDevDeps = case pdmSection pyproject of
        Nothing -> []
        Just pdm -> case pdmDevDependencies pdm of
          Nothing -> []
          Just devDeps -> 
            -- We need to handle more complex dependency specifications
            concatMap (parsePDMDeps EnvDevelopment) (Map.toList devDeps)
  in pep621Deps ++ pdmDevDeps

-- | Get name from Req type (simplified version of depName function from Strategy.Python.Util)
getReqName :: Req -> Text
getReqName (NameReq nm _ _ _) = nm
getReqName (UrlReq nm _ _ _) = nm

-- | Parse PDM-style dependencies list into Dependency objects
parsePDMDeps :: DepEnvironment -> (Text, [Req]) -> [Dependency]
parsePDMDeps env (category, reqs) = 
  map (addEnvironment env . enhancePDMDependency) reqs
  where
    enhancePDMDependency :: Req -> Dependency
    enhancePDMDependency req =
      let 
        name = getReqName req
        -- Get the base dependency from reqToDependency
        baseDep = reqToDependency req
        -- Extract complex dependency info (git, path, url) based on content
        updatedDep = case name of
          n | "git+" `Text.isPrefixOf` n -> 
              -- Handle git URL dependency format
              fromMaybe baseDep (parseGitDependency n)
          n | "file:" `Text.isPrefixOf` n ->
              -- Handle path dependency format
              fromMaybe baseDep (parsePathDependency n)
          n | ("http://" `Text.isPrefixOf` n) || ("https://" `Text.isPrefixOf` n) ->
              -- Handle URL dependency format
              fromMaybe baseDep (parseUrlDependency n)
          _ -> 
              -- For plain version strings, use reqToDependency directly which handles version constraints
              baseDep
      in updatedDep

-- | Extract dependencies from a PEP 621 project
extractPEP621Dependencies :: PyProjectGeneric -> [Dependency]
extractPEP621Dependencies pyproject =
  case projectMetadata pyproject of
    Nothing -> []
    Just metadata ->
      let 
        -- Production dependencies
        prodDeps = case projectDependencies metadata of
          Nothing -> []
          Just reqs -> map (addEnvironment EnvProduction) $ parsePEP621Deps reqs
        
        -- Optional dependencies (treated as development dependencies)
        optDeps = case projectOptionalDependencies metadata of
          Nothing -> []
          Just optDepMap -> 
            concatMap (parsePEP621CategoryDeps EnvDevelopment) (Map.toList optDepMap)
      in
        prodDeps ++ optDeps

-- | Parse PEP 621 style dependencies list
parsePEP621Deps :: [Req] -> [Dependency]
parsePEP621Deps = map enhancePEP621Dependency
  where
    enhancePEP621Dependency :: Req -> Dependency
    enhancePEP621Dependency req =
      let 
        name = getReqName req
        -- Get the base dependency from reqToDependency
        baseDep = reqToDependency req
        -- Extract complex dependency info (git, path, url) based on content
        updatedDep = case name of
          n | "git+" `Text.isPrefixOf` n -> 
              -- Handle git URL dependency format
              fromMaybe baseDep (parseGitDependency n)
          n | "file:" `Text.isPrefixOf` n ->
              -- Handle path dependency format
              fromMaybe baseDep (parsePathDependency n)
          n | ("http://" `Text.isPrefixOf` n) || ("https://" `Text.isPrefixOf` n) ->
              -- Handle URL dependency format
              fromMaybe baseDep (parseUrlDependency n)
          _ -> 
              -- For plain version strings, keep the base dependency
              baseDep
      in updatedDep

-- | Parse PEP 621 style optional dependencies by category
parsePEP621CategoryDeps :: DepEnvironment -> (Text, [Req]) -> [Dependency]
parsePEP621CategoryDeps env (category, reqs) = 
  map (addEnvironment env) $ parsePEP621Deps reqs

-- | Add environment to a dependency
addEnvironment :: DepEnvironment -> Dependency -> Dependency
addEnvironment env dep = dep { dependencyEnvironments = Set.singleton env }

-- | Parse version constraint from text
parseVersion :: Text -> Version
parseVersion text =
  -- Simple parser for common version formats
  if "^" `Text.isPrefixOf` text
    then Version OpCompatible (Text.drop 1 text)
    else if ">=" `Text.isPrefixOf` text
      then Version OpGtEq (Text.drop 2 text)
      else if "<=" `Text.isPrefixOf` text
        then Version OpLtEq (Text.drop 2 text)
      else if ">" `Text.isPrefixOf` text
        then Version OpGt (Text.drop 1 text)
      else if "<" `Text.isPrefixOf` text
        then Version OpLt (Text.drop 1 text)
      else if "==" `Text.isPrefixOf` text
        then Version OpEq (Text.drop 2 text)
      else if "~=" `Text.isPrefixOf` text
        then Version OpCompatible (Text.drop 2 text)
      else if "!=" `Text.isPrefixOf` text
        then Version OpNot (Text.drop 2 text)
      else if "*" == text
        then Version OpArbitrary text
      else Version OpEq text

-- | Parse version constraint from text into VerConstraint
parseVersionConstraint :: Text -> Maybe VerConstraint
parseVersionConstraint text =
  if "," `Text.isInfixOf` text
    then 
      -- Handle combined constraints like ">=1.0.0,<2.0.0"
      let parts = Text.splitOn "," text
          versions = map parseVersion parts
      in Just $ toConstraint versions
    else Just $ toConstraint [parseVersion text]

-- | Parse Git dependency from specification
parseGitDependency :: Text -> Maybe Dependency
parseGitDependency text =
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
parseUrlDependency :: Text -> Maybe Dependency
parseUrlDependency text =
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
           , DepTypes.dependencyVersion = Just $ CURI text
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
parsePathDependency :: Text -> Maybe Dependency
parsePathDependency text =
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
parseComplexDependency :: Text -> Text -> Maybe Dependency
parseComplexDependency name spec =
  -- Try parsing as one of the specialized dependency types first
  if "git+" `Text.isPrefixOf` spec
    then parseGitDependency spec
  else if ("http://" `Text.isPrefixOf` spec) || ("https://" `Text.isPrefixOf` spec)
    then parseUrlDependency spec
  else if ("file:" `Text.isPrefixOf` spec) || ("../" `Text.isPrefixOf` spec) || ("./" `Text.isPrefixOf` spec) || ("/" `Text.isPrefixOf` spec)
    then parsePathDependency spec
  else 
    -- If not a specialized type, use simple version constraint parsing
    Just $ Dependency
      { dependencyType = PipType
      , dependencyName = name
      , DepTypes.dependencyVersion = parseVersionConstraint spec
      , dependencyLocations = []
      , dependencyEnvironments = mempty
      , dependencyTags = mempty
      }