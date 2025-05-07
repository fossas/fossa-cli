{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module Strategy.Python.PyProjectGeneric
  ( PyProjectGeneric (..)
  , PyProjectType (..)
  , LockFileType (..)
  , discover
  , analyze
  , parseGenericPyProject
  , extractDependencies
  , extractPoetryDependencies
  , extractPDMDependencies
  , extractPEP621Dependencies
  , PyProjectProject (..)
  -- Lock file handling
  , findLockFileByType
  , collectLockFiles
  -- Re-exports for tests
  , parseVersionConstraint
  , parseGitDependency
  , parseUrlDependency
  , parsePathDependency
  , parseComplexDependency
  ) where

import App.Fossa.Analyze.Types (AnalyzeProject(..))
import Control.Applicative ((<|>))
import Control.Effect.Diagnostics (Diagnostics, context, fatalText, recover)
import Control.Effect.Reader (Reader)
import Data.Aeson (ToJSON, Value)
import Data.Foldable (foldl', find)
import Data.Map qualified as Map
import Data.Maybe (isJust, fromMaybe, mapMaybe, maybeToList, catMaybes)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import DepTypes
  ( DepEnvironment (EnvDevelopment, EnvProduction)
  , DepType (PipType, URLType, GitType, UnresolvedPathType)
  , Dependency (..)
  , VerConstraint (..)
  )
import Discovery.Filters (AllFilters, withToolFilter)
import Discovery.Walk
  ( WalkStep (WalkContinue, WalkSkipSome)
  , findFileNamed
  , walkWithFilters'
  )
import Effect.ReadFS (Has, ReadFS, readContentsToml, doesFileExist)
import Effect.Logger (Logger)
import GHC.Generics (Generic)
import Graphing (Graphing, directs)
import Path (Abs, Dir, File, Path, parent, (</>), mkRelFile)
import Strategy.Python.Dependency
  ( PythonDependency
  , toDependency
  , fromPoetryDependency
  , fromPDMDependency
  , fromPEP621Dependency
  , fromReq
  , versionConstraint
  , gitDependency
  , urlDependency
  , pathDependency
  , complexDependency
  )
import Strategy.Python.PDM.Pdm qualified as PDM
import Strategy.Python.PDM.PdmLock qualified as PDMLock
import Strategy.Python.Poetry qualified as Poetry
import Strategy.Python.Poetry.PoetryLock qualified as PoetryLock
import Strategy.Python.PyProjectGeneric.Types
  ( PyProjectGeneric (..)
  , PyProjectMetadata (..)
  , PyProjectType (..)
  , LockFileType (..)
  , PyProjectPoetry (..)
  , PyProjectPDM (..)
  , PoetryDependency (..)
  , PyProjectDetailedVersionDependency (..)
  , PyProjectGitDependency (..)
  , PyProjectPathDependency (..)
  , PyProjectUrlDependency (..)
  , detectProjectType
  , projectTypePriority
  , prioritizeProjectType
  , dependencyVersion
  , gitUrl
  , sourcePath
  , sourceUrl
  )
import Strategy.Python.Util (Req (..), toConstraint, Version (..), Operator (..))
import Toml qualified
import Types
  ( DependencyResults (..)
  , DiscoveredProject (..)
  , DiscoveredProjectType (GenericPyProjectType, PdmProjectType, PoetryProjectType)
  , GraphBreadth (Complete, Partial)
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
discover dir = withToolFilter GenericPyProjectType $ do
  -- Find all PyProject.toml files
  projects <- findProjects dir
  -- Process each project to determine its type and create DiscoveredProject
  discoveredProjects <- mapM processProject projects
  pure discoveredProjects
  where
    processProject :: (Has ReadFS sig m, Has Diagnostics sig m) => PyProjectProject -> m (DiscoveredProject PyProjectProject)
    processProject project = do
      -- Parse the file to determine project type
      pyProject <- parseGenericPyProject (pyprojectFile project)
      
      -- Get project type from the parsed file
      let detectedType = Strategy.Python.PyProjectGeneric.Types.projectType pyProject
      
      -- Update the project data with the correct project type
      let projectWithType = project { Strategy.Python.PyProjectGeneric.projectType = detectedType }
      
      -- Create a DiscoveredProject with the appropriate type
      let discoveredType = case detectedType of
            PoetryProject -> PoetryProjectType  -- Use Poetry type for Poetry projects 
            PDMProject -> PdmProjectType        -- Use PDM type for PDM projects
            _ -> GenericPyProjectType           -- Use Generic type for others (PEP621, Unknown)
      
      pure $ mkProject discoveredType projectWithType

-- | Find PyProject.toml files and associated lock files
findProjects :: 
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  , Has (Reader AllFilters) sig m
  ) => 
  Path Abs Dir -> 
  m [PyProjectProject]
findProjects = walkWithFilters' $ \dir _ files -> do
  let pyprojectFile = findFileNamed "pyproject.toml" files
      collectedLockFiles = collectLockFiles files
  
  case pyprojectFile of
    Just pyproject -> do
      -- Create a new PyProjectProject with UnknownProject type
      -- The actual project type will be set later in processProject
      pure ([PyProjectProject pyproject dir collectedLockFiles UnknownProject], WalkSkipSome [".venv"])
    Nothing -> pure ([], WalkContinue)

-- | Data type representing a PyProject.toml project
data PyProjectProject = PyProjectProject
  { pyprojectFile :: Path Abs File
  , projectDir :: Path Abs Dir
  , lockFiles :: [(LockFileType, Path Abs File)]  -- Named lock files
  , projectType :: PyProjectType  -- Store project type directly
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON PyProjectProject

-- | Instance for AnalyzeProject
instance AnalyzeProject PyProjectProject where
  analyzeProject _ project = analyzeWithProject project
  analyzeProjectStaticOnly _ project = analyzeWithProject project

-- | Analyze using PyProjectProject data
analyzeWithProject ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  ) =>
  PyProjectProject ->
  m DependencyResults
analyzeWithProject project = do
  -- Parse the PyProject.toml file to get project metadata
  pyproject <- parseGenericPyProject (pyprojectFile project)
  
  -- Use different analysis strategy based on project type and available lock files
  case Strategy.Python.PyProjectGeneric.projectType project of
    -- For PDM projects
    PDMProject -> do
      -- Check if we have a PDM lock file
      let maybePdmLockPath = findLockFileByType PDMLock (lockFiles project)
      
      case maybePdmLockPath of
        Just pdmLockPath -> do
          -- Use PDM lock file for complete graph
          pdmAnalyzeWithLock (pyprojectFile project) pdmLockPath
        Nothing -> do
          -- Fall back to direct dependencies from pyproject.toml
          pdmAnalyzeWithoutLock pyproject (pyprojectFile project)
          
    -- For Poetry projects
    PoetryProject -> do
      -- Check if we have a Poetry lock file
      let maybePoetryLockPath = findLockFileByType PoetryLock (lockFiles project)
      
      case maybePoetryLockPath of
        Just poetryLockPath -> do
          -- Use Poetry lock file for complete graph
          poetryAnalyzeWithLock (pyprojectFile project) poetryLockPath
        Nothing -> do
          -- Fall back to direct dependencies from pyproject.toml
          poetryAnalyzeWithoutLock pyproject (pyprojectFile project)
          
    -- For PEP 621 and Unknown projects
    _ -> do
      -- Extract dependencies from pyproject.toml
      let dependencies = extractDependencies pyproject
      
      pure $
        DependencyResults
          { dependencyGraph = directs dependencies
          , dependencyGraphBreadth = Partial
          , dependencyManifestFiles = [pyprojectFile project]
          }

-- | Create a DiscoveredProject from PyProjectProject with the provided project type
mkProject :: DiscoveredProjectType -> PyProjectProject -> DiscoveredProject PyProjectProject
mkProject projectType project =
  DiscoveredProject
    { Types.projectType = projectType  -- Use the provided project type
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

-- | Analyze a PyProject project and extract dependencies
-- Will use the lock file if available, otherwise falls back to pyproject.toml
analyze ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  ) =>
  Path Abs File ->
  m DependencyResults
analyze pyprojectFile = do
  -- Parse the PyProject.toml file to get project metadata
  let projectDir = parent pyprojectFile
  pyproject <- parseGenericPyProject pyprojectFile
  
  -- Determine project type
  let pyprojectType = Strategy.Python.PyProjectGeneric.Types.projectType pyproject
  
  -- Find potential lock files in the same directory
  let pyprojectDir = parent pyprojectFile
  -- Check for lock files in the directory
  let pdmLockPath = pyprojectDir </> $(mkRelFile "pdm.lock")
  let poetryLockPath = pyprojectDir </> $(mkRelFile "poetry.lock")
  
  pdmLockExists <- doesFileExist pdmLockPath
  poetryLockExists <- doesFileExist poetryLockPath
  
  -- Create a lock files collection
  let lockFiles = catMaybes
        [ if pdmLockExists then Just (PDMLock, pdmLockPath) else Nothing
        , if poetryLockExists then Just (PoetryLock, poetryLockPath) else Nothing
        ]
  
  -- Create a temporary PyProjectProject to use with analyzeWithProject
  let project = PyProjectProject
        { pyprojectFile = pyprojectFile
        , projectDir = projectDir
        , lockFiles = lockFiles
        , Strategy.Python.PyProjectGeneric.projectType = pyprojectType
        }
  
  -- Use the common implementation
  analyzeWithProject project

-- | Extract dependencies from a PyProjectGeneric based on its project type
extractDependencies :: PyProjectGeneric -> [Dependency]
extractDependencies pyproject = 
  case Strategy.Python.PyProjectGeneric.Types.projectType pyproject of
    PoetryProject -> extractPoetryDependencies pyproject
    PDMProject -> extractPDMDependencies pyproject
    PEP621Project -> extractPEP621Dependencies pyproject
    UnknownProject -> [] -- Unknown project type, no dependencies

-- | Extract dependencies from a Poetry project using unified conversion functions
extractPoetryDependencies :: PyProjectGeneric -> [Dependency]
extractPoetryDependencies pyproject =
  case poetrySection pyproject of
    Nothing -> []
    Just poetry -> 
      let 
        -- Convert production dependencies
        prodPythonDeps = Map.foldrWithKey 
          (\name dep acc -> fromPoetryDependency name EnvProduction dep : acc) 
          []
          (poetryDependencies poetry)
          
        -- Convert development dependencies
        devPythonDeps = Map.foldrWithKey 
          (\name dep acc -> fromPoetryDependency name EnvDevelopment dep : acc) 
          []
          (poetryDevDependencies poetry)
          
        -- Convert to common Dependency type
        prodDeps = map toDependency prodPythonDeps
        devDeps = map toDependency devPythonDeps
      in 
        prodDeps ++ devDeps

-- | Extract dependencies from a PDM project using unified conversion functions
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
            concatMap (extractCategoryDeps EnvDevelopment) (Map.toList devDeps)
  in pep621Deps ++ pdmDevDeps
  where
    extractCategoryDeps :: DepEnvironment -> (Text, [Req]) -> [Dependency]
    extractCategoryDeps env (_, reqs) = 
      map (toDependency . fromPDMDependency env) reqs

-- | Extract dependencies from a PEP 621 project using unified conversion functions
extractPEP621Dependencies :: PyProjectGeneric -> [Dependency]
extractPEP621Dependencies pyproject =
  case projectMetadata pyproject of
    Nothing -> []
    Just metadata ->
      let 
        -- Production dependencies
        prodDeps = case projectDependencies metadata of
          Nothing -> []
          Just reqs -> map (toDependency . fromPEP621Dependency EnvProduction) reqs
        
        -- Optional dependencies (treated as development dependencies)
        optDeps = case projectOptionalDependencies metadata of
          Nothing -> []
          Just optDepMap -> 
            concatMap extractOptionalDeps (Map.toList optDepMap)
      in
        prodDeps ++ optDeps
  where
    extractOptionalDeps :: (Text, [Req]) -> [Dependency]
    extractOptionalDeps (_, reqs) = 
      map (toDependency . fromPEP621Dependency EnvDevelopment) reqs

-- | Helper functions for lock file handling

-- | Find a lock file of a specific type from a list of lock files
findLockFileByType :: LockFileType -> [(LockFileType, Path Abs File)] -> Maybe (Path Abs File)
findLockFileByType targetType = fmap snd . find ((== targetType) . fst)

-- | Collect lock files from a directory with files
collectLockFiles :: [Path Abs File] -> [(LockFileType, Path Abs File)]
collectLockFiles files =
  catMaybes
    [ fmap (\f -> (PDMLock, f)) (findFileNamed "pdm.lock" files)
    , fmap (\f -> (PoetryLock, f)) (findFileNamed "poetry.lock" files)
    -- Add additional lock file types here in the future
    ]

-- | Re-exported functions for tests

-- | Parse version constraint from text into VerConstraint
parseVersionConstraint :: Text -> Maybe VerConstraint
parseVersionConstraint = Strategy.Python.Dependency.versionConstraint

-- | Parse Git dependency from specification
parseGitDependency :: Text -> Maybe Dependency
parseGitDependency = Strategy.Python.Dependency.gitDependency

-- | Parse URL dependency from specification
parseUrlDependency :: Text -> Maybe Dependency
parseUrlDependency = Strategy.Python.Dependency.urlDependency

-- | Parse Path dependency from specification
parsePathDependency :: Text -> Maybe Dependency
parsePathDependency = Strategy.Python.Dependency.pathDependency

-- | Convert complex dependency specifications to Dependency type
parseComplexDependency :: Text -> Text -> Maybe Dependency
parseComplexDependency name spec = Strategy.Python.Dependency.complexDependency name spec

-- | Helper functions for PDM and Poetry analysis

-- | Analyze a PDM project with a lock file
pdmAnalyzeWithLock ::
  (Has ReadFS sig m, Has Diagnostics sig m) =>
  Path Abs File -> -- pyproject.toml
  Path Abs File -> -- pdm.lock
  m DependencyResults
pdmAnalyzeWithLock pyprojectFile pdmLockFile = do
  pyproject <- parseGenericPyProject pyprojectFile
  
  -- Extract dependencies from pyproject.toml for PDM
  let (prodReqs, optsReqs) = extractReqsFromPyProject pyproject
  let otherReqs = extractReqsFromPDMMetadata pyproject
  let devReqs = optsReqs <> otherReqs
  
  -- Use PDM lock file analyzer
  pdmLock <- readContentsToml pdmLockFile
  let graph = PDMLock.buildGraph prodReqs devReqs pdmLock
  
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Complete
      , dependencyManifestFiles = [pyprojectFile, pdmLockFile]
      }

-- | Analyze a PDM project without a lock file (direct deps only)
pdmAnalyzeWithoutLock ::
  (Has ReadFS sig m, Has Diagnostics sig m) =>
  PyProjectGeneric ->
  Path Abs File ->
  m DependencyResults
pdmAnalyzeWithoutLock pyproject pyprojectFile = do
  -- Extract dependencies from pyproject.toml for PDM (direct deps only)
  let dependencies = extractPDMDependencies pyproject
  
  pure $
    DependencyResults
      { dependencyGraph = directs dependencies
      , dependencyGraphBreadth = Partial
      , dependencyManifestFiles = [pyprojectFile]
      }

-- | Analyze a Poetry project with a lock file
poetryAnalyzeWithLock ::
  (Has ReadFS sig m, Has Diagnostics sig m, Has Logger sig m) =>
  Path Abs File -> -- pyproject.toml
  Path Abs File -> -- poetry.lock
  m DependencyResults
poetryAnalyzeWithLock pyprojectFile poetryLockFile = do
  -- Read the files
  pyproject <- readContentsToml pyprojectFile
  poetryLock <- readContentsToml poetryLockFile
  
  -- Use Poetry's implementation to analyze both files together
  let graph = Poetry.graphFromPyProjectAndLockFile pyproject poetryLock
  
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Complete
      , dependencyManifestFiles = [pyprojectFile, poetryLockFile]
      }

-- | Analyze a Poetry project without a lock file (direct deps only)
poetryAnalyzeWithoutLock ::
  (Has ReadFS sig m, Has Diagnostics sig m, Has Logger sig m) =>
  PyProjectGeneric ->
  Path Abs File ->
  m DependencyResults
poetryAnalyzeWithoutLock pyproject pyprojectFile = do
  -- Extract dependencies from pyproject.toml for Poetry (direct deps only)
  let dependencies = extractPoetryDependencies pyproject
  
  pure $
    DependencyResults
      { dependencyGraph = directs dependencies
      , dependencyGraphBreadth = Partial
      , dependencyManifestFiles = [pyprojectFile]
      }

-- | Helper functions to extract Reqs from PyProject.toml for PDM
extractReqsFromPyProject :: PyProjectGeneric -> ([Req], [Req])
extractReqsFromPyProject pyproject = 
  case projectMetadata pyproject of
    Nothing -> ([], [])
    Just metadata -> 
      let prodReqs = fromMaybe [] (projectDependencies metadata)
          optReqs = case projectOptionalDependencies metadata of
            Nothing -> []
            Just deps -> concat $ Map.elems deps
      in (prodReqs, optReqs)

-- | Extract dev dependencies from PDM-specific section
extractReqsFromPDMMetadata :: PyProjectGeneric -> [Req]
extractReqsFromPDMMetadata pyproject =
  case pdmSection pyproject of
    Nothing -> []
    Just pdm -> 
      case pdmDevDependencies pdm of
        Nothing -> []
        Just devDeps -> concat $ Map.elems devDeps