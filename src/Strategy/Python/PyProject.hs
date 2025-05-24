{-# LANGUAGE TemplateHaskell #-}

module Strategy.Python.PyProject
  ( PyProjectProject (..)
  , PyProjectType (..)
  , discover
  , analyze
  ) where

import App.Fossa.Analyze.Types (AnalyzeProject(..))
import Control.Effect.Diagnostics (Diagnostics, Has, context)
import Control.Effect.Reader (Reader)
import Data.Aeson (ToJSON(toJSON), defaultOptions, genericToJSON)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import DepTypes (DepEnvironment(..), DepType(..), Dependency(..), VerConstraint(..))
import Graphing qualified
import Strategy.Python.Util (Req(..), toConstraint)
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk
  ( WalkStep (WalkContinue, WalkSkipAll)
  , findFileNamed
  , walkWithFilters'
  )
import Effect.Logger (Logger, Pretty (pretty), logDebug)
import Effect.ReadFS (ReadFS, doesFileExist, readContentsToml)
import GHC.Generics (Generic)
import Path (Abs, Dir, File, Path, mkRelFile, toFilePath, (</>))
import qualified Strategy.Python.PyProject.PyProjectToml as PyProject
import Strategy.Python.PyProject.PdmLock (buildGraph)
import qualified Strategy.Python.PyProject.PoetryLock as PoetryLock
import qualified Types
import Types (DependencyResults(..), DiscoveredProject(DiscoveredProject, projectData, projectPath, projectBuildTargets), DiscoveredProjectType(GenericPyProjectType), GraphBreadth(..))

data LockFileType
  = PoetryLock
  | PDMLock
  deriving (Eq, Show)

determineLockFileType :: Path Abs File -> Maybe LockFileType
determineLockFileType lockPath
  | "poetry.lock" `Text.isSuffixOf` Text.pack (toFilePath lockPath) = Just PoetryLock
  | "pdm.lock" `Text.isSuffixOf` Text.pack (toFilePath lockPath) = Just PDMLock
  | otherwise = Nothing

data PyProjectType
  = PoetryProject
  | PDMProject  
  | PEP621Project
  deriving (Eq, Ord, Show, Generic)

instance ToJSON PyProjectType where
  toJSON = genericToJSON defaultOptions

data PyProjectProject = PyProjectProject
  { pyprojectFile :: Path Abs File
  , projectDir :: Path Abs Dir
  , projectType :: PyProjectType
  , lockFile :: Maybe (Path Abs File)
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON PyProjectProject where
  toJSON = genericToJSON defaultOptions

instance AnalyzeProject PyProjectProject where
  analyzeProject _ = analyze
  analyzeProjectStaticOnly _ = analyze

discover :: 
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  , Has (Reader AllFilters) sig m
  ) =>
  Path Abs Dir -> m [DiscoveredProject PyProjectProject]
discover = simpleDiscover findProjects mkProject GenericPyProjectType

findProjects ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  , Has (Reader AllFilters) sig m
  ) =>
  Path Abs Dir -> m [PyProjectProject]
findProjects = walkWithFilters' $ \dir _ files -> do
  case findFileNamed "pyproject.toml" files of
    Nothing -> pure ([], WalkContinue)
    Just pyprojectPath -> do
      
      let poetryLockPath = dir </> $(mkRelFile "poetry.lock")
      let pdmLockPath = dir </> $(mkRelFile "pdm.lock")
      
      poetryLockExists <- doesFileExist poetryLockPath
      pdmLockExists <- doesFileExist pdmLockPath
      
      let foundLockFile = if poetryLockExists 
                         then Just poetryLockPath
                         else if pdmLockExists
                              then Just pdmLockPath
                              else Nothing
      
      projectType <- detectProjectType pyprojectPath poetryLockExists pdmLockExists
      
      let project = PyProjectProject
            { pyprojectFile = pyprojectPath
            , projectDir = dir
            , projectType = projectType
            , lockFile = foundLockFile
            }
      
      logDebug $ "Found PyProject: " <> pretty (show project)
      
      pure ([project], WalkSkipAll)

detectProjectType ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  ) =>
  Path Abs File -> Bool -> Bool -> m PyProjectType
detectProjectType pyprojectPath poetryLockExists pdmLockExists = do
  if poetryLockExists
    then pure PoetryProject
    else if pdmLockExists
      then pure PDMProject
      else do
        pyproject <- readContentsToml @PyProject.PyProject pyprojectPath
        let hasPoetryTool = isJust (PyProject.pyprojectPoetry =<< PyProject.pyprojectTool pyproject)
        let hasPDMTool = isJust (PyProject.pyprojectPdm =<< PyProject.pyprojectTool pyproject)
        let hasPEP621 = isJust (PyProject.pyprojectProject pyproject)
        
        if hasPoetryTool
          then pure PoetryProject
          else if hasPDMTool
            then pure PDMProject
            else if hasPEP621
              then pure PEP621Project
              else pure PEP621Project

pyProjectTypeToDiscoveredType :: PyProjectType -> Types.DiscoveredProjectType
pyProjectTypeToDiscoveredType PoetryProject = Types.PoetryProjectType
pyProjectTypeToDiscoveredType PDMProject = Types.PdmProjectType
pyProjectTypeToDiscoveredType PEP621Project = Types.GenericPyProjectType

mkProject :: PyProjectProject -> DiscoveredProject PyProjectProject
mkProject project =
  DiscoveredProject
    { Types.projectType = pyProjectTypeToDiscoveredType (projectType project)
    , projectBuildTargets = mempty
    , projectPath = projectDir project
    , projectData = project
    }

analyze ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  ) =>
  PyProjectProject -> m DependencyResults
analyze project = context "PyProject analysis" $ do
  case projectType project of
    PoetryProject -> analyzePoetryProject project
    PDMProject -> analyzePDMProject project
    PEP621Project -> analyzePEP621Project project

analyzePoetryProject ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  ) =>
  PyProjectProject -> m DependencyResults
analyzePoetryProject project = do
  pyproject <- readContentsToml @PyProject.PyProject (pyprojectFile project)
  
  deps <- case lockFile project of
    Just lockPath | Just PoetryLock <- determineLockFileType lockPath -> do
      poetryLock <- readContentsToml @PoetryLock.PoetryLock lockPath
      pure $ buildGraphFromPoetryLock pyproject poetryLock
    _ -> do
      let fallbackDeps = extractPoetryDeps pyproject
      pure $ Graphing.fromList fallbackDeps
  
  pure $ DependencyResults
    { dependencyGraph = deps
    , dependencyGraphBreadth = Complete
    , dependencyManifestFiles = [pyprojectFile project]
    }

extractPoetryDeps :: PyProject.PyProject -> [Dependency]
extractPoetryDeps pyproject = filter notNamedPython $ prodDeps ++ devDeps
  where
    notNamedPython = (/= "python") . dependencyName
    prodDeps = concatMap (\(name, dep) -> [toDependency name dep EnvProduction]) (Map.toList $ PyProject.allPoetryProductionDeps pyproject)
    devDeps = concatMap (\(name, dep) -> [toDependency name dep EnvDevelopment]) (Map.toList $ PyProject.allPoetryNonProductionDeps pyproject)
    
    toDependency name dep env = Dependency
      { dependencyType = PipType
      , dependencyName = name  
      , dependencyVersion = extractVersion dep
      , dependencyLocations = []
      , dependencyEnvironments = Set.singleton env
      , dependencyTags = Map.empty
      }
    
    extractVersion = \case
      PyProject.PoetryTextVersion v -> Just (CEq v)
      _ -> Nothing

buildGraphFromPoetryLock :: PyProject.PyProject -> PoetryLock.PoetryLock -> Graphing.Graphing Dependency
buildGraphFromPoetryLock pyproject poetryLock = 
  let lockPkgs = PoetryLock.poetryLockPackages poetryLock
      
      -- Get both Poetry and PEP621 dependencies for environment classification
      pyprojectProdDeps = PyProject.allPoetryProductionDeps pyproject
      pyprojectDevDeps = PyProject.allPoetryNonProductionDeps pyproject
      pep621ProdDeps = getPEP621ProductionDeps pyproject
      
      -- Convert ALL packages from lock file using resolved versions
      deps = map (poetryLockToDependency pyprojectProdDeps pyprojectDevDeps pep621ProdDeps) lockPkgs
      
  in Graphing.fromList $ filter ((/= "python") . dependencyName) deps

-- Extract PEP621 production dependencies for environment classification
getPEP621ProductionDeps :: PyProject.PyProject -> Set.Set Text
getPEP621ProductionDeps pyproject = 
  case PyProject.pyprojectProject pyproject of
    Nothing -> Set.empty
    Just metadata -> 
      case PyProject.pyprojectDependencies metadata of
        Nothing -> Set.empty
        Just reqs -> Set.fromList $ map reqToName reqs
  where
    reqToName (NameReq name _ _ _) = name
    reqToName (UrlReq name _ _ _) = name

poetryLockToDependency :: Map Text PyProject.PoetryDependency -> Map Text PyProject.PoetryDependency -> Set.Set Text -> PoetryLock.PoetryLockPackage -> Dependency
poetryLockToDependency poetryProdDeps poetryDevDeps pep621ProdDeps pkg = 
  Dependency
    { dependencyType = PipType
    , dependencyName = pkgName
    , dependencyVersion = Just (CEq $ PoetryLock.poetryLockPackageVersion pkg)
    , dependencyLocations = []
    , dependencyEnvironments = determineEnvironment poetryProdDeps poetryDevDeps pep621ProdDeps pkgName
    , dependencyTags = Map.empty
    }
  where
    pkgName = PoetryLock.unPackageName (PoetryLock.poetryLockPackageName pkg)
    
    determineEnvironment poetryProd poetryDev pep621Prod name
      | Map.member name poetryProd = Set.singleton EnvProduction  -- Poetry production deps
      | Set.member name pep621Prod = Set.singleton EnvProduction  -- PEP621 production deps  
      | Map.member name poetryDev = Set.singleton EnvDevelopment  -- Poetry dev deps
      | otherwise = Set.singleton EnvProduction  -- Default to production

analyzePDMProject ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  ) =>
  PyProjectProject -> m DependencyResults
analyzePDMProject project = do
  pyproject <- readContentsToml @PyProject.PyProject (pyprojectFile project)
  
  let (prodReqs, optsReqs) = reqsFromPyProject pyproject
  let otherReqs = reqsFromPdmMetadata pyproject
  let devReqs = optsReqs <> otherReqs
  
  deps <- case lockFile project of
    Just lockPath | Just PDMLock <- determineLockFileType lockPath -> do
      pdmLock <- readContentsToml lockPath
      pure $ buildGraph prodReqs devReqs pdmLock
    _ -> pure $ Graphing.directs $
      (toDependency EnvProduction <$> prodReqs) ++
      (toDependency EnvDevelopment <$> devReqs)
  
  pure $ DependencyResults
    { dependencyGraph = deps
    , dependencyGraphBreadth = Complete
    , dependencyManifestFiles = [pyprojectFile project]
    }

analyzePEP621Project ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  ) =>
  PyProjectProject -> m DependencyResults
analyzePEP621Project project = do
  pyproject <- readContentsToml @PyProject.PyProject (pyprojectFile project)
  let deps = pyProjectDeps pyproject
  
  pure $ DependencyResults
    { dependencyGraph = deps
    , dependencyGraphBreadth = Complete
    , dependencyManifestFiles = [pyprojectFile project]
    }

-- Helper functions from old PDM strategy

pyProjectDeps :: PyProject.PyProject -> Graphing.Graphing Dependency
pyProjectDeps pyproject = Graphing.fromList $ filter notNamedPython $ prodDeps ++ devDeps
  where
    notNamedPython = (/= "python") . dependencyName
    prodDeps = concatMap (\(name, dep) -> [toDependency name dep EnvProduction]) (Map.toList $ PyProject.allPoetryProductionDeps pyproject)
    devDeps = concatMap (\(name, dep) -> [toDependency name dep EnvDevelopment]) (Map.toList $ PyProject.allPoetryNonProductionDeps pyproject)
    
    toDependency name dep env = Dependency
      { dependencyType = PipType
      , dependencyName = name  
      , dependencyVersion = extractVersion dep
      , dependencyLocations = []
      , dependencyEnvironments = Set.singleton env
      , dependencyTags = Map.empty
      }
    
    extractVersion = \case
      PyProject.PoetryTextVersion v -> Just (CEq v)
      _ -> Nothing

reqsFromPyProject :: PyProject.PyProject -> ([Req], [Req])
reqsFromPyProject pyproject = 
  case PyProject.pyprojectProject pyproject of
    Nothing -> ([], [])
    Just metadata ->
      let prodDeps = maybe [] id (PyProject.pyprojectDependencies metadata)
          optionalDeps = maybe [] (concatMap snd . Map.toList) (PyProject.pyprojectOptionalDependencies metadata)
      in (prodDeps, optionalDeps)

reqsFromPdmMetadata :: PyProject.PyProject -> [Req]
reqsFromPdmMetadata pyproject =
  case PyProject.pyprojectTool pyproject >>= PyProject.pyprojectPdm of
    Nothing -> []
    Just pdm -> maybe [] (concatMap snd . Map.toList) (PyProject.pdmDevDependencies pdm)

toDependency :: DepEnvironment -> Req -> Dependency
toDependency env req =
  Dependency
    { dependencyType = reqDepType req
    , dependencyName = reqDepName req
    , dependencyVersion = reqDepVersion req
    , dependencyLocations = mempty
    , dependencyEnvironments = Set.singleton env
    , dependencyTags = mempty
    }

reqDepName :: Req -> Text
reqDepName (NameReq name _ _ _) = name
reqDepName (UrlReq name _ url _) =
  if "file://" `Text.isPrefixOf` (Text.pack $ show url)
    then name
    else Text.pack $ show url

reqDepType :: Req -> DepType
reqDepType (NameReq{}) = PipType
reqDepType (UrlReq _ _ url _) =
  if "file://" `Text.isPrefixOf` (Text.pack $ show url)
    then UnresolvedPathType
    else URLType

reqDepVersion :: Req -> Maybe VerConstraint
reqDepVersion (UrlReq{}) = Nothing
reqDepVersion (NameReq _ _ Nothing _) = Nothing
reqDepVersion (NameReq _ _ (Just ver) _) = Just . toConstraint $ ver

getDeps ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  ) =>
  PyProjectProject -> m DependencyResults
getDeps = analyze