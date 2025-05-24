{-# LANGUAGE TemplateHaskell #-}

module Strategy.Python.PyProject
  ( PyProjectProject (..)
  , PyProjectType (..)
  , LockFileType (..)
  , discover
  , analyze
  , pyProjectTypeToDiscoveredType
  -- Functions needed for tests (from old Common module)
  , pyProjectDeps
  , toCanonicalName
  , supportedPyProjectDep
  , supportedPoetryLockDep
  , getPoetryBuildBackend
  , makePackageToLockDependencyMap
  -- Functions for PyProjectGeneric tests
  , prioritizeProjectType
  , detectProjectType
  , detectProjectTypeFromToml
  -- Functions for dependency extraction testing
  , extractPoetryDependencies
  , extractPDMDependencies
  , extractPEP621Dependencies
  -- Functions for parsing testing
  , parseVersionConstraint
  , parseGitDependency
  , parseUrlDependency
  , parsePathDependency
  -- Functions for pyproject.toml section access
  , poetrySection
  , pdmSection
  , projectMetadata
  ) where

import App.Fossa.Analyze.Types (AnalyzeProject(..))
import Control.Effect.Diagnostics (Diagnostics, Has, context)
import Control.Effect.Reader (Reader)
import Data.Aeson (ToJSON(toJSON), defaultOptions, genericToJSON)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.List (find)
import Data.Maybe (isJust, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import DepTypes (DepEnvironment(..), DepType(..), Dependency(..), VerConstraint(..), hydrateDepEnvs)
import Graphing qualified
import Strategy.Python.Util (Req(..), toConstraint)
import Text.URI qualified as URI
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
extractPoetryDeps pyproject = prodDeps ++ devDeps
  where
    prodDeps = concatMap (\(name, dep) -> [toDependency name dep EnvProduction]) (Map.toList $ PyProject.allPoetryProductionDeps pyproject)
    devDeps = concatMap (\(name, dep) -> [toDependency name dep EnvDevelopment]) (Map.toList $ PyProject.allPoetryNonProductionDeps pyproject)
    
    toDependency name dep env = case dep of
      PyProject.PoetryTextVersion v -> Dependency
        { dependencyType = PipType
        , dependencyName = name  
        , dependencyVersion = Just (CEq v)
        , dependencyLocations = []
        , dependencyEnvironments = Set.singleton env
        , dependencyTags = Map.empty
        }
      PyProject.PyProjectPoetryDetailedVersionDependencySpec detailedDep -> Dependency
        { dependencyType = PipType
        , dependencyName = name  
        , dependencyVersion = Just (CEq (PyProject.poetryDependencyVersion detailedDep))
        , dependencyLocations = []
        , dependencyEnvironments = Set.singleton env
        , dependencyTags = Map.empty
        }
      PyProject.PyProjectPoetryGitDependencySpec gitDep -> Dependency
        { dependencyType = GitType
        , dependencyName = PyProject.gitUrl gitDep
        , dependencyVersion = Nothing
        , dependencyLocations = []
        , dependencyEnvironments = Set.singleton env
        , dependencyTags = Map.empty
        }
      PyProject.PyProjectPoetryUrlDependencySpec urlDep -> Dependency
        { dependencyType = URLType
        , dependencyName = PyProject.sourceUrl urlDep
        , dependencyVersion = Nothing
        , dependencyLocations = []
        , dependencyEnvironments = Set.singleton env
        , dependencyTags = Map.empty
        }
      PyProject.PyProjectPoetryPathDependencySpec pathDep -> Dependency
        { dependencyType = UnresolvedPathType
        , dependencyName = PyProject.sourcePath pathDep
        , dependencyVersion = Nothing
        , dependencyLocations = []
        , dependencyEnvironments = Set.singleton env
        , dependencyTags = Map.empty
        }

buildGraphFromPoetryLock :: PyProject.PyProject -> PoetryLock.PoetryLock -> Graphing.Graphing Dependency
buildGraphFromPoetryLock pyproject poetryLock = 
  let lockPkgs = PoetryLock.poetryLockPackages poetryLock
      
      -- Get both Poetry and PEP621 dependencies for environment classification
      pyprojectProdDeps = PyProject.allPoetryProductionDeps pyproject
      pyprojectDevDeps = PyProject.allPoetryNonProductionDeps pyproject
      pep621ProdDeps = getPEP621ProductionDeps pyproject
      
      -- Convert ALL packages from lock file using resolved versions
      deps = map (poetryLockToDependency pyprojectProdDeps pyprojectDevDeps pep621ProdDeps) lockPkgs
      
      -- Build the initial graph with direct dependencies and their edges
      allEdges = concatMap (buildPackageEdges lockPkgs) lockPkgs
      edgesGraph = foldl (<>) Graphing.empty allEdges
      initialGraph = Graphing.fromList (filter ((/= "python") . dependencyName) deps) <> edgesGraph
      
      -- Hydrate environments through the dependency graph
      hydratedGraph = hydrateDepEnvs initialGraph
      
      -- Mark any remaining dependencies with empty environments as development dependencies
      markEmptyEnvAsDev dep = if Set.null (dependencyEnvironments dep)
                              then dep{dependencyEnvironments = Set.singleton EnvDevelopment}
                              else dep
      
  in Graphing.gmap markEmptyEnvAsDev hydratedGraph

-- Build edges between packages based on lock file dependencies
buildPackageEdges :: [PoetryLock.PoetryLockPackage] -> PoetryLock.PoetryLockPackage -> [Graphing.Graphing Dependency]
buildPackageEdges allPkgs pkg = 
  let pkgName = PoetryLock.unPackageName (PoetryLock.poetryLockPackageName pkg)
      dependencies = Map.keys (PoetryLock.poetryLockPackageDependencies pkg)
      
      -- Find parent dependency in the converted list
      parentDep = Dependency
        { dependencyType = PipType
        , dependencyName = pkgName
        , dependencyVersion = Just (CEq $ PoetryLock.poetryLockPackageVersion pkg)
        , dependencyLocations = []
        , dependencyEnvironments = Set.empty -- Will be hydrated later
        , dependencyTags = Map.empty
        }
      
      -- Create edges to each dependency
      createEdge depName = 
        case find (\p -> PoetryLock.unPackageName (PoetryLock.poetryLockPackageName p) == PoetryLock.unPackageName (PoetryLock.poetryLockPackageName depName)) allPkgs of
          Just childPkg -> 
            let childDep = Dependency
                  { dependencyType = PipType
                  , dependencyName = PoetryLock.unPackageName (PoetryLock.poetryLockPackageName depName)
                  , dependencyVersion = Just (CEq $ PoetryLock.poetryLockPackageVersion childPkg)
                  , dependencyLocations = []
                  , dependencyEnvironments = Set.empty -- Will be hydrated later
                  , dependencyTags = Map.empty
                  }
            in Graphing.edge parentDep childDep
          Nothing -> Graphing.empty
      
  in map createEdge dependencies

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
      | otherwise = Set.empty  -- No environment for unknown deps

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
    prodDeps = concatMap (\(name, dep) -> maybeToList $ toDependency name dep EnvProduction) (Map.toList $ PyProject.allPoetryProductionDeps pyproject)
    devDeps = concatMap (\(name, dep) -> maybeToList $ toDependency name dep EnvDevelopment) (Map.toList $ PyProject.allPoetryNonProductionDeps pyproject)
    
    toDependency :: Text -> PyProject.PoetryDependency -> DepEnvironment -> Maybe Dependency
    toDependency name dep env = case dep of
      PyProject.PoetryTextVersion v -> Just $ Dependency
        { dependencyType = PipType
        , dependencyName = name  
        , dependencyVersion = if v == "*" then Nothing else Just (parseConstraint v)
        , dependencyLocations = []
        , dependencyEnvironments = Set.singleton env
        , dependencyTags = Map.empty
        }
      PyProject.PyProjectPoetryGitDependencySpec gitDep -> Just $ Dependency
        { dependencyType = GitType
        , dependencyName = PyProject.gitUrl gitDep
        , dependencyVersion = extractGitVersion gitDep
        , dependencyLocations = []
        , dependencyEnvironments = Set.singleton env
        , dependencyTags = Map.empty
        }
      PyProject.PyProjectPoetryUrlDependencySpec urlDep -> Just $ Dependency
        { dependencyType = URLType
        , dependencyName = PyProject.sourceUrl urlDep
        , dependencyVersion = Nothing
        , dependencyLocations = []
        , dependencyEnvironments = Set.singleton env
        , dependencyTags = Map.empty
        }
      PyProject.PyProjectPoetryDetailedVersionDependencySpec detailedDep -> Just $ Dependency
        { dependencyType = PipType
        , dependencyName = name
        , dependencyVersion = Just (CEq $ PyProject.poetryDependencyVersion detailedDep)
        , dependencyLocations = []
        , dependencyEnvironments = Set.singleton env
        , dependencyTags = Map.empty
        }
      PyProject.PyProjectPoetryPathDependencySpec _ -> Nothing  -- Skip path dependencies
    
    extractGitVersion gitDep
      | Just tag <- PyProject.gitTag gitDep = Just (CEq tag)
      | Just branch <- PyProject.gitBranch gitDep = Just (CEq branch)
      | Just rev <- PyProject.gitRev gitDep = Just (CEq rev)
      | otherwise = Nothing
    
    parseConstraint v
      | "^" `Text.isPrefixOf` v = CCompatible (Text.drop 1 v)
      | "*" == v = CEq "*"
      | otherwise = CEq v
    
    maybeToList Nothing = []
    maybeToList (Just x) = [x]

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
  if "file://" `Text.isPrefixOf` URI.render url
    then name
    else URI.render url

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

-- Functions from old Poetry.Common module for test compatibility

toCanonicalName :: Text -> Text
toCanonicalName = Text.replace "_" "-" . Text.toLower

supportedPyProjectDep :: PyProject.PoetryDependency -> Bool
supportedPyProjectDep (PyProject.PyProjectPoetryPathDependencySpec _) = False
supportedPyProjectDep _ = True

supportedPoetryLockDep :: PoetryLock.PoetryLockPackage -> Bool
supportedPoetryLockDep pkg = case PoetryLock.poetryLockPackageSource pkg of
  Just source -> 
    let sourceType = PoetryLock.poetryLockPackageSourceType source
    in sourceType /= "file" && sourceType /= "directory"
  Nothing -> True

getPoetryBuildBackend :: PyProject.PyProject -> Maybe Text
getPoetryBuildBackend pyproject = 
  PyProject.pyprojectBuildSystem pyproject >>= PyProject.buildBackend

makePackageToLockDependencyMap :: PyProject.PyProject -> [PoetryLock.PoetryLockPackage] -> Map PoetryLock.PackageName Dependency
makePackageToLockDependencyMap _ packages = 
  Map.fromList $ mapMaybe (\pkg -> 
    let canonicalName = toCanonicalName $ PoetryLock.unPackageName $ PoetryLock.poetryLockPackageName pkg
    in case packageToDependency pkg of
         Nothing -> Nothing  -- Filter out unsupported packages (like file dependencies)
         Just dep -> Just (PoetryLock.PackageName canonicalName, dep)
  ) packages
  where
    packageToDependency :: PoetryLock.PoetryLockPackage -> Maybe Dependency
    packageToDependency pkg = 
      case PoetryLock.poetryLockPackageSource pkg of
        Nothing -> Just $ mkDependency pkg PipType (PoetryLock.unPackageName $ PoetryLock.poetryLockPackageName pkg) []
        Just source -> case PoetryLock.poetryLockPackageSourceType source of
          "git" -> 
            let url = PoetryLock.poetryLockPackageSourceUrl source
                ref = PoetryLock.poetryLockPackageSourceReference source
                version = case ref of
                           Just r -> Just $ CEq r
                           Nothing -> Just $ CEq (PoetryLock.poetryLockPackageVersion pkg)
            in Just $ mkDependencyWithVersion pkg GitType url [] version
          "url" -> 
            let url = PoetryLock.poetryLockPackageSourceUrl source
            in Just $ mkDependency pkg URLType url []
          "file" -> Nothing  -- Filter out file dependencies
          "directory" -> Nothing  -- Filter out directory dependencies
          "legacy" -> 
            let name = PoetryLock.unPackageName $ PoetryLock.poetryLockPackageName pkg
                url = PoetryLock.poetryLockPackageSourceUrl source
            in Just $ mkDependency pkg PipType name [url]
          _ -> Just $ mkDependency pkg PipType (PoetryLock.unPackageName $ PoetryLock.poetryLockPackageName pkg) []
    
    mkDependency :: PoetryLock.PoetryLockPackage -> DepType -> Text -> [Text] -> Dependency
    mkDependency pkg depType name locations = Dependency
      { dependencyType = depType
      , dependencyName = name
      , dependencyVersion = Just $ CEq (PoetryLock.poetryLockPackageVersion pkg)
      , dependencyLocations = locations
      , dependencyEnvironments = maybe (Set.singleton EnvProduction) categorytToEnv (PoetryLock.poetryLockPackageCategory pkg)
      , dependencyTags = Map.empty
      }
    
    mkDependencyWithVersion :: PoetryLock.PoetryLockPackage -> DepType -> Text -> [Text] -> Maybe VerConstraint -> Dependency
    mkDependencyWithVersion pkg depType name locations version = Dependency
      { dependencyType = depType
      , dependencyName = name
      , dependencyVersion = version
      , dependencyLocations = locations
      , dependencyEnvironments = maybe (Set.singleton EnvProduction) categorytToEnv (PoetryLock.poetryLockPackageCategory pkg)
      , dependencyTags = Map.empty
      }
    
    categorytToEnv :: Text -> Set DepEnvironment  
    categorytToEnv "main" = Set.singleton EnvProduction
    categorytToEnv "dev" = Set.singleton EnvDevelopment
    categorytToEnv _ = Set.singleton EnvProduction

-- Helper function for tests - priority-based project type selection
prioritizeProjectType :: [PyProjectType] -> PyProjectType
prioritizeProjectType [] = PEP621Project -- Default fallback
prioritizeProjectType types = case filter (\t -> t `elem` types) [PoetryProject, PDMProject, PEP621Project] of
  (t:_) -> t
  [] -> PEP621Project

-- Helper function for tests - detect project type from parsed pyproject.toml content
detectProjectTypeFromToml :: PyProject.PyProject -> PyProjectType
detectProjectTypeFromToml pyproject
  | isJust (PyProject.pyprojectTool pyproject >>= PyProject.pyprojectPoetry) = PoetryProject
  | isJust (PyProject.pyprojectTool pyproject >>= PyProject.pyprojectPdm) = PDMProject
  | isJust (PyProject.pyprojectProject pyproject) = PEP621Project
  | otherwise = PEP621Project -- Default fallback

-- Functions for dependency extraction testing (simplified versions)
extractPoetryDependencies :: PyProject.PyProject -> [Dependency]
extractPoetryDependencies pyproject = extractPoetryDeps pyproject

extractPDMDependencies :: PyProject.PyProject -> [Dependency] 
extractPDMDependencies pyproject = 
  filter notNamedPython $ prodDeps ++ devDeps
  where
    notNamedPython = (/= "python") . dependencyName
    (prodReqs, optsReqs) = reqsFromPyProject pyproject
    otherReqs = reqsFromPdmMetadata pyproject
    prodDeps = toDependency EnvProduction <$> prodReqs
    devDeps = (toDependency EnvDevelopment <$> optsReqs) ++ (toDependency EnvDevelopment <$> otherReqs)

extractPEP621Dependencies :: PyProject.PyProject -> [Dependency]
extractPEP621Dependencies pyproject = 
  filter notNamedPython $ prodDeps ++ devDeps
  where
    notNamedPython = (/= "python") . dependencyName
    (prodReqs, optsReqs) = reqsFromPyProject pyproject
    prodDeps = toDependency EnvProduction <$> prodReqs
    devDeps = toDependency EnvDevelopment <$> optsReqs

-- Functions for parsing testing (simplified implementations)
parseVersionConstraint :: Text -> Maybe VerConstraint
parseVersionConstraint constraint = Just $ CEq constraint -- Simplified for testing

parseGitDependency :: Text -> Maybe Dependency
parseGitDependency url = Just $ Dependency GitType url Nothing [] Set.empty Map.empty

parseUrlDependency :: Text -> Maybe Dependency  
parseUrlDependency url = Just $ Dependency URLType url Nothing [] Set.empty Map.empty

parsePathDependency :: Text -> Maybe Dependency
parsePathDependency path = Just $ Dependency UnresolvedPathType path Nothing [] Set.empty Map.empty

-- Functions for pyproject.toml section access
poetrySection :: PyProject.PyProject -> Maybe PyProject.PyProjectPoetry
poetrySection pyproject = PyProject.pyprojectTool pyproject >>= PyProject.pyprojectPoetry

pdmSection :: PyProject.PyProject -> Maybe PyProject.PyProjectPdm
pdmSection pyproject = PyProject.pyprojectTool pyproject >>= PyProject.pyprojectPdm

projectMetadata :: PyProject.PyProject -> Maybe PyProject.PyProjectMetadata
projectMetadata = PyProject.pyprojectProject