{-# LANGUAGE TemplateHaskell #-}

module Strategy.Python.PyProject
  ( PyProjectProject (..)
  , PyProjectType (..)
  , discover
  , analyze
  ) where

import App.Fossa.Analyze.Types (AnalyzeProject(..))
import Control.Effect.Diagnostics (Diagnostics, context)
import Control.Effect.Reader (Reader)
import Data.Aeson (ToJSON, defaultOptions, genericToJSON)
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import DepTypes (DepEnvironment(..), DepType(..), Dependency(..), VerConstraint)
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
import Path (Abs, Dir, File, Path, (</>))
import qualified Strategy.Python.PyProject.PyProjectToml as PyProject
import Strategy.Python.PyProject.PdmLock (buildGraph)
import TH.RelativePaths (mkRelFile)
import Types (DependencyResults(..), DiscoveredProject(..), DiscoveredProjectType(GenericPyProjectType), GraphBreadth(..))

data LockFileType
  = PoetryLock
  | PDMLock
  deriving (Eq, Show)

determineLockFileType :: Path Abs File -> Maybe LockFileType
determineLockFileType lockPath
  | "poetry.lock" `Text.isSuffixOf` Text.pack (show lockPath) = Just PoetryLock
  | "pdm.lock" `Text.isSuffixOf` Text.pack (show lockPath) = Just PDMLock
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
  analyzeProject = getDeps
  analyzeProjectStaticOnly = getDeps

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
    Just pyprojectFile -> do
      let pyprojectPath = dir </> pyprojectFile
      
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
        let hasPEP621 = isJust (PyProject.pyprojectMetadata pyproject)
        
        if hasPoetryTool
          then pure PoetryProject
          else if hasPDMTool
            then pure PDMProject
            else if hasPEP621
              then pure PEP621Project
              else pure PEP621Project

mkProject :: PyProjectProject -> DiscoveredProject PyProjectProject
mkProject project =
  DiscoveredProject
    { projectType = GenericPyProjectType
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
  let deps = extractPoetryDeps pyproject
  
  pure $ DependencyResults
    { dependencyGraph = Graphing.fromList deps
    , dependencyGraphBreadth = Complete
    , dependencyManifestFiles = [pyprojectFile project]
    }

extractPoetryDeps :: PyProject.PyProject -> [Dependency]
extractPoetryDeps pyproject = filter notNamedPython $ 
  PyProject.allPoetryProductionDeps pyproject ++ PyProject.allPoetryNonProductionDeps pyproject
  where
    notNamedPython = (/= "python") . dependencyName

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
  deps <- pyProjectDeps pyproject
  
  pure $ DependencyResults
    { dependencyGraph = deps
    , dependencyGraphBreadth = Complete
    , dependencyManifestFiles = [pyprojectFile project]
    }

-- Helper functions from old PDM strategy

reqsFromPyProject :: PyProject.PyProject -> ([Req], [Req])
reqsFromPyProject pyproject = 
  case PyProject.pyprojectMetadata pyproject of
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