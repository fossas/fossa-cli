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
import Data.Maybe (isJust)
import Data.Text qualified as Text
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
import qualified Strategy.Python.Poetry.PyProject as PyProject
import Strategy.Python.Poetry.Common (pyProjectDeps)
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
  pyproject <- readContentsToml @PyProject.PyProject (pyprojectFile project)
  deps <- pyProjectDeps pyproject
  
  pure $ DependencyResults
    { dependencyGraph = deps
    , dependencyGraphBreadth = Complete
    , dependencyManifestFiles = [pyprojectFile project]
    }

getDeps ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  , Has Logger sig m
  ) =>
  PyProjectProject -> m DependencyResults
getDeps = analyze