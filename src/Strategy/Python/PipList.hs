{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Strategy.Python.PipList
  ( discover
  , analyze

  , PipListDep(..)
  , buildGraph
  )
  where

import Control.Effect.Diagnostics
import Data.Aeson
import Data.Foldable (find)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import DepTypes
import Discovery.Walk
import Effect.Exec
import Graphing (Graphing)
import qualified Graphing
import Path
import Types

discover :: HasDiscover sig m => Path Abs Dir -> m ()
discover = walk $ \dir _ files -> do
  case find (\f -> fileName f `elem` ["setup.py", "requirements.txt"]) files of
    Nothing -> pure ()
    Just _ -> runSimpleStrategy "python-piplist" PythonGroup $ analyze dir

  pure WalkContinue

pipListCmd :: Text -> Command
pipListCmd baseCmd = Command
  { cmdName = baseCmd
  , cmdArgs = ["list", "--format=json"]
  , cmdAllowErr = Never
  }

analyze :: (Has Exec sig m, Has Diagnostics sig m) => Path Abs Dir -> m ProjectClosureBody
analyze dir = do
  deps <- execJson @[PipListDep] dir (pipListCmd "pip3")
            <||> execJson @[PipListDep] dir (pipListCmd "pip")
  pure (mkProjectClosure dir deps)

mkProjectClosure :: Path Abs Dir -> [PipListDep] -> ProjectClosureBody
mkProjectClosure dir deps = ProjectClosureBody
  { bodyModuleDir    = dir
  , bodyDependencies = dependencies
  , bodyLicenses     = []
  }
  where
  dependencies = ProjectDependencies
    { dependenciesGraph    = buildGraph deps
    , dependenciesOptimal  = NotOptimal
    , dependenciesComplete = NotComplete
    }

buildGraph :: [PipListDep] -> Graphing Dependency
buildGraph = Graphing.fromList . map toDependency
  where
  toDependency PipListDep{..} =
    Dependency { dependencyType = PipType
               , dependencyName = depName
               , dependencyVersion = Just (CEq depVersion)
               , dependencyLocations = []
               , dependencyEnvironments = []
               , dependencyTags = M.empty
               }

data PipListDep = PipListDep
  { depName    :: Text
  , depVersion :: Text
  } deriving (Eq, Ord, Show)

instance FromJSON PipListDep where
  parseJSON = withObject "PipListDep" $ \obj ->
    PipListDep <$> obj .: "name"
               <*> obj .: "version"
