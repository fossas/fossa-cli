module Strategy.Python.PipList
  ( discover
  , analyze

  , PipListDep(..)
  , buildGraph
  )
  where

import Prologue

import Control.Carrier.Lift
import Control.Carrier.Error.Either
import qualified Data.Map.Strict as M

import DepTypes
import Discovery.Walk
import Effect.Exec
import Graphing
import Types

discover :: HasDiscover sig m => Path Abs Dir -> m ()
discover = walk $ \dir _ files -> do
  case find (\f -> fileName f `elem` ["setup.py", "requirements.txt"]) files of
    Nothing -> pure ()
    Just _ -> runSimpleStrategy "python-piplist" PythonGroup $ analyze dir

  walkContinue

pipListCmd :: Command
pipListCmd = Command
  { cmdNames = ["pip3", "pip"]
  , cmdBaseArgs = ["list", "--format=json"]
  , cmdAllowErr = Never
  }

analyze :: (Has Exec sig m, Has (Error ExecErr) sig m) => Path Rel Dir -> m ProjectClosure
analyze dir = mkProjectClosure dir <$> execJson @[PipListDep] dir pipListCmd []

mkProjectClosure :: Path Rel Dir -> [PipListDep] -> ProjectClosure
mkProjectClosure dir deps = ProjectClosure
  { closureStrategyGroup = PythonGroup
  , closureStrategyName  = "python-piplist"
  , closureModuleDir     = dir
  , closureDependencies  = dependencies
  , closureLicenses      = []
  }
  where
  dependencies = ProjectDependencies
    { dependenciesGraph    = buildGraph deps
    , dependenciesOptimal  = NotOptimal
    , dependenciesComplete = NotComplete
    }

buildGraph :: [PipListDep] -> Graphing Dependency
buildGraph xs = unfold xs (const []) toDependency
  where
  toDependency PipListDep{..} =
    Dependency { dependencyType = PipType
               , dependencyName = depName
               , dependencyVersion = Just (CEq depVersion)
               , dependencyLocations = []
               , dependencyTags = M.empty
               }

data PipListDep = PipListDep
  { depName    :: Text
  , depVersion :: Text
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON PipListDep where
  parseJSON = withObject "PipListDep" $ \obj ->
    PipListDep <$> obj .: "name"
               <*> obj .: "version"
