
module App.Fossa.Analyze.Project
  ( Project(..)
  , ProjectStrategy(..)

  , mkProjects
  ) where

import Prologue

import qualified Data.Map as M
import Data.Function (on)
import Data.List (sortBy)
import Data.Ord

import App.Fossa.Analyze.GraphMangler (graphingToGraph)
import DepTypes
import Graphing
import Types

data Project = Project
  { projectPath       :: Path Abs Dir
  , projectStrategies :: [ProjectStrategy]
  }
  deriving (Eq, Ord, Show, Generic)

data ProjectStrategy = ProjectStrategy
  { projStrategyName     :: Text
  , projStrategyGraph    :: Graphing Dependency
  , projStrategyOptimal  :: Optimal
  , projStrategyComplete :: Complete
  } deriving (Eq, Ord, Show, Generic)

mkProjects :: [ProjectClosure] -> [Project]
mkProjects = toProjects . grouping
  where
  toProjects :: Map (StrategyGroup, Path Abs Dir) [ProjectClosure] -> [Project]
  toProjects = fmap toProject . M.toList

  toProject :: ((StrategyGroup, Path Abs Dir), [ProjectClosure]) -> Project
  toProject ((_, dir), closures) = Project
    { projectPath = dir
    , projectStrategies = fmap toProjectStrategy $
        sortBy (comparator `on` closureDependencies) closures
    }

  comparator :: ProjectDependencies -> ProjectDependencies -> Ordering
  comparator = comparing dependenciesOptimal <> comparing dependenciesComplete

  toProjectStrategy :: ProjectClosure -> ProjectStrategy
  toProjectStrategy ProjectClosure{..} =
    ProjectStrategy { projStrategyName = closureStrategyName
                    , projStrategyGraph = dependenciesGraph closureDependencies
                    , projStrategyOptimal = dependenciesOptimal closureDependencies
                    , projStrategyComplete = dependenciesComplete closureDependencies
                    }

  grouping :: [ProjectClosure] -> Map (StrategyGroup, Path Abs Dir) [ProjectClosure]
  grouping closures = M.fromListWith (<>) $ toList $ do
    closure <- closures
    let strategyGroup = closureStrategyGroup closure
        moduleDir = closureModuleDir closure

    pure ((strategyGroup, moduleDir), [closure])

instance ToJSON Project where
  toJSON Project{..} = object
    [ "path"       .= projectPath
    , "strategies" .= projectStrategies
    ]

instance ToJSON ProjectStrategy where
  toJSON ProjectStrategy{..} = object
    [ "name"     .= projStrategyName
    , "graph"    .= graphingToGraph projStrategyGraph
    , "optimal"  .= projStrategyOptimal
    , "complete" .= projStrategyComplete
    ]
