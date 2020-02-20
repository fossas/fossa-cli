
module App.Scan.Project
  ( Project(..)
  , ProjectStrategy(..)

  , mkProjects
  ) where

import Prologue

import qualified Data.Map as M
import qualified Data.Sequence as S
import Data.Function (on)
import Data.Ord

import App.Scan.Graph (Graph)
import App.Scan.GraphMangler (graphingToGraph)
import Types

data Project = Project
  { projectPath       :: Path Rel Dir
  , projectStrategies :: S.Seq ProjectStrategy
  }
  deriving (Eq, Ord, Show, Generic)

data ProjectStrategy = ProjectStrategy
  { projStrategyName     :: Text
  , projStrategyGraph    :: Graph
  , projStrategyOptimal  :: Optimal
  , projStrategyComplete :: Complete
  } deriving (Eq, Ord, Show, Generic)

mkProjects :: S.Seq ProjectClosure -> [Project]
mkProjects = toProjects . grouping
  where
  toProjects :: Map (StrategyGroup, Path Rel Dir) (S.Seq ProjectClosure) -> [Project]
  toProjects = fmap toProject . M.toList

  toProject :: ((StrategyGroup, Path Rel Dir), S.Seq ProjectClosure) -> Project
  toProject ((_, dir), closures) = Project
    { projectPath = dir
    , projectStrategies = fmap toProjectStrategy $
        S.sortBy (comparator `on` closureDependencies) closures
    }

  comparator :: ProjectDependencies -> ProjectDependencies -> Ordering
  comparator = comparing dependenciesOptimal <> comparing dependenciesComplete

  toProjectStrategy :: ProjectClosure -> ProjectStrategy
  toProjectStrategy ProjectClosure{..} =
    ProjectStrategy { projStrategyName = closureStrategyName
                    , projStrategyGraph = graphingToGraph (dependenciesGraph closureDependencies)
                    , projStrategyOptimal = dependenciesOptimal closureDependencies
                    , projStrategyComplete = dependenciesComplete closureDependencies
                    }

  grouping :: S.Seq ProjectClosure -> Map (StrategyGroup, Path Rel Dir) (S.Seq ProjectClosure)
  grouping closures = M.fromListWith (<>) $ toList $ do
    closure <- closures
    let strategyGroup = closureStrategyGroup closure
        moduleDir = closureModuleDir closure

    pure ((strategyGroup, moduleDir), S.singleton closure)

instance ToJSON Project where
  toJSON Project{..} = object
    [ "path"       .= projectPath
    , "strategies" .= projectStrategies
    ]

instance ToJSON ProjectStrategy where
  toJSON ProjectStrategy{..} = object
    [ "name"     .= projStrategyName
    , "graph"    .= projStrategyGraph
    , "optimal"  .= projStrategyOptimal
    , "complete" .= projStrategyComplete
    ]
