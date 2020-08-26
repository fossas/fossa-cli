{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Analyze.Project
  ( Project(..)
  , ProjectStrategy(..)

  , mkProjects
  ) where

import App.Fossa.Analyze.GraphMangler (graphingToGraph)
import Data.Aeson
import Data.Foldable (toList)
import Data.Function (on)
import Data.List (sortBy)
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Ord
import Data.Text (Text)
import DepTypes
import Graphing
import Path
import Types

data Project = Project
  { projectPath       :: Path Abs Dir
  , projectStrategies :: [ProjectStrategy]
  }
  deriving (Eq, Ord, Show)

data ProjectStrategy = ProjectStrategy
  { projStrategyName     :: Text
  , projStrategyGraph    :: Graphing Dependency
  , projStrategyOptimal  :: Optimal
  , projStrategyComplete :: Complete
  } deriving (Eq, Ord, Show)

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
