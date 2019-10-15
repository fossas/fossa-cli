
module App.Scan.Project
  ( Project(..)
  , ProjectStrategy(..)

  , mkProjects
  ) where

import Prologue

import qualified Data.Map as M
import Data.List (findIndex, sortOn)

import Graph (Graph)
import Types

data Project = Project
  { projectPath       :: Path Rel Dir
  , projectStrategies :: [ProjectStrategy]
  }
  deriving (Eq, Ord, Show, Generic)

data ProjectStrategy = ProjectStrategy
  { projStrategyName     :: String
  , projStrategyGraph    :: Graph
  , projStrategyOptimal  :: Optimal
  , projStrategyComplete :: Complete
  } deriving (Eq, Ord, Show, Generic)

type StrategyName = String
type StrategyGroupName = String

mkProjects :: [StrategyGroup] -> [CompletedStrategy] -> [Project]
mkProjects groups = toProjects . grouping
  where
  toProjects :: Map (StrategyGroupName, Path Rel Dir) [CompletedStrategy] -> [Project]
  toProjects = map toProject . M.toList

  toProject :: ((StrategyGroupName, Path Rel Dir), [CompletedStrategy]) -> Project
  toProject ((_, dir), completed) = Project
    { projectPath = dir
    , projectStrategies = map toProjectStrategy (sortOn (ixInGroup . completedName) completed)
    }

  toProjectStrategy :: CompletedStrategy -> ProjectStrategy
  toProjectStrategy CompletedStrategy{..} =
    ProjectStrategy { projStrategyName = completedName
                    , projStrategyGraph = completedGraph
                    , projStrategyOptimal = completedOptimal
                    , projStrategyComplete = completedComplete
                    }

  grouping :: [CompletedStrategy] -> Map (StrategyGroupName, Path Rel Dir) [CompletedStrategy]
  grouping completed = M.fromListWith (++)
    [((groupName, moduleDir), [complete])
      | complete <- completed
      , let groupName = completedToGroup complete
      , let moduleDir = completedModule complete
      ]

  completedToGroup :: CompletedStrategy -> StrategyGroupName
  completedToGroup CompletedStrategy{completedName} =
    case M.lookup completedName groupsByStrategy of
      Just name -> name
      Nothing -> completedName -- use the strategy name as a group name if a group doesn't exist

  ixInGroup :: StrategyName -> Int
  ixInGroup stratName = fromMaybe 0 $ do -- Maybe monad
    groupName <- M.lookup stratName groupsByStrategy
    group     <- M.lookup groupName groupsByName

    ix <- findIndex (\(SomeStrategy strat) -> strategyName strat == stratName)
                    (groupStrategies group)

    pure ix

  groupsByName :: Map StrategyGroupName StrategyGroup
  groupsByName = M.fromList [(groupName group, group) | group <- groups]

  groupsByStrategy :: Map StrategyName StrategyGroupName
  groupsByStrategy = M.fromList
    [(stratName, groupName) | StrategyGroup groupName strategies <- groups
                            , SomeStrategy strat <- strategies
                            , let stratName = strategyName strat
                            ]
