
module App.Scan.Project
  ( Project(..)
  , ProjectStrategy(..)

  , mkProjects
  ) where

import Prologue

import qualified Data.Map as M
import qualified Data.Sequence as S
import Data.List (findIndex)

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

type StrategyName = Text
type StrategyGroupName = Text

mkProjects :: [StrategyGroup] -> S.Seq CompletedStrategy -> [Project]
mkProjects groups = toProjects . grouping
  where
  toProjects :: Map (StrategyGroupName, Path Rel Dir) (S.Seq CompletedStrategy) -> [Project]
  toProjects = fmap toProject . M.toList

  toProject :: ((StrategyGroupName, Path Rel Dir), S.Seq CompletedStrategy) -> Project
  toProject ((_, dir), completed) = Project
    { projectPath = dir
    , projectStrategies = fmap toProjectStrategy (S.sortOn (ixInGroup . completedName) completed)
    }

  toProjectStrategy :: CompletedStrategy -> ProjectStrategy
  toProjectStrategy CompletedStrategy{..} =
    ProjectStrategy { projStrategyName = completedName
                    , projStrategyGraph = graphingToGraph completedGraph
                    , projStrategyOptimal = completedOptimal
                    , projStrategyComplete = completedComplete
                    }

  grouping :: S.Seq CompletedStrategy -> Map (StrategyGroupName, Path Rel Dir) (S.Seq CompletedStrategy)
  grouping completed = M.fromListWith (<>) $ toList $ do
    complete <- completed
    let groupName = completedToGroup complete
        moduleDir = completedModule complete

    pure ((groupName, moduleDir), S.singleton complete)

  completedToGroup :: CompletedStrategy -> StrategyGroupName
  completedToGroup CompletedStrategy{completedName} =
    -- use the strategy name as a group name if a group doesn't exist
    fromMaybe completedName (M.lookup completedName groupsByStrategy)

  ixInGroup :: StrategyName -> Int
  ixInGroup stratName = fromMaybe 0 $ do -- Maybe monad
    groupName <- M.lookup stratName groupsByStrategy
    group     <- M.lookup groupName groupsByName

    findIndex (\(SomeStrategy strat) -> strategyName strat == stratName)
              (groupStrategies group)

  groupsByName :: Map StrategyGroupName StrategyGroup
  groupsByName = M.fromList [(groupName group, group) | group <- groups]

  groupsByStrategy :: Map StrategyName StrategyGroupName
  groupsByStrategy = M.fromList
    [(stratName, groupName) | StrategyGroup groupName strategies <- groups
                            , SomeStrategy strat <- strategies
                            , let stratName = strategyName strat
                            ]

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
