
module App.Scan
  ( scanMain
  ) where

import Prologue

import Control.Concurrent
import Data.List (findIndex, sortBy)
import Data.Ord
import qualified Data.Map.Strict as M
import Path.IO
import Polysemy
import Polysemy.Async
import Polysemy.Error
import Polysemy.Output
import Polysemy.Resource
import Polysemy.Trace
import Text.Pretty.Simple (pPrint)

import Control.Parallel
import Diagnostics
import Discovery
import Effect.Exec
import Effect.ReadFS
import Graph (Graph)
import Types

scanMain :: Path Abs Dir -> IO ()
scanMain basedir = runFinal
        . embedToFinal @IO
        . resourceToIOFinal
        . asyncToIOFinal
        . traceToIO
        $ scan basedir

scan :: Members '[Final IO, Embed IO, Resource, Async, Trace] r => Path Abs Dir -> Sem r ()
scan basedir = do
  setCurrentDir basedir
  capabilities <- embed getNumCapabilities

  (results, ()) <- runActions capabilities (map ADiscover discoverFuncs) (runAction basedir) updateProgress
    & outputToIOMonoidAssocR (:[])

  let projects = mkProjects strategyGroups results
  embed (pPrint projects)

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
    , projectStrategies = map toProjectStrategy (sortBy (comparing (ixInGroup . completedName)) completed)
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

runAction :: Members '[Final IO, Embed IO, Trace, Output CompletedStrategy] r => Path Abs Dir -> (Action -> Sem r ()) -> Action -> Sem r ()
runAction basedir enqueue = \case
  ADiscover Discover{..} -> do
    trace $ "Starting discovery: " <> discoverName
    result <- discoverFunc basedir
      & readFSToIO
      & execToIO
      & errorToIOFinal @CLIErr
      & runOutputSem @ConfiguredStrategy (enqueue . AStrategy)

    case result of
      Right () -> trace $ "Finished discovery: " <> discoverName
      Left err -> trace $ "ERROR in discovery: " <> discoverName <> " " <> show err

    trace ""

  AStrategy (ConfiguredStrategy Strategy{..} opts) -> do
    result <- strategyAnalyze opts
      & readFSToIO
      & execToIO
      & errorToIOFinal @CLIErr

    case result of
      Right graph -> do
        trace $ "Finished analysis: " <> strategyName <> " " <> show (strategyModule opts) <> ": " <> show graph
        output (CompletedStrategy strategyName (strategyModule opts) graph strategyOptimal strategyComplete)
      Left err -> trace $ "ERROR in strategy: " <> strategyName <> " " <> show (strategyModule opts) <> ": " <> show err

    trace ""

updateProgress :: Member Trace r => Progress -> Sem r ()
updateProgress Progress{..} =
  trace ( "Queued: "
        <> show pQueued
        <> ", Running: "
        <> show pRunning
        <> ", Completed: "
        <> show pCompleted)

data Action =
    ADiscover Discover
  | AStrategy ConfiguredStrategy
