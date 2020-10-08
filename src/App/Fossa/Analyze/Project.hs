{-# LANGUAGE RecordWildCards #-}

module App.Fossa.Analyze.Project
  ( ProjectResult(..)
  , mkResult
  ) where

import Data.Text (Text)
import DepTypes
import Graphing (Graphing)
import qualified Graphing
import Path
import Types

mkResult :: DiscoveredProject -> Graphing Dependency -> ProjectResult
mkResult project graph = ProjectResult
  { projectResultType = projectType project
  , projectResultPath = projectPath project
  , projectResultGraph = Graphing.pruneUnreachable graph
  }

data ProjectResult = ProjectResult
  { projectResultType :: Text
  , projectResultPath :: Path Abs Dir
  , projectResultGraph :: Graphing Dependency
  }
