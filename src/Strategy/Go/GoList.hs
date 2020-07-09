{-# language TemplateHaskell #-}

module Strategy.Go.GoList
  ( discover
  , analyze

  , Require(..)
  )
  where

import Prologue hiding ((<?>))

import Control.Effect.Diagnostics
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import DepTypes
import Discovery.Walk
import Effect.Exec
import Effect.Grapher
import Graphing (Graphing)
import Strategy.Go.Transitive (fillInTransitive)
import Strategy.Go.Types
import Types

discover :: HasDiscover sig m => Path Abs Dir -> m ()
discover = walk $ \_ _ files -> do
  case find (\f -> fileName f == "go.mod") files of
    Nothing -> pure ()
    Just file  -> runSimpleStrategy "golang-golist" GolangGroup $ analyze (parent file)

  pure $ WalkSkipSome [$(mkRelDir "vendor")]

data Require = Require
  { reqPackage :: Text
  , reqVersion :: Text
  } deriving (Eq, Ord, Show, Generic)

golistCmd :: Command
golistCmd = Command
  { cmdName = "go"
  , cmdArgs = ["list", "-m", "all"]
  , cmdAllowErr = Never
  }

analyze ::
  ( Has Exec sig m
  , Has Diagnostics sig m
  )
  => Path Abs Dir -> m ProjectClosureBody
analyze dir = fmap (mkProjectClosure dir) . graphingGolang $ do
  stdout <- execThrow dir golistCmd

  let gomodLines = drop 1 . T.lines . T.filter (/= '\r') . decodeUtf8 . BL.toStrict $ stdout -- the first line is our package
      requires = mapMaybe toRequire gomodLines

      toRequire :: Text -> Maybe Require
      toRequire line =
        case T.splitOn " " line of
          [package, version] -> Just (Require package version)
          _ -> Nothing

  buildGraph requires

  _ <- recover (fillInTransitive dir)
  pure ()

mkProjectClosure :: Path Abs Dir -> Graphing Dependency -> ProjectClosureBody
mkProjectClosure dir graph = ProjectClosureBody
  { bodyModuleDir    = dir
  , bodyDependencies = dependencies
  , bodyLicenses     = []
  }
  where
  dependencies = ProjectDependencies
    { dependenciesGraph    = graph
    , dependenciesOptimal  = Optimal
    , dependenciesComplete = NotComplete
    }

buildGraph :: Has GolangGrapher sig m => [Require] -> m ()
buildGraph = traverse_ go
  where

  go :: Has GolangGrapher sig m => Require -> m ()
  go Require{..} = do
    let pkg = mkGolangPackage reqPackage
    direct pkg
    label pkg (mkGolangVersion reqVersion)
