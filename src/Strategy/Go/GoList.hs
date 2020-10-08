{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Strategy.Go.GoList
  ( analyze'

  , Require(..)
  )
  where

import Control.Effect.Diagnostics
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (traverse_)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import DepTypes
import Effect.Exec
import Effect.Grapher
import Graphing (Graphing)
import Path
import Strategy.Go.Transitive (fillInTransitive)
import Strategy.Go.Types

data Require = Require
  { reqPackage :: Text
  , reqVersion :: Text
  } deriving (Eq, Ord, Show)

golistCmd :: Command
golistCmd = Command
  { cmdName = "go"
  , cmdArgs = ["list", "-m", "all"]
  , cmdAllowErr = Never
  }

analyze' ::
  ( Has Exec sig m
  , Has Diagnostics sig m
  )
  => Path Abs Dir -> m (Graphing Dependency)
analyze' dir = graphingGolang $ do
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

buildGraph :: Has GolangGrapher sig m => [Require] -> m ()
buildGraph = traverse_ go
  where

  go :: Has GolangGrapher sig m => Require -> m ()
  go Require{..} = do
    let pkg = mkGolangPackage reqPackage
    direct pkg
    label pkg (mkGolangVersion reqVersion)
