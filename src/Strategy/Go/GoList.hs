{-# LANGUAGE RecordWildCards #-}

module Strategy.Go.GoList (
  analyze',
  Require (..),
  GoListModule (..),
  GoModuleReplacement (..),
) where

import Control.Effect.Diagnostics hiding (fromMaybe)
import Control.Monad (void)
import Data.Aeson (FromJSON, withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types (formatError, parseJSON)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.String.Conversion (toText)
import Data.Text (Text)
import DepTypes (Dependency)
import Diag.Common (
  MissingDeepDeps (MissingDeepDeps),
  MissingEdges (MissingEdges),
 )
import Effect.Exec (
  AllowErr (Never),
  Command (..),
  Exec,
  ExecErr (CommandParseError),
  execThrow,
 )
import Effect.Grapher (deep, direct, label)
import Graphing (Graphing)
import Path
import Strategy.Go.Transitive (decodeMany, fillInTransitive)
import Strategy.Go.Types (
  GolangGrapher,
  graphingGolang,
  mkGolangPackage,
  mkGolangVersion,
 )
import Types (GraphBreadth (..))

data Require = Require
  { reqPackage :: Text
  , reqVersion :: Text
  , isDirect :: Bool
  }
  deriving (Eq, Ord, Show)

goListJsonCmd :: Command
goListJsonCmd =
  Command
    { cmdName = "go"
    , cmdArgs = ["list", "-m", "-json", "all"]
    , cmdAllowErr = Never
    }

data GoModuleReplacement = GoModuleReplacement
  { pathReplacement :: Text
  , versionReplacement :: Text
  }
  deriving (Show, Eq, Ord)

instance FromJSON GoModuleReplacement where
  parseJSON = withObject "GoModuleReplacement" $ \obj ->
    GoModuleReplacement
      <$> obj .: "Path"
      <*> obj .: "Version"

data GoListModule = GoListModule
  { path :: Text
  , version :: Maybe Text
  , isMain :: Bool
  , isIndirect :: Bool
  , moduleReplacement :: Maybe GoModuleReplacement
  }
  deriving (Show, Eq, Ord)

instance FromJSON GoListModule where
  parseJSON = withObject "GoListModule" $ \obj ->
    GoListModule
      <$> obj .: "Path"
      <*> obj .:? "Version"
      <*> (obj .:? "Main" .!= False)
      <*> (obj .:? "Indirect" .!= False)
      <*> obj .:? "Replace"

-- | Analyze using `go list`, and build dependency graph.
--
-- Since, sometimes go list directive includes test transitive dependencies in the listing
-- We may include test transitive dependencies as deep dependencies on the graph without any edges.
analyze' ::
  ( Has Exec sig m
  , Has Diagnostics sig m
  ) =>
  Path Abs Dir ->
  m (Graphing Dependency, GraphBreadth)
analyze' dir = do
  graph <- graphingGolang $ do
    stdout <- context ("Getting direct dependencies using, " <> toText (show goListJsonCmd)) $ execThrow dir goListJsonCmd
    case decodeMany stdout of
      Left (path, err) -> fatal (CommandParseError goListJsonCmd (toText (formatError path err)))
      Right (mods :: [GoListModule]) -> do
        context "Adding direct dependencies" $ buildGraph (toRequires mods)
        void
          . recover
          . warnOnErr MissingDeepDeps
          . warnOnErr MissingEdges
          $ fillInTransitive dir
        pure ()
  pure (graph, Complete)
  where
    toRequires :: [GoListModule] -> [Require]
    toRequires src = map (\m -> Require (path m) (fromMaybe "LATEST" $ version m) (not $ isIndirect m)) (withoutMain src)

    withoutMain :: [GoListModule] -> [GoListModule]
    withoutMain = filter (not . isMain)

buildGraph :: Has GolangGrapher sig m => [Require] -> m ()
buildGraph = traverse_ go
  where
    go :: Has GolangGrapher sig m => Require -> m ()
    go Require{..} = do
      let pkg = mkGolangPackage reqPackage
      if isDirect
        then direct pkg
        else deep pkg
      label pkg (mkGolangVersion reqVersion)
