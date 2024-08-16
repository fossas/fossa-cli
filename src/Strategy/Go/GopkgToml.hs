{-# LANGUAGE RecordWildCards #-}

module Strategy.Go.GopkgToml (
  Gopkg (..),
  PkgConstraint (..),
  analyze',
  buildGraph,
) where

import Control.Applicative ((<|>))
import Control.Effect.Diagnostics
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import DepTypes
import Diag.Common (
  MissingDeepDeps (MissingDeepDeps),
  MissingEdges (MissingEdges),
 )
import Effect.Exec
import Effect.Grapher
import Effect.ReadFS
import Graphing (Graphing)
import Path
import Strategy.Go.Transitive (fillInTransitive)
import Strategy.Go.Types
import Toml.Schema qualified

data Gopkg = Gopkg
  { pkgConstraints :: [PkgConstraint]
  , pkgOverrides :: [PkgConstraint]
  }
  deriving (Eq, Ord, Show)

instance Toml.Schema.FromValue Gopkg where
  fromValue =
    Toml.Schema.parseTableFromValue $
      Gopkg
        <$> Toml.Schema.reqKey "constraint"
        <*> Toml.Schema.reqKey "override"

data PkgConstraint = PkgConstraint
  { constraintName :: Text
  , constraintSource :: Maybe Text
  , constraintVersion :: Maybe Text
  , constraintBranch :: Maybe Text
  , constraintRevision :: Maybe Text
  }
  deriving (Eq, Ord, Show)

instance Toml.Schema.FromValue PkgConstraint where
  fromValue =
    Toml.Schema.parseTableFromValue $
      PkgConstraint
        <$> Toml.Schema.reqKey "name"
        <*> Toml.Schema.optKey "source"
        <*> Toml.Schema.optKey "version"
        <*> Toml.Schema.optKey "branch"
        <*> Toml.Schema.optKey "revision"

analyze' ::
  ( Has ReadFS sig m
  , Has Exec sig m
  , Has Diagnostics sig m
  ) =>
  Path Abs File ->
  m (Graphing Dependency)
analyze' file = graphingGolang $ do
  gopkg <- readContentsToml file
  context "Building dependency graph" $ buildGraph gopkg
  void
    . recover
    . warnOnErr MissingDeepDeps
    . warnOnErr MissingEdges
    $ fillInTransitive (parent file)

buildGraph :: Has GolangGrapher sig m => Gopkg -> m ()
buildGraph = void . Map.traverseWithKey go . resolve
  where
    go :: Has GolangGrapher sig m => Text -> PkgConstraint -> m ()
    go name PkgConstraint{..} = do
      let pkg = mkGolangPackage name

      direct pkg

      -- label version when it exists
      traverse_
        (label pkg . mkGolangVersion)
        (constraintVersion <|> constraintBranch <|> constraintRevision)

      -- label location when it exists
      traverse_ (label pkg . GolangLabelLocation) constraintSource

-- TODO: handling version constraints
resolve :: Gopkg -> Map Text PkgConstraint -- Map Package (Maybe Version)
resolve gopkg = overridden
  where
    overridden = foldr inserting constraints (pkgOverrides gopkg)
    constraints = foldr inserting Map.empty (pkgConstraints gopkg)

    inserting :: PkgConstraint -> Map Text PkgConstraint -> Map Text PkgConstraint
    inserting constraint = Map.insert (constraintName constraint) constraint
