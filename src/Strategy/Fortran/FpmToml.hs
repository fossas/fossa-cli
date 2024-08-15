module Strategy.Fortran.FpmToml (
  analyzeFpmToml,

  -- * for testing
  FpmToml (..),
  FpmDependency (..),
  FpmPathDependency (..),
  FpmGitDependency (..),
  FpmTomlExecutables (..),
  buildGraph,
) where

import Control.Effect.Diagnostics (Diagnostics, context)
import Data.Foldable (asum)
import Data.Map (Map, elems)
import Data.Set qualified as Set
import Data.Text (Text)
import DepTypes (
  DepEnvironment (EnvDevelopment, EnvProduction),
  DepType (GitType),
  Dependency (..),
  VerConstraint (CEq),
 )
import Effect.ReadFS (Has, ReadFS, readContentsToml)
import Graphing (Graphing, directs, induceJust)
import Path
import Toml qualified
import Toml.Schema qualified

-- | Represents the content of the fpm manifest.
-- Reference: https://github.com/fortran-lang/fpm/blob/main/manifest-reference.md
data FpmToml = FpmToml
  { fpmDependencies :: Map Text FpmDependency
  , fpmDevDependencies :: Map Text FpmDependency
  , fpmExecutables :: [FpmTomlExecutables]
  }
  deriving (Eq, Ord, Show)

instance Toml.Schema.FromValue FpmToml where
  fromValue =
    Toml.Schema.parseTableFromValue $
      FpmToml
        <$> Toml.Schema.pickKey [Toml.Schema.Key "dependencies" Toml.Schema.fromValue, Toml.Schema.Else $ pure mempty]
        <*> Toml.Schema.pickKey [Toml.Schema.Key "dev-dependencies" Toml.Schema.fromValue, Toml.Schema.Else $ pure mempty]
        <*> Toml.Schema.pickKey [Toml.Schema.Key "executable" Toml.Schema.fromValue, Toml.Schema.Else $ pure []]

newtype FpmTomlExecutables = FpmTomlExecutables
  { fpmExecutableDependencies :: Map Text FpmDependency
  }
  deriving (Eq, Ord, Show)

instance Toml.Schema.FromValue FpmTomlExecutables where
  fromValue =
    Toml.Schema.parseTableFromValue $
      FpmTomlExecutables
        <$> Toml.Schema.pickKey [Toml.Schema.Key "dependencies" Toml.Schema.fromValue, Toml.Schema.Else $ pure mempty]

data FpmDependency
  = FpmGitDep FpmGitDependency
  | FpmPathDep FpmPathDependency
  deriving (Eq, Ord, Show)

instance Toml.Schema.FromValue FpmDependency where
  fromValue v@(Toml.Schema.Table' l t) =
    Toml.Schema.parseTable
      ( Toml.Schema.pickKey
          [ Toml.Schema.Key "git" (const (FpmGitDep <$> Toml.Schema.fromValue v))
          , Toml.Schema.Key "path" (const (FpmPathDep <$> Toml.Schema.fromValue v))
          , Toml.Schema.Else (Toml.Schema.failAt (Toml.valueAnn v) "Expected either 'git' or 'path' key got: ")
          ]
      )
      l
      t
  fromValue v = Toml.Schema.failAt (Toml.valueAnn v) "Expected a table"

newtype FpmPathDependency = FpmPathDependency
  { pathOf :: Text
  }
  deriving (Eq, Ord, Show)

instance Toml.Schema.FromValue FpmPathDependency where
  fromValue = Toml.Schema.parseTableFromValue $ FpmPathDependency <$> Toml.Schema.reqKey "path"

data FpmGitDependency = FpmGitDependency
  { url :: Text
  , branch :: Maybe Text
  , tag :: Maybe Text
  , rev :: Maybe Text
  }
  deriving (Eq, Ord, Show)

instance Toml.Schema.FromValue FpmGitDependency where
  fromValue =
    Toml.Schema.parseTableFromValue $
      FpmGitDependency
        <$> Toml.Schema.reqKey "git"
        <*> Toml.Schema.optKey "branch"
        <*> Toml.Schema.optKey "tag"
        <*> Toml.Schema.optKey "rev"

buildGraph :: FpmToml -> Graphing Dependency
buildGraph fpmToml = induceJust $ foldMap directs [deps, execDeps, devDeps]
  where
    deps :: [Maybe Dependency]
    deps = map toProdDependency (elems $ fpmDependencies fpmToml)

    execDeps :: [Maybe Dependency]
    execDeps = map toProdDependency (foldMap (elems . fpmExecutableDependencies) (fpmExecutables fpmToml))

    devDeps :: [Maybe Dependency]
    devDeps = map toDevDependency (elems $ fpmDevDependencies fpmToml)

    toProdDependency :: FpmDependency -> Maybe Dependency
    toProdDependency = toDependency $ Just EnvProduction

    toDevDependency :: FpmDependency -> Maybe Dependency
    toDevDependency = toDependency $ Just EnvDevelopment

    toDependency :: Maybe DepEnvironment -> FpmDependency -> Maybe Dependency
    toDependency _ (FpmPathDep _) = Nothing
    toDependency env (FpmGitDep dep) =
      Just $
        Dependency
          { dependencyType = GitType
          , dependencyName = url dep
          , dependencyVersion = CEq <$> asum [rev dep, tag dep, branch dep]
          , dependencyLocations = []
          , dependencyEnvironments =
              maybe mempty Set.singleton env
          , dependencyTags = mempty
          }

analyzeFpmToml ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  ) =>
  Path Abs File ->
  m (Graphing Dependency)
analyzeFpmToml tomlFile = do
  fpmTomlContent <- readContentsToml tomlFile
  context "Building dependency graph from fpm.toml" $ pure $ buildGraph fpmTomlContent
