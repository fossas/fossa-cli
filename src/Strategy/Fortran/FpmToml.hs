module Strategy.Fortran.FpmToml (
  analyzeFpmToml,

  -- * for testing
  FpmToml (..),
  FpmDependency (..),
  FpmPathDependency (..),
  FpmGitDependency (..),
  buildGraph,
  fpmTomlCodec,
) where

import Control.Applicative (Alternative ((<|>)))
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
import Toml (TomlCodec, (.=))
import Toml qualified

-- | Represents the content of the fpm manifest.
-- Reference: https://github.com/fortran-lang/fpm/blob/main/manifest-reference.md
data FpmToml = FpmToml
  { fpmDependencies :: Map Text FpmDependency
  , fpmDevDependencies :: Map Text FpmDependency
  , fpmExecutables :: [Map Text FpmDependency]
  }
  deriving (Eq, Ord, Show)

fpmTomlCodec :: TomlCodec FpmToml
fpmTomlCodec =
  FpmToml
    <$> Toml.tableMap Toml._KeyText fpmDependenciesCodec "dependencies" .= fpmDependencies
    <*> Toml.tableMap Toml._KeyText fpmDependenciesCodec "dev-dependencies" .= fpmDevDependencies
    <*> Toml.list fpmExecutableDependenciesCodec "executable" .= fpmExecutables

data FpmDependency
  = FpmGitDep FpmGitDependency
  | FpmPathDep FpmPathDependency
  deriving (Eq, Ord, Show)

newtype FpmPathDependency = FpmPathDependency
  { pathOf :: Text
  }
  deriving (Eq, Ord, Show)

data FpmGitDependency = FpmGitDependency
  { url :: Text
  , branch :: Maybe Text
  , tag :: Maybe Text
  , rev :: Maybe Text
  }
  deriving (Eq, Ord, Show)

fpmExecutableDependenciesCodec :: TomlCodec (Map Text FpmDependency)
fpmExecutableDependenciesCodec = Toml.tableMap Toml._KeyText fpmDependenciesCodec "dependencies"

fpmDependenciesCodec :: Toml.Key -> TomlCodec FpmDependency
fpmDependenciesCodec key =
  Toml.dimatch matchFpmPathDep FpmPathDep (Toml.table fpmPathDependencyCodec key)
    <|> Toml.dimatch matchFpmGitDep FpmGitDep (Toml.table fpmGitDependencyCodec key)
  where
    matchFpmPathDep :: FpmDependency -> Maybe FpmPathDependency
    matchFpmPathDep (FpmPathDep pathDep) = Just pathDep
    matchFpmPathDep _ = Nothing

    matchFpmGitDep :: FpmDependency -> Maybe FpmGitDependency
    matchFpmGitDep (FpmGitDep gitDep) = Just gitDep
    matchFpmGitDep _ = Nothing

    fpmPathDependencyCodec :: TomlCodec FpmPathDependency
    fpmPathDependencyCodec = FpmPathDependency <$> Toml.text "path" .= pathOf

    fpmGitDependencyCodec :: TomlCodec FpmGitDependency
    fpmGitDependencyCodec =
      FpmGitDependency
        <$> Toml.text "git" .= url
        <*> Toml.dioptional (Toml.text "branch") .= branch
        <*> Toml.dioptional (Toml.text "tag") .= tag
        <*> Toml.dioptional (Toml.text "rev") .= rev

buildGraph :: FpmToml -> Graphing Dependency
buildGraph fpmToml = induceJust $ foldMap directs [deps, execDeps, devDeps]
  where
    deps :: [Maybe Dependency]
    deps = map toProdDependency (elems $ fpmDependencies fpmToml)

    execDeps :: [Maybe Dependency]
    execDeps = map toProdDependency (foldMap elems $ fpmExecutables fpmToml)

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
  fpmTomlContent <- readContentsToml fpmTomlCodec tomlFile
  context "Building dependency graph from fpm.toml" $ pure $ buildGraph fpmTomlContent
