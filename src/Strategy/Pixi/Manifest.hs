-- | Reads the dependencies a pixi manifest declares.
--
-- Written against pixi 0.83.0. Dependency tables are keyed by package name:
-- <https://pixi.prefix.dev/v0.83.0/reference/pixi_manifest/>
--
-- > [dependencies]
-- > python = "==3.11"
-- >
-- > [feature.test.pypi-dependencies]
-- > pyBigWig = "*"
--
-- In @pyproject.toml@ they sit under @[tool.pixi]@:
-- <https://pixi.prefix.dev/v0.83.0/python/pyproject_toml/>
module Strategy.Pixi.Manifest (
  PixiManifestDeps (..),
  PyprojectPixiDeps (..),
  reqParts,
) where

import Control.Monad (join)
import Data.Foldable (fold)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Strategy.Python.Poetry.PyProject (PyProjectMetadata (..))
import Strategy.Python.Util (Marker, Req (NameReq, UrlReq))
import Toml qualified
import Toml.Schema qualified

-- | Declared dependency names, by package kind.
data PixiManifestDeps = PixiManifestDeps
  { manifestCondaDeps :: Set Text
  , manifestPypiDeps :: Map Text (Set Text)
  -- ^ With the extras each requests.
  }
  deriving (Eq, Ord, Show)

instance Semigroup PixiManifestDeps where
  PixiManifestDeps conda pypi <> PixiManifestDeps conda' pypi' = PixiManifestDeps (conda <> conda') (Map.unionWith (<>) pypi pypi')

instance Monoid PixiManifestDeps where
  mempty = PixiManifestDeps mempty mempty

-- | Reads the manifest, a feature, an environment, or a target. pixi installs
-- run, host, and build dependencies alike.
instance Toml.Schema.FromValue PixiManifestDeps where
  fromValue = Toml.Schema.parseTableFromValue $ do
    conda <- traverse names ["dependencies", "host-dependencies", "build-dependencies"]
    pypi <- maybe Map.empty (Map.map unPypiExtras) <$> Toml.Schema.optKey "pypi-dependencies"
    nested <- traverse (fmap fold . Toml.Schema.optKey @(Map Text PixiManifestDeps)) ["target", "feature"]
    environments <- fold <$> Toml.Schema.optKey @(Map Text EnvironmentDeps) "environments"
    pure $ PixiManifestDeps (Set.unions conda) pypi <> fold (fold nested) <> unEnvironmentDeps (fold environments)
    where
      names key = maybe Set.empty (Map.keysSet @Text @Toml.Value) <$> Toml.Schema.optKey key

-- | A version string, or a table that can request extras.
newtype PypiExtras = PypiExtras {unPypiExtras :: Set Text}

instance Toml.Schema.FromValue PypiExtras where
  fromValue (Toml.Text' _ _) = pure $ PypiExtras mempty
  fromValue v = PypiExtras . Set.fromList . fold <$> Toml.Schema.parseTableFromValue (Toml.Schema.optKey "extras") v

-- | An environment is a list of features, or a table that can also declare dependencies.
newtype EnvironmentDeps = EnvironmentDeps {unEnvironmentDeps :: PixiManifestDeps}
  deriving (Semigroup, Monoid)

instance Toml.Schema.FromValue EnvironmentDeps where
  fromValue v@(Toml.List' _ _) = mempty <$ Toml.Schema.fromValue @[Text] v
  fromValue v = EnvironmentDeps <$> Toml.Schema.fromValue v

-- | Nothing when there's no @[tool.pixi]@ table. pixi also installs
-- @[project]@ dependencies, optional dependencies, and dependency groups
-- from PyPI, and declares python for @requires-python@ even when it's unset.
newtype PyprojectPixiDeps = PyprojectPixiDeps (Maybe PixiManifestDeps)

instance Toml.Schema.FromValue PyprojectPixiDeps where
  fromValue = Toml.Schema.parseTableFromValue $ do
    tool <- Toml.Schema.optKeyOf "tool" . Toml.Schema.parseTableFromValue $ Toml.Schema.optKey "pixi"
    case join tool of
      Nothing -> pure $ PyprojectPixiDeps Nothing
      Just pixi -> do
        project <- Toml.Schema.optKey "project"
        groups <- Toml.Schema.optKey @(Map Text [DependencyGroupEntry]) "dependency-groups"
        let reqs =
              foldMap (fold . pyprojectDependencies) project
                <> foldMap (fold . fold . pyprojectOptionalDependencies) project
                <> [req | DependencyGroupEntry (Just req) <- foldMap fold groups]
        pure . PyprojectPixiDeps . Just $ pixi <> PixiManifestDeps (Set.singleton "python") (Map.fromListWith (<>) [(name, Set.fromList extras) | (name, extras, _) <- map reqParts reqs])

-- | A PEP 508 requirement, or an @{include-group = "..."}@ table naming a group read on its own.
newtype DependencyGroupEntry = DependencyGroupEntry (Maybe Req)

instance Toml.Schema.FromValue DependencyGroupEntry where
  fromValue v@(Toml.Table' _ _) = DependencyGroupEntry Nothing <$ Toml.Schema.parseTableFromValue (Toml.Schema.reqKey @Text "include-group") v
  fromValue v = DependencyGroupEntry . Just <$> Toml.Schema.fromValue v

-- | A requirement's name, the extras it requests, and its marker.
reqParts :: Req -> (Text, [Text], Maybe Marker)
reqParts = \case
  NameReq name extras _ marker -> (name, fold extras, marker)
  UrlReq name extras _ marker -> (name, fold extras, marker)
