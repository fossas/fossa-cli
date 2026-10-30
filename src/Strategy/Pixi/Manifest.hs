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
  Feature (..),
  PyprojectPixiDeps (..),
  environmentPypiDeps,
  reqParts,
) where

import Control.Monad (join)
import Data.Foldable (fold)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
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
  , manifestPypiDeps :: Map Feature (Map Text (Set Text))
  -- ^ By feature, with the extras each requests.
  , manifestEnvironments :: Map Text [Feature]
  -- ^ The features each environment includes. Others include only the default feature.
  }
  deriving (Eq, Ord, Show)

-- | pixi names the default feature @default@.
data Feature
  = Feature Text
  | -- | The dependencies an environment declares itself.
    EnvironmentFeature Text
  deriving (Eq, Ord, Show)

defaultFeature :: Feature
defaultFeature = Feature "default"

instance Semigroup PixiManifestDeps where
  a <> b =
    PixiManifestDeps
      { manifestCondaDeps = manifestCondaDeps a <> manifestCondaDeps b
      , manifestPypiDeps = Map.unionWith (Map.unionWith (<>)) (manifestPypiDeps a) (manifestPypiDeps b)
      , manifestEnvironments = manifestEnvironments a <> manifestEnvironments b
      }

instance Monoid PixiManifestDeps where
  mempty = PixiManifestDeps{manifestCondaDeps = mempty, manifestPypiDeps = mempty, manifestEnvironments = mempty}

-- | The PyPI dependencies an environment's features declare, with the extras each requests.
environmentPypiDeps :: PixiManifestDeps -> Text -> Map Text (Set Text)
environmentPypiDeps deps env =
  Map.unionsWith (<>) . mapMaybe (`Map.lookup` manifestPypiDeps deps) $
    Map.findWithDefault [defaultFeature] env (manifestEnvironments deps)

-- | Reads the manifest, a feature, an environment, or a target. pixi installs
-- run, host, and build dependencies alike.
instance Toml.Schema.FromValue PixiManifestDeps where
  fromValue = Toml.Schema.parseTableFromValue $ do
    conda <- traverse names ["dependencies", "host-dependencies", "build-dependencies"]
    pypi <- maybe Map.empty (Map.map unPypiExtras) <$> Toml.Schema.optKey "pypi-dependencies"
    targets <- fold <$> Toml.Schema.optKey @(Map Text PixiManifestDeps) "target"
    features <- fold <$> Toml.Schema.optKey @(Map Text PixiManifestDeps) "feature"
    environments <- fold <$> Toml.Schema.optKey @(Map Text Environment) "environments"
    pure $
      mempty{manifestCondaDeps = Set.unions conda, manifestPypiDeps = Map.singleton defaultFeature pypi}
        <> fold targets
        <> Map.foldMapWithKey (inFeature . Feature) features
        <> Map.foldMapWithKey environment environments
    where
      names key = maybe Set.empty (Map.keysSet @Text @Toml.Value) <$> Toml.Schema.optKey key
      environment name (Environment features deps) =
        inFeature (EnvironmentFeature name) deps <> mempty{manifestEnvironments = Map.singleton name (EnvironmentFeature name : features)}

-- | Files a table's PyPI dependencies, including its targets', under one feature.
inFeature :: Feature -> PixiManifestDeps -> PixiManifestDeps
inFeature feature deps = deps{manifestPypiDeps = Map.singleton feature . Map.unionsWith (<>) $ manifestPypiDeps deps}

-- | A version string, or a table that can request extras.
newtype PypiExtras = PypiExtras {unPypiExtras :: Set Text}

instance Toml.Schema.FromValue PypiExtras where
  fromValue (Toml.Text' _ _) = pure $ PypiExtras mempty
  fromValue v = PypiExtras . Set.fromList . fold <$> Toml.Schema.parseTableFromValue (Toml.Schema.optKey "extras") v

-- | The features an environment includes, and the dependencies it declares
-- itself. Unless it sets @no-default-feature@, it includes the default feature.
data Environment = Environment [Feature] PixiManifestDeps

-- | An environment is a list of features, or a table that can also declare dependencies.
instance Toml.Schema.FromValue Environment where
  fromValue v@(Toml.List' _ _) = (\features -> Environment (map Feature features <> [defaultFeature]) mempty) <$> Toml.Schema.fromValue v
  fromValue v = do
    (features, noDefault) <- Toml.Schema.parseTableFromValue ((,) <$> Toml.Schema.optKey "features" <*> Toml.Schema.optKey "no-default-feature") v
    Environment (map Feature (fold features) <> if noDefault == Just True then [] else [defaultFeature]) <$> Toml.Schema.fromValue v

-- | Nothing when there's no @[tool.pixi]@ table. pixi also installs
-- @[project]@ dependencies from PyPI in the default feature, and optional
-- dependencies and dependency groups as features of the same name. It
-- declares python for @requires-python@ even when it's unset.
newtype PyprojectPixiDeps = PyprojectPixiDeps (Maybe PixiManifestDeps)

instance Toml.Schema.FromValue PyprojectPixiDeps where
  fromValue = Toml.Schema.parseTableFromValue $ do
    tool <- Toml.Schema.optKeyOf "tool" . Toml.Schema.parseTableFromValue $ Toml.Schema.optKey "pixi"
    case join tool of
      Nothing -> pure $ PyprojectPixiDeps Nothing
      Just pixi -> do
        project <- Toml.Schema.optKey "project"
        groups <- fold <$> Toml.Schema.optKey @(Map Text [DependencyGroupEntry]) "dependency-groups"
        let features =
              Map.unionsWith
                (<>)
                [ Map.singleton "default" $ foldMap (fold . pyprojectDependencies) project
                , foldMap (fold . pyprojectOptionalDependencies) project
                , Map.mapWithKey (\name _ -> groupRequirements groups name) groups
                ]
            requested = Map.fromListWith (<>) . map ((\(name, extras, _) -> (name, Set.fromList extras)) . reqParts)
        pure . PyprojectPixiDeps . Just $ pixi <> mempty{manifestCondaDeps = Set.singleton "python", manifestPypiDeps = Map.map requested $ Map.mapKeys Feature features}

-- | A PEP 508 requirement, or an @{include-group = "..."}@ table naming another group.
data DependencyGroupEntry = GroupRequirement Req | IncludeGroup Text

instance Toml.Schema.FromValue DependencyGroupEntry where
  fromValue v@(Toml.Table' _ _) = IncludeGroup <$> Toml.Schema.parseTableFromValue (Toml.Schema.reqKey "include-group") v
  fromValue v = GroupRequirement <$> Toml.Schema.fromValue v

-- | A group's requirements, including those of the groups it includes. PEP 735
-- forbids cycles, so a group that's already being expanded is skipped.
groupRequirements :: Map Text [DependencyGroupEntry] -> Text -> [Req]
groupRequirements groups = expand Set.empty
  where
    expand seen group = foldMap (requirements $ Set.insert group seen) $ Map.findWithDefault [] group groups
    requirements seen = \case
      GroupRequirement req -> [req]
      IncludeGroup group -> if Set.member group seen then [] else expand seen group

-- | A requirement's name, the extras it requests, and its marker.
reqParts :: Req -> (Text, [Text], Maybe Marker)
reqParts = \case
  NameReq name extras _ marker -> (name, fold extras, marker)
  UrlReq name extras _ marker -> (name, fold extras, marker)
