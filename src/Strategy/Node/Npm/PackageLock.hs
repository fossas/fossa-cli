{-# LANGUAGE RecordWildCards #-}

module Strategy.Node.Npm.PackageLock (
  analyze,
  buildGraph,
  PkgLockJson (..),
  PkgLockDependency (..),
  NpmResolved (..),
  PkgLockPackage (..),
) where

import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  context,
  run,
 )
import Control.Monad (when)
import Data.Aeson (
  FromJSON (parseJSON),
  Value (Bool, String),
  withObject,
  (.!=),
  (.:),
  (.:?),
 )
import Data.Foldable (asum, traverse_)
import Data.Functor (void)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tagged (unTag)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Extra qualified as TE
import DepTypes (
  DepEnvironment (EnvDevelopment, EnvProduction),
  DepType (NodeJSType),
  Dependency (..),
  VerConstraint (CEq),
  insertEnvironment,
  insertLocation,
 )
import Effect.Grapher (
  LabeledGrapher,
  deep,
  direct,
  edge,
  label,
  withLabeling,
 )
import Effect.ReadFS (ReadFS, readContentsJson)
import Graphing (Graphing)
import Path (Abs, File, Path)
import Strategy.Node.PackageJson (
  Development,
  FlatDeps (devDeps, directDeps),
  NodePackage (pkgName),
  Production,
  WorkspacePackageNames (WorkspacePackageNames),
 )

data PkgLockPackage = PkgLockPackage
  { pkgPeerDeps :: Map Text Text
  , pkgResolved :: Maybe Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON PkgLockPackage where
  parseJSON = withObject "PkgLockPackage" $ \obj ->
    -- Note that the object with a key of "" represents the current project we're
    -- analyzing. It has a different set of keys/values.
    PkgLockPackage
      <$> obj .:? "peerDependencies" .!= Map.empty
      <*> obj .:? "resolved"

data PkgLockJson = PkgLockJson
  { lockDependencies :: Map Text PkgLockDependency
  , lockPackages :: Map Text PkgLockPackage
  }
  deriving (Eq, Ord, Show)

data PkgLockDependency = PkgLockDependency
  { depVersion :: Text
  , depDev :: Bool
  , depResolved :: NpmResolved
  , depRequires :: Map Text Text
  -- ^ name to version spec
  , depDependencies :: Map Text PkgLockDependency
  }
  deriving (Eq, Ord, Show)

instance FromJSON PkgLockJson where
  parseJSON = withObject "PkgLockJson" $ \obj ->
    PkgLockJson
      <$> obj .: "dependencies"
      <*> obj .:? "packages" .!= Map.empty

newtype NpmResolved = NpmResolved {unNpmResolved :: Maybe Text}
  deriving (Eq, Ord, Show)

instance FromJSON NpmResolved where
  parseJSON (String t) = pure . NpmResolved $ Just t
  parseJSON (Bool _) = pure . NpmResolved $ Nothing
  parseJSON _ = fail "Failed to parse key 'resolved'."

instance FromJSON PkgLockDependency where
  parseJSON = withObject "PkgLockDependency" $ \obj ->
    PkgLockDependency
      <$> obj .: "version"
      <*> obj .:? "dev" .!= False
      <*> obj .:? "resolved" .!= NpmResolved Nothing
      <*> obj .:? "requires" .!= mempty
      <*> obj .:? "dependencies" .!= mempty

analyze :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> FlatDeps -> WorkspacePackageNames -> m (Graphing Dependency)
analyze file flatdeps workspacePackages = context "Analyzing Npm Lockfile" $ do
  packageLockJson <- context "Parsing package-lock.json" $ readContentsJson @PkgLockJson file
  context "Building dependency graph" $ pure $ buildGraph packageLockJson directDepsSet workspacePackages
  where
    directDepsSet =
      Set.map pkgName $
        (unTag @Production $ directDeps flatdeps) <> (unTag @Development $ devDeps flatdeps)

-- |Node in the package-lock.json dep graph
data NpmDepVertex = NpmDepVertex
  { lockName :: Text
  , lockVersion :: Text
  }
  deriving (Eq, Ord, Show)

type NpmGrapher = LabeledGrapher NpmDepVertex NpmDepVertexLabel

data NpmDepVertexLabel = NpmDepVertexEnv DepEnvironment | NpmDepVertexLocation Text
  deriving (Eq, Ord, Show)

-- |The @packages@ object contains keys which are file paths to a package npm
-- downloaded to @node_modules@. This function will adjust map keys to be names
-- like in the @dependencies@ key by stripping out path components besides the final one..
--
-- When npm installs a dep inside of another dep (for version conflicts) we get the string
-- @node_modules/a/node_modules/b@.  We actually don't want to do sub-package matching here,
-- so we only drop the first @node_modules/@, and if it has a sub-package, then we just
-- won't ever query for that key.
packagePathsToNames :: Map Text a -> Map Text a
packagePathsToNames = Map.mapKeys (TE.dropPrefix "node_modules/")

buildGraph :: PkgLockJson -> Set Text -> WorkspacePackageNames -> Graphing Dependency
buildGraph packageJson directSet (WorkspacePackageNames workspacePackages) =
  run . withLabeling toDependency $
    void $
      traverseWithKey' (maybeAddDep False) packageDeps
  where
    packageDeps = lockDependencies packageJson

    -- Packages from the `packages` key in package-lock.json. peerDependencies are recorded here
    pkgJsonPackages = packagePathsToNames . lockPackages $ packageJson

    -- Skip adding deps if we think it's a workspace package.
    maybeAddDep isRecursive name dep@PkgLockDependency{..} parentDepRequires =
      if name `Set.member` workspacePackages || "file:" `Text.isPrefixOf` depVersion
        then pure ()
        else addDep isRecursive name dep parentDepRequires

    -- Look up if a given npm package has peer dependencies, then add them to
    -- the graph recursively. All peer dependencies added via this function will
    -- be added as deep dependencies. If there are direct peer dependencies
    -- those are discovered when reading @package.json@ and added like a regular
    -- direct dependency in 'maybeAddDep'.
    addPeerDeps :: Has NpmGrapher sig m => NpmDepVertex -> m ()
    addPeerDeps currentPkg =
      traverse_ graphPeerDeps (Map.lookup (lockName currentPkg) pkgJsonPackages)
      where
        graphPeerDeps :: Has NpmGrapher sig m => PkgLockPackage -> m ()
        graphPeerDeps =
          -- ignore the version range specified in "peerDependencies", we'll use
          -- the resolved one from the main list of dependencies.
          traverse_ (addNodeAndEdge . fst)
            . Map.toList
            . pkgPeerDeps

        addNodeAndEdge :: Has NpmGrapher sig m => Text -> m ()
        addNodeAndEdge peerDepName =
          case Map.lookup peerDepName packageDeps of
            Just npmDep -> do
              -- It's important to not recurse here as there can be cycles
              -- involving deps and peerDeps.
              --
              -- npmDep may never have been seen yet at this point. Because
              -- it's in packageDeps it will be processed eventually.
              -- It will be set as direct/deep and labeled then.
              edge currentPkg $ NpmDepVertex peerDepName (depVersion npmDep)
            Nothing -> pure ()

    -- isRecursive lets us know if we are parsing a top-level or nested dep.
    addDep :: Has NpmGrapher sig m => Bool -> Text -> PkgLockDependency -> Map Text PkgLockDependency -> m ()
    addDep isRecursive name PkgLockDependency{..} parentDepRequires = do
      let pkg = NpmDepVertex name depVersion

      -- DEEP/DIRECT
      -- Allow entry of orphan deps.  We may prune these later.
      deep pkg
      -- Try marking non-recursively-discovered deps as direct
      when (not isRecursive && Set.member name directSet) $
        direct pkg

      -- If there are peerDeps, add them and process their dependencies as well
      addPeerDeps pkg

      -- LOCATION/ENVIRONMENT
      -- Mark prod/dev
      label pkg $ NpmDepVertexEnv $ if depDev then EnvDevelopment else EnvProduction
      -- Add locations from "resolved"
      traverse_ (label pkg . NpmDepVertexLocation) (unNpmResolved depResolved)

      -- EDGES
      -- Add edges to packages in "requires"
      void $
        Map.traverseWithKey
          ( \reqName reqVer -> do
              let requiredPkg =
                    NpmDepVertex reqName $
                      getResolvedVersion
                        [ depDependencies
                        , -- some times, when lockfile has dependency with multiple versions,
                          -- it may be defined at $.dependencies.dep or $.dependencies.otherDep.dependencies.dep
                          -- we include `parentDepRequires` to handle such cases!
                          parentDepRequires
                        , lockDependencies packageJson
                        ]
                        reqName
                        reqVer
              edge pkg requiredPkg
              -- The below is a safety net for when a required package doesn't
              -- exist at the top-level dependencies object or in the parent
              -- package's "dependencies" field. This is safe because a package
              -- that is labeled both development and prod will be treated as
              -- prod during pruning and labeling is idempotent
              when depDev $
                label requiredPkg (NpmDepVertexEnv EnvDevelopment)
          )
          depRequires

      -- RECURSION
      -- Recurse to dep nodes
      void $ traverseWithKey' (maybeAddDep True) depDependencies

    getResolvedVersion :: [Map Text PkgLockDependency] -> Text -> Text -> Text
    getResolvedVersion lookups reqName reqVersion = maybe reqVersion depVersion foundVersion
      where
        foundVersion = asum $ map (Map.lookup reqName) lookups

    toDependency :: NpmDepVertex -> Set NpmDepVertexLabel -> Dependency
    toDependency pkg = foldr addLabel (start pkg)

    addLabel :: NpmDepVertexLabel -> Dependency -> Dependency
    addLabel (NpmDepVertexEnv env) = insertEnvironment env
    addLabel (NpmDepVertexLocation loc) = insertLocation loc

    start :: NpmDepVertex -> Dependency
    start NpmDepVertex{..} =
      Dependency
        { dependencyType = NodeJSType
        , dependencyName = lockName
        , dependencyVersion = Just $ CEq lockVersion
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = Map.empty
        }

-- | Traverse Map by Key, Value and Map.
traverseWithKey' :: Applicative f => (a -> b -> Map a b -> f c) -> Map a b -> f (Map a c)
traverseWithKey' f mapped = Map.traverseWithKey (\k v -> f k v mapped) mapped
