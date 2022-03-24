{-# LANGUAGE RecordWildCards #-}

module Strategy.Node.Npm.PackageLock (
  analyze,
  buildGraph,
  NpmPackageJson (..),
  NpmDep (..),
  NpmResolved (..),
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
import Data.Maybe (isNothing, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tagged (unTag)
import Data.Text (Text, splitOn)
import Data.Text qualified as Text
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
import Strategy.Node.PackageJson (Development, FlatDeps (devDeps, directDeps), NodePackage (pkgName), Production)

data NpmPackagesPkg = NpmPackagesPkg
  { pkgPeerDeps :: Map Text Text
  , pkgResolved :: Maybe Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON NpmPackagesPkg where
  parseJSON = withObject "NpmPackagesPkg" $ \obj ->
    -- TODO: should this be changed to exclude the current package with key ""?
    -- It has a different structure compared to the others that represent dependencies
    NpmPackagesPkg
      <$> obj .:? "peerDependencies" .!= Map.empty
      <*> obj .:? "resolved"

data NpmPackageJson = NpmPackageJson
  { packageDependencies :: Map Text NpmDep
  , -- |  Data from the "packages" field of package-json.lock
    packagePackages :: Map Text NpmPackagesPkg
  }
  deriving (Eq, Ord, Show)

data NpmDep = NpmDep
  { depVersion :: Text
  , depDev :: Bool
  , depResolved :: NpmResolved
  , -- | name to version spec
    depRequires :: Map Text Text
  , depDependencies :: Map Text NpmDep
  }
  deriving (Eq, Ord, Show)

instance FromJSON NpmPackageJson where
  parseJSON = withObject "NpmPackageJson" $ \obj ->
    NpmPackageJson
      <$> obj .: "dependencies"
      <*> obj .:? "packages" .!= Map.empty

newtype NpmResolved = NpmResolved {unNpmResolved :: Maybe Text}
  deriving (Eq, Ord, Show)

instance FromJSON NpmResolved where
  parseJSON (String t) = pure . NpmResolved $ Just t
  parseJSON (Bool _) = pure . NpmResolved $ Nothing
  parseJSON _ = fail "Failed to parse key 'resolved'."

instance FromJSON NpmDep where
  parseJSON = withObject "NpmDep" $ \obj ->
    NpmDep <$> obj .: "version"
      <*> obj .:? "dev" .!= False
      <*> obj .:? "resolved" .!= NpmResolved Nothing
      <*> obj .:? "requires" .!= mempty
      <*> obj .:? "dependencies" .!= mempty

analyze :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> FlatDeps -> m (Graphing Dependency)
analyze file flatdeps = context "Analyzing Npm Lockfile" $ do
  packageJson <- context "Parsing package-lock.json" $ readContentsJson @NpmPackageJson file
  context "Building dependency graph" $ pure $ buildGraph packageJson directDepsSet
  where
    directDepsSet =
      Set.map pkgName $
        (unTag @Production $ directDeps flatdeps) <> (unTag @Development $ devDeps flatdeps)

data NpmPackage = NpmPackage
  { lockName :: Text
  , lockVersion :: Text
  }
  deriving (Eq, Ord, Show)

type NpmGrapher = LabeledGrapher NpmPackage NpmPackageLabel

data NpmPackageLabel = NpmPackageEnv DepEnvironment | NpmPackageLocation Text
  deriving (Eq, Ord, Show)

-- |The @packages@ object contains keys which are file paths to a package npm
-- downloaded to @node_modules@. This function will adjust map keys to be names
-- like in the @dependencies@ key.
--
-- It will also eliminate any keys which represent a nested path
-- e.g. @node_modules\/foo\/node_modules/bar@. This nesting happens when there are
-- multiple versions of the same package in the dependency tree of the
-- @package-lock.json@ file because one of the versions gets vendored.
packagePathsToNames :: Map Text a -> Map Text a
packagePathsToNames =
  Map.fromList
    . mapMaybe fixName
    . Map.toList
  where
    fixName :: (Text, a) -> Maybe (Text, a)
    fixName (k, v) = case (filter (/= "node_modules") . splitOn "/" $ k) of
      [k'] -> Just (k', v)
      _ -> Nothing

buildGraph :: NpmPackageJson -> Set Text -> Graphing Dependency
buildGraph packageJson directSet =
  run . withLabeling toDependency $
    void $ Map.traverseWithKey (maybeAddDep False Nothing) packageDeps
  where
    packageDeps = packageDependencies packageJson

    -- Packages from the `packages` key in package-lock.json. peerDependencies are recorded here
    lockPackages = packagePathsToNames . packagePackages $ packageJson

    -- Skip adding deps if we think it's a workspace package.
    maybeAddDep isRecursive parent name dep@NpmDep{..} =
      if isNothing (unNpmResolved depResolved) || "file:" `Text.isPrefixOf` depVersion
        then pure ()
        else addDep isRecursive parent name dep

    -- Look up if a given npm package has peer dependencies, then add them to
    -- the graph recursively. All peer dependencies added via this function will
    -- be added as deep dependencies. If there are direct peer dependencies
    -- those are discovered when reading @package.json@ and added like a regular
    -- dependency in 'maybeAddDep'.
    addPeerDeps :: Has NpmGrapher sig m => NpmPackage -> m ()
    addPeerDeps currentPkg =
      maybe (pure ()) graphPeerDeps (Map.lookup (lockName currentPkg) lockPackages)
      where
        addNodeAndEdge :: Has NpmGrapher sig m => Text -> m ()
        addNodeAndEdge peerDepName =
          case Map.lookup peerDepName packageDeps of
            Just npmDep -> maybeAddDep True (Just currentPkg) peerDepName npmDep
            Nothing -> pure ()

        graphPeerDeps :: Has NpmGrapher sig m => NpmPackagesPkg -> m ()
        graphPeerDeps =
          -- ignore the version specified in "peerDependencies", we'll use the
          -- resolved one from the main list of dependencies.
          traverse_ (addNodeAndEdge . fst)
            . Map.toList
            . pkgPeerDeps

    -- If not resolved, then likely a workspace dep, should be ignored.
    -- isRecursive lets us know if we are parsing a top-level or nested dep.
    addDep :: Has NpmGrapher sig m => Bool -> Maybe NpmPackage -> Text -> NpmDep -> m ()
    addDep isRecursive parent name NpmDep{..} = do
      let pkg = NpmPackage name depVersion

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
      label pkg $ NpmPackageEnv $ if depDev then EnvDevelopment else EnvProduction
      -- Add locations from "resolved"
      traverse_ (label pkg . NpmPackageLocation) (unNpmResolved depResolved)

      -- EDGES
      -- Add edges to packages in "requires"
      void $
        Map.traverseWithKey
          ( \reqName reqVer ->
              edge pkg $
                NpmPackage reqName $
                  getResolvedVersion [depDependencies, packageDependencies packageJson] reqName reqVer
          )
          depRequires
      -- Add edge from parent
      case parent of
        Nothing -> pure ()
        Just parentPkg -> edge parentPkg pkg

      -- RECURSION
      -- Recurse to dep nodes
      void $ Map.traverseWithKey (maybeAddDep True (Just pkg)) depDependencies

    getResolvedVersion :: [Map Text NpmDep] -> Text -> Text -> Text
    getResolvedVersion lookups reqName reqVersion = maybe reqVersion depVersion foundVersion
      where
        foundVersion = asum $ map (Map.lookup reqName) lookups

    toDependency :: NpmPackage -> Set NpmPackageLabel -> Dependency
    toDependency pkg = foldr addLabel (start pkg)

    addLabel :: NpmPackageLabel -> Dependency -> Dependency
    addLabel (NpmPackageEnv env) = insertEnvironment env
    addLabel (NpmPackageLocation loc) = insertLocation loc

    start :: NpmPackage -> Dependency
    start NpmPackage{..} =
      Dependency
        { dependencyType = NodeJSType
        , dependencyName = lockName
        , dependencyVersion = Just $ CEq lockVersion
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = Map.empty
        }
