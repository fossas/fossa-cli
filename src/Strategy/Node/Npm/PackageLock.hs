{-# LANGUAGE RecordWildCards #-}

module Strategy.Node.Npm.PackageLock (
  analyze,
  buildGraph,
  NpmPackageJson (..),
  NpmDep (..),
) where

import Control.Effect.Diagnostics
import Control.Monad (when)
import Data.Aeson
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tagged (unTag)
import Data.Text (Text)
import Data.Text qualified as Text
import DepTypes
import Effect.Grapher
import Effect.ReadFS
import Graphing (Graphing)
import Path
import Strategy.Node.PackageJson (Development, FlatDeps (devDeps, directDeps), NodePackage (pkgName), Production)

data NpmPackageJson = NpmPackageJson
  { packageName :: Text
  , packageDependencies :: Map Text NpmDep
  }
  deriving (Eq, Ord, Show)

data NpmDep = NpmDep
  { depVersion :: Text
  , depDev :: Bool
  , depResolved :: Maybe Text
  , -- | name to version spec
    depRequires :: Map Text Text
  , depDependencies :: Map Text NpmDep
  }
  deriving (Eq, Ord, Show)

instance FromJSON NpmPackageJson where
  parseJSON = withObject "NpmPackageJson" $ \obj ->
    NpmPackageJson <$> obj .: "name"
      <*> obj .: "dependencies"

instance FromJSON NpmDep where
  parseJSON = withObject "NpmDep" $ \obj ->
    NpmDep <$> obj .: "version"
      <*> obj .:? "dev" .!= False
      <*> obj .:? "resolved"
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

buildGraph :: NpmPackageJson -> Set Text -> Graphing Dependency
buildGraph packageJson directSet =
  run . withLabeling toDependency $
    void $ Map.traverseWithKey (maybeAddDep False) (packageDependencies packageJson)
  where
    -- Skip adding deps if we think it's a workspace package.
    maybeAddDep isRecursive name dep@NpmDep{..} =
      if isNothing depResolved || "file:" `Text.isPrefixOf` depVersion
        then pure ()
        else addDep isRecursive name dep

    -- If not resolved, then likely a workspace dep, should be ignored.
    -- isRecursive lets us know if we are parsing a top-level or nested dep.
    addDep :: Has NpmGrapher sig m => Bool -> Text -> NpmDep -> m ()
    addDep isRecursive name NpmDep{..} = do
      let pkg = NpmPackage name depVersion

      -- Allow entry of orphan deps.  We may prune these later.
      deep pkg

      -- Try marking non-recursively-discovered deps as direct
      when (not isRecursive && Set.member name directSet) $
        direct pkg

      label pkg $ NpmPackageEnv $ if depDev then EnvDevelopment else EnvProduction

      traverse_ (label pkg . NpmPackageLocation) depResolved

      -- add edges to required packages
      void $ Map.traverseWithKey (\reqName reqVer -> edge pkg $ NpmPackage reqName $ getResolvedVersion reqName reqVer) depRequires

      -- add dependency nodes
      void $ Map.traverseWithKey (maybeAddDep True) depDependencies

    getResolvedVersion :: Text -> Text -> Text
    getResolvedVersion reqName reqVersion = maybe reqVersion depVersion (Map.lookup reqName $ packageDependencies packageJson)

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
