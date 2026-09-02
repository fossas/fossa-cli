{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Strategy.Node.PackageJson (
  buildGraph,
  analyze,
  unresolvableSpecifiers,
  Development,
  FlatDeps (..),
  Manifest (..),
  NodePackage (..),
  PackageJson (..),
  PkgJsonLicense (..),
  PkgJsonLicenseObj (..),
  PkgJsonGraph (..),
  PkgJsonWorkspaces (..),
  Production,
  pkgFileList,
  WorkspacePackageNames (..),
) where

import Algebra.Graph.AdjacencyMap qualified as AM
import Control.Applicative ((<|>))
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  context,
  run,
 )
import Control.Monad (unless)
import Data.Aeson (
  FromJSON (parseJSON),
  KeyValue ((.=)),
  ToJSON (toJSON),
  ToJSONKey,
  Value (Array, Object),
  object,
  withObject,
  (.!=),
  (.:),
  (.:?),
 )
import Data.Glob (Glob)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.String.Conversion (ToText (toText))
import Data.Tagged (Tagged)
import Data.Text (Text)
import Data.Text qualified as Text
import DepTypes (
  DepEnvironment (..),
  DepType (NodeJSType),
  Dependency (..),
  VerConstraint (CCompatible),
  insertEnvironment,
 )
import Effect.Grapher (
  LabeledGrapher,
  direct,
  label,
  withLabeling,
 )
import Effect.Logger (Logger, logWarn, pretty)
import GHC.Generics (Generic)
import Graphing (Graphing)
import Path (Abs, File, Path, Rel)

newtype WorkspacePackageNames = WorkspacePackageNames (Set Text)

analyze :: (Has Diagnostics sig m, Has Logger sig m) => [PackageJson] -> m (Graphing Dependency)
analyze manifests = do
  let unresolvable = concatMap unresolvableSpecifiers manifests
  unless (null unresolvable) $
    logWarn . pretty $
      "Skipping "
        <> toText (show (length unresolvable))
        <> " dependencies whose version is a workspace reference this strategy cannot resolve without a lockfile ("
        <> Text.intercalate ", " unresolvable
        <> "). Analyze from the workspace root to include them."
  context "Building dependency graph" . pure $ foldMap buildGraph manifests

-- | Specifier protocols that name a location in the workspace rather than a
-- version range.
--
-- @catalog:@ (pnpm catalogs), @workspace:@ (the workspace protocol) and
-- @link:@ are all resolved from files this strategy does not read — the
-- lockfile, or pnpm-workspace.yaml. Recording the raw specifier as the version
-- produced locators like @npm+left-pad$catalog:@, a dependency pinned to the
-- literal version "catalog:", which does not exist in any registry.
--
-- @file:@ is deliberately not in this list. It is equally unresolvable, but it
-- long predates the workspace protocols and npm projects have been reporting
-- it this way for years; changing that is a separate decision.
workspaceProtocols :: [Text]
workspaceProtocols = ["catalog:", "workspace:", "link:"]

isWorkspaceReference :: Text -> Bool
isWorkspaceReference constraint = any (`Text.isPrefixOf` constraint) workspaceProtocols

-- | @name\@specifier@ for every dependency dropped by 'buildGraph', for warning.
unresolvableSpecifiers :: PackageJson -> [Text]
unresolvableSpecifiers PackageJson{..} =
  map (\(name, constraint) -> name <> "@" <> constraint)
    . filter (isWorkspaceReference . snd)
    $ Map.toList packageDeps <> Map.toList packageDevDeps

type NodeGrapher = LabeledGrapher NodePackage NodePackageLabel

newtype NodePackageLabel = NodePackageEnv DepEnvironment
  deriving (Eq, Ord, Show)

buildGraph :: PackageJson -> Graphing Dependency
buildGraph PackageJson{..} = run . withLabeling toDependency $ do
  _ <- Map.traverseWithKey (addDep EnvProduction) (Map.filter (not . isWorkspaceReference) packageDeps)
  _ <- Map.traverseWithKey (addDep EnvDevelopment) (Map.filter (not . isWorkspaceReference) packageDevDeps)
  pure ()
  where
    addDep :: Has NodeGrapher sig m => DepEnvironment -> Text -> Text -> m ()
    addDep env name constraint = do
      let pkg = NodePackage name constraint
      direct pkg
      label pkg (NodePackageEnv env)

    toDependency :: NodePackage -> Set NodePackageLabel -> Dependency
    toDependency dep = foldr addLabel (start dep)

    addLabel :: NodePackageLabel -> Dependency -> Dependency
    addLabel (NodePackageEnv env) = insertEnvironment env

    start :: NodePackage -> Dependency
    start NodePackage{..} =
      Dependency
        { dependencyType = NodeJSType
        , dependencyName = pkgName
        , dependencyVersion = Just (CCompatible pkgConstraint)
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = Map.empty
        }

newtype PkgJsonWorkspaces = PkgJsonWorkspaces {unWorkspaces :: [Glob Rel]}
  deriving (Eq, Ord, Show, ToJSON, Semigroup, Monoid)

-- Name and version are required for workspace sub-projects.
data PackageJson = PackageJson
  { packageName :: Maybe Text
  , packageVersion :: Maybe Text
  , packageWorkspaces :: PkgJsonWorkspaces
  , packageDeps :: Map Text Text
  , packageDevDeps :: Map Text Text
  , packageLicense :: Maybe PkgJsonLicense
  , packageLicenses :: Maybe [PkgJsonLicenseObj]
  , packagePeerDeps :: Map Text Text
  }
  deriving (Eq, Ord, Show)

data PkgJsonLicenseObj = PkgJsonLicenseObj
  { licenseType :: Text
  , licenseUrl :: Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON PkgJsonLicenseObj where
  parseJSON = withObject "PkgJsonLicenseObj" $ \obj ->
    PkgJsonLicenseObj
      <$> obj .: "type"
      <*> obj .: "url"

instance ToJSON PkgJsonLicenseObj where
  toJSON PkgJsonLicenseObj{..} =
    object
      [ "type" .= toJSON licenseType
      , "url" .= toJSON licenseUrl
      ]

data PkgJsonLicense
  = -- LicenseText is likely SPDX, but it isn't a requirement per
    -- https://docs.npmjs.com/cli/v8/configuring-npm/package-json#license
    LicenseText Text
  | LicenseObj PkgJsonLicenseObj
  deriving (Eq, Ord, Show, Generic)

instance ToJSON PkgJsonLicense where
  toJSON (LicenseText t) = toJSON t
  toJSON (LicenseObj o) = toJSON o

instance FromJSON PkgJsonLicense where
  parseJSON v =
    LicenseText <$> parseJSON v
      <|> LicenseObj <$> parseJSON v

instance FromJSON PkgJsonWorkspaces where
  parseJSON (Array x) = PkgJsonWorkspaces <$> parseJSON (Array x)
  parseJSON (Object x) = withObject "PkgJsonWorkspaces" go (Object x)
    where
      -- We might find a "nohoist" key, but it only tells us where to find
      -- installed deps, rather than which deps are installed.
      -- https://classic.yarnpkg.com/blog/2018/02/15/nohoist/
      go obj = PkgJsonWorkspaces <$> obj .:? "packages" .!= []
  parseJSON _ = fail "'workspaces' must be an array or an object"

instance FromJSON PackageJson where
  parseJSON = withObject "PackageJson" $ \obj ->
    PackageJson
      <$> obj .:? "name"
      <*> obj .:? "version"
      <*> obj .:? "workspaces" .!= PkgJsonWorkspaces []
      <*> obj .:? "dependencies" .!= Map.empty
      <*> obj .:? "devDependencies" .!= Map.empty
      <*> obj .:? "license"
      <*> obj .:? "licenses"
      <*> obj .:? "peerDependencies" .!= Map.empty

instance ToJSON PackageJson where
  toJSON PackageJson{..} =
    object
      [ "name" .= packageName
      , "version" .= packageVersion
      , "workspaces" .= packageWorkspaces
      , "dependencies" .= packageDeps
      , "devDependencies" .= packageDevDeps
      , "license" .= packageLicense
      , "licenses" .= packageLicenses
      , "peerDependencies" .= packagePeerDeps
      ]

newtype Manifest = Manifest {unManifest :: Path Abs File}
  deriving (Eq, Show, Ord, Generic, ToJSONKey, ToJSON)
  deriving (ToText) via (Path Abs File)

data PkgJsonGraph = PkgJsonGraph
  { jsonGraph :: AM.AdjacencyMap Manifest
  , jsonLookup :: Map Manifest PackageJson
  }
  deriving (Eq, Ord, Show, Generic)

pkgFileList :: PkgJsonGraph -> [Path Abs File]
pkgFileList (PkgJsonGraph _ mapping) = map unManifest $ Map.keys mapping

instance ToJSON PkgJsonGraph where
  toJSON PkgJsonGraph{..} =
    object
      [ "jsonGraph" .= AM.adjacencyMap jsonGraph
      , "jsonLookup" .= toJSON jsonLookup
      ]

-- Tag types for the sets in FlatDeps
data Production
data Development

data FlatDeps = FlatDeps
  { directDeps :: Tagged Production (Set NodePackage)
  , devDeps :: Tagged Development (Set NodePackage)
  , manifests :: Set Manifest
  }
  deriving (Eq, Ord, Show)

instance Semigroup FlatDeps where
  (<>) (FlatDeps direct1 dev1 files1) (FlatDeps direct2 dev2 files2) = FlatDeps (direct1 <> direct2) (dev1 <> dev2) (files1 <> files2)

instance Monoid FlatDeps where
  mempty = FlatDeps mempty mempty mempty

-- TODO: decode version constraints
data NodePackage = NodePackage
  { pkgName :: Text
  , pkgConstraint :: Text
  }
  deriving (Eq, Ord, Show)

instance ToText NodePackage where
  toText NodePackage{pkgName, pkgConstraint} =
    pkgName <> "@" <> pkgConstraint
