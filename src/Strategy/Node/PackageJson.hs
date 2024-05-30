{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Strategy.Node.PackageJson (
  buildGraph,
  analyze,
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
import Data.String.Conversion (ToText)
import Data.Tagged (Tagged)
import Data.Text (Text)
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
import GHC.Generics (Generic)
import Graphing (Graphing)
import Path (Abs, File, Path, Rel)

newtype WorkspacePackageNames = WorkspacePackageNames (Set Text)

analyze :: (Has Diagnostics sig m) => [PackageJson] -> m (Graphing Dependency)
analyze manifests = do
  context "Building dependency graph" . pure $ foldMap buildGraph manifests

type NodeGrapher = LabeledGrapher NodePackage NodePackageLabel

newtype NodePackageLabel = NodePackageEnv DepEnvironment
  deriving (Eq, Ord, Show)

buildGraph :: PackageJson -> Graphing Dependency
buildGraph PackageJson{..} = run . withLabeling toDependency $ do
  _ <- Map.traverseWithKey (addDep EnvProduction) packageDeps
  _ <- Map.traverseWithKey (addDep EnvDevelopment) packageDevDeps
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
  deriving (Eq, Ord, Show, ToJSON)

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
