{-# LANGUAGE RecordWildCards #-}

module Strategy.Node.YarnV1.YarnLock (
  analyze,
  buildGraph,
  mangleParseErr,
) where

import Control.Effect.Diagnostics (Diagnostics, Has, context, tagError, warn)
import Control.Monad (when)
import Data.Foldable (for_, traverse_)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes)
import Data.MultiKeyedMap qualified as MKM
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String.Conversion (toString)
import Data.Tagged (unTag)
import Data.Text (Text)
import DepTypes (
  DepEnvironment (EnvDevelopment, EnvProduction),
  DepType (NodeJSType),
  Dependency (..),
  VerConstraint (CEq),
  hydrateDepEnvs,
  insertEnvironment,
  insertLocation,
 )
import Effect.Grapher (
  deep,
  direct,
  edge,
  label,
  withLabeling,
 )
import Effect.Logger (
  AnsiStyle,
  Doc,
  hsep,
  pretty,
 )
import Effect.ReadFS (ReadFS, ReadFSErr (FileParseError), readContentsText)
import Graphing (Graphing)
import Path (Abs, File, Path)
import Strategy.Node.PackageJson (Development, FlatDeps (..), NodePackage (..), Production)
import Yarn.Lock qualified as YL
import Yarn.Lock.Types qualified as YL

analyze ::
  forall m sig.
  ( Has Diagnostics sig m
  , Has ReadFS sig m
  ) =>
  Path Abs File ->
  FlatDeps ->
  m (Graphing Dependency)
analyze yarnFile flatdeps = do
  contents <- context "Reading yarn.lock file" $ readContentsText yarnFile
  let yarnpath = toString yarnFile
  parsed <- context "Parsing yarn.lock file" . tagError (mangleParseErr yarnpath) $ YL.parse yarnpath contents
  context "Building yarn.lock package graph" $ buildGraph parsed flatdeps

mangleParseErr :: FilePath -> YL.LockfileError -> ReadFSErr
mangleParseErr path err = FileParseError path (YL.prettyLockfileError err)

data YarnV1Label
  = NodeEnvironment DepEnvironment
  | NodeLocation Text
  deriving (Eq, Ord, Show)

data YarnV1Package = YarnV1Package
  { fullPackageName :: Text
  , packageVersion :: Text
  }
  deriving (Eq, Ord, Show)

buildGraph ::
  forall m sig.
  ( Has Diagnostics sig m
  ) =>
  YL.Lockfile ->
  FlatDeps ->
  m (Graphing Dependency)
buildGraph lockfile FlatDeps{..} = fmap hydrateDepEnvs . withLabeling toDependency $
  for_ (map extractPkgTriple $ MKM.toList lockfile) $ \(pkgName, keys, pkg) -> do
    let parent :: YarnV1Package
        parent = YarnV1Package pkgName $ YL.version pkg

        allKeysAsNodePackages :: [NodePackage]
        allKeysAsNodePackages = toNodePackage <$> keys

        childrenSpecs :: [YL.PackageKey]
        childrenSpecs = YL.dependencies pkg

    -- Fetch dependencies and their resolved versions from the lockfile
    -- Logs a debug message if a map lookup error occurs.
    children <- catMaybes <$> traverse (resolveVersion lockfile) childrenSpecs

    -- Insert all deps as deep to prevent missing isolated deps.
    deep parent
    -- Add location label
    traverse_ (label parent . NodeLocation) $ getLocations $ YL.remote pkg
    -- Add edges from current parent
    traverse_ (edge parent) children
    let promote env pkgSet =
          when (any (`Set.member` pkgSet) allKeysAsNodePackages) $
            do
              direct parent
              label parent $ NodeEnvironment env

    -- Mark as direct if present in any relevant package.json direct list
    -- Mark as dev if present in any relevant package.json dev list
    promote EnvProduction $ unTag @Production directDeps
    promote EnvDevelopment $ unTag @Development devDeps

getLocations :: Maybe YL.Remote -> [Text]
getLocations = \case
  Just (YL.FileRemote url _) -> [url]
  Just (YL.FileRemoteNoIntegrity url) -> [url]
  Just (YL.GitRemote url rev) -> [url <> "@" <> rev]
  Just (YL.DirectoryLocal dirpath) -> [dirpath]
  Just (YL.DirectoryLocalSymLinked dirpath) -> [dirpath]
  _ -> []

toDependency :: YarnV1Package -> Set YarnV1Label -> Dependency
toDependency YarnV1Package{..} = foldr applyLabel start
  where
    applyLabel :: YarnV1Label -> Dependency -> Dependency
    applyLabel (NodeEnvironment env) = insertEnvironment env
    applyLabel (NodeLocation loc) = insertLocation loc

    start =
      Dependency
        { dependencyType = NodeJSType
        , dependencyName = fullPackageName
        , dependencyVersion = Just $ CEq packageVersion
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = mempty
        }

toNodePackage :: YL.PackageKey -> NodePackage
toNodePackage key = NodePackage (extractFullName key) (YL.npmVersionSpec key)

resolveVersion :: Has Diagnostics sig m => YL.Lockfile -> YL.PackageKey -> m (Maybe YarnV1Package)
resolveVersion lockfile key = logMaybePackage key $ pairToPackage key <$> MKM.lookup key lockfile

logMaybePackage :: Has Diagnostics sig m => YL.PackageKey -> Maybe a -> m (Maybe a)
logMaybePackage key something = do
  case something of
    -- In some (currently unknown) cases, we don't find the key we expect to find.
    -- This is rare and potentially problematic, but we can technically still
    -- partially succeed anyway, so we just log a warning for now.
    -- If a valid case is discovered, it's likely a bug elsewhere (perhaps
    -- in the 'yarn-lock' package), and should be fixed.
    Nothing -> warn $ missingResolvedVersionErrorMsg key
    _ -> pure ()
  pure something

missingResolvedVersionErrorMsg :: YL.PackageKey -> Doc AnsiStyle
missingResolvedVersionErrorMsg key =
  hsep
    [ "Yarn graph error: could not resolve"
    , pretty $ extractFullName key
    , "in the yarn lockfile."
    , "It may not be present in the list of dependencies,"
    , "or it may have an unresolved or incorrect version."
    ]

pairToPackage :: YL.PackageKey -> YL.Package -> YarnV1Package
pairToPackage key pkg = YarnV1Package (extractFullName key) (YL.version pkg)

extractPkgTriple :: (NE.NonEmpty YL.PackageKey, b) -> (Text, [YL.PackageKey], b)
extractPkgTriple (neList, pkg) = (extractFullName $ NE.head neList, NE.toList neList, pkg)

extractFullName :: YL.PackageKey -> Text
extractFullName key = case YL.name key of
  YL.SimplePackageKey name -> name
  YL.ScopedPackageKey scope name -> "@" <> scope <> "/" <> name
