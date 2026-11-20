{-# LANGUAGE QuasiQuotes #-}

-- | Dynamic Swift package analysis, letting SwiftPM resolve the package graph with
-- @swift package show-dependencies@ instead of parsing @Package.swift@ ourselves.
module Strategy.Swift.ShowDependencies (
  analyzeShowDependencies,

  -- * for testing
  ShowDependenciesPackage (..),
  buildGraph,
) where

import Control.Effect.Diagnostics (Diagnostics, context, errCtx, errDoc, errHelp, recover)
import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Data.Foldable (asum, for_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import DepTypes (DepType (GitType, SwiftType, UnresolvedPathType), Dependency (..), VerConstraint (CEq))
import Effect.Exec (AllowErr (Never), Command (..), Exec, Has, execJson)
import Effect.Grapher (direct, edge, evalGrapher, run)
import Effect.ReadFS (ReadFS, doesFileExist)
import Graphing (Graphing)
import Path (Abs, Dir, Path, relfile, (</>))
import Strategy.Swift.Errors (FailedToShowDependencies (..), FailedToShowDependenciesHelp (..), swiftFossaDocUrl)
import Strategy.Swift.PackageResolved (SwiftPackageResolvedFile (..), SwiftResolvedPackage (..), parsePackageResolved)

-- | One node of the tree printed by @swift package show-dependencies --format json@.
-- The root is the package being analyzed; every other node is a resolved package, repeated
-- under each package that depends on it.
data ShowDependenciesPackage = ShowDependenciesPackage
  { packageIdentity :: Text
  , packageUrl :: Text
  -- ^ The URL the package was declared with, or its absolute path for a local package.
  , packageVersion :: Text
  -- ^ The resolved version tag, or @"unspecified"@ for branch, revision and local packages.
  , packagePath :: Text
  -- ^ Absolute path of the package's checkout.
  , packageDependencies :: [ShowDependenciesPackage]
  }
  deriving (Show, Eq, Ord)

instance FromJSON ShowDependenciesPackage where
  parseJSON = withObject "show-dependencies package" $ \obj ->
    ShowDependenciesPackage
      <$> obj .: "identity"
      <*> obj .: "url"
      <*> obj .: "version"
      <*> obj .: "path"
      <*> obj .: "dependencies"

showDependenciesCmd :: Command
showDependenciesCmd =
  Command
    { cmdName = "swift"
    , cmdArgs = ["package", "show-dependencies", "--format", "json"]
    , cmdAllowErr = Never
    , cmdEnvVars = Map.empty
    }

analyzeShowDependencies :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m (Graphing Dependency)
analyzeShowDependencies projectDir = do
  tree <-
    errCtx (FailedToShowDependencies projectDir)
      . errHelp FailedToShowDependenciesHelp
      . errDoc swiftFossaDocUrl
      $ execJson @ShowDependenciesPackage projectDir showDependenciesCmd
  -- Resolving writes Package.resolved, so it is looked up only after the command has run
  -- rather than relying on whether discovery saw one.
  pins <- context "Reading pinned revisions from Package.resolved" $ readPins projectDir
  context "Building dependency graph" $ pure $ buildGraph pins tree

-- | Pins keyed by repository URL. Empty when the file is missing or cannot be parsed.
readPins :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m (Map Text SwiftResolvedPackage)
readPins projectDir = do
  let resolvedFile = projectDir </> [relfile|Package.resolved|]
  exists <- doesFileExist resolvedFile
  resolved <- if exists then recover (parsePackageResolved resolvedFile) else pure Nothing
  pure $ maybe Map.empty (Map.fromList . map (\pin -> (repositoryURL pin, pin)) . pinnedPackages) resolved

-- | Every package in the tree becomes a vertex and every nesting an edge. The root package
-- itself is not part of the graph.
buildGraph :: Map Text SwiftResolvedPackage -> ShowDependenciesPackage -> Graphing Dependency
buildGraph pins root = run . evalGrapher $
  for_ (packageDependencies root) $ \pkg -> do
    direct (toDependency pins pkg)
    addEdges pkg
  where
    addEdges parent =
      for_ (packageDependencies parent) $ \child -> do
        edge (toDependency pins parent) (toDependency pins child)
        addEdges child

toDependency :: Map Text SwiftResolvedPackage -> ShowDependenciesPackage -> Dependency
toDependency pins pkg
  -- A local package is reported with its path as the url. Remote packages are checked out
  -- under the build directory, so their url and path never match.
  | packageUrl pkg == packagePath pkg = mkDependency UnresolvedPathType (packagePath pkg) Nothing
  | packageVersion pkg /= "unspecified" = mkDependency SwiftType (packageUrl pkg) (Just $ packageVersion pkg)
  -- Branch and revision requirements resolve to a commit rather than a version, which only
  -- Package.resolved records.
  | otherwise = mkDependency GitType (packageUrl pkg) (pinnedRevision =<< Map.lookup (packageUrl pkg) pins)
  where
    pinnedRevision :: SwiftResolvedPackage -> Maybe Text
    pinnedRevision pin = asum [repositoryRevision pin, repositoryVersion pin, repositoryBranch pin]

    mkDependency :: DepType -> Text -> Maybe Text -> Dependency
    mkDependency depType name version =
      Dependency
        { dependencyType = depType
        , dependencyName = name
        , dependencyVersion = CEq <$> version
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = mempty
        }
