{-# LANGUAGE LambdaCase #-}

module Strategy.Nim.NimbleLock (
  analyze,
  analyze',

  -- * for testing
  NimbleLock (..),
  PackageName (..),
  NimPackage (..),
  NimbleDownloadMethod (..),
  NimbleDump (..),
  NimbleRequire (..),
  buildGraph,
) where

import Algebra.Graph.AdjacencyMap qualified as AM
import Control.Effect.Diagnostics (
  Diagnostics,
  ToDiagnostic (renderDiagnostic),
  context,
  errCtx,
  recover,
  warnOnErr,
 )
import Data.Aeson (
  FromJSON (parseJSON),
  FromJSONKey,
  Value,
  withObject,
  withText,
  (.:),
 )
import Data.Aeson.KeyMap qualified as Object
import Data.Aeson.Types (Parser)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Traversable (for)
import DepTypes (
  DepType (GitType),
  Dependency (Dependency),
  VerConstraint (CEq),
 )
import Effect.Exec (AllowErr (Always), Command (..), Exec, execJson)
import Effect.ReadFS (Has, ReadFS, readContentsJson)
import GHC.Generics (Generic)
import Graphing (
  Graphing,
  directs,
  gmap,
  induceJust,
  toAdjacencyMap,
  unfoldDeep,
 )
import Path (Abs, Dir, File, Path)
import Types (GraphBreadth (..))

-- | Represents nimble lock file.
-- Reference: https://github.com/nim-lang/nimble#nimble-lock
newtype NimbleLock = NimbleLock {packages :: [NimPackage]}
  deriving (Show, Eq, Ord)

data NimPackage = NimPackage
  { -- Name of the package.
    name :: PackageName
  , -- Version of the packages.
    version :: Text
  , -- The URL of the repository of the package.
    url :: Text
  , -- The download method: git or hg according to the type of the repository at url.
    downloadMethod :: NimbleDownloadMethod
  , -- The revision at which the dependency is locked.
    vcsRevision :: Text
  , -- The direct dependencies of the package.
    dependencies :: [PackageName]
  }
  deriving (Show, Eq, Ord)

instance FromJSON NimbleLock where
  parseJSON = withObject "NimbleLock" $ \obj -> do
    pkgs <- parsePkgs =<< (obj .: "packages")
    pure $ NimbleLock pkgs
    where
      parsePkgs :: Value -> Parser [NimPackage]
      parsePkgs = withObject "NimPackage" $ \o ->
        for (Object.toList o) $ \(pkgName, pkgMeta) ->
          parseNimPackageWithName (PackageName $ toText pkgName) pkgMeta

      parseNimPackageWithName :: PackageName -> Value -> Parser NimPackage
      parseNimPackageWithName name = withObject "parseNimPackageWithName" $ \metaO ->
        NimPackage name
          <$> metaO .: "version"
          <*> metaO .: "url"
          <*> metaO .: "downloadMethod"
          <*> metaO .: "vcsRevision"
          <*> metaO .: "dependencies"

data NimbleDownloadMethod
  = NimbleDownloadMethodGit
  | NimbleDownloadMethodOther
  deriving (Show, Eq, Ord)

instance FromJSON NimbleDownloadMethod where
  parseJSON = withText "NimbleDownloadMethod" $ \case
    "git" -> pure NimbleDownloadMethodGit
    _ -> pure NimbleDownloadMethodOther

newtype PackageName = PackageName {unPackageName :: Text}
  deriving (Show, Eq, Ord, Generic, FromJSONKey)

instance FromJSON PackageName where
  parseJSON = withText "PackageName" $ \s -> pure $ PackageName s

-- | Builds the graph from nimble lock file, and enriches with output of nimble dump.
buildGraph :: NimbleLock -> Maybe NimbleDump -> Graphing Dependency
buildGraph lockFile nimbleDump =
  Graphing.induceJust
    . Graphing.gmap toDependency
    . applyDirect
    $ Graphing.unfoldDeep (packages lockFile) getTransitives id
  where
    pkgRegistry :: Map PackageName NimPackage
    pkgRegistry = Map.fromList $ map (\p -> (name p, p)) (packages lockFile)

    getTransitives :: NimPackage -> [NimPackage]
    getTransitives pkg = mapMaybe (`Map.lookup` pkgRegistry) (dependencies pkg)

    getVerticesWithoutPredecessors :: Graphing NimPackage -> [NimPackage]
    getVerticesWithoutPredecessors gr = filter (\a -> Set.null $ AM.preSet a $ Graphing.toAdjacencyMap gr) (packages lockFile)

    -- When nimble dump command fails to retrieve direct dependencies,
    --  Approximate by inferring dependencies which do not have any incoming edges as direct!
    --  This should hold for *most* cases, but fails when you have, direct dependency requiring another direct dependency.
    --
    --    Failure Case:
    --      (A: direct) → (B: indirect)
    --       ↓
    --      (D: direct)
    --
    -- When nimble dump command succeeds, use provided direct dependencies.
    applyDirect :: Graphing NimPackage -> Graphing NimPackage
    applyDirect gr = case nimbleDump of
      Nothing -> Graphing.directs (getVerticesWithoutPredecessors gr) <> gr
      Just nd -> Graphing.directs (mapMaybe ((`Map.lookup` pkgRegistry) . nameOf) (requires nd)) <> gr

    toDependency :: NimPackage -> Maybe Dependency
    toDependency nimPkg = case downloadMethod nimPkg of
      NimbleDownloadMethodOther -> Nothing
      NimbleDownloadMethodGit ->
        Just $
          Dependency
            GitType
            (url nimPkg)
            (Just $ CEq $ vcsRevision nimPkg)
            []
            mempty
            mempty

-- | Performs 'nimble dump --json' and is tolerant to non-zero exit status.
nimbleDumpJsonCmd :: Command
nimbleDumpJsonCmd =
  Command
    { cmdName = "nimble"
    , cmdArgs = ["dump", "--json"]
    , cmdAllowErr = Always
    }

-- | Represents content retrieved from @nimbleDumpJsonCmd@.
newtype NimbleDump = NimbleDump {requires :: [NimbleRequire]} deriving (Show, Eq, Ord)

newtype NimbleRequire = NimbleRequire {nameOf :: PackageName} deriving (Show, Eq, Ord)

instance FromJSON NimbleDump where
  parseJSON = withObject "NimbleDump" $ \obj ->
    NimbleDump <$> obj .: "requires"

instance FromJSON NimbleRequire where
  parseJSON = withObject "NimbleRequire" $ \obj ->
    NimbleRequire <$> obj .: "name"

analyze ::
  ( Has ReadFS sig m
  , Has Exec sig m
  , Has Diagnostics sig m
  ) =>
  Path Abs Dir ->
  Path Abs File ->
  m (Graphing Dependency, GraphBreadth)
analyze dir lockFile = do
  lockContents <- context "Reading nimble.lock" $ readContentsJson lockFile
  nimbleDumpContent :: Maybe NimbleDump <-
    recover
      . context "Performing nimble dump --json to identify direct dependencies"
      . warnOnErr MissingEdgesBetweenDirectDeps
      . errCtx CmdNimbleDumpFailed
      $ execJson dir nimbleDumpJsonCmd
  context "building graphing from nimble.lock" $ pure (buildGraph lockContents nimbleDumpContent, Complete)

analyze' ::
  ( Has ReadFS sig m
  , Has Diagnostics sig m
  ) =>
  Path Abs Dir ->
  Path Abs File ->
  m (Graphing Dependency, GraphBreadth)
analyze' _ lockFile = do
  lockContents <- context "Reading nimble.lock" $ readContentsJson lockFile
  context "building graphing from nimble.lock" $ pure (buildGraph lockContents Nothing, Complete)

data MissingEdgesBetweenDirectDeps = MissingEdgesBetweenDirectDeps
instance ToDiagnostic MissingEdgesBetweenDirectDeps where
  renderDiagnostic _ = "Could not infer edges between direct dependencies."

data CmdNimbleDumpFailed = CmdNimbleDumpFailed
instance ToDiagnostic CmdNimbleDumpFailed where
  renderDiagnostic _ = "We could not retrieve nimble packages metadata using nimble's dump subcommand."
