{-# LANGUAGE TemplateHaskell #-}

module Strategy.Maven.Pom.Resolver (
  GlobalClosure (..),
  buildGlobalClosure,
) where

import Algebra.Graph.AdjacencyMap qualified as AM
import Control.Algebra (Has)
import Control.Carrier.State.Strict (
  State,
  get,
  modify,
  runState,
 )
import Control.Effect.Diagnostics (
  Diagnostics,
  context,
  fatal,
  recover,
  (<||>),
 )
import Control.Monad (unless)
import Data.Bifunctor (first, second)
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Effect.ReadFS (
  ReadFS,
  ReadFSErr (FileReadError),
  doesFileExist,
  readContentsXML,
  resolveDir,
  resolveFile,
 )
import Path (Abs, Dir, File, Path, mkRelFile, parent, (</>))
import Strategy.Maven.Pom.PomFile (
  MavenCoordinate,
  Pom (pomCoord, pomParentCoord),
  RawParent (rawParentRelativePath),
  RawPom (rawPomModules, rawPomParent),
  validatePom,
 )

data GlobalClosure = GlobalClosure
  { globalGraph :: AM.AdjacencyMap MavenCoordinate
  , globalPoms :: Map MavenCoordinate (Path Abs File, Pom)
  }
  deriving (Eq, Ord, Show)

buildGlobalClosure :: (Has ReadFS sig m, Has Diagnostics sig m) => [Path Abs File] -> m GlobalClosure
buildGlobalClosure files = do
  (loadState, ()) <- runState @LoadState (Map.empty, []) $ traverse_ recursiveLoadPom files
  let (loadResults, moduleEdges) = loadState

  -- TODO: diagnostics/warnings?
  let validated :: Map (Path Abs File) Pom
      validated = Map.mapMaybe (validatePom =<<) loadResults

  pure (buildClosure validated moduleEdges)
  where
    -- notably, we're not building edges based on <relativePath> from poms.
    --
    -- From the docs:
    -- "However, the group ID, artifact ID and version are still required, and must match the file in the location given or it will revert to the repository for the POM."
    --
    -- Because the group/artifact/version are required to match, we can just build edges between _coordinates_, rather than between _pom files_
    --
    -- The graph contains both kinds of POM-to-POM build relation Maven has, each
    -- in the form it is declared: <parent> edges derive from coordinates the child
    -- declares (so they survive even when the parent pom file was never loaded),
    -- while <modules> edges derive from (aggregator, child) path pairs resolved at
    -- load time ('LoadState'). A module listed only via <modules>, with no <parent>
    -- of its own, is legal Maven; without a <modules> edge it would stay a
    -- disconnected vertex and leak into the reported graph as a fake external
    -- dependency instead of belonging to the aggregator's closure.
    buildClosure :: Map (Path Abs File) Pom -> [(Path Abs File, Path Abs File)] -> GlobalClosure
    buildClosure cache moduleEdges =
      GlobalClosure
        { globalGraph =
            AM.vertices (map pomCoord (Map.elems cache))
              `AM.overlay` AM.edges (parentEdges ++ moduleEdgeCoords)
        , globalPoms = indexBy (pomCoord . snd) (Map.toList cache)
        }
      where
        parentEdges :: [(MavenCoordinate, MavenCoordinate)]
        parentEdges =
          [ (parentCoord, pomCoord pom)
          | pom <- Map.elems cache
          , Just parentCoord <- [pomParentCoord pom]
          ]

        -- Pairs missing either endpoint (no validated pom, hence no coordinate)
        -- are skipped, same treatment as parent edges.
        moduleEdgeCoords :: [(MavenCoordinate, MavenCoordinate)]
        moduleEdgeCoords =
          [ (pomCoord parentPom, pomCoord childPom)
          | (parentPath, childPath) <- moduleEdges
          , Just parentPom <- [Map.lookup parentPath cache]
          , Just childPom <- [Map.lookup childPath cache]
          ]

-- TODO: reuse this in other strategies
indexBy :: Ord k => (v -> k) -> [v] -> Map k v
indexBy f = Map.fromList . map (\v -> (f v, v))

type LoadResults = Map (Path Abs File) (Maybe RawPom)

-- Loaded poms plus the resolved <module> pairs (aggregator path, child path)
-- recorded during loading; 'buildClosure' turns the latter into graph edges.
type LoadState = (LoadResults, [(Path Abs File, Path Abs File)])

-- Recursively load a pom and its adjacent poms (parent, submodules)
recursiveLoadPom :: forall sig m. (Has ReadFS sig m, Has (State LoadState) sig m, Has Diagnostics sig m) => Path Abs File -> m ()
recursiveLoadPom path = do
  (results, _) <- get @LoadState

  case Map.lookup path results of
    -- don't re-inspect this same path
    Just _ -> pure ()
    Nothing -> do
      (res :: Maybe RawPom) <- recover (readContentsXML path)
      modify @LoadState (first (Map.insert path res))
      traverse_ loadAdjacent res
  where
    loadAdjacent :: RawPom -> m ()
    loadAdjacent raw = loadParent raw *> loadSubmodules raw

    loadParent pom = case rawPomParent pom of
      Nothing -> pure ()
      -- the default relative path is "../pom.xml"
      --
      -- from the docs:
      -- "The relative path of the parent <code>pom.xml</code> file within the check out. If not specified, it defaults to <code>../pom.xml</code>"
      Just mvnParent -> recurseRelative (fromMaybe "../pom.xml" (rawParentRelativePath mvnParent))

    -- Record each resolved (aggregator, child) pair so 'buildClosure' can turn it
    -- into a <modules> edge (see there for why). Duplicate <module> entries may
    -- duplicate pairs; AM.edges dedupes them.
    loadSubmodules :: RawPom -> m ()
    loadSubmodules raw = traverse_ recurseModule (rawPomModules raw)

    recurseModule :: Text {- relative filepath -} -> m ()
    recurseModule rel = do
      resolvedPath :: Maybe (Path Abs File) <- recover $ resolvePomPath (parent path) rel
      case resolvedPath of
        Nothing -> pure ()
        Just childPath -> do
          modify @LoadState (second ((path, childPath) :))
          recursiveLoadPom childPath

    recurseRelative :: Text {- relative filepath -} -> m ()
    recurseRelative rel = do
      resolvedPath :: Maybe (Path Abs File) <- recover $ resolvePomPath (parent path) rel
      traverse_ recursiveLoadPom resolvedPath

-- resolve a Filepath (in Text) that may either point to a directory or an exact
-- pom file. when it's a directory, we default to pointing at the "pom.xml" in
-- that directory.
resolvePomPath :: forall sig m. (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> Text -> m (Path Abs File)
resolvePomPath cur txt = context "Resolving parent pom.xml path" $ do
  let resolveToFile :: m (Path Abs File)
      resolveToFile = do
        file <- resolveFile cur txt
        checkFile file

      resolveToDir :: m (Path Abs File)
      resolveToDir = do
        dir <- resolveDir cur txt
        let file = dir </> $(mkRelFile "pom.xml")
        checkFile file

      checkFile file = do
        exists <- doesFileExist file
        unless exists $
          fatal (FileReadError (show file) "resolvePath: resolved file does not exist")
        pure file

  resolveToFile <||> resolveToDir
