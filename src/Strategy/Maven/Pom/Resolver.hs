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
  MavenCoordinate (MavenCoordinate),
  Pom (Pom, pomCoord, pomParentCoord),
  RawParent (rawParentRelativePath),
  RawPom (rawPomModules, rawPomParent),
  coordArtifact,
  pomModules,
  validatePom,
 )

data GlobalClosure = GlobalClosure
  { globalGraph :: AM.AdjacencyMap MavenCoordinate
  , globalPoms :: Map MavenCoordinate (Path Abs File, Pom)
  }
  deriving (Eq, Ord, Show)

buildGlobalClosure :: (Has ReadFS sig m, Has Diagnostics sig m) => [Path Abs File] -> m GlobalClosure
buildGlobalClosure files = do
  (loadResults, ()) <- runState @LoadResults Map.empty $ traverse_ recursiveLoadPom files

  -- TODO: diagnostics/warnings?
  let validated :: Map (Path Abs File) Pom
      validated = Map.mapMaybe (validatePom =<<) loadResults

  pure (buildClosure validated)
  where
    -- notably, we're not building edges based on <relativePath> from poms.
    --
    -- From the docs:
    -- "However, the group ID, artifact ID and version are still required, and must match the file in the location given or it will revert to the repository for the POM."
    --
    -- Because the group/artifact/version are required to match, we can just build edges between _coordinates_, rather than between _pom files_
    buildClosure :: Map (Path Abs File) Pom -> GlobalClosure
    buildClosure cache =
      GlobalClosure
        { globalGraph =
            AM.vertices (map pomCoord (Map.elems cache))
              `AM.overlay` AM.edges
                [ (parentCoord, pomCoord pom)
                | pom <- Map.elems cache
                , Just parentCoord <- [pomParentCoord pom]
                , isSubmoduleChild parentCoord (coordArtifact . pomCoord $ pom)
                ]
        , globalPoms = allPoms
        }
      where
        allPoms :: Map MavenCoordinate (Path Abs File, Pom)
        allPoms = indexBy (pomCoord . snd) (Map.toList cache)

        -- This check isn't foolproof because a `<module>` tag only includes a module name not a full coordinate.
        -- This means we could potentially match a module A with its parent (B) if B references a child
        -- with the same artifact id as A but that isn't actually A.
        -- In practice, this is probably uncommon.
        isSubmoduleChild :: MavenCoordinate -> Text -> Bool
        isSubmoduleChild parentMod childName = maybe False (\(_, pom) -> childName `elem` pomModules pom) $ Map.lookup parentMod allPoms

-- TODO: reuse this in other strategies
indexBy :: Ord k => (v -> k) -> [v] -> Map k v
indexBy f = Map.fromList . map (\v -> (f v, v))

type LoadResults = Map (Path Abs File) (Maybe RawPom)

-- Recursively load a pom and its adjacent poms (parent, submodules)
recursiveLoadPom :: forall sig m. (Has ReadFS sig m, Has (State LoadResults) sig m, Has Diagnostics sig m) => Path Abs File -> m ()
recursiveLoadPom path = do
  results <- get @LoadResults

  case Map.lookup path results of
    -- don't re-inspect this same path
    Just _ -> pure ()
    Nothing -> do
      (res :: Maybe RawPom) <- recover (readContentsXML path)
      modify @LoadResults (Map.insert path res)
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

    loadSubmodules raw = traverse_ recurseRelative (rawPomModules raw)

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
