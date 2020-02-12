{-# language TemplateHaskell #-}
module Strategy.Maven.Pom.Resolver
  ( GlobalClosure(..)
  , buildGlobalClosure
  ) where

import Prologue

import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Data.Map.Strict as M
import Polysemy
import Polysemy.Error
import Polysemy.NonDet
import Polysemy.State

import Diagnostics
import Effect.ReadFS
import Strategy.Maven.Pom.PomFile

data GlobalClosure = GlobalClosure
  { globalGraph :: AM.AdjacencyMap MavenCoordinate
  , globalPoms  :: Map MavenCoordinate (Path Abs File, Pom)
  } deriving (Eq, Ord, Show, Generic)

buildGlobalClosure :: Member ReadFS r => [Path Abs File] -> Sem r GlobalClosure
buildGlobalClosure files = do
  (loadResults,()) <- runState @LoadResults M.empty $ traverse_ recursiveLoadPom files

  -- TODO: diagnostics/warnings?
  let validated :: Map (Path Abs File) Pom
      validated = M.mapMaybe (validatePom <=< eitherToMaybe) loadResults

  pure (buildClosure validated)

  where

  eitherToMaybe :: Either a b -> Maybe b
  eitherToMaybe (Left _) = Nothing
  eitherToMaybe (Right b) = Just b

  -- notably, we're not building edges based on <relativePath> from poms.
  --
  -- From the docs:
  -- "However, the group ID, artifact ID and version are still required, and must match the file in the location given or it will revert to the repository for the POM."
  --
  -- Because the group/artifact/version are required to match, we can just build edges between _coordinates_, rather than between _pom files_
  buildClosure :: Map (Path Abs File) Pom -> GlobalClosure
  buildClosure cache = GlobalClosure
    { globalGraph = AM.overlays
        [AM.edge parentCoord (pomCoord pom)
          | pom <- M.elems cache
          , Just parentCoord <- [pomParentCoord pom]]
    , globalPoms = indexBy (pomCoord . snd) (M.toList cache)
    }

-- TODO: reuse this in other strategies
indexBy :: Ord k => (v -> k) -> [v] -> Map k v
indexBy f = M.fromList . map (\v -> (f v, v))

type LoadResults = Map (Path Abs File) (Either ReadFSErr RawPom)

-- Recursively load a pom and its adjacent poms (parent, submodules)
recursiveLoadPom :: Members '[ReadFS, State LoadResults] r => Path Abs File -> Sem r ()
recursiveLoadPom path = do
  results <- get @LoadResults

  case M.lookup path results of
    -- don't re-inspect this same path
    Just _ -> pure ()
    Nothing -> do
      (res :: Either ReadFSErr RawPom) <- runError (readContentsXML path)
      modify @LoadResults (M.insert path res)
      traverse_ loadAdjacent res

  where

  loadAdjacent :: Members '[ReadFS, State LoadResults] r => RawPom -> Sem r ()
  loadAdjacent raw = loadParent raw *> loadSubmodules raw

  loadParent pom = case rawPomParent pom of
    Nothing -> pure ()
    -- the default relative path is "../pom.xml"
    --
    -- from the docs:
    -- "The relative path of the parent <code>pom.xml</code> file within the check out. If not specified, it defaults to <code>../pom.xml</code>"
    Just mvnParent -> recurseRelative (fromMaybe "../pom.xml" (rawParentRelativePath mvnParent))

  loadSubmodules raw = traverse_ recurseRelative (rawPomModules raw)

  recurseRelative :: Members '[ReadFS, State LoadResults] r => Text {- relative filepath -} -> Sem r ()
  recurseRelative rel = do
    (resolvedPath :: Either ReadFSErr (Path Abs File)) <- runError (resolvePath (parent path) rel)
    -- TODO: diagnostics/warnings?
    traverse_ recursiveLoadPom resolvedPath

-- resolve a Filepath (in Text) that may either point to a directory or an exact
-- pom file. when it's a directory, we default to pointing at the "pom.xml" in
-- that directory.
resolvePath :: Members '[ReadFS, Error ReadFSErr] r => Path Abs Dir -> Text -> Sem r (Path Abs File)
resolvePath cur txt = nonDetToError (ResolveError "Resolved file doesn't exist") $ do
  let resolveToFile :: Members '[NonDet, ReadFS, Error ReadFSErr] r => Sem r (Path Abs File)
      resolveToFile = do
        file <- resolveFile cur txt
        checkFile file

      resolveToDir :: Members '[NonDet, ReadFS, Error ReadFSErr] r => Sem r (Path Abs File)
      resolveToDir = do
        dir <- resolveDir cur txt
        let file = dir </> $(mkRelFile "pom.xml")
        checkFile file

      checkFile file = do
        exists <- doesFileExist file
        guard exists
        pure file

  resolveToFile <|> resolveToDir
