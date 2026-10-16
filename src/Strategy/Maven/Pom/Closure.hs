{-# LANGUAGE RecordWildCards #-}

module Strategy.Maven.Pom.Closure (
  extractSubmoduleFromCoordinate,
  findProjects,
  MavenProjectClosure (..),
  buildProjectClosures,
  submodulesFromCoordinate,
) where

import Algebra.Graph.AdjacencyMap qualified as AM
import Algebra.Graph.AdjacencyMap.Algorithm qualified as AM
import Control.Algebra
import Control.Applicative ((<|>))
import Control.Carrier.State.Strict
import Control.Effect.Diagnostics
import Data.Aeson (ToJSON, object, toJSON, (.=))
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.List (isSuffixOf)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String.Conversion (toText)
import Discovery.Walk
import Effect.ReadFS
import GHC.Generics (Generic)
import Path
import Path.IO qualified as PIO
import Strategy.Maven.Pom.PomFile
import Strategy.Maven.Pom.Resolver

import Control.Effect.Reader (Reader)
import Data.Text (Text)
import Discovery.Filters (AllFilters)

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [MavenProjectClosure]
findProjects basedir = do
  pomFiles <- context "Finding pom files" $ findPomFiles basedir
  globalClosure <- context "Building global closure" $ buildGlobalClosure pomFiles
  context "Building project closures" $ buildProjectClosures basedir globalClosure

findPomFiles :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [Path Abs File]
findPomFiles dir =
  execState @[Path Abs File] [] $
    flip walkWithFilters' dir $ \_ _ files -> do
      let poms = filter (\file -> "pom.xml" `isSuffixOf` fileName file || ".pom" `isSuffixOf` fileName file) files
      traverse_ (modify . (:)) poms

      pure ((), WalkSkipSome ["target"])

buildProjectClosures :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> GlobalClosure -> m [MavenProjectClosure]
buildProjectClosures analysisRoot global = do
  -- WHY: the closure stores poms under two mixed textual path forms — walked poms
  -- are keyed by the (possibly non-canonical) analysis-root prefix, while poms
  -- re-resolved during loading (<parent> relativePath and <modules>) are stored
  -- under OS-canonicalized strings. Those can differ textually from analysisRoot
  -- while denoting the same location (e.g. on Windows, an NTFS 8.3 short name like
  -- RUNNER~1 versus the long form runneradmin), in which case a string-based
  -- under-root check against only one form would silently drop project roots.
  -- Compare against both forms instead; fall back to the given directory if
  -- canonicalization fails.
  canonRoot :: Path Abs Dir <- Data.Maybe.fromMaybe analysisRoot <$> recover (resolveDir analysisRoot (toText ("." :: String)))
  let closures = map (\(path, (coord, pom)) -> toClosure path coord pom) (Map.toList projectRoots)
      toClosure :: Path Abs File -> MavenCoordinate -> Pom -> MavenProjectClosure
      toClosure path coord pom = MavenProjectClosure analysisRoot path coord pom reachableGraph reachablePomMap closureSubmodules
        where
          reachableGraph = AM.induce (`Set.member` reachablePoms) $ globalGraph global
          reachablePomMap = Map.filterWithKey (\k _ -> Set.member k reachablePoms) $ globalPoms global
          reachablePoms = bidirectionalReachable coord (globalGraph global)
          closureSubmodules = submodulesFromCoordinate reachablePomMap

      projectRoots :: Map (Path Abs File) (MavenCoordinate, Pom)
      projectRoots = determineProjectRoots analysisRoot canonRoot global graphRoots

      graphRoots :: [MavenCoordinate]
      graphRoots = sourceVertices (globalGraph global)
  pure closures

submodulesFromCoordinate :: Map MavenCoordinate a -> Set Text
submodulesFromCoordinate = Set.fromList . map extractSubmoduleFromCoordinate . Map.keys

extractSubmoduleFromCoordinate :: MavenCoordinate -> Text
extractSubmoduleFromCoordinate (MavenCoordinate group artifact _) = group <> ":" <> artifact

-- Find reachable nodes both below (children, grandchildren, ...) and above (parents, grandparents) the node
bidirectionalReachable :: Ord a => a -> AM.AdjacencyMap a -> Set.Set a
bidirectionalReachable node gr = Set.fromList $ AM.reachable gr node ++ AM.reachable (AM.transpose gr) node

sourceVertices :: Ord a => AM.AdjacencyMap a -> [a]
sourceVertices graph = [v | v <- AM.vertexList graph, Set.null (AM.preSet v graph)]

determineProjectRoots ::
  -- | the analysis root as passed in
  Path Abs Dir ->
  -- | the canonically resolved form of that root (may be identical)
  Path Abs Dir ->
  GlobalClosure ->
  [MavenCoordinate] ->
  Map (Path Abs File) (MavenCoordinate, Pom)
determineProjectRoots rootDir canonRoot closure = go . Set.fromList
  where
    -- A stored path may be in either textual form of the root directory (see
    -- buildProjectClosures); accept it if it is under that root in either form.
    underEither :: Path Abs File -> Maybe ()
    underEither p = PIO.makeRelative rootDir p $> () <|> PIO.makeRelative canonRoot p $> ()

    go :: Set MavenCoordinate -> Map (Path Abs File) (MavenCoordinate, Pom)
    go coordRoots
      | Set.null coordRoots = Map.empty
      | otherwise = Map.union projects (go frontier)
      where
        inRoot :: Set (MavenCoordinate, Path Abs File, Pom)
        inRoot =
          Set.fromList $
            mapMaybe
              ( \coord -> do
                  (abspath, pom) <- Map.lookup coord (globalPoms closure)
                  -- This ensures that the absolute path is relative to the root
                  -- directory, whichever textual form of it the path was stored in
                  _ <- underEither abspath
                  Just (coord, abspath, pom)
              )
              (Set.toList coordRoots)

        inRootCoords :: Set MavenCoordinate
        inRootCoords = Set.map (\(c, _, _) -> c) inRoot

        remainingCoords :: Set MavenCoordinate
        remainingCoords = coordRoots Set.\\ inRootCoords

        projects :: Map (Path Abs File) (MavenCoordinate, Pom)
        projects = Map.fromList $ Set.toList $ Set.map (\(coord, path, pom) -> (path, (coord, pom))) inRoot

        frontier :: Set MavenCoordinate
        frontier = Set.unions $ Set.map (\coord -> AM.postSet coord (globalGraph closure)) remainingCoords

data MavenProjectClosure = MavenProjectClosure
  { closureAnalysisRoot :: Path Abs Dir
  -- ^ the root of global fossa-analyze analysis; needed for declared license scan
  , closurePath :: Path Abs File
  -- ^ path of the pom file used as the root of this project closure
  , closureRootCoord :: MavenCoordinate
  , closureRootPom :: Pom
  , closureGraph :: AM.AdjacencyMap MavenCoordinate
  , closurePoms :: Map MavenCoordinate (Path Abs File, Pom)
  , closureSubmodules :: Set Text
  -- ^ all of the submodules in the maven project ; used for submodule filtering
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON MavenProjectClosure where
  toJSON MavenProjectClosure{..} =
    object
      [ "closureAnalysisRoot" .= closureAnalysisRoot
      , "closurePath" .= closurePath
      , "closureRootCoord" .= closureRootCoord
      , "closureRootPom" .= closureRootPom
      , "closureGraph" .= AM.adjacencyMap closureGraph
      , "closurePoms" .= closurePoms
      , "closureSubmodules" .= closureSubmodules
      ]
