module Strategy.Maven.Pom.Closure
  ( findProjects
  , MavenProjectClosure(..)
  ) where

import Prologue

import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Path.IO as PIO
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Polysemy
import Polysemy.Output

import Discovery.Walk
import Effect.ReadFS
import Strategy.Maven.Pom.PomFile
import Strategy.Maven.Pom.Resolver

findProjects :: Members '[Embed IO, ReadFS] r => Path Abs Dir -> Sem r [MavenProjectClosure]
findProjects basedir = do
  pomFiles <- findPomFiles basedir
  globalClosure <- buildGlobalClosure pomFiles
  pure (buildProjectClosures basedir globalClosure)

findPomFiles :: Member (Embed IO) r => Path Abs Dir -> Sem r [Path Abs File]
findPomFiles dir = do
  (relPaths,_) <- runOutputList @(Path Rel File) $
    flip walk dir $ \_ _ files -> do
      case find ((== "pom.xml") . fileName) files of
        Just file -> output file
        Nothing -> pure ()

      walkContinue

  -- FIXME: exceptions
  traverse (embed @IO . PIO.makeAbsolute) relPaths

buildProjectClosures :: Path Abs Dir -> GlobalClosure -> [MavenProjectClosure]
buildProjectClosures basedir global = closures
  where
  closures = map (\(path, (coord, pom)) -> toClosure path coord pom) (M.toList projectRoots)

  toClosure :: Path Rel File -> MavenCoordinate -> Pom -> MavenProjectClosure
  toClosure path coord pom = MavenProjectClosure path coord pom (globalGraph global) (globalPoms global)

  projectRoots :: Map (Path Rel File) (MavenCoordinate, Pom)
  projectRoots = determineProjectRoots basedir global graphRoots

  graphRoots :: [MavenCoordinate]
  graphRoots = sourceVertices (globalGraph global)

sourceVertices :: Ord a => AM.AdjacencyMap a -> [a]
sourceVertices graph = [v | v <- AM.vertexList graph, S.null (AM.preSet v graph)]

determineProjectRoots :: Path Abs Dir -> GlobalClosure -> [MavenCoordinate] -> Map (Path Rel File) (MavenCoordinate, Pom)
determineProjectRoots rootDir closure = go . S.fromList
  where
  go :: Set MavenCoordinate -> Map (Path Rel File) (MavenCoordinate, Pom)
  go coordRoots
    | S.null coordRoots = M.empty
    | otherwise = M.union projects (go frontier)
    where
    inRoot :: Set (MavenCoordinate, Path Rel File, Pom)
    inRoot = S.fromList $
      mapMaybe (\coord -> do
                   (abspath, pom) <- M.lookup coord (globalPoms closure)
                   relpath <- PIO.makeRelative rootDir abspath
                   pure (coord, relpath, pom)) (S.toList coordRoots)

    inRootCoords :: Set MavenCoordinate
    inRootCoords = S.map (\(c,_,_) -> c) inRoot

    remainingCoords :: Set MavenCoordinate
    remainingCoords = coordRoots S.\\ inRootCoords

    projects :: Map (Path Rel File) (MavenCoordinate, Pom)
    projects = M.fromList $ S.toList $ S.map (\(coord,path,pom) -> (path, (coord, pom))) inRoot

    frontier :: Set MavenCoordinate
    frontier = S.unions $ S.map (\coord -> AM.postSet coord (globalGraph closure)) remainingCoords


data MavenProjectClosure = MavenProjectClosure
  { closurePath      :: Path Rel File
  , closureRootCoord :: MavenCoordinate
  , closureRootPom   :: Pom
  , closureGraph     :: AM.AdjacencyMap MavenCoordinate
  , closurePoms      :: Map MavenCoordinate (Path Abs File, Pom)
  } deriving (Eq, Ord, Show, Generic)
