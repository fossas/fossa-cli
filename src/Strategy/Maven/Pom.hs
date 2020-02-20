module Strategy.Maven.Pom
  ( discover
  ) where

import Prologue

import qualified Algebra.Graph.AdjacencyMap as AM
import Control.Carrier.Error.Either
import Control.Effect.Output
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import DepTypes
import Effect.LabeledGrapher
import qualified Graphing as G
import Strategy.Maven.Pom.Closure
import Strategy.Maven.Pom.PomFile
import Types

data MavenStrategyOpts = MavenStrategyOpts
  { strategyPath  :: Path Rel File
  , strategyGraph :: G.Graphing Dependency
  } deriving (Eq, Ord, Show, Generic)

discover :: HasDiscover sig m => Path Abs Dir -> m ()
discover dir = runStrategy "maven-pom" MavenGroup $ do
  (mvnClosures :: [MavenProjectClosure]) <- findProjects dir
  traverse_ (output . mkProjectClosure) mvnClosures

mkProjectClosure :: MavenProjectClosure -> ProjectClosure
mkProjectClosure mvnClosure = ProjectClosure
  { closureStrategyGroup = MavenGroup
  , closureStrategyName  = "maven-pom"
  , closureModuleDir     = parent (closurePath mvnClosure)
  , closureDependencies  = dependencies
  , closureLicenses      = []
  }
  where
  dependencies = ProjectDependencies
    { dependenciesGraph    = buildProjectGraph mvnClosure
    , dependenciesOptimal  = NotOptimal
    , dependenciesComplete = NotComplete
    }

type Version = Text
data MavenPackage = MavenPackage Group Artifact (Maybe Version)
  deriving (Eq, Ord, Show, Generic)

type instance PkgLabel MavenPackage = MavenLabel

data MavenLabel =
    MavenLabelClassifier Text
  | MavenLabelScope Text
  | MavenLabelOptional Text
  deriving (Eq, Ord, Show, Generic)

toDependency :: MavenPackage -> Set MavenLabel -> Dependency
toDependency (MavenPackage group artifact version) = foldr applyLabel start
  where
  start :: Dependency
  start = Dependency
    { dependencyType = MavenType
    , dependencyName = group <> ":" <> artifact
    , dependencyVersion = CEq <$> version
    , dependencyLocations = []
    , dependencyTags = M.empty
    }

  applyLabel :: MavenLabel -> Dependency -> Dependency
  applyLabel lbl dep = case lbl of
    MavenLabelClassifier classifier -> addTag "classifier" classifier dep
    MavenLabelScope scope -> addTag "scope" scope dep
    MavenLabelOptional opt -> addTag "optional" opt dep

  -- TODO: reuse this in other strategies
  addTag :: Text -> Text -> Dependency -> Dependency
  addTag key value dep = dep { dependencyTags = M.insertWith (++) key [value] (dependencyTags dep) }

-- TODO: set top-level direct deps as direct instead of the project?
buildProjectGraph :: MavenProjectClosure -> G.Graphing Dependency
buildProjectGraph closure = run . withLabeling toDependency $ do
  direct (coordToPackage (closureRootCoord closure))
  go (closureRootCoord closure) (closureRootPom closure)
  where
  go :: Has (LabeledGrapher MavenPackage) sig m => MavenCoordinate -> Pom -> m ()
  go coord incompletePom = do
    _ <- M.traverseWithKey addDep deps
    for_ childPoms $ \(childCoord,childPom) -> do
      edge (coordToPackage coord) (coordToPackage childCoord)
      go childCoord childPom

    where
    completePom :: Pom
    completePom = overlayParents incompletePom

    overlayParents :: Pom -> Pom
    overlayParents pom = fromMaybe pom $ do
      parentCoord <- pomParentCoord pom
      (_,parentPom) <- M.lookup parentCoord (closurePoms closure)
      pure (pom <> overlayParents parentPom)

    deps :: Map (Group,Artifact) MvnDepBody
    deps = reifyDeps completePom

    addDep :: Has (LabeledGrapher MavenPackage) sig m => (Group,Artifact) -> MvnDepBody -> m ()
    addDep (group,artifact) body = do
      let interpolatedVersion = naiveInterpolate (pomProperties completePom) <$> depVersion body
          depPackage = MavenPackage group artifact interpolatedVersion

      edge (coordToPackage coord) depPackage

    childPoms :: [(MavenCoordinate,Pom)]
    childPoms = [(childCoord,pom) | childCoord <- S.toList (AM.postSet coord (closureGraph closure))
                                  , Just (_,pom) <- [M.lookup childCoord (closurePoms closure)]]

coordToPackage :: MavenCoordinate -> MavenPackage
coordToPackage coord = MavenPackage (coordGroup coord) (coordArtifact coord) (Just (coordVersion coord))

reifyDeps :: Pom -> Map (Group, Artifact) MvnDepBody
reifyDeps pom = M.mapWithKey overlayDepManagement (pomDependencies pom)
  where
  overlayDepManagement :: (Group,Artifact) -> MvnDepBody -> MvnDepBody
  overlayDepManagement key body = maybe body (body <>) (M.lookup key (pomDependencyManagement pom))

-- Naively interpolate properties into a Text. This only interpolates Text that
-- starts with "${" and ends with "}", e.g.,
--     "${myproperty}"
-- will interpolate the value of "myproperty", but
--     "blah-${myproperty}"
-- will not have its property interpolated
naiveInterpolate :: Map Text Text -> Text -> Text
naiveInterpolate properties text
  | T.isPrefixOf "${" text
  , T.isSuffixOf "}" text =
      let stripped = T.drop 2 (T.init text)
       in fromMaybe ("PROPERTY NOT FOUND: " <> text) (M.lookup stripped properties)
  | otherwise = text
