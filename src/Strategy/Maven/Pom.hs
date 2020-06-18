module Strategy.Maven.Pom
  ( discover
  ) where

import Prologue

import qualified Algebra.Graph.AdjacencyMap as AM
import Control.Carrier.Error.Either
import Control.Effect.Output
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import DepTypes
import Effect.Grapher
import qualified Graphing as G
import qualified Path.IO as Path
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
  traverse_ (output . mkProjectClosure dir) mvnClosures

mkProjectClosure :: Path Abs Dir -> MavenProjectClosure -> ProjectClosure
mkProjectClosure basedir mvnClosure = ProjectClosure
  { closureStrategyGroup = MavenGroup
  , closureStrategyName  = "maven-pom"
  , closureModuleDir     = parent (closurePath mvnClosure)
  , closureDependencies  = dependencies
  , closureLicenses      = licenses
  }
  where
  dependencies = ProjectDependencies
    { dependenciesGraph    = buildProjectGraph mvnClosure
    , dependenciesOptimal  = NotOptimal
    , dependenciesComplete = NotComplete
    }

  licenses :: [LicenseResult]
  licenses = do
    (abspath,pom) <- M.elems (closurePoms mvnClosure)
    case Path.makeRelative basedir abspath of
      Nothing -> []
      Just relpath ->
        let path = toFilePath relpath
            validated = mapMaybe validateLicense (pomLicenses pom)
         in pure (LicenseResult path validated)

  -- we prefer URLs over SPDX because name isn't guaranteed to be an SPDX expression
  validateLicense :: PomLicense -> Maybe License
  validateLicense license = licenseAsUrl <|> licenseAsSpdx
    where
      licenseAsUrl = License LicenseURL <$> pomLicenseUrl license
      licenseAsSpdx = License LicenseSPDX <$> pomLicenseName license

type Version = Text
data MavenPackage = MavenPackage Group Artifact (Maybe Version)
  deriving (Eq, Ord, Show, Generic)

type MavenGrapher = LabeledGrapher MavenPackage MavenLabel

data MavenLabel =
    MavenLabelScope Text
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
    , dependencyEnvironments = []
    , dependencyTags = M.empty
    }

  applyLabel :: MavenLabel -> Dependency -> Dependency
  applyLabel lbl dep = case lbl of
    MavenLabelScope scope ->
      if scope == "test"
        then dep { dependencyEnvironments = EnvTesting : dependencyEnvironments dep }
        else addTag "scope" scope dep
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
  go :: Has MavenGrapher sig m => MavenCoordinate -> Pom -> m ()
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

    addDep :: Has MavenGrapher sig m => (Group,Artifact) -> MvnDepBody -> m ()
    addDep (group,artifact) body = do
      let interpolatedVersion = classify . naiveInterpolate (pomProperties completePom) <$> depVersion body
          -- maven classifiers are appended to the end of versions, e.g., 3.0.0 with a classifier
          -- of "sources" would result in "3.0.0-sources"
          classify version = case depClassifier body of
            Nothing -> version
            Just classifier -> version <> "-" <> classifier
          depPackage = MavenPackage group artifact interpolatedVersion

      edge (coordToPackage coord) depPackage
      traverse_ (label depPackage . MavenLabelScope) (depScope body)
      traverse_ (label depPackage . MavenLabelOptional) (depOptional body)

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
