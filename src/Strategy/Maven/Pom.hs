module Strategy.Maven.Pom (
  analyze',
  getLicenses,
  interpolateProperties,
  buildMavenPackage,
  MavenPackage (..),
) where

import Algebra.Graph.AdjacencyMap qualified as AM
import Control.Applicative ((<|>))
import Control.Effect.Diagnostics hiding (fromMaybe)
import Data.Foldable (for_, traverse_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Extra (breakOnAndRemove)
import DepTypes
import Effect.Grapher
import Graphing (Graphing)
import Path
import Path.IO qualified as Path
import Strategy.Maven.Pom.Closure
import Strategy.Maven.Pom.PomFile
import Types

data MavenStrategyOpts = MavenStrategyOpts
  { strategyPath :: Path Rel File
  , strategyGraph :: Graphing Dependency
  }
  deriving (Eq, Ord, Show)

analyze' :: MavenProjectClosure -> Graphing Dependency
analyze' = buildProjectGraph

getLicenses :: MavenProjectClosure -> [LicenseResult]
getLicenses closure = do
  (abspath, pom) <- Map.elems (closurePoms closure)
  case Path.makeRelative (closureAnalysisRoot closure) abspath of
    Nothing -> []
    Just relpath ->
      let path = toFilePath relpath
          validated = mapMaybe validateLicense (pomLicenses pom)
       in pure (LicenseResult path validated)
  where
    -- we prefer URLs over SPDX because name isn't guaranteed to be an SPDX expression
    validateLicense :: PomLicense -> Maybe License
    validateLicense license = licenseAsUrl <|> licenseAsSpdx
      where
        licenseAsUrl = License LicenseURL <$> pomLicenseUrl license
        licenseAsSpdx = License LicenseSPDX <$> pomLicenseName license

type Version = Text

data MavenPackage = MavenPackage Group Artifact (Maybe Version)
  deriving (Eq, Ord, Show)

type MavenGrapher = LabeledGrapher MavenPackage MavenLabel

data MavenLabel
  = MavenLabelScope Text
  | MavenLabelOptional Text
  deriving (Eq, Ord, Show)

toDependency :: MavenPackage -> Set MavenLabel -> Dependency
toDependency (MavenPackage group artifact version) = foldr applyLabel start
  where
    start :: Dependency
    start =
      Dependency
        { dependencyType = MavenType
        , dependencyName = group <> ":" <> artifact
        , dependencyVersion = CEq <$> version
        , dependencyLocations = []
        , dependencyEnvironments = mempty
        , dependencyTags = Map.empty
        }

    applyLabel :: MavenLabel -> Dependency -> Dependency
    applyLabel lbl dep = case lbl of
      MavenLabelScope scope ->
        if scope == "test"
          then insertEnvironment EnvTesting dep
          else addTag "scope" scope dep
      MavenLabelOptional opt -> addTag "optional" opt dep

    -- TODO: reuse this in other strategies
    addTag :: Text -> Text -> Dependency -> Dependency
    addTag key value dep = dep{dependencyTags = Map.insertWith (++) key [value] (dependencyTags dep)}

-- TODO: set top-level direct deps as direct instead of the project?
buildProjectGraph :: MavenProjectClosure -> Graphing Dependency
buildProjectGraph closure = run . withLabeling toDependency $ do
  direct (coordToPackage (closureRootCoord closure))
  go (closureRootCoord closure) (closureRootPom closure)
  where
    go :: Has MavenGrapher sig m => MavenCoordinate -> Pom -> m ()
    go coord incompletePom = do
      _ <- Map.traverseWithKey addDep deps
      for_ childPoms $ \(childCoord, childPom) -> do
        edge (coordToPackage coord) (coordToPackage childCoord)
        go childCoord childPom
      where
        completePom :: Pom
        completePom = overlayParents incompletePom

        overlayParents :: Pom -> Pom
        overlayParents pom = fromMaybe pom $ do
          parentCoord <- pomParentCoord pom
          (_, parentPom) <- Map.lookup parentCoord (closurePoms closure)
          pure (pom <> overlayParents parentPom)

        deps :: Map (Group, Artifact) MvnDepBody
        deps = reifyDeps completePom

        addDep :: Has MavenGrapher sig m => (Group, Artifact) -> MvnDepBody -> m ()
        addDep (group, artifact) body = do
          let depPackage = buildMavenPackage completePom group artifact body

          edge (coordToPackage coord) depPackage
          traverse_ (label depPackage . MavenLabelScope) (depScope body)
          traverse_ (label depPackage . MavenLabelOptional) (depOptional body)

        childPoms :: [(MavenCoordinate, Pom)]
        childPoms =
          [ (childCoord, pom) | childCoord <- Set.toList (AM.postSet coord (closureGraph closure)), Just (_, pom) <- [Map.lookup childCoord (closurePoms closure)]
          ]

-- | Build a MavenPackage for a dependency in a pom file
--
-- This does ${property} interpolation for dependency group/artifact/version
-- and appends the dependency classifier to the version
buildMavenPackage :: Pom -> Group -> Artifact -> MvnDepBody -> MavenPackage
buildMavenPackage pom group artifact body = MavenPackage interpolatedGroup interpolatedArtifact interpolatedVersion
  where
    interpolatedGroup :: Group
    interpolatedGroup = interpolateProperties pom group

    interpolatedArtifact :: Artifact
    interpolatedArtifact = interpolateProperties pom artifact

    interpolatedVersion :: Maybe Text
    interpolatedVersion = classify . interpolateProperties pom <$> depVersion body

    -- maven classifiers are appended to the end of versions, e.g., 3.0.0 with a classifier
    -- of "sources" would result in "3.0.0-sources". However, current fetcher implementation uses ":",
    -- to separate classifier when resolving.

    classify :: Version -> Version
    classify version = case depClassifier body of
      Nothing -> version
      Just classifier -> version <> ":" <> classifier

coordToPackage :: MavenCoordinate -> MavenPackage
coordToPackage coord = MavenPackage (coordGroup coord) (coordArtifact coord) (Just (coordVersion coord))

-- | Reify dependencies in a pom file by overlaying information from the <dependencyManagement> section.
--
-- Notably, the dependencies in the resulting @Map@ _do not_ have their ${properties} interpolated
reifyDeps :: Pom -> Map (Group, Artifact) MvnDepBody
reifyDeps pom = Map.mapWithKey overlayDepManagement (pomDependencies pom)
  where
    overlayDepManagement :: (Group, Artifact) -> MvnDepBody -> MvnDepBody
    overlayDepManagement key body = maybe body (body <>) (Map.lookup key (pomDependencyManagement pom))

-- | Interpolate Pom properties into a string with the ${property} format This
-- interpolates both computed/built-in properties and user-specified properties,
-- preferring user-specified properties
interpolateProperties :: Pom -> Text -> Text
interpolateProperties pom = interpolate (pomProperties pom <> computeBuiltinProperties pom)

-- | Compute the most-commonly-used builtin properties for package resolution
computeBuiltinProperties :: Pom -> Map Text Text
computeBuiltinProperties pom =
  Map.fromList
    [ ("project.groupId", coordGroup (pomCoord pom))
    , ("project.artifactId", coordArtifact (pomCoord pom))
    , ("project.version", coordVersion (pomCoord pom))
    ]

interpolate :: Map Text Text -> Text -> Text
interpolate properties initialProperty =
  case splitMavenProperty initialProperty of
    Nothing -> initialProperty
    Just (prefix, property, suffix) ->
      case (Map.lookup property properties) of
        Nothing -> interpolate properties $ prefix <> "PROPERTY NOT FOUND: " <> property <> suffix
        -- This block catches an infinite loop with interpolate
        -- For the example of: <junit5.version>${junit5.version}</junit5.version>
        -- The map will have ("junit5.version","${junit5.version}")
        -- splitMavenProperty will remove the "${}" from the value and return the same key which causes infinite recursion.
        -- Just foundProperty -> if fullProperty == initialProperty then property else interpolate properties fullProperty
        Just foundProperty -> case splitMavenProperty $ prefix <> foundProperty <> suffix of
          Nothing -> property
          Just (_, property2, _) -> if property2 == property then property else interpolate properties $ prefix <> foundProperty <> suffix

-- where
--   fullProperty = prefix <> foundProperty <> suffix
-- find the first maven property in the string, e.g., `${foo}`, returning text
-- before the property, the property, and the text after the property
splitMavenProperty :: Text -> Maybe (Text, Text, Text)
splitMavenProperty text = do
  (beforeBegin, afterBegin) <- breakOnAndRemove "${" text
  (property, afterEnd) <- breakOnAndRemove "}" afterBegin
  pure (beforeBegin, property, afterEnd)
