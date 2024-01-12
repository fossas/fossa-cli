{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Strategy.Haskell.Cabal (
  discover,

  -- * Testing
  BuildPlan (..),
  PlanId (..),
  InstallPlan (..),
  PlanStyle (..),
  PlanType (..),
  buildGraph,
) where

import App.Fossa.Analyze.Types (AnalyzeProject (analyzeProjectStaticOnly), analyzeProject)
import Control.Effect.Diagnostics (
  Diagnostics,
  Has,
  ToDiagnostic (..),
  context,
  errCtx,
  fatal,
  fatalText,
 )
import Control.Effect.Reader (Reader)
import Control.Monad (when)
import Data.Aeson.Types (
  FromJSON (parseJSON),
  ToJSON,
  withObject,
  (.!=),
  (.:),
  (.:?),
 )
import Data.Foldable (for_)
import Data.List (isSuffixOf)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String.Conversion (toString)
import Data.Text (Text)
import Data.Text qualified as Text
import Discovery.Filters (AllFilters)
import Discovery.Simple (simpleDiscover)
import Discovery.Walk (
  WalkStep (WalkContinue, WalkSkipAll),
  fileName,
  walkWithFilters',
 )
import Effect.Exec (
  AllowErr (Never),
  Command (..),
  Exec,
  execThrow,
 )
import Effect.Grapher (
  MappedGrapher,
  direct,
  edge,
  mapping,
  withMapping,
 )
import Effect.ReadFS (ReadFS, readContentsJson)
import GHC.Generics (Generic)
import Graphing (Graphing)
import Graphing qualified as G
import Path (Abs, Dir, File, Path, Rel, mkRelFile, (</>))
import Types (
  DepType (HackageType),
  Dependency (..),
  DependencyResults (..),
  DiscoveredProject (..),
  DiscoveredProjectType (CabalProjectType),
  GraphBreadth (Complete),
  VerConstraint (CEq),
 )

newtype BuildPlan = BuildPlan {installPlans :: [InstallPlan]} deriving (Eq, Ord, Show)
newtype Component = Component {componentDeps :: Set PlanId} deriving (Eq, Ord, Show)
newtype PlanId = PlanId {unPlanId :: Text} deriving (FromJSON, Eq, Ord, Show)

data InstallPlan = InstallPlan
  { planType :: PlanType
  , planId :: PlanId
  , planName :: Text
  , planVersion :: Text
  , planDepends :: Set PlanId
  , planStyle :: Maybe PlanStyle
  , planComponents :: Set PlanId
  }
  deriving (Eq, Ord, Show)

data PlanStyle
  = Local
  | Global
  deriving (Eq, Ord, Show)

data PlanType
  = PreExisting
  | Configured
  deriving (Eq, Ord, Show)

instance FromJSON BuildPlan where
  parseJSON = withObject "BuildPlan" $ \obj ->
    BuildPlan <$> obj .: "install-plan"

instance FromJSON Component where
  parseJSON = withObject "Component" $ \obj ->
    Component <$> obj .: "depends"

instance FromJSON InstallPlan where
  parseJSON = withObject "InstallPlan" $ \obj ->
    InstallPlan
      <$> (obj .: "type" >>= parsePlanType)
      <*> obj .: "id"
      <*> obj .: "pkg-name"
      <*> obj .: "pkg-version"
      <*> (obj .:? "depends" .!= Set.empty)
      <*> (obj .:? "style" >>= traverse parsePlanStyle)
      <*> fmap mergeComponents (obj .:? "components" .!= Map.empty)

mergeComponents :: Map Text Component -> Set PlanId
mergeComponents mapA = Set.unions . map componentDeps $ Map.elems mapA

installPlanDepends :: InstallPlan -> Set PlanId
installPlanDepends InstallPlan{..} = planComponents <> planDepends

isDirectDep :: InstallPlan -> Bool
isDirectDep InstallPlan{..} = planStyle == Just Local && planType == Configured

parsePlanStyle :: MonadFail f => Text -> f PlanStyle
parsePlanStyle style = case Text.toLower style of
  "global" -> pure Global
  "local" -> pure Local
  _ -> fail $ "unknown install plan style" ++ toString style

parsePlanType :: MonadFail m => Text -> m PlanType
parsePlanType typ = case Text.toLower typ of
  "configured" -> pure Configured
  "pre-existing" -> pure PreExisting
  _ -> fail $ "unknown install plan type" ++ toString typ

cabalGenPlanCmd :: Command
cabalGenPlanCmd =
  Command
    { cmdName = "cabal"
    , cmdArgs = ["v2-build", "all", "--dry-run"]
    , cmdAllowErr = Never
    }

cabalPlanFilePath :: Path Rel File
cabalPlanFilePath = $(mkRelFile "dist-newstyle/cache/plan.json")

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [DiscoveredProject CabalProject]
discover = simpleDiscover findProjects mkProject CabalProjectType

isCabalFile :: Path Abs File -> Bool
isCabalFile file = isDotCabal || isCabalDotProject
  where
    name = fileName file
    isDotCabal = ".cabal" `isSuffixOf` name
    isCabalDotProject = "cabal.project" == name

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m, Has (Reader AllFilters) sig m) => Path Abs Dir -> m [CabalProject]
findProjects = walkWithFilters' $ \dir _ files -> do
  -- NOTE: the long-term more-accurate version here is to parse the `cabal.project` file and look
  -- for relevant cabal files to mark as manifests.
  let manifestFiles = filter isCabalFile files

  if not (null manifestFiles)
    then pure ([CabalProject dir manifestFiles], WalkSkipAll)
    else pure ([], WalkContinue)

mkProject :: CabalProject -> DiscoveredProject CabalProject
mkProject project =
  DiscoveredProject
    { projectType = CabalProjectType
    , projectBuildTargets = mempty
    , projectPath = cabalDir project
    , projectData = project
    }

getDeps :: (Has ReadFS sig m, Has Exec sig m, Has Diagnostics sig m) => CabalProject -> m DependencyResults
getDeps project =
  context "Cabal" $
    context "Dynamic analysis" $
      analyze project

data CabalProject = CabalProject
  { cabalDir :: Path Abs Dir
  , cabalFiles :: [Path Abs File]
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON CabalProject

instance AnalyzeProject CabalProject where
  analyzeProject _ = getDeps
  analyzeProjectStaticOnly _ = const $ fatalText "Cannot analyze cabal project statically"

doGraph :: Has (MappedGrapher PlanId InstallPlan) sig m => InstallPlan -> m ()
doGraph plan = do
  let parentId = planId plan
  mapping parentId plan
  for_ (installPlanDepends plan) $ \dep -> do
    edge parentId dep
    when (isDirectDep plan) (direct dep)

shouldInclude :: InstallPlan -> Bool
shouldInclude plan = not $ isDirectDep plan || planType plan == PreExisting

ignorePlanId :: PlanId -> InstallPlan -> InstallPlan
ignorePlanId _ x = x

buildGraph :: Has Diagnostics sig m => BuildPlan -> m (Graphing Dependency)
buildGraph plan = do
  result <- withMapping ignorePlanId $ traverse doGraph (installPlans plan)
  case result of
    Left err -> fatal err
    Right gr -> pure . G.gmap toDependency $ G.filter shouldInclude gr

toDependency :: InstallPlan -> Dependency
toDependency plan =
  Dependency
    { dependencyType = HackageType
    , dependencyName = planName plan
    , dependencyVersion = Just $ CEq $ planVersion plan
    , dependencyLocations = []
    , dependencyEnvironments = mempty
    , dependencyTags = Map.empty
    }

analyze :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m) => CabalProject -> m DependencyResults
analyze project = do
  let dir = cabalDir project
  _ <- errCtx FailedToGenCabalPlan $ execThrow dir cabalGenPlanCmd
  plans <- readContentsJson @BuildPlan (dir </> cabalPlanFilePath)
  graph <- context "Building dependency graph" $ buildGraph plans
  pure $
    DependencyResults
      { dependencyGraph = graph
      , dependencyGraphBreadth = Complete
      , dependencyManifestFiles = cabalFiles project
      }

data FailedToGenCabalPlan = FailedToGenCabalPlan
instance ToDiagnostic FailedToGenCabalPlan where
  renderDiagnostic _ = "We could not dry run cabal build for dependency analysis."
