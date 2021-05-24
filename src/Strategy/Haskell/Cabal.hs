{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Strategy.Haskell.Cabal
  ( discover,

    -- * Testing
    BuildPlan (..),
    PlanId (..),
    InstallPlan (..),
    PlanStyle (..),
    PlanType (..),
    buildGraph,
  )
where

import Control.Effect.Diagnostics
import Control.Monad (when)
import Data.Aeson.Types
import Data.Foldable (for_)
import Data.List (isSuffixOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Discovery.Walk
import Effect.Exec
import Effect.Grapher
import Effect.ReadFS
import Graphing (Graphing)
import qualified Graphing as G
import Path
import Types

newtype BuildPlan = BuildPlan {installPlans :: [InstallPlan]} deriving (Eq, Ord, Show)
newtype Component = Component {componentDeps :: Set PlanId} deriving (Eq, Ord, Show)
newtype PlanId = PlanId {unPlanId :: Text} deriving (FromJSON, Eq, Ord, Show)

data InstallPlan
  = InstallPlan
      { planType :: PlanType,
        planId :: PlanId,
        planName :: Text,
        planVersion :: Text,
        planDepends :: Set PlanId,
        planStyle :: Maybe PlanStyle,
        planComponents :: Set PlanId
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
    InstallPlan <$> (obj .: "type" >>= parsePlanType)
      <*> obj .: "id"
      <*> obj .: "pkg-name"
      <*> obj .: "pkg-version"
      <*> (obj .:? "depends" .!= S.empty)
      <*> (obj .:? "style" >>= traverse parsePlanStyle)
      <*> fmap mergeComponents (obj .:? "components" .!= M.empty)

mergeComponents :: Map Text Component -> Set PlanId
mergeComponents mapA = S.unions . map componentDeps $ M.elems mapA

installPlanDepends :: InstallPlan -> Set PlanId
installPlanDepends InstallPlan {..} = planComponents <> planDepends

isDirectDep :: InstallPlan -> Bool
isDirectDep InstallPlan {..} = planStyle == Just Local && planType == Configured

parsePlanStyle :: MonadFail f => Text -> f PlanStyle
parsePlanStyle style = case T.toLower style of
  "global" -> pure Global
  "local" -> pure Local
  _ -> fail $ "unknown install plan style" ++ T.unpack style

parsePlanType :: MonadFail m => Text -> m PlanType
parsePlanType typ = case T.toLower typ of
  "configured" -> pure Configured
  "pre-existing" -> pure PreExisting
  _ -> fail $ "unknown install plan type" ++ T.unpack typ

cabalGenPlanCmd :: Command
cabalGenPlanCmd =
  Command
    { cmdName = "cabal",
      cmdArgs = ["v2-build", "--dry-run"],
      cmdAllowErr = Never
    }

cabalPlanFilePath :: Path Rel File
cabalPlanFilePath = $(mkRelFile "dist-newstyle/cache/plan.json")

isCabalFile :: Path Abs File -> Bool
isCabalFile file = isDotCabal || isCabalDotProject
  where
    name = fileName file
    isDotCabal = ".cabal" `isSuffixOf` name
    isCabalDotProject = "cabal.project" == name

discover :: (Has ReadFS sig m, Has Diagnostics sig m, Has ReadFS rsig run, Has Exec rsig run, Has Diagnostics rsig run) => Path Abs Dir -> m [DiscoveredProject run]
discover dir = context "Cabal" $ do
  projects <- context "Finding projects" $ findProjects dir
  pure (map mkProject projects)

findProjects :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m [CabalProject]
findProjects = walk' $ \dir _ files -> do
  let project =
        CabalProject
          { cabalDir = dir
          }

  if any isCabalFile files
    then pure ([project], WalkSkipAll)
    else pure ([], WalkContinue)

mkProject :: (Has ReadFS sig n, Has Exec sig n, Has Diagnostics sig n) => CabalProject -> DiscoveredProject n
mkProject project =
  DiscoveredProject
    { projectType = "cabal",
      projectBuildTargets = mempty,
      projectDependencyGraph = const $ getDeps project,
      projectPath = cabalDir project,
      projectLicenses = pure []
    }

getDeps :: (Has ReadFS sig m, Has Exec sig m, Has Diagnostics sig m) => CabalProject -> m (Graphing Dependency)
getDeps project =
  context "Cabal" $
    context "Dynamic analysis" $
      analyze (cabalDir project)

newtype CabalProject = CabalProject
  { cabalDir :: Path Abs Dir
  } deriving (Eq, Ord, Show)

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
    { dependencyType = HackageType,
      dependencyName = planName plan,
      dependencyVersion = Just $ CEq $ planVersion plan,
      dependencyLocations = [],
      dependencyEnvironments = [],
      dependencyTags = M.empty
    }

analyze :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> m (Graphing Dependency)
analyze dir = do
  _ <- execThrow dir cabalGenPlanCmd
  plans <- readContentsJson @BuildPlan (dir </> cabalPlanFilePath)
  context "Building dependency graph" $ buildGraph plans
