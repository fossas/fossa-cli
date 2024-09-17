module App.Fossa.Reachability.Upload (
  analyzeForReachability,
  upload,
  dependenciesOf,
  callGraphOf,
  onlyFoundUnits,
) where

import App.Fossa.Analyze.Debug (diagToDebug)
import App.Fossa.Analyze.Project (ProjectResult (..))
import App.Fossa.Analyze.Types (
  DiscoveredProjectScan (..),
  SourceUnitReachabilityAttempt (..),
  dpiProjectType,
 )
import App.Fossa.Reachability.Gradle (gradleJarCallGraph)
import App.Fossa.Reachability.Jar (callGraphFromJars)
import App.Fossa.Reachability.Maven (mavenJarCallGraph)
import App.Fossa.Reachability.Types (
  CallGraphAnalysis (..),
  ContentRef (..),
  ParsedJar (..),
  ReachabilityConfig (..),
  SourceUnitReachability (..),
  reachabilityEndpointJson,
  reachabilityRawJson,
 )
import App.Types (ProjectMetadata, ProjectRevision)
import Control.Algebra (Has)
import Control.Carrier.Diagnostics qualified as Diag
import Control.Effect.Debug (Debug, debugMetadata)
import Control.Effect.Diagnostics (Diagnostics, context)
import Control.Effect.FossaApiClient (FossaApiClient, uploadBuildForReachability, uploadContentForReachability)
import Control.Effect.Lift (Lift)
import Control.Effect.Reader (Reader, ask)
import Data.List (nub)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Diag.Result (Result (..))
import Effect.Exec (Exec)
import Effect.Logger (Logger, logDebug, logInfo, pretty)
import Effect.ReadFS (ReadFS)
import Srclib.Converter (projectToSourceUnit)
import Srclib.Types (
  Locator,
  SourceUnit (..),
  SourceUnitBuild (..),
  SourceUnitDependency (..),
 )
import Types (DiscoveredProjectType (..), GraphBreadth (..))

analyzeForReachability ::
  ( Has Logger sig m
  , Has ReadFS sig m
  , Has Diagnostics sig m
  , Has Exec sig m
  , Has (Lift IO) sig m
  , Has Debug sig m
  , Has (Reader ReachabilityConfig) sig m
  ) =>
  [DiscoveredProjectScan] ->
  m [SourceUnitReachabilityAttempt]
analyzeForReachability analyzerResult = context "reachability" $ do
  units <- traverse callGraphOf analyzerResult
  debugMetadata reachabilityRawJson (onlyFoundUnits units)
  pure units

upload ::
  ( Has FossaApiClient sig m
  , Has Debug sig m
  ) =>
  ProjectRevision ->
  ProjectMetadata ->
  [SourceUnitReachability] ->
  m ()
upload revision metadata units = do
  units' <- traverse uploadReachability units
  debugMetadata reachabilityEndpointJson units'
  uploadBuildForReachability revision metadata units'

uploadReachability ::
  (Has FossaApiClient sig m) =>
  SourceUnitReachability ->
  m SourceUnitReachability
uploadReachability unit = case callGraphAnalysis unit of
  NoCallGraphAnalysis -> pure unit
  JarAnalysis [] -> pure unit
  JarAnalysis someJars -> do
    updatedJars <- traverse uploadJarAnalysis someJars
    pure $ unit{callGraphAnalysis = JarAnalysis updatedJars}

uploadJarAnalysis ::
  (Has FossaApiClient sig m) =>
  ParsedJar ->
  m ParsedJar
uploadJarAnalysis jar = case parsedJarContent jar of
  ContentStoreKey _ -> pure jar
  ContentRaw bs -> do
    key <- uploadContentForReachability bs
    pure $ jar{parsedJarContent = ContentStoreKey key}

callGraphOf ::
  ( Has Logger sig m
  , Has ReadFS sig m
  , Has Diagnostics sig m
  , Has Exec sig m
  , Has (Lift IO) sig m
  , Has Debug sig m
  , Has (Reader ReachabilityConfig) sig m
  ) =>
  DiscoveredProjectScan ->
  m SourceUnitReachabilityAttempt
callGraphOf (Scanned dpi (Success _ projectResult)) = do
  let projectPath = projectResultPath projectResult
  let srcUnit = projectToSourceUnit False projectResult
  let dependencies = dependenciesOf srcUnit
  let displayId = sourceUnitType srcUnit <> "@" <> sourceUnitManifest srcUnit
  let unit =
        SourceUnitReachability
          { srcUnitType = sourceUnitType srcUnit
          , srcUnitManifest = sourceUnitManifest srcUnit
          , srcUnitName = sourceUnitName srcUnit
          , srcUnitOriginPaths = sourceUnitOriginPaths srcUnit
          , srcUnitDependencies = dependencies
          , callGraphAnalysis = NoCallGraphAnalysis
          }
  (reachabilityConfig :: ReachabilityConfig) <- ask
  let reachabilityJarsByProject = configReachabilityJvmOutputs reachabilityConfig
  case (projectResultGraphBreadth projectResult, dpiProjectType dpi) of
    -- if we do not have complete graph, i.e.. missing transitive dependencies
    -- it is impossible to perform reachability, as we may not have all symbols
    -- used in the application to perform accurate analysis
    (Partial, _) -> do
      logInfo . pretty $ "FOSSA CLI does not support reachability analysis, with partial dependencies graph (skipping: " <> displayId <> ")"
      pure . SourceUnitReachabilitySkippedPartialGraph $ dpi
    (Complete, MavenProjectType) -> context "maven" $ do
      analysis <- Diag.errorBoundaryIO . diagToDebug $ case Map.lookup projectPath reachabilityJarsByProject of
        Just jars -> do
          logDebug . pretty $ "Using user-specified jars for maven project: " <> show projectPath
          logDebug . pretty $ "  " <> show jars
          callGraphFromJars jars
        Nothing -> do
          logDebug . pretty $ "Trying to infer build jars from maven project: " <> show (projectResultPath projectResult)
          mavenJarCallGraph (projectResultPath projectResult)
      case analysis of
        Success wg r -> pure $ SourceUnitReachabilityFound dpi (Success wg $ unit{callGraphAnalysis = r})
        Failure wg eg -> pure $ SourceUnitReachabilityFound dpi (Failure wg eg)
    (Complete, GradleProjectType) -> context "gradle" $ do
      analysis <- Diag.errorBoundaryIO . diagToDebug $ case Map.lookup projectPath reachabilityJarsByProject of
        Just jars -> do
          logDebug . pretty $ "Using user-specified jars for gradle project: " <> show projectPath
          logDebug . pretty $ "  " <> show jars
          callGraphFromJars jars
        Nothing -> do
          logDebug . pretty $ "Trying to infer build jars from gradle project: " <> show (projectResultPath projectResult)
          gradleJarCallGraph (projectResultPath projectResult)
      case analysis of
        Success wg r -> pure $ SourceUnitReachabilityFound dpi (Success wg $ unit{callGraphAnalysis = r})
        Failure wg eg -> pure $ SourceUnitReachabilityFound dpi (Failure wg eg)
    -- Exclude units for package manager/language we cannot support yet!
    _ -> do
      -- Update docs: ./docs/features/vuln_reachability.md
      logInfo . pretty $ "FOSSA CLI does not support reachability analysis for: " <> displayId <> " yet. (skipping)"
      pure . SourceUnitReachabilitySkippedNotSupported $ dpi
-- Not possible to perform reachability analysis for projects
-- which were not scanned (skipped due to filter), as we do not
-- complete dependency graph for them
callGraphOf (SkippedDueToProvidedFilter dpi) = pure . SourceUnitReachabilitySkippedMissingDependencyAnalysis $ dpi
callGraphOf (SkippedDueToDefaultFilter dpi) = pure . SourceUnitReachabilitySkippedMissingDependencyAnalysis $ dpi
callGraphOf (Scanned dpi (Failure _ _)) = pure . SourceUnitReachabilitySkippedMissingDependencyAnalysis $ dpi

-- | Unique locators from SourceUnit
dependenciesOf :: SourceUnit -> [Locator]
dependenciesOf srcUnit = maybe [] allLocators (sourceUnitBuild srcUnit)

-- | Unique locators from SourceUnitBuild
allLocators :: SourceUnitBuild -> [Locator]
allLocators unit =
  nub $
    buildImports unit
      ++ concatMap (\ud -> sourceDepLocator ud : sourceDepImports ud) (buildDependencies unit)

onlyFoundUnits :: [SourceUnitReachabilityAttempt] -> [SourceUnitReachability]
onlyFoundUnits = mapMaybe toFoundUnits

toFoundUnits :: SourceUnitReachabilityAttempt -> Maybe SourceUnitReachability
toFoundUnits (SourceUnitReachabilityFound _ (Success _ src)) = Just src
toFoundUnits _ = Nothing
