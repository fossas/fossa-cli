module App.Fossa.Reachability.Upload (
  analyzeForReachability,
  reachabilityRawJson,
  reachabilityEndpointJson,
  upload,
  dependenciesOf,
  callGraphOf,
) where

import App.Fossa.Analyze.Project (ProjectResult (..))
import App.Fossa.Analyze.Types (
  AnalysisScanResult (..),
  DiscoveredProjectScan (..),
  dpiProjectType,
 )
import App.Fossa.Reachability.Gradle (gradleJarCallGraph)
import App.Fossa.Reachability.Maven (mavenJarCallGraph)
import App.Fossa.Reachability.Types (
  CallGraphAnalysis (..),
  ContentRef (..),
  ParsedJar (..),
  SourceUnitReachability (..),
 )
import App.Types (ProjectMetadata, ProjectRevision)
import Control.Algebra (Has)
import Control.Effect.Debug (Debug, debugMetadata)
import Control.Effect.Diagnostics (Diagnostics, context)
import Control.Effect.FossaApiClient (FossaApiClient, uploadBuildForReachability, uploadContentForReachability)
import Control.Effect.Lift (Lift)
import Data.List (nub)
import Data.Maybe (catMaybes)
import Data.Text (Text)
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
  ) =>
  AnalysisScanResult ->
  m [SourceUnitReachability]
analyzeForReachability analysisResult = context "reachability" $ do
  let analyzerResult = analyzersScanResult analysisResult
  units <- catMaybes <$> (traverse callGraphOf analyzerResult)
  debugMetadata reachabilityRawJson units
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
  ( Has FossaApiClient sig m
  ) =>
  SourceUnitReachability ->
  m SourceUnitReachability
uploadReachability unit = case callGraphAnalysis unit of
  NoCallGraphAnalysis -> pure unit
  JarAnalysis [] -> pure unit
  JarAnalysis someJars -> do
    updatedJars <- traverse uploadJarAnalysis someJars
    pure $ unit{callGraphAnalysis = JarAnalysis updatedJars}

uploadJarAnalysis ::
  ( Has FossaApiClient sig m
  ) =>
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
  ) =>
  DiscoveredProjectScan ->
  m (Maybe SourceUnitReachability)
callGraphOf (Scanned dpi (Success _ projectResult)) = do
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
  case (projectResultGraphBreadth projectResult, dpiProjectType dpi) of
    -- if we do not have complete graph, i.e.. missing transitive dependencies
    -- it is impossible to perform reachability, as we may not have all symbols
    -- used in the application to perform accurate analysis
    (Partial, _) -> do
      logInfo . pretty $ "FOSSA CLI does not support reachability analysis, with partial dependencies graph (skipping: " <> displayId <> ")"
      pure Nothing
    (Complete, MavenProjectType) -> context "maven" $ do
      logDebug . pretty $ "Trying to infer build jars from maven project: " <> show (projectResultPath projectResult)
      analysis <- mavenJarCallGraph (projectResultPath projectResult)
      pure . Just $ unit{callGraphAnalysis = analysis}
    (Complete, GradleProjectType) -> context "gradle" $ do
      logDebug . pretty $ "Trying to infer build jars from gradle project: " <> show (projectResultPath projectResult)
      analysis <- gradleJarCallGraph (projectResultPath projectResult)
      pure . Just $ unit{callGraphAnalysis = analysis}

    -- Exclude units for package manager/language we cannot support yet!
    _ -> do
      logInfo . pretty $ "FOSSA CLI does not support reachability analysis for: " <> displayId <> " yet. (skipping)"
      pure Nothing
-- Not possible to perform reachability analysis for projects
-- which were not scanned (skipped due to filter), as we do not
-- complete dependency graph for them
callGraphOf _ = pure Nothing

-- | Unique locators from SourceUnit
dependenciesOf :: SourceUnit -> [Locator]
dependenciesOf srcUnit = maybe [] allLocators (sourceUnitBuild srcUnit)

-- | Unique locators from SourceUnitBuild
allLocators :: SourceUnitBuild -> [Locator]
allLocators unit =
  nub $
    buildImports unit
      ++ concatMap (\ud -> sourceDepLocator ud : sourceDepImports ud) (buildDependencies unit)

reachabilityRawJson :: Text
reachabilityRawJson = "reachability.raw.json"

reachabilityEndpointJson :: Text
reachabilityEndpointJson = "reachability.endpoint.json"
