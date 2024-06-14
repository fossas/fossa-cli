{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.Analyze.ScanSummary (
  renderScanSummary,
) where

import App.Docs (fossaAnalyzeDefaultFilterDocUrl, staticAndDynamicStrategies)
import App.Fossa.Analyze.Project (
  ProjectResult (projectResultPath),
  projectResultType,
 )
import App.Fossa.Analyze.Types (
  AnalysisScanResult (AnalysisScanResult),
  DiscoveredProjectIdentifier (dpiProjectPath, dpiProjectType),
  DiscoveredProjectScan (..),
  SourceUnitReachabilityAttempt (..),
 )
import App.Fossa.Config.Analyze (AnalysisTacticTypes (StaticOnly))
import App.Fossa.Config.Analyze qualified as Config
import App.Fossa.Lernie.Types (LernieMatch (..), LernieMatchData (..), LernieResults (..))
import App.Fossa.Reachability.Types (SourceUnitReachability (..))
import App.Version (fullVersionDescription)
import Control.Carrier.Lift
import Control.Effect.Diagnostics qualified as Diag (Diagnostics)
import Control.Monad (join)
import Data.Flag (fromFlag)
import Data.Foldable (foldl', traverse_)
import Data.Functor.Extra ((<$$>))
import Data.List (sort)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe, maybeToList)
import Data.Monoid.Extra (isMempty)
import Data.String.AnsiEscapeCodes.Strip.Text (stripAnsiEscapeCodes)
import Data.String.Conversion (showText, toText)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Diag.Result (EmittedWarn (IgnoredErrGroup), Result (Failure, Success), renderFailure, renderSuccess)
import Effect.Logger (
  AnsiStyle,
  Logger,
  Pretty (pretty),
  Severity (SevDebug),
  hsep,
  logInfo,
 )
import Path (
  Abs,
  Dir,
  File,
  Path,
  Rel,
  fromAbsFile,
  mkRelFile,
  (</>),
 )
import Path.IO
import Prettyprinter (
  Doc,
  align,
  annotate,
  defaultLayoutOptions,
  layoutPretty,
  line,
  plural,
  unAnnotate,
  vcat,
  viaShow,
  vsep,
 )
import Prettyprinter.Render.Terminal (
  Color (Green, Red, Yellow),
  bold,
  color,
  renderStrict,
 )
import Srclib.Converter (depTypeToFetcher)
import Srclib.Types (
  AdditionalDepData (remoteDeps, userDefinedDeps),
  Locator (locatorFetcher, locatorProject),
  SourceRemoteDep (srcRemoteDepName),
  SourceUnit (SourceUnit, additionalData, sourceUnitBuild),
  SourceUnitBuild (..),
  SourceUnitDependency (sourceDepLocator),
  SourceUserDefDep (srcUserDepName),
  renderLocator,
  sourceUnitOriginPaths,
 )
import Types (DepType (ArchiveType), DiscoveredProjectType, projectTypeToText)

data ScanCount = ScanCount
  { numProjects :: Int
  , numSkipped :: Int
  , numSucceeded :: Int
  , numFailed :: Int
  , numWarnings :: Int
  }
  deriving (Show, Eq, Ord)

instance Semigroup ScanCount where
  (ScanCount l1 l2 l3 l4 l5) <> (ScanCount r1 r2 r3 r4 r5) = ScanCount (l1 + r1) (l2 + r2) (l3 + r3) (l4 + r4) (l5 + r5)

instance Monoid ScanCount where
  mempty = ScanCount 0 0 0 0 0

getScanCount :: [DiscoveredProjectScan] -> ScanCount
getScanCount = foldl' countOf (ScanCount 0 0 0 0 0)
  where
    countOf :: ScanCount -> DiscoveredProjectScan -> ScanCount
    countOf tsc (SkippedDueToProvidedFilter _) = tsc{numProjects = numProjects tsc + 1, numSkipped = numSkipped tsc + 1}
    countOf tsc (SkippedDueToDefaultFilter _) = tsc{numProjects = numProjects tsc + 1, numSkipped = numSkipped tsc + 1}
    countOf tsc (Scanned _ (Failure _ _)) = tsc{numProjects = numProjects tsc + 1, numFailed = numFailed tsc + 1}
    countOf tsc (Scanned _ (Success wg _)) = tsc{numProjects = numProjects tsc + 1, numSucceeded = numSucceeded tsc + 1, numWarnings = numWarnings tsc + countWarnings wg}

instance Pretty ScanCount where
  pretty ScanCount{..} =
    hsep
      [ pretty numProjects <> " projects scanned; "
      , pretty numSkipped <> " skipped, "
      , pretty numSucceeded <> " succeeded, "
      , pretty numFailed <> " failed, "
      , pretty numWarnings <> " analysis " <> plural "warning" "warnings" numWarnings
      ]

staticOnlyAnalysisMessage :: Doc a
staticOnlyAnalysisMessage =
  vcat
    [ "FOSSA CLI was constrained to static-only analysis."
    , "Results may be different in comparison to an analysis using build tools."
    , "More information: " <> pretty staticAndDynamicStrategies
    ]

-- | Renders Analysis Scan Summary with `ServInfo` severity.
--
-- It renders summary of all scanned projects (including skipped projects due to filters),
-- and it's status - failed, successful, successful with warning count,
-- if project count is greater than 0, otherwise it does nothing.
--
-- ### Example
--
-- Scan Summary
-- - - - - - - -
-- Using fossa-cli: `v3.0.0`
--
-- 4 projects scanned;  2 skipped,  2 succeeded,  0 failed,  0 analysis warnings
--
-- * __poetry__ project in path: succeeded
-- * __poetry__ project in path: succeeded
-- * __fpm__ project in path: skipped
-- * __setuptools__ project in path: skipped
renderScanSummary :: (Has Diag.Diagnostics sig m, Has Logger sig m, Has (Lift IO) sig m) => Severity -> (Maybe Text) -> AnalysisScanResult -> Config.AnalyzeConfig -> m ()
renderScanSummary severity maybeEndpointVersion analysisResults cfg = do
  let endpointVersion = fromMaybe "N/A" maybeEndpointVersion
  let hasDefaultFilters = (not $ fromFlag Config.WithoutDefaultFilters $ Config.withoutDefaultFilters cfg)

  case summarize cfg endpointVersion analysisResults of
    Nothing -> pure ()
    Just summary -> do
      let someFilters = isMempty cfg.filterSet
      logInfoVsep summary
      logInfo "Notes"
      logInfo "-----"
      logInfoVsep $ renderNotes someFilters hasDefaultFilters (severity /= SevDebug)

      summaryWithWarnErrorsTmpFile <- dumpResultLogsToTempFile cfg endpointVersion analysisResults
      logInfo . pretty $ "You can also view analysis summary with warning and error messages at: " <> show summaryWithWarnErrorsTmpFile
      logInfo "------------"

renderNotes :: Bool -> Bool -> Bool -> [Doc AnsiStyle]
renderNotes hasUserProvidedFilers hasDefaultFilters isNotDebugMode =
  (if hasUserProvidedFilers then renderUserProvidedSkippedTargetHelp else [])
    <> (if hasDefaultFilters then renderDefaultSkippedTargetHelp else [])
    <> (if isNotDebugMode then renderDebugOptionsHelp else [])

renderDebugOptionsHelp :: [Doc AnsiStyle]
renderDebugOptionsHelp =
  [ "* You can pass `--debug` option to eagerly show all warning and failure messages."
  , ""
  ]

renderUserProvidedSkippedTargetHelp :: [Doc AnsiStyle]
renderUserProvidedSkippedTargetHelp =
  [ "* Some projects may not appear in the summary if they were filtered during discovery."
  , "You can run `fossa list-targets` to see all discoverable projects."
  , ""
  ]

renderDefaultSkippedTargetHelp :: [Doc AnsiStyle]
renderDefaultSkippedTargetHelp =
  [ "* Some projects analysis may be skipped, due to default filters."
  , "Learn more: " <> pretty fossaAnalyzeDefaultFilterDocUrl
  , ""
  ]

summarize :: Config.AnalyzeConfig -> Text -> AnalysisScanResult -> Maybe ([Doc AnsiStyle])
summarize cfg endpointVersion (AnalysisScanResult dps vsi binary manualDeps dynamicLinkingDeps lernie reachabilityAttempts) =
  if (numProjects totalScanCount <= 0)
    then Nothing
    else
      Just $
        [ ""
        , "Scan Summary"
        , "------------"
        , pretty fullVersionDescription
        , pretty $ "fossa endpoint server version: " <> endpointVersion
        , if (cfg.allowedTacticTypes == StaticOnly)
            then align $ line <> staticOnlyAnalysisMessage <> line
            else ""
        , pretty totalScanCount
        , ""
        ]
          <> itemize listSymbol summarizeProjectScan projects
          <> ["-"]
          <> vsiResults
          <> summarizeSrcUnit "binary-deps analysis" (Just getBinaryIdentifier) binary
          <> summarizeSrcUnit "dynamic linked dependency analysis" (Just getBinaryIdentifier) dynamicLinkingDeps
          <> summarizeSrcUnit "fossa-deps file analysis" (Just getManualVendorDepsIdentifier) manualDeps
          <> summarizeSrcUnit "Keyword Search" (Just getLernieIdentifier) (lernieResultsKeywordSearches <$$> lernie)
          <> summarizeSrcUnit "Custom-License Search" (Just getLernieIdentifier) (lernieResultsCustomLicenses <$$> lernie)
          <> reachabilitySummary
          <> [""]
  where
    reachabilitySummary =
      if null reachabilityAttempts
        then
          []
        else summarizeReachability "Reachability analysis" reachabilityAttempts
    vsiResults = summarizeSrcUnit "vsi analysis" (Just (join . map vsiSourceUnits)) vsi
    projects = sort dps
    totalScanCount =
      mconcat
        [ getScanCount projects
        , vsiSrcUnitsToScanCount vsi
        , srcUnitToScanCount binary
        , srcUnitToScanCount manualDeps
        , srcUnitToScanCount dynamicLinkingDeps
        , srcUnitToScanCount lernie
        ]

    -- This function relies on the fact that there is only ever one package in a vsi source unit dep graph.
    -- It is not generally usable forall all SourceUnits.
    vsiSourceUnits :: SourceUnit -> [Text]
    vsiSourceUnits sUnit =
      let renderOriginPath =
            case vsiSrcUnitLocator sUnit of
              Just loc -> \originPath -> (toText originPath) <> " (locator: " <> renderLocator loc <> ")"
              _ -> toText
       in map renderOriginPath (sourceUnitOriginPaths sUnit)

    vsiSrcUnitLocator :: SourceUnit -> Maybe Locator
    vsiSrcUnitLocator SourceUnit{sourceUnitBuild = Just SourceUnitBuild{buildImports = [locator]}} = Just locator
    vsiSrcUnitLocator _ = Nothing

listSymbol :: Doc AnsiStyle
listSymbol = "* "

itemize :: Doc ann -> (a -> Doc ann) -> [a] -> [Doc ann]
itemize symbol f = map ((symbol <>) . f)

getBinaryIdentifier :: SourceUnit -> [Text]
getBinaryIdentifier srcUnit = maybe [] (srcUserDepName <$>) (userDefinedDeps =<< additionalData srcUnit)

-- A LernieMatch has many LernieMatchData. getLernieIdentifier creates one line of output per LernieMatchData.
-- Example output:
-- Proprietary License - /Users/scott/fossa/license-scan-dirs/grepper/one.txt (lines 1-1)
-- <name of regex from config> - <path to file> (lines <startLine>-<endLine>)
getLernieIdentifier :: [LernieMatch] -> [Text]
getLernieIdentifier [] = ["No results found"]
getLernieIdentifier matches = concatMap renderLernieMatch matches
  where
    renderLernieMatch :: LernieMatch -> [Text]
    renderLernieMatch LernieMatch{..} = map (renderLernieMatchData lernieMatchPath) lernieMatchMatches

    renderLernieMatchData :: Text -> LernieMatchData -> Text
    renderLernieMatchData path LernieMatchData{..} = lernieMatchDataName <> " - " <> path <> " (lines " <> showText lernieMatchDataStartLine <> "-" <> showText lernieMatchDataEndLine <> ")"

getManualVendorDepsIdentifier :: SourceUnit -> [Text]
getManualVendorDepsIdentifier srcUnit = refDeps ++ foundRemoteDeps ++ customDeps ++ vendorDeps
  where
    vendorDeps :: [Text]
    vendorDeps =
      withPostfix "vendor" $
        map (locatorProject) $
          filter (\l -> locatorFetcher l == depTypeToFetcher ArchiveType) allBuildDeps

    refDeps :: [Text]
    refDeps =
      withPostfix "reference" $
        map (locatorProject) $
          filter (\l -> locatorFetcher l /= depTypeToFetcher ArchiveType) allBuildDeps

    allBuildDeps :: [Locator]
    allBuildDeps = maybe [] (map sourceDepLocator . buildDependencies) (sourceUnitBuild srcUnit)

    customDeps :: [Text]
    customDeps =
      withPostfix "custom" $
        maybe [] (srcUserDepName <$>) (userDefinedDeps =<< additionalData srcUnit)

    foundRemoteDeps :: [Text]
    foundRemoteDeps =
      withPostfix "remote" $
        maybe [] (srcRemoteDepName <$>) (remoteDeps =<< additionalData srcUnit)

    withPostfix :: Text -> [Text] -> [Text]
    withPostfix bracketText = map (<> " (" <> bracketText <> ")")

vsiSrcUnitsToScanCount :: Result (Maybe [SourceUnit]) -> ScanCount
vsiSrcUnitsToScanCount (Failure _ _) = ScanCount 0 0 0 0 0
vsiSrcUnitsToScanCount (Success wg (Just units)) =
  let unitLen = length units
   in ScanCount unitLen 0 unitLen 0 (length wg)
vsiSrcUnitsToScanCount (Success _ Nothing) = ScanCount 0 0 0 0 0

srcUnitToScanCount :: Result (Maybe a) -> ScanCount
srcUnitToScanCount (Failure _ _) = ScanCount 1 0 0 1 0
srcUnitToScanCount (Success _ Nothing) = ScanCount 0 0 0 0 0
srcUnitToScanCount (Success wg (Just _)) = ScanCount 1 0 1 0 (countWarnings wg)

summarizeSrcUnit ::
  Doc AnsiStyle ->
  Maybe (a -> [Text]) ->
  Result (Maybe a) ->
  [Doc AnsiStyle]
summarizeSrcUnit analysisHeader maybeGetter (Success wg (Just unit)) =
  case maybeGetter <*> Just unit of
    Just txts ->
      [successColorCoded wg $ (listSymbol <> analysisHeader) <> renderSucceeded wg]
        <> itemize ("  *" <> listSymbol) pretty txts
    Nothing -> [successColorCoded wg (listSymbol <> analysisHeader <> renderSucceeded wg)]
summarizeSrcUnit analysisHeader _ (Failure _ _) = [failColorCoded $ annotate bold $ listSymbol <> analysisHeader <> renderFailed]
summarizeSrcUnit _ _ _ = []

summarizeProjectScan :: DiscoveredProjectScan -> Doc AnsiStyle
summarizeProjectScan (Scanned dpi (Failure _ _)) = failColorCoded $ renderDiscoveredProjectIdentifier dpi <> renderFailed
summarizeProjectScan (Scanned _ (Success wg pr)) = successColorCoded wg $ renderProjectResult pr <> renderSucceeded wg
summarizeProjectScan (SkippedDueToProvidedFilter dpi) = renderDiscoveredProjectIdentifier dpi <> skippedDueFilter
summarizeProjectScan (SkippedDueToDefaultFilter dpi) = renderDiscoveredProjectIdentifier dpi <> skippedDueDefaultFilter

summarizeReachability ::
  Doc AnsiStyle ->
  [SourceUnitReachabilityAttempt] ->
  [Doc AnsiStyle]
summarizeReachability analysisHeader unitAttempts =
  [analysisHeader] <> itemize ("  *" <> listSymbol) summarizeReachabilityAttempt unitAttempts

summarizeReachabilityAttempt :: SourceUnitReachabilityAttempt -> Doc AnsiStyle
summarizeReachabilityAttempt (SourceUnitReachabilityFound _ (Success wg unit)) = successColorCoded wg $ renderReachabilityResult unit <> renderSucceeded wg
summarizeReachabilityAttempt (SourceUnitReachabilityFound dpi (Failure _ _)) = failColorCoded $ renderDiscoveredProjectIdentifier dpi <> renderFailed
summarizeReachabilityAttempt (SourceUnitReachabilitySkippedPartialGraph dpi) = renderDiscoveredProjectIdentifier dpi <> skippedReachabilityDueToPartialGraph
summarizeReachabilityAttempt (SourceUnitReachabilitySkippedNotSupported dpi) = renderDiscoveredProjectIdentifier dpi <> skippedReachabilityDueToNotSupported
summarizeReachabilityAttempt (SourceUnitReachabilitySkippedMissingDependencyAnalysis dpi) = renderDiscoveredProjectIdentifier dpi <> skippedReachabilityDueToMissingAnalysis

---------- Rendering Helpers

logInfoVsep :: (Has Logger sig m) => [Doc AnsiStyle] -> m ()
logInfoVsep = traverse_ logInfo

renderReachabilityResult :: SourceUnitReachability -> Doc AnsiStyle
renderReachabilityResult unit = annotate bold projectTypeDoc <> pathDoc
  where
    pathDoc = " project in " <> viaShow (srcUnitManifest unit)
    projectTypeDoc = pretty $ srcUnitType unit

renderDiscoveredProjectIdentifier :: DiscoveredProjectIdentifier -> Doc AnsiStyle
renderDiscoveredProjectIdentifier dpi = renderProjectPathAndType (dpiProjectType dpi) (dpiProjectPath dpi)

renderProjectResult :: ProjectResult -> Doc AnsiStyle
renderProjectResult pr = renderProjectPathAndType (projectResultType pr) (projectResultPath pr)

renderProjectPathAndType :: DiscoveredProjectType -> Path Abs Dir -> Doc AnsiStyle
renderProjectPathAndType pt path = annotate bold projectTypeDoc <> pathDoc
  where
    pathDoc = " project in " <> viaShow path
    projectTypeDoc = pretty $ projectTypeToText pt

successColorCoded :: [EmittedWarn] -> Doc AnsiStyle -> Doc AnsiStyle
successColorCoded ew =
  if countWarnings ew == 0
    then annotate $ color Green
    else annotate $ color Yellow

failColorCoded :: Doc AnsiStyle -> Doc AnsiStyle
failColorCoded = annotate $ color Red

skippedDueFilter :: Doc AnsiStyle
skippedDueFilter = ": skipped (exclusion filters)"

skippedDueDefaultFilter :: Doc AnsiStyle
skippedDueDefaultFilter = ": skipped (default filters)"

skippedReachabilityDueToPartialGraph :: Doc AnsiStyle
skippedReachabilityDueToPartialGraph = ": skipped (partial graph)"

skippedReachabilityDueToNotSupported :: Doc AnsiStyle
skippedReachabilityDueToNotSupported = ": skipped (not supported)"

skippedReachabilityDueToMissingAnalysis :: Doc AnsiStyle
skippedReachabilityDueToMissingAnalysis = ": skipped (no dependency analysis)"

renderSucceeded :: [EmittedWarn] -> Doc AnsiStyle
renderSucceeded ew =
  if countWarnings ew == 0
    then ": succeeded"
    else ": succeeded with " <> viaShow (countWarnings ew) <> plural " warning" " warnings" numWarns
  where
    numWarns :: Int
    numWarns = countWarnings ew

renderFailed :: Doc AnsiStyle
renderFailed = ": failed"

-- | Counts number of displayed warning.
-- It ignores warning aggregated under @IgnoredErrGroup@.
-- We do this to ensure warning count is consistent with rendered warnings.
countWarnings :: [EmittedWarn] -> Int
countWarnings ws =
  case notIgnoredErrs of
    [] -> 0
    ws' -> length ws'
  where
    notIgnoredErrs :: [EmittedWarn]
    notIgnoredErrs = filter (not . isIgnoredErrGroup) ws

    isIgnoredErrGroup :: EmittedWarn -> Bool
    isIgnoredErrGroup IgnoredErrGroup{} = True
    isIgnoredErrGroup _ = False

dumpResultLogsToTempFile :: (Has (Lift IO) sig m) => Config.AnalyzeConfig -> Text -> AnalysisScanResult -> m (Path Abs File)
dumpResultLogsToTempFile cfg endpointVersion (AnalysisScanResult projects vsi binary manualDeps dynamicLinkingDeps lernieResults reachabilityAttempts) = do
  let doc =
        stripAnsiEscapeCodes
          . renderStrict
          . layoutPretty defaultLayoutOptions
          . unAnnotate
          . mconcat
          $ scanSummary
            ++ (mapMaybe renderDiscoveredProjectScanResult (sort projects))
            ++ catMaybes
              [ renderSourceUnit "vsi analysis" vsi
              , renderSourceUnit "binary-deps analysis" binary
              , renderSourceUnit "dynamic linked dependency analysis" dynamicLinkingDeps
              , renderSourceUnit "fossa-deps analysis" manualDeps
              , renderSourceUnit "Custom-license scan & Keyword Search" lernieResults
              ]
            ++ renderReachability

  tmpDir <- sendIO getTempDir
  sendIO $ TIO.writeFile (fromAbsFile $ tmpDir </> scanSummaryFileName) doc
  pure (tmpDir </> scanSummaryFileName)
  where
    scanSummary :: [Doc AnsiStyle]
    scanSummary = maybeToList (vsep <$> summarize cfg endpointVersion (AnalysisScanResult projects vsi binary manualDeps dynamicLinkingDeps lernieResults reachabilityAttempts))

    renderSourceUnit :: Doc AnsiStyle -> Result (Maybe a) -> Maybe (Doc AnsiStyle)
    renderSourceUnit header (Failure ws eg) = Just $ renderFailure ws eg $ vsep $ summarizeSrcUnit header Nothing (Failure ws eg)
    renderSourceUnit header success@(Success ws (Just _)) = renderSuccess ws $ vsep $ summarizeSrcUnit header Nothing success
    renderSourceUnit _ _ = Nothing

    renderDiscoveredProjectScanResult :: DiscoveredProjectScan -> Maybe (Doc AnsiStyle)
    renderDiscoveredProjectScanResult (Scanned dpi (Failure ws eg)) = Just $ renderFailure ws eg $ summarizeProjectScan (Scanned dpi (Failure ws eg))
    renderDiscoveredProjectScanResult (Scanned dpi (Success ws res)) = renderSuccess ws $ summarizeProjectScan (Scanned dpi (Success ws res))
    renderDiscoveredProjectScanResult _ = Nothing

    renderReachability :: [Doc AnsiStyle]
    renderReachability = ["Reachability"] <> (mapMaybe renderReachabilityDetailed reachabilityAttempts)

    renderReachabilityDetailed :: SourceUnitReachabilityAttempt -> Maybe (Doc AnsiStyle)
    renderReachabilityDetailed (SourceUnitReachabilityFound _ (Success wg unit)) = renderSuccess wg $ renderReachabilityResult unit <> renderSucceeded wg
    renderReachabilityDetailed (SourceUnitReachabilityFound dpi (Failure ws eg)) = Just $ renderFailure ws eg $ renderDiscoveredProjectIdentifier dpi <> renderFailed
    renderReachabilityDetailed res = Just $ summarizeReachabilityAttempt res

scanSummaryFileName :: Path Rel File
scanSummaryFileName = $(mkRelFile "fossa-analyze-scan-summary.txt")
