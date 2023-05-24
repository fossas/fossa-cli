{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.Analyze.ScanSummary (
  renderScanSummary,
) where

import App.Fossa.Analyze.Project (
  ProjectResult (projectResultPath),
  projectResultType,
 )
import App.Fossa.Analyze.Types (
  AnalysisScanResult (AnalysisScanResult),
  DiscoveredProjectIdentifier (dpiProjectPath, dpiProjectType),
  DiscoveredProjectScan (..),
 )
import App.Version (fullVersionDescription)
import Control.Carrier.Lift
import Control.Effect.Diagnostics qualified as Diag (Diagnostics)
import Control.Monad (when)
import Data.Foldable (foldl', traverse_)
import Data.List (sort)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe, maybeToList)
import Data.Monoid.Extra (isMempty)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Diag.Result (EmittedWarn (IgnoredErrGroup), Result (Failure, Success), renderFailure, renderSuccess)
import Discovery.Filters (AllFilters)
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
  annotate,
  defaultLayoutOptions,
  indent,
  layoutPretty,
  plural,
  unAnnotate,
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
  SourceUnit (additionalData, sourceUnitBuild),
  SourceUnitBuild (buildDependencies),
  SourceUnitDependency (sourceDepLocator),
  SourceUserDefDep (srcUserDepName),
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
    countOf tsc (SkippedDueToDefaultProductionFilter _) = tsc{numProjects = numProjects tsc + 1, numSkipped = numSkipped tsc + 1}
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
renderScanSummary :: (Has Diag.Diagnostics sig m, Has Logger sig m, Has (Lift IO) sig m) => Severity -> (Maybe Text) -> AnalysisScanResult -> AllFilters -> m ()
renderScanSummary severity maybeEndpointVersion analysisResults filters = do
  let endpointVersion = fromMaybe "N/A" maybeEndpointVersion

  case summarize endpointVersion analysisResults of
    Nothing -> pure ()
    Just summary -> do
      let someFilters = isMempty filters
      logInfoVsep summary
      when someFilters $ do
        logInfoVsep $
          map
            (indent 2)
            [ "Some projects may not appear in the summary if they were filtered during discovery."
            , "You can run `fossa list-targets` to see all discoverable projects."
            ]
      logInfo ""
      when (severity /= SevDebug) $ do
        logInfo "You can pass `--debug` option to eagerly show all warning and failure messages."

      summaryWithWarnErrorsTmpFile <- dumpResultLogsToTempFile endpointVersion analysisResults
      logInfo . pretty $ "You can also view analysis summary with warning and error messages at: " <> show summaryWithWarnErrorsTmpFile
      logInfo "------------"

summarize :: Text -> AnalysisScanResult -> Maybe ([Doc AnsiStyle])
summarize endpointVersion (AnalysisScanResult dps vsi binary manualDeps dynamicLinkingDeps) =
  if (numProjects totalScanCount <= 0)
    then Nothing
    else
      Just $
        [ ""
        , "Scan Summary"
        , "------------"
        , pretty fullVersionDescription
        , pretty $ "fossa endpoint server version: " <> endpointVersion
        , ""
        , pretty totalScanCount
        , ""
        ]
          <> itemize listSymbol summarizeProjectScan projects
          <> ["-"]
          <> summarizeSrcUnit "vsi analysis" Nothing vsi
          <> summarizeSrcUnit "binary-deps analysis" (Just getBinaryIdentifier) binary
          <> summarizeSrcUnit "dynamic linked dependency analysis" (Just getBinaryIdentifier) dynamicLinkingDeps
          <> summarizeSrcUnit "fossa-deps file analysis" (Just getManualVendorDepsIdentifier) manualDeps
          <> [""]
  where
    projects = sort dps
    totalScanCount =
      mconcat
        [ getScanCount projects
        , srcUnitToScanCount vsi
        , srcUnitToScanCount binary
        , srcUnitToScanCount manualDeps
        , srcUnitToScanCount dynamicLinkingDeps
        ]

listSymbol :: Doc AnsiStyle
listSymbol = "* "

itemize :: Doc ann -> (a -> Doc ann) -> [a] -> [Doc ann]
itemize symbol f = map ((symbol <>) . f)

getBinaryIdentifier :: SourceUnit -> [Text]
getBinaryIdentifier srcUnit = maybe [] (srcUserDepName <$>) (userDefinedDeps =<< additionalData srcUnit)

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

srcUnitToScanCount :: Result (Maybe SourceUnit) -> ScanCount
srcUnitToScanCount (Failure _ _) = ScanCount 1 0 0 1 0
srcUnitToScanCount (Success _ Nothing) = ScanCount 0 0 0 0 0
srcUnitToScanCount (Success wg (Just _)) = ScanCount 1 0 1 0 (countWarnings wg)

summarizeSrcUnit ::
  Doc AnsiStyle ->
  Maybe (SourceUnit -> [Text]) ->
  Result (Maybe SourceUnit) ->
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
summarizeProjectScan (SkippedDueToDefaultProductionFilter dpi) = renderDiscoveredProjectIdentifier dpi <> skippedDueNonProductionPathFiltering

---------- Rendering Helpers

logInfoVsep :: (Has Logger sig m) => [Doc AnsiStyle] -> m ()
logInfoVsep = traverse_ logInfo

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

skippedDueNonProductionPathFiltering :: Doc AnsiStyle
skippedDueNonProductionPathFiltering = ": skipped (non-production path filtering)"

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

dumpResultLogsToTempFile :: (Has (Lift IO) sig m) => Text -> AnalysisScanResult -> m (Path Abs File)
dumpResultLogsToTempFile endpointVersion (AnalysisScanResult projects vsi binary manualDeps dynamicLinkingDeps) = do
  let doc =
        renderStrict
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
              ]

  tmpDir <- sendIO getTempDir
  sendIO $ TIO.writeFile (fromAbsFile $ tmpDir </> scanSummaryFileName) doc
  pure (tmpDir </> scanSummaryFileName)
  where
    scanSummary :: [Doc AnsiStyle]
    scanSummary = maybeToList (vsep <$> summarize endpointVersion (AnalysisScanResult projects vsi binary manualDeps dynamicLinkingDeps))

    renderSourceUnit :: Doc AnsiStyle -> Result (Maybe SourceUnit) -> Maybe (Doc AnsiStyle)
    renderSourceUnit header (Failure ws eg) = Just $ renderFailure ws eg $ vsep $ summarizeSrcUnit header Nothing (Failure ws eg)
    renderSourceUnit header (Success ws (Just res)) = renderSuccess ws $ vsep $ summarizeSrcUnit header Nothing (Success ws (Just res))
    renderSourceUnit _ _ = Nothing

    renderDiscoveredProjectScanResult :: DiscoveredProjectScan -> Maybe (Doc AnsiStyle)
    renderDiscoveredProjectScanResult (Scanned dpi (Failure ws eg)) = Just $ renderFailure ws eg $ summarizeProjectScan (Scanned dpi (Failure ws eg))
    renderDiscoveredProjectScanResult (Scanned dpi (Success ws res)) = renderSuccess ws $ summarizeProjectScan (Scanned dpi (Success ws res))
    renderDiscoveredProjectScanResult _ = Nothing

scanSummaryFileName :: Path Rel File
scanSummaryFileName = $(mkRelFile "fossa-analyze-scan-summary.txt")
