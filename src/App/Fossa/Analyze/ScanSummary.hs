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
  DiscoveredProjectIdentifier (dpiProjectPath, dpiProjectType),
  DiscoveredProjectScan (..),
 )
import App.Version (fullVersionDescription)
import Control.Carrier.Lift
import Control.Effect.Diagnostics qualified as Diag (Diagnostics)
import Control.Monad (when)
import Data.Foldable (traverse_)
import Data.List (sort)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Diag.Result (EmittedWarn (IgnoredErrGroup), Result (Failure, Success), renderFailure, renderSuccess)
import Effect.Logger (
  AnsiStyle,
  Logger,
  Pretty (pretty),
  hsep,
  logInfo,
 )
import Path
import Path.IO
import Prettyprinter (
  Doc,
  annotate,
  defaultLayoutOptions,
  layoutPretty,
  plural,
  unAnnotate,
  viaShow,
  vsep,
 )
import Prettyprinter.Render.Terminal (Color (Green, Red, Yellow), bold, color, renderStrict)
import Srclib.Types (
  AdditionalDepData (userDefinedDeps),
  SourceUnit (additionalData),
  SourceUserDefDep (srcUserDepName),
 )
import Types (DiscoveredProjectType, projectTypeToText)

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
getScanCount = foldl countOf (ScanCount 0 0 0 0 0)
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
renderScanSummary ::
  (Has Diag.Diagnostics sig m, Has Logger sig m, Has (Lift IO) sig m) =>
  [DiscoveredProjectScan] ->
  -- | Resulted source unit from @analyzeVSI@
  Result (Maybe SourceUnit) ->
  -- | Resulted source unit from @analyzeDiscoverBinaries@
  Result (Maybe SourceUnit) ->
  -- | Resulted source unit from @manualSrcUnits@
  Result (Maybe SourceUnit) ->
  m ()
renderScanSummary dps vsi binary manualDeps = do
  let projects = sort dps -- consistent ordering for repeated analysis
  let totalScanCount =
        mconcat
          [ getScanCount projects
          , srcUnitToScanCount vsi
          , srcUnitToScanCount binary
          , srcUnitToScanCount manualDeps
          ]

  when (numProjects totalScanCount > 0) $ do
    logInfoVsep
      [ ""
      , "Scan Summary"
      , "------------"
      , pretty fullVersionDescription
      , ""
      , pretty totalScanCount
      ]
    logInfo ""
    logInfoVsep $ itemize listSymbol summarizeProjectScan projects
    logInfoVsep $ summarizeSrcUnit "vsi analysis" Nothing vsi
    logInfoVsep $ summarizeSrcUnit "binary-deps analysis" (Just getBinaryIdentifier) binary
    logInfoVsep $ summarizeSrcUnit "fossa-deps analysis" Nothing manualDeps
    logInfo ""
    logInfo "You can pass `--debug` option to show all warning and failure messages."
    scanSummaryLogs <- dumpResultLogsToTempFile projects vsi binary manualDeps
    logInfo . pretty $ "You can also view them at: " <> show scanSummaryLogs
    logInfo "------------"

listSymbol :: Doc AnsiStyle
listSymbol = "* "

itemize :: Doc ann -> (a -> Doc ann) -> [a] -> [Doc ann]
itemize symbol f = map ((symbol <>) . f)

getBinaryIdentifier :: SourceUnit -> [Text]
getBinaryIdentifier srcUnit = maybe [] (srcUserDepName <$>) (userDefinedDeps =<< additionalData srcUnit)

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
    Nothing -> [successColorCoded wg (listSymbol <> analysisHeader <> renderSucceeded wg)]
    Just txts -> map (\i -> successColorCoded wg (listSymbol <> analysisHeader <> pretty (" for " <> i) <> renderSucceeded wg)) txts
summarizeSrcUnit analysisHeader _ (Failure _ _) = [failColorCoded $ annotate bold analysisHeader <> renderFailed]
summarizeSrcUnit _ _ _ = []

summarizeProjectScan :: DiscoveredProjectScan -> Doc AnsiStyle
summarizeProjectScan (Scanned dpi (Failure _ _)) = failColorCoded $ renderDiscoveredProjectIdentifier dpi <> renderFailed
summarizeProjectScan (Scanned _ (Success wg pr)) = successColorCoded wg $ renderProjectResult pr <> renderSucceeded wg
summarizeProjectScan (SkippedDueToProvidedFilter dpi) = renderDiscoveredProjectIdentifier dpi <> renderSkipped
summarizeProjectScan (SkippedDueToDefaultProductionFilter dpi) = renderDiscoveredProjectIdentifier dpi <> renderSkipped

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

renderSkipped :: Doc AnsiStyle
renderSkipped = ": skipped"

renderSucceeded :: [EmittedWarn] -> Doc AnsiStyle
renderSucceeded ew =
  if countWarnings ew == 0
    then ": succeeded"
    else ": succeeded with " <> viaShow (countWarnings ew) <> ": warning"

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

dumpResultLogsToTempFile ::
  (Has (Lift IO) sig m) =>
  [DiscoveredProjectScan] ->
  -- | Resulted source unit from @analyzeVSI@
  Result (Maybe SourceUnit) ->
  -- | Resulted source unit from @analyzeDiscoverBinaries@
  Result (Maybe SourceUnit) ->
  -- | Resulted source unit from @manualSrcUnits@
  Result (Maybe SourceUnit) ->
  m (Path Abs File)
dumpResultLogsToTempFile projects vsi binary manualDeps = do
  let doc =
        renderStrict
          . layoutPretty defaultLayoutOptions
          . unAnnotate
          . mconcat
          $ (mapMaybe renderDiscoveredProjectScanResult projects)
            ++ catMaybes
              [ renderSourceUnit "vsi analysis" vsi
              , renderSourceUnit "binary-deps analysis" binary
              , renderSourceUnit "fossa-deps analysis" manualDeps
              ]

  tmpDir <- sendIO getTempDir
  sendIO $ TIO.writeFile (fromAbsFile $ tmpDir </> scanSummaryFileName) doc
  pure (tmpDir </> scanSummaryFileName)
  where
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
