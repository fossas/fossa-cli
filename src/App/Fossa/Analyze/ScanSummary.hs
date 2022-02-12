{-# LANGUAGE RecordWildCards #-}

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
import Control.Effect.Diagnostics qualified as Diag (Diagnostics)
import Control.Monad (when)
import Data.Foldable (traverse_)
import Data.List (sort)
import Data.Text (Text)
import Diag.Result (EmittedWarn (IgnoredErrGroup), Result (Failure, Success))
import Effect.Logger (
  AnsiStyle,
  Has,
  Logger,
  Pretty (pretty),
  hsep,
  logInfo,
 )
import Path
import Prettyprinter (Doc, annotate, viaShow)
import Prettyprinter.Render.Terminal (Color (Green, Red, Yellow), bold, color)
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

instance Semigroup (ScanCount) where
  (ScanCount l1 l2 l3 l4 l5) <> (ScanCount r1 r2 r3 r4 r5) = ScanCount (l1 + r1) (l2 + r2) (l3 + r3) (l4 + r4) (l5 + r5)

instance Monoid (ScanCount) where
  mempty = ScanCount 0 0 0 0 0

getScanCount :: [DiscoveredProjectScan] -> ScanCount
getScanCount = foldl countOf (ScanCount 0 0 0 0 0)
  where
    countOf :: ScanCount -> DiscoveredProjectScan -> ScanCount
    countOf tsc (SkippedDueToProvidedFilter _) =
      tsc
        { numProjects = numProjects tsc + 1
        , numSkipped = numSkipped tsc + 1
        }
    countOf tsc (SkippedDueToDefaultProductionFilter _) =
      tsc
        { numProjects = numProjects tsc + 1
        , numSkipped = numSkipped tsc + 1
        }
    countOf tsc (Scanned _ (Failure _ _)) =
      tsc
        { numProjects = numProjects tsc + 1
        , numFailed = numFailed tsc + 1
        }
    countOf tsc (Scanned _ (Success wg _)) =
      tsc
        { numProjects = numProjects tsc + 1
        , numSucceeded = numSucceeded tsc + 1
        , numWarnings = numWarnings tsc + countWarnings wg
        }

instance Pretty ScanCount where
  pretty ScanCount{..} =
    hsep
      [ pretty numProjects <> " projects scanned; "
      , pretty numSkipped <> " skipped, "
      , pretty numSucceeded <> " succeeded, "
      , pretty numFailed <> " failed, "
      , pretty numWarnings <> " analysis warnings"
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
  (Has Diag.Diagnostics sig m, Has Logger sig m) =>
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

    -- Additional analysis done outside of standard strategy flow
    summarizeSrcUnit "vsi analysis" Nothing vsi
    summarizeSrcUnit "binary-deps analysis" (Just getBinaryIdentifier) binary
    summarizeSrcUnit "manual and vendor dependencies" Nothing manualDeps

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
  (Has Diag.Diagnostics sig m, Has Logger sig m) =>
  Doc AnsiStyle ->
  Maybe (SourceUnit -> [Text]) ->
  Result (Maybe SourceUnit) ->
  m ()
summarizeSrcUnit analysisHeader maybeGetter (Success wg (Just unit)) = do
  logInfo $ successColorCoded wg $ (listSymbol <> analysisHeader) <> renderSucceeded wg
  case maybeGetter <*> Just unit of
    Just txts -> logInfoVsep $ itemize ("  *" <> listSymbol) viaShow txts
    Nothing -> pure ()
summarizeSrcUnit analysisHeader _ (Failure _ _) = logInfo . failColorCoded $ annotate bold analysisHeader <> renderFailed
summarizeSrcUnit _ _ _ = pure ()

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
    projectTypeDoc = viaShow $ projectTypeToText pt

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
