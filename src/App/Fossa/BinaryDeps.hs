module App.Fossa.BinaryDeps (
  analyzeBinaryDeps,
  analyzeSingleBinary,
) where

import App.Fossa.Analyze.Project (ProjectResult (..))
import App.Fossa.BinaryDeps.Jar (resolveJar)
import App.Fossa.VSI.Fingerprint (Fingerprint, fingerprintRaw)
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, context)
import Control.Effect.Lift (Lift)
import Control.Monad (filterM)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Discovery.Filters (AllFilters (..), combinedPaths)
import Discovery.Walk (WalkStep (WalkContinue), walk')
import Effect.Logger (Logger)
import Effect.ReadFS (ReadFS, contentIsBinary)
import Path (Abs, Dir, File, Path, isProperPrefixOf, (</>))
import Path.Extra (tryMakeRelative)
import Srclib.Converter qualified as Srclib
import Srclib.Types (AdditionalDepData (..), SourceUnit (..), SourceUserDefDep (..))
import Types (DiscoveredProjectType (BinaryDepsProjectType), GraphBreadth (Complete))

-- | Binary detection is sufficiently different from other analysis types that it cannot be just another strategy.
-- Instead, binary detection is run separately over the entire scan directory, outputting its own source unit.
-- The goal of this feature is to enable a FOSSA user to flag all vendored binaries (as defined by git) in the project as dependencies.
-- Users may then use standard FOSSA UX flows to ignore or add license information to the detected binaries.
analyzeBinaryDeps :: (Has (Lift IO) sig m, Has Diagnostics sig m, Has Logger sig m, Has ReadFS sig m) => Path Abs Dir -> AllFilters -> m (Maybe SourceUnit)
analyzeBinaryDeps dir filters = do
  binaryPaths <- findBinaries (toPathFilters dir filters) dir
  if null binaryPaths
    then pure Nothing
    else do
      resolvedBinaries <- traverse (analyzeSingleBinary dir) binaryPaths
      pure . Just $ toSourceUnit (toProject dir) resolvedBinaries

-- | Equivalent to @analyzeBinaryDeps@, but analyzes a specific binary instead of discovering and analyzing all binaries in a directory.
--
-- This analysis uses strategies to attempt to extract some information from the binary, see the internal @strategies@ function for details.
--
-- The @Path Abs Dir@ provided is used to render the name of the resulting dependency:
-- if we fallback to a plain "unknown binary" strategy its name is reported as the relative path between the provided @Path Abs Dir@ and the @Path Abs File@.
-- If the path can't be made relative, the dependency name is the absolute path of the binary.
analyzeSingleBinary :: (Has (Lift IO) sig m, Has Logger sig m, Has ReadFS sig m, Has Diagnostics sig m) => Path Abs Dir -> Path Abs File -> m SourceUserDefDep
analyzeSingleBinary root file = context ("Analyzing " <> toText file) $ resolveBinary strategies root file

findBinaries :: (Has (Lift IO) sig m, Has Diagnostics sig m, Has ReadFS sig m) => PathFilters -> Path Abs Dir -> m [Path Abs File]
findBinaries filters = walk' $ \dir _ files -> do
  if shouldFingerprintDir dir filters
    then do
      binaries <- filterM contentIsBinary files
      pure (binaries, WalkContinue)
    else pure ([], WalkContinue)

-- | PathFilters is a specialized filter mechanism that operates only on absolute directory paths.
data PathFilters = PathFilters
  { include :: [Path Abs Dir]
  , exclude :: [Path Abs Dir]
  }
  deriving (Show)

toPathFilters :: Path Abs Dir -> AllFilters -> PathFilters
toPathFilters root filters =
  PathFilters
    { include = map (root </>) (combinedPaths $ includeFilters filters)
    , exclude = map (root </>) (combinedPaths $ excludeFilters filters)
    }

shouldFingerprintDir :: Path Abs Dir -> PathFilters -> Bool
shouldFingerprintDir dir filters = (not shouldExclude) && shouldInclude
  where
    shouldExclude = (isPrefixedOrEqual dir) `any` (exclude filters)
    shouldInclude = null (include filters) || (isPrefixedOrEqual dir) `any` (include filters)
    isPrefixedOrEqual a b = a == b || isProperPrefixOf b a -- swap order of isProperPrefixOf comparison because we want to know if dir is prefixed by any filter

toProject :: Path Abs Dir -> ProjectResult
toProject dir = ProjectResult BinaryDepsProjectType dir mempty Complete []

toSourceUnit :: ProjectResult -> [SourceUserDefDep] -> SourceUnit
toSourceUnit project deps = do
  let unit = Srclib.projectToSourceUnit False project
  unit{additionalData = Just $ AdditionalDepData (Just deps) Nothing}

-- | Just render the first few characters of the fingerprint.
-- The goal is to provide a high confidence that future binaries with the same name won't collide,
-- and we don't need all 256 bits for that.
renderFingerprint :: Fingerprint t -> Text
renderFingerprint fingerprint = Text.take 12 $ toText fingerprint

-- | Try the next strategy in the list. If successful, evaluate to its result; if not move down the list of strategies and try again.
-- Eventually falls back to strategyRawFingerprint if no other strategy succeeds.
resolveBinary :: (Has (Lift IO) sig m, Has ReadFS sig m, Has Diagnostics sig m) => [(Path Abs Dir -> Path Abs File -> m (Maybe SourceUserDefDep))] -> Path Abs Dir -> Path Abs File -> m SourceUserDefDep
resolveBinary (resolve : remainingStrategies) = \root file -> do
  result <- resolve root file
  case result of
    Just r -> pure r
    Nothing -> resolveBinary remainingStrategies root file
resolveBinary [] = strategyRawFingerprint

-- | Functions which may be able to resolve a binary to a dependency.
strategies :: (Has (Lift IO) sig m, Has Diagnostics sig m, Has Logger sig m, Has ReadFS sig m) => [(Path Abs Dir -> Path Abs File -> m (Maybe SourceUserDefDep))]
strategies =
  [resolveJar]

-- | Fallback strategy: resolve to a user defined dependency for the binary, where the name is the relative path and the version is the fingerprint.
-- This strategy is used if no other strategy succeeds at resolving the binary.
strategyRawFingerprint :: (Has ReadFS sig m, Has (Lift IO) sig m, Has Diagnostics sig m) => Path Abs Dir -> Path Abs File -> m SourceUserDefDep
strategyRawFingerprint root file = do
  fp <- fingerprintRaw file
  let rel = tryMakeRelative root file
  pure $ SourceUserDefDep (toText rel) (renderFingerprint fp) "" (Just "Binary discovered in source tree") Nothing (Just rel)
