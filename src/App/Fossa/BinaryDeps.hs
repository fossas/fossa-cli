{-# LANGUAGE RecordWildCards #-}

module App.Fossa.BinaryDeps (analyzeBinaryDeps) where

import App.Fossa.Analyze.Project (ProjectResult (..))
import App.Fossa.VSI.IAT.Fingerprint (fingerprintRaw)
import App.Fossa.VSI.IAT.Types (Fingerprint (..))
import Control.Algebra (Has)
import Control.Carrier.Diagnostics (Diagnostics, fromEither)
import Control.Effect.Lift (Lift)
import Data.ByteString qualified as BS
import Data.Maybe (catMaybes)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Discovery.Filters (AllFilters (..), FilterCombination (combinedPaths))
import Discovery.Walk (WalkStep (WalkContinue), walk')
import Effect.ReadFS (ReadFS, readContentsBSLimit)
import Path (Abs, Dir, File, Path, isProperPrefixOf, stripProperPrefix, toFilePath, (</>))
import Srclib.Converter qualified as Srclib
import Srclib.Types (AdditionalDepData (..), SourceUnit (..), SourceUserDefDep (..))
import Types (GraphBreadth (Complete))

data BinaryFile = BinaryFile
  { binaryPath :: Path Abs File
  , binaryFingerprint :: Fingerprint
  }

-- | Binary detection is sufficiently different from other analysis types that it cannot be just another strategy.
-- Instead, binary detection is run separately over the entire scan directory, outputting its own source unit.
-- The goal of this feature is to enable a FOSSA user to flag all vendored binaries (as defined by git) in the project as dependencies.
-- Users may then use standard FOSSA UX flows to ignore or add license information to the detected binaries.
analyzeBinaryDeps :: (Has (Lift IO) sig m, Has Diagnostics sig m, Has ReadFS sig m) => Path Abs Dir -> AllFilters -> m (Maybe SourceUnit)
analyzeBinaryDeps dir filters = do
  binaries <- fingerprintBinaries (toPathFilters dir filters) dir
  if null binaries
    then pure Nothing
    else pure . Just $ toSourceUnit (toProject dir) binaries

fingerprintBinaries :: (Has (Lift IO) sig m, Has Diagnostics sig m, Has ReadFS sig m) => PathFilters -> Path Abs Dir -> m [BinaryFile]
fingerprintBinaries filters = walk' $ \dir _ files -> do
  if shouldFingerprintDir dir filters
    then do
      someBinaries <- traverse fingerprintIfBinary files
      pure (catMaybes someBinaries, WalkContinue)
    else pure ([], WalkContinue)

fingerprintIfBinary :: (Has (Lift IO) sig m, Has Diagnostics sig m, Has ReadFS sig m) => Path Abs File -> m (Maybe BinaryFile)
fingerprintIfBinary file = do
  isBinary <- fileIsBinary file
  if isBinary
    then do
      fp <- fingerprintRaw file
      pure . Just $ BinaryFile file fp
    else pure Nothing

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
toProject dir = ProjectResult "binary-deps" dir mempty Complete []

toDependency :: Path Abs Dir -> BinaryFile -> SourceUserDefDep
toDependency root BinaryFile{..} =
  SourceUserDefDep
    { srcUserDepName = renderRelative root binaryPath
    , srcUserDepVersion = renderFingerprint binaryFingerprint
    , srcUserDepLicense = ""
    , srcUserDepDescription = Just "Binary discovered in source tree"
    , srcUserDepHomepage = Nothing
    }

toSourceUnit :: ProjectResult -> [BinaryFile] -> SourceUnit
toSourceUnit project binaries = do
  let unit = Srclib.toSourceUnit project
  let deps = map (toDependency $ projectResultPath project) binaries
  unit{additionalData = Just $ AdditionalDepData (Just deps) Nothing}

-- | Just render the first few characters of the fingerprint.
-- The goal is to provide a high confidence that future binaries with the same name won't collide,
-- and we don't need all 256 bits for that.
renderFingerprint :: Fingerprint -> Text
renderFingerprint fingerprint = Text.take 12 $ unFingerprint fingerprint

renderRelative :: Path Abs Dir -> Path Abs File -> Text
renderRelative absDir absFile =
  case stripProperPrefix absDir absFile of
    Left _ -> toText . toFilePath $ absFile
    Right relFile -> toText . toFilePath $ relFile

-- | Determine if a file is binary using the same method as git:
-- "is there a zero byte in the first 8000 bytes of the file"
fileIsBinary :: (Has ReadFS sig m, Has Diagnostics sig m) => Path Abs File -> m Bool
fileIsBinary file = do
  attemptedContent <- readContentsBSLimit file 8000
  content <- fromEither attemptedContent
  pure $ BS.elem 0 content
