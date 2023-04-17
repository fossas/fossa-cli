{-# LANGUAGE RecordWildCards #-}

module App.Fossa.RunThemis (
  execThemis,
  execRawThemis,
  -- `themisCommand` is used by Hubble.
  themisCommand,
  themisFlags,
) where

import App.Fossa.EmbeddedBinary (
  BinaryPaths,
  ThemisBins (..),
  ThemisIndex,
  toPath,
 )
import Control.Effect.Diagnostics (Diagnostics, Has)
import Data.ByteString.Lazy qualified as BL
import Data.String.Conversion (toText)
import Data.Tagged (Tagged, unTag)
import Data.Text (Text)
import Effect.Exec (
  AllowErr (Never),
  Command (..),
  Exec,
  execJson,
  execThrow,
 )
import Path (Abs, Dir, Path, parent)
import Srclib.Types (LicenseUnit)
import Types (GlobFilter (unGlobFilter), LicenseScanPathFilters (..))

execRawThemis :: (Has Exec sig m, Has Diagnostics sig m) => ThemisBins -> Path Abs Dir -> [Text] -> m BL.ByteString
execRawThemis themisBins scanDir flags = execThrow scanDir $ themisCommand themisBins "" flags

-- TODO: We should log the themis version and index version
execThemis :: (Has Exec sig m, Has Diagnostics sig m) => ThemisBins -> Text -> Path Abs Dir -> [Text] -> m [LicenseUnit]
execThemis themisBins pathPrefix scanDir flags = do
  execJson @[LicenseUnit] scanDir $ themisCommand themisBins pathPrefix flags

themisCommand :: ThemisBins -> Text -> [Text] -> Command
themisCommand ThemisBins{..} pathPrefix flags = do
  Command
    { cmdName = toText . toPath $ unTag themisBinaryPaths
    , cmdArgs = generateThemisArgs indexBinaryPaths pathPrefix flags
    , cmdAllowErr = Never
    }

generateThemisArgs :: Tagged ThemisIndex BinaryPaths -> Text -> [Text] -> [Text]
generateThemisArgs taggedThemisIndex pathPrefix flags =
  [ "--license-data-dir"
  , toText . parent . toPath $ unTag taggedThemisIndex
  , "--path-prefix"
  , pathPrefix
  ]
    <> flags
    <> ["."]

themisFlags :: Maybe LicenseScanPathFilters -> Bool -> [Text]
themisFlags Nothing fullFileUploads = if fullFileUploads then ["--srclib-with-full-files"] else ["--srclib-with-matches"]
themisFlags (Just filters) fullFileUploads =
  let defaultFilter = if fullFileUploads then ["--srclib-with-full-files"] else ["--srclib-with-matches"]
      onlyFilters = concatMap (\only -> ["--only-paths", unGlobFilter only]) $ licenseScanPathFiltersOnly filters
      exceptFilters = concatMap (\exclude -> ["--exclude-paths", unGlobFilter exclude]) $ licenseScanPathFiltersExclude filters
   in defaultFilter ++ onlyFilters ++ exceptFilters
