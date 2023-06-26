{-# LANGUAGE RecordWildCards #-}

module App.Fossa.RunThemis (
  execThemis,
  execRawThemis,
  -- `themisCommand` is used by Hubble.
  themisCommand,
  themisFlags,
  getThemisVersion,
) where

import App.Fossa.EmbeddedBinary (
  BinaryPaths,
  ThemisBins (..),
  ThemisIndex,
  toPath,
 )
import App.Types (FullFileUploads (unFullFileUploads))
import Control.Carrier.Diagnostics qualified as Diag
import Control.Effect.Diagnostics (Diagnostics, Has, (<||>))
import Data.ByteString.Lazy qualified as BL
import Data.String.Conversion (ConvertUtf8 (decodeUtf8), toText)
import Data.Tagged (Tagged, unTag)
import Data.Text (Text)
import Data.Text qualified as T
import Diag.Result
import Effect.Exec (
  AllowErr (Never),
  Command (..),
  Exec,
  execJson,
  execThrow,
  execThrow',
 )
import Effect.ReadFS (ReadFS)
import Path (Abs, Dir, Path, parent)
import Srclib.Types (LicenseUnit)
import Types (GlobFilter (unGlobFilter), LicenseScanPathFilters (..))

execRawThemis :: (Has Exec sig m, Has Diagnostics sig m) => ThemisBins -> Path Abs Dir -> [Text] -> m BL.ByteString
execRawThemis themisBins scanDir flags = execThrow scanDir $ themisCommand themisBins "" flags

-- get the themis version without requiring a directory to run in
-- The output will look something like
-- 2023/06/26 14:37:54 Version: ee69aa92245697a5f6d32a27129ec2b4d77dc423
-- So we just split on the first space, counting from the end of the string
getThemisVersion :: (Has Exec sig m, Has ReadFS sig m, Has Diagnostics sig m) => ThemisBins -> m Text
getThemisVersion themisBins = do
  bytes <- Diag.runDiagnostics (execThrow' (themisCommand themisBins "" ["--version"]) <||> pure BL.empty)
  case bytes of
    Success _ bs -> do
      let versionText = decodeUtf8 bs
      let version = T.takeWhileEnd (/= ' ') $ T.strip versionText
      pure version
    _ -> pure ""

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

themisFlags :: Maybe LicenseScanPathFilters -> FullFileUploads -> [Text]
themisFlags Nothing fullFileUploads = if unFullFileUploads fullFileUploads then ["--srclib-with-full-files"] else ["--srclib-with-matches"]
themisFlags (Just filters) fullFileUploads =
  let defaultFilter = if unFullFileUploads fullFileUploads then ["--srclib-with-full-files"] else ["--srclib-with-matches"]
      onlyFilters = concatMap (\only -> ["--only-paths", unGlobFilter only]) $ licenseScanPathFiltersOnly filters
      exceptFilters = concatMap (\exclude -> ["--exclude-paths", unGlobFilter exclude]) $ licenseScanPathFiltersExclude filters
   in defaultFilter ++ onlyFilters ++ exceptFilters
