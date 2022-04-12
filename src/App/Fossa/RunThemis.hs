{-# LANGUAGE RecordWildCards #-}

module App.Fossa.RunThemis (
  execThemis,
) where

import App.Fossa.EmbeddedBinary (
  BinaryPaths,
  ThemisBins (..),
  ThemisIndex,
  toPath,
 )
import Control.Effect.Diagnostics (Diagnostics, Has)
import Data.String.Conversion (toText)
import Data.Tagged (Tagged, unTag)
import Data.Text (Text)
import Effect.Exec (
  AllowErr (Never),
  Command (..),
  Exec,
  execJson,
 )
import Path (Abs, Dir, Path, SomeBase (Abs, Rel), parent)
import Path.Extra (tryMakeRelative)
import Srclib.Types (LicenseUnit)

-- TODO: We should log the themis version and index version
execThemis :: (Has Exec sig m, Has Diagnostics sig m) => ThemisBins -> Path Abs Dir -> Path Abs Dir -> m [LicenseUnit]
execThemis themisBins baseDir scanDir = do
  execJson @[LicenseUnit] scanDir $ themisCommand themisBins pathPrefix
  where
    relPath = tryMakeRelative baseDir scanDir
    pathPrefix = case relPath of
      Path.Abs _ -> ""
      Path.Rel path -> toText path

themisCommand :: ThemisBins -> Text -> Command
themisCommand ThemisBins{..} pathPrefix = do
  Command
    { cmdName = toText . toPath $ unTag themisBinaryPaths
    , cmdArgs = generateThemisArgs indexBinaryPaths pathPrefix
    , cmdAllowErr = Never
    }

generateThemisArgs :: Tagged ThemisIndex BinaryPaths -> Text -> [Text]
generateThemisArgs taggedThemisIndex pathPrefix =
  [ "--license-data-dir"
  , toText . parent . toPath $ unTag taggedThemisIndex
  , "--path-prefix"
  , pathPrefix
  , "--srclib-with-matches"
  , "."
  ]
