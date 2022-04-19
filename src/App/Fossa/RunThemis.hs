{-# LANGUAGE RecordWildCards #-}

module App.Fossa.RunThemis (
  execThemis,
  execRawThemis,
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

execRawThemis :: (Has Exec sig m, Has Diagnostics sig m) => ThemisBins -> Path Abs Dir -> m BL.ByteString
execRawThemis themisBins scanDir = execThrow scanDir $ themisCommand themisBins

-- TODO: We should log the themis version and index version
execThemis :: (Has Exec sig m, Has Diagnostics sig m) => ThemisBins -> Text -> Path Abs Dir -> m [LicenseUnit]
execThemis themisBins pathPrefix scanDir = do
  execJson @[LicenseUnit] scanDir $ themisCommand themisBins pathPrefix

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
