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
import Path (Abs, Dir, Path, parent)
import Srclib.Types (LicenseUnit)

-- TODO: We should log the themis version and index version
execThemis :: (Has Exec sig m, Has Diagnostics sig m) => ThemisBins -> Path Abs Dir -> m [LicenseUnit]
execThemis themisBins scanDir = do
  execJson @[LicenseUnit] scanDir $ themisCommand themisBins

themisCommand :: ThemisBins -> Command
themisCommand ThemisBins{..} = do
  Command
    { cmdName = toText . toPath $ unTag themisBinaryPaths
    , cmdArgs = generateThemisArgs indexBinaryPaths
    , cmdAllowErr = Never
    }

generateThemisArgs :: Tagged ThemisIndex BinaryPaths -> [Text]
generateThemisArgs taggedThemisIndex =
  [ "--license-data-dir"
  , toText . parent . toPath $ unTag taggedThemisIndex
  , "--srclib-with-matches"
  , "."
  ]
