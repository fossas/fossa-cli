{-# LANGUAGE RecordWildCards #-}

module App.Fossa.RunThemis (
  execThemis,
) where

import Control.Carrier.Error.Either (Has)
import Control.Effect.Diagnostics (Diagnostics)
import Data.String.Conversion (toText)
import Data.Tagged (Tagged, unTag)
import Data.Text (Text)
import Path (Abs, Dir, Path, fromAbsFile)
import Srclib.Types (LicenseUnit)

import App.Fossa.EmbeddedBinary (
  BinaryPaths (..),
  ThemisBins (..),
  ThemisIndex,
  toPath,
 )

import Effect.Exec (
  AllowErr (Never),
  Command (..),
  Exec,
  execJson,
 )

-- TODO: We should log the themis version and index version
execThemis :: (Has Exec sig m, Has Diagnostics sig m) => ThemisBins -> Path Abs Dir -> m [LicenseUnit]
execThemis themisBins scanDir = do
  execJson @[LicenseUnit] scanDir $ themisCommand themisBins

themisCommand :: ThemisBins -> Command
themisCommand ThemisBins{..} = do
  Command
    { cmdName = toText $ fromAbsFile $ toPath $ unTag themisBinaryPaths
    , cmdArgs = generateThemisArgs indexBinaryPaths
    , cmdAllowErr = Never
    }

generateThemisArgs :: Tagged ThemisIndex BinaryPaths -> [Text]
generateThemisArgs taggedThemisIndex =
  [ "--license-data-dir"
  , toText . binaryPathContainer $ unTag taggedThemisIndex
  , "--srclib-with-matches"
  , "."
  ]
