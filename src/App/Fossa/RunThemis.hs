{-# LANGUAGE RecordWildCards #-}

module App.Fossa.RunThemis (
  execThemis,
) where

import Control.Carrier.Error.Either (Has)
import Control.Effect.Diagnostics (Diagnostics)
import Data.String.Conversion (toText)
import Data.Tagged (unTag)
import Data.Text (Text)
import Effect.Logger (Logger, logDebug)
import Path (Abs, Dir, Path, fromAbsFile)
import Prettyprinter (Pretty (pretty))
import Srclib.Types (LicenseUnit)

import App.Fossa.EmbeddedBinary (
  BinaryPaths (..),
  ThemisBins (..),
  toPath,
 )

import Effect.Exec (
  AllowErr (Never),
  Command (..),
  Exec,
  execJson,
 )

execThemis :: (Has Exec sig m, Has Diagnostics sig m, Has Logger sig m) => ThemisBins -> Path Abs Dir -> m [LicenseUnit]
execThemis themisBins scanDir = do
  let cmd = themisCommand themisBins
  logDebug $ pretty $ "themis command: " ++ show cmd
  logDebug $ pretty $ "scanDir: " ++ show scanDir
  res <- execJson @[LicenseUnit] scanDir cmd
  logDebug "back from command"
  pure res

themisCommand :: ThemisBins -> Command
themisCommand ThemisBins{..} = do
  Command
    { cmdName = toText $ fromAbsFile $ toPath $ unTag themisBinaryPaths
    , cmdArgs = generateThemisArgs $ unTag indexBinaryPaths
    , cmdAllowErr = Never
    }

generateThemisArgs :: BinaryPaths -> [Text]
generateThemisArgs BinaryPaths{..} =
  [ "--license-data-dir"
  , toText binaryPathContainer
  , "--srclib-with-matches"
  , "."
  ]
