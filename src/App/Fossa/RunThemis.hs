{-# LANGUAGE RecordWildCards #-}

module App.Fossa.RunThemis (
  execThemis,
  generateThemisOpts,
  ThemisCLIOpts (..),
) where

import Control.Carrier.Error.Either (Has)
import Control.Effect.Diagnostics (Diagnostics)
import Data.String.Conversion (toText)
import Data.Tagged (unTag)
import Data.Text (Text)
import Effect.Logger (Logger, logDebug)
import Path (Abs, Dir, Path, Rel, fromAbsFile, (</>))
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

data ThemisCLIOpts = ThemisCLIOpts
  { themisCLIScanDir :: Path Abs Dir
  , themisCLIArgs :: [Text]
  }
  deriving (Show, Eq, Ord)

generateThemisOpts :: Path Abs Dir -> Path Rel Dir -> ThemisCLIOpts
generateThemisOpts baseDir vendoredDepDir =
  ThemisCLIOpts{themisCLIScanDir = fullDir, themisCLIArgs = []}
  where
    fullDir = baseDir </> vendoredDepDir

execThemis :: (Has Exec sig m, Has Diagnostics sig m, Has Logger sig m) => ThemisBins -> ThemisCLIOpts -> m [LicenseUnit]
execThemis themisBins opts = do
  let cmd = themisCommand themisBins
      scanDir = themisCLIScanDir opts
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
