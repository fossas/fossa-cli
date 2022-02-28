{-# LANGUAGE RecordWildCards #-}

module App.Fossa.RunThemis (
  execThemis,
  generateThemisOpts,
  ThemisCLIOpts (..),
) where

import Control.Carrier.Error.Either (Has)
import Control.Effect.Diagnostics (Diagnostics, fatal)
import Data.ByteString.Lazy qualified as BL
import Data.String.Conversion (toText, toString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Path (Abs, Dir, Path, Rel, fromAbsFile, parseRelDir, (</>))
import Effect.Logger (Logger, logDebug, logInfo)
import Prettyprinter (Pretty (pretty))

import App.Fossa.EmbeddedBinary (
  toExecutablePath,
  BinaryPaths (..),
  )
import Options.Applicative (CommandFields)

import Effect.Exec (
  AllowErr (Never),
  Command (..),
  Exec,
  exec,
  execThrow,
 )
import Control.Monad (when)
import Options.Applicative.Help (cmdDesc)
import Text.Megaparsec.Error.Builder (err)

data ThemisCLIOpts = ThemisCLIOpts
  { themisCLIScanDir :: Path Abs Dir
  , themisCLIArgs :: [Text]
  }
  deriving (Show, Eq, Ord)

generateThemisOpts :: Path Abs Dir -> Path Rel Dir -> ThemisCLIOpts
generateThemisOpts baseDir vendoredDepDir =
  ThemisCLIOpts{ themisCLIScanDir = fullDir, themisCLIArgs = ["--help"]}
    where
      fullDir = baseDir </> vendoredDepDir

execThemis :: (Has Exec sig m, Has Diagnostics sig m, Has Logger sig m) => BinaryPaths -> BinaryPaths -> ThemisCLIOpts -> m BL.ByteString
execThemis themisBinaryPath indexGobPath opts = do
  let cmd = themisCommand themisBinaryPath indexGobPath
      scanDir = themisCLIScanDir opts
  logDebug $ pretty $ "themis command: " ++ show cmd
  logDebug $ pretty $ "scanDir: " ++ show scanDir
  res <- execThrow scanDir cmd
  logDebug "back from command"
  pure res
  -- case exec scanDir cmd of
  --   Left err -> fatal err
  --   Right stdout -> pure stdout

  -- execThrow (themisCLIScanDir opts) (themisCommand themisBinaryPath indexGobPath)

themisCommand :: BinaryPaths -> BinaryPaths -> Command
themisCommand themisBin indexGobBin = do
  Command
    { cmdName = toText $ fromAbsFile $ toExecutablePath themisBin
    , cmdArgs = generateThemisArgs indexGobBin
    , cmdAllowErr = Never
    }

generateThemisArgs :: BinaryPaths -> [Text]
generateThemisArgs indexPath =
  [ "--license-data-dir"
  -- , toText $ binaryPathContainer indexPath
  , "/Users/scott/fossa/fossa-cli/vendor-bins"
  , "--srclib-with-matches"
  , "."
  ]

