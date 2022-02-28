module App.Fossa.RunThemis (
  execThemis,
  generateThemisOpts,
  ThemisCLIOpts (..),
) where

import Control.Carrier.Error.Either (Has)
import Control.Effect.Diagnostics (Diagnostics)
import Data.ByteString.Lazy qualified as BL
import Data.String.Conversion (toText)
import Data.Text (Text)
import Path (Abs, Dir, Path, Rel, fromAbsFile, (</>))
import Effect.Logger (Logger, logDebug)
import Prettyprinter (Pretty (pretty))

import App.Fossa.EmbeddedBinary (
  toExecutablePath,
  BinaryPaths (..),
  )

import Effect.Exec (
  AllowErr (Never),
  Command (..),
  Exec,
  execThrow,
 )

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
  , toText $ binaryPathContainer indexPath
  , "--srclib-with-matches"
  , "."
  ]

