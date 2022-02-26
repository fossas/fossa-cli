{-# LANGUAGE RecordWildCards #-}

module App.Fossa.RunThemis (
  execThemis,
  generateThemisOpts,
  ThemisCLIOpts (..),
) where

import Control.Carrier.Error.Either (Has)
import Control.Effect.Diagnostics (Diagnostics)
import Data.ByteString.Lazy qualified as BL
import Data.String.Conversion (toText, toString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Path (Abs, Dir, Path, Rel, fromAbsFile, parseRelDir, (</>))

import App.Fossa.EmbeddedBinary (BinaryPaths, toExecutablePath)
import Options.Applicative (CommandFields)

import Effect.Exec (
  AllowErr (Never),
  Command (..),
  Exec,
  execThrow,
 )
import Control.Monad (when)

data ThemisCLIOpts = ThemisCLIOpts
  { themisCLIScanDir :: Path Abs Dir
  , themisCLIArgs :: [Text]
  }

generateThemisOpts :: Path Abs Dir -> Path Rel Dir -> ThemisCLIOpts
generateThemisOpts baseDir vendoredDepDir =
  ThemisCLIOpts{ themisCLIScanDir = fullDir, themisCLIArgs = ["--help"]}
    where
      fullDir = baseDir </> vendoredDepDir

execThemis :: (Has Exec sig m, Has Diagnostics sig m) => BinaryPaths -> ThemisCLIOpts -> m BL.ByteString
execThemis binaryPaths opts = execThrow (themisCLIScanDir opts) (themisCommand binaryPaths opts)

themisCommand :: BinaryPaths -> ThemisCLIOpts -> Command
themisCommand bin ThemisCLIOpts{..} = do
  Command
    { cmdName = toText $ fromAbsFile $ toExecutablePath bin
    , cmdArgs = themisCLIArgs
    , cmdAllowErr = Never
    }

