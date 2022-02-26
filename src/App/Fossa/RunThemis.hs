{-# LANGUAGE RecordWildCards #-}

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
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Path (Abs, Dir, Path, Rel, fromAbsFile)

import App.Fossa.EmbeddedBinary (BinaryPaths, toExecutablePath)
import Options.Applicative (CommandFields)

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

generateThemisOpts :: Path Abs Dir -> ThemisCLIOpts
generateThemisOpts dir = ThemisCLIOpts{ themisCLIScanDir = dir, themisCLIArgs = ["--help"]}

execThemis :: (Has Exec sig m, Has Diagnostics sig m) => BinaryPaths -> ThemisCLIOpts -> m Text
execThemis binaryPaths opts = decodeUtf8 . BL.toStrict <$> execThrow (themisCLIScanDir opts) (themisCommand binaryPaths opts)

themisCommand :: BinaryPaths -> ThemisCLIOpts -> Command
themisCommand bin ThemisCLIOpts{..} = do
  Command
    { cmdName = toText $ fromAbsFile $ toExecutablePath bin
    , cmdArgs = themisCLIArgs
    , cmdAllowErr = Never
    }

