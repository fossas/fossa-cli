{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}


module App.Fossa.Compatibility
  ( compatibilityMain,
    argumentParser,
    Argument,
  )
where

import App.Fossa.EmbeddedBinary (BinaryPaths, toExecutablePath, withCLIv1Binary)
import Control.Effect.Lift (sendIO)
import Data.Text (Text, pack)
import Data.Foldable (traverse_)
import Effect.Exec (CmdFailure(cmdFailureStdout), AllowErr (Never), Command (..), exec, runExecIO, cmdFailureStderr)
import Path
import qualified Data.ByteString.Lazy.Char8 as BL
import Options.Applicative (Parser, argument, help, metavar, str)
import System.Exit (exitFailure, exitSuccess)
import Data.Text.Lazy.Encoding
import Effect.Logger (Pretty(pretty), logInfo, logSticky, Severity(SevInfo), withLogger)

type Argument = Text

argumentParser :: Parser Argument
argumentParser = pack <$> argument str (metavar "ARGS" <> help "arguments to fossa v1 analyze")

compatibilityMain ::
  [Argument] ->
  IO ()
compatibilityMain args = withLogger SevInfo . runExecIO . withCLIv1Binary $ \v1Bin -> do
  logSticky "[ Waiting for fossa analyze completion ]"
  cmd <- exec [reldir|.|] $ v1Command v1Bin $ args
  logSticky ""

  case cmd of
    Left err -> do
      traverse_ (\accessor -> logInfo . pretty . decodeUtf8 $ accessor err)  [cmdFailureStderr, cmdFailureStdout]
      sendIO exitFailure
    Right out -> sendIO (BL.putStr out >> exitSuccess)

v1Command :: BinaryPaths -> [Text] -> Command
v1Command bin args =
  Command
    { cmdName = pack . toFilePath $ toExecutablePath bin,
      cmdArgs = "analyze" : args,
      cmdAllowErr = Never
    }
