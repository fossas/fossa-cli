{-# LANGUAGE QuasiQuotes #-}


module App.Fossa.Compatibility
  ( compatibilityMain,
    argumentParser,
    Argument,
  )
where

import App.Fossa.EmbeddedBinary (BinaryPaths, toExecutablePath, withCLIv1Binary)
import Control.Carrier.StickyLogger (runStickyLogger, logSticky)
import Control.Effect.Lift (sendIO)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.String.Conversion (decodeUtf8)
import Data.Text (Text, pack)
import Effect.Exec (AllowErr (Never), CmdFailure (cmdFailureStdout), Command (..), cmdFailureStderr, exec, runExecIO)
import Effect.Logger (Pretty (pretty), Severity (SevInfo), logInfo, withDefaultLogger)
import Options.Applicative (Parser, argument, help, metavar, str)
import Path
import System.Exit (exitFailure, exitSuccess)

type Argument = Text

argumentParser :: Parser Argument
argumentParser = pack <$> argument str (metavar "ARGS" <> help "arguments to fossa v1 analyze")

compatibilityMain ::
  [Argument] ->
  IO ()
compatibilityMain args = withDefaultLogger SevInfo . runExecIO . withCLIv1Binary $ \v1Bin -> do
  cmd <- runStickyLogger SevInfo $ do
    logSticky "[ Waiting for fossa analyze completion ]"
    exec [reldir|.|] $ v1Command v1Bin args

  case cmd of
    Left err -> do
      logInfo . pretty @Text . decodeUtf8 $ cmdFailureStderr err
      logInfo . pretty @Text . decodeUtf8 $ cmdFailureStdout err
      sendIO exitFailure
    Right out -> sendIO (BL.putStr out >> exitSuccess)

v1Command :: BinaryPaths -> [Text] -> Command
v1Command bin args =
  Command
    { cmdName = pack . toFilePath $ toExecutablePath bin,
      cmdArgs = "analyze" : args,
      cmdAllowErr = Never
    }
