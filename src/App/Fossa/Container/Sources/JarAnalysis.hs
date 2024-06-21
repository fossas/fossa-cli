module App.Fossa.Container.Sources.JarAnalysis (analyzeJar) where

import App.Fossa.EmbeddedBinary (BinaryPaths, toPath, withMillhoneBinary)
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, context, fatal)
import Control.Effect.Lift (Lift)
import Control.Monad (unless)
import Data.ByteString.Lazy qualified as BL
import Data.String.Conversion (decodeUtf8, toText)
import Data.Text (Text)
import Effect.Exec (AllowErr (Never), CmdSuccess (..), Command (..), Exec, execReturningStderr)
import Effect.Logger (Logger, logDebug, logStdout, pretty)
import Path (Abs, File, Path, parent)

-- | Generate a command to run millhone's jar analyze code given a millhone binary and tar file path.
millhoneJarAnalyzeCmd :: BinaryPaths -> Path Abs File -> Command
millhoneJarAnalyzeCmd cmdPath imageTarFile =
  Command
    { cmdName = toText . toPath $ cmdPath
    , cmdArgs =
        [ "--log-to"
        , "stderr"
        , "analyze-jars"
        , toText imageTarFile
        ]
    , cmdAllowErr = Never
    }

analyzeJar :: (Has Logger sig m, Has Exec sig m, Has (Lift IO) sig m, Has Diagnostics sig m) => Path Abs File -> m ()
analyzeJar imagePath = withMillhoneBinary $ \binaryPaths ->
  context ("Searching for JARs in " <> toText imagePath) $ do
    result <- execReturningStderr (parent imagePath) (millhoneJarAnalyzeCmd binaryPaths imagePath)
    jarOutput <- context "Run millhone (analyze jars)" $
      case result of
        Left e -> fatal e
        Right CmdSuccess{cmdSuccessStdout, cmdSuccessStderr} -> do
          unless (BL.null cmdSuccessStderr) $
            context "Millhone jar analyze traces" $
              logDebug . pretty . decodeUtf8 @Text $
                cmdSuccessStderr
          let outJson = decodeUtf8 @Text $ cmdSuccessStdout
          context "Millhone jar analyze output " $ logDebug (pretty outJson)
          pure outJson

    logStdout jarOutput
