{-# LANGUAGE DerivingVia #-}

module App.Fossa.Container.Sources.JarAnalysis (
  analyzeContainerJars,
) where

import App.Fossa.EmbeddedBinary (BinaryPaths, toPath, withMillhoneBinary)
import Container.Types (DiscoveredJars)
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, context, fatal)
import Control.Effect.Lift (Lift)
import Control.Monad (unless)
import Data.Aeson (
  eitherDecode,
 )
import Data.ByteString.Lazy qualified as BL
import Data.String.Conversion (decodeUtf8, toText)
import Data.Text (Text)
import Effect.Exec (AllowErr (Never), CmdSuccess (..), Command (..), Exec, execReturningStderr)
import Effect.Logger (Logger, logDebug, pretty)
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

-- | Analyze a single Jar file for fingerprints using Millhone.
analyzeContainerJars :: (Has Logger sig m, Has Exec sig m, Has (Lift IO) sig m, Has Diagnostics sig m) => Path Abs File -> m DiscoveredJars
analyzeContainerJars imagePath = withMillhoneBinary $ \binaryPaths ->
  context ("Searching for JARs in " <> toText imagePath) $ do
    result <- execReturningStderr (parent imagePath) (millhoneJarAnalyzeCmd binaryPaths imagePath)
    context "Run millhone (analyze jars)" $
      case result of
        Left e -> fatal e
        Right CmdSuccess{cmdSuccessStdout, cmdSuccessStderr} -> do
          unless (BL.null cmdSuccessStderr) $
            context "Millhone jar analyze traces" $
              logDebug . pretty . decodeUtf8 @Text $
                cmdSuccessStderr
          context "Millhone jar analyze output " $
            logDebug (pretty . decodeUtf8 @Text $ cmdSuccessStdout)
          either fatal pure $ eitherDecode @DiscoveredJars cmdSuccessStdout
