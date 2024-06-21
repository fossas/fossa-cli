{-# LANGUAGE DerivingVia #-}

module App.Fossa.Container.Sources.JarAnalysis (analyzeJar) where

import App.Fossa.EmbeddedBinary (BinaryPaths, toPath, withMillhoneBinary)
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, context, fatal)
import Control.Effect.Lift (Lift)
import Control.Monad (unless)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, eitherDecode, parseJSON, withObject, (.:))
import Data.Aeson.KeyMap (toMapText)
import Data.ByteString.Lazy qualified as BL
import Data.Map qualified as Map
import Data.String.Conversion (ToText, decodeUtf8, showText, toText)
import Data.Text (Text)
import Effect.Exec (AllowErr (Never), CmdSuccess (..), Command (..), Exec, execReturningStderr)
import Effect.Logger (Logger, logDebug, logStdout, pretty)
import Path (Abs, File, Path, parent)

newtype JarPath = JarPath Text
  deriving (Eq, Ord, Show)
  deriving (FromJSON, FromJSONKey) via Text

newtype LayerPath = LayerPath Text
  deriving (Eq, Ord, Show)
  deriving (FromJSONKey, FromJSON) via Text

newtype FingerprintKind = FingerprintKind Text
  deriving (Eq, Ord, Show)
  deriving (ToText, ToJSON, FromJSON, FromJSONKey) via Text

newtype Fingerprint = Fingerprint Text
  deriving (Eq, Ord, Show)
  deriving (ToText, ToJSON, FromJSON) via Text

data JarFingerprints = JarFingerprints
  { jarFilePath :: JarPath
  , fingerprints :: [(FingerprintKind, Fingerprint)]
  }
  deriving (Eq, Ord, Show)

instance FromJSON JarFingerprints where
  parseJSON = withObject "JarFingerprints" $ \o ->
    do
      jarFilePath <- o .: "path"
      prints <- o .: "fingerprints"
      fingerprints <- traverse parseJSON . Map.mapKeys FingerprintKind . toMapText $ prints
      pure JarFingerprints{jarFilePath, fingerprints = Map.toList fingerprints}

newtype PathPrints = PathPrints [JarFingerprints]
  deriving (Eq, Ord, Show, FromJSON)

newtype JarInput = JarInput
  { discoveredJars :: Map.Map LayerPath PathPrints
  }
  deriving (Eq, Ord, Show)

instance FromJSON JarInput where
  parseJSON = withObject "JarInput" $ \o -> JarInput <$> o .: "discovered_jars"

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
analyzeJar :: (Has Logger sig m, Has Exec sig m, Has (Lift IO) sig m, Has Diagnostics sig m) => Path Abs File -> m JarInput
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
          context "Millhone jar analyze output " $
            logDebug (pretty . decodeUtf8 @Text $ cmdSuccessStdout)
          either fatal pure $ eitherDecode @JarInput cmdSuccessStdout

    logStdout . showText $ jarOutput
    pure jarOutput
