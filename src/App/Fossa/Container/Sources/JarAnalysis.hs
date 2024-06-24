{-# LANGUAGE DerivingVia #-}

module App.Fossa.Container.Sources.JarAnalysis (
  analyzeContainerJars,
  JarObservation,
) where

import App.Fossa.EmbeddedBinary (BinaryPaths, toPath, withMillhoneBinary)
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, context, fatal)
import Control.Effect.Lift (Lift)
import Control.Monad (unless)
import Data.Aeson (
  FromJSON,
  FromJSONKey,
  ToJSON,
  Value,
  eitherDecode,
  parseJSON,
  withObject,
  (.:),
 )
import Data.ByteString.Lazy qualified as BL
import Data.Map qualified as Map
import Data.String.Conversion (decodeUtf8, toText)
import Data.Text (Text)
import Effect.Exec (AllowErr (Never), CmdSuccess (..), Command (..), Exec, execReturningStderr)
import Effect.Logger (Logger, logDebug, pretty)
import GHC.Generics (Generic)
import Path (Abs, File, Path, parent)

newtype LayerPath = LayerPath Text
  deriving (Eq, Ord, Show)
  deriving (FromJSONKey, FromJSON) via Text

-- The CLI doesn't look at these values, so don't bother parsing them to anything specific.
-- This lets millhone be the single source of truth for Jar observations.
newtype JarObservation = JarObservation
  {inner :: Value}
  deriving (Eq, Ord, Show, Generic)
  deriving (ToJSON, FromJSON) via Value

-- | Output parse type for millhone.
newtype DiscoveredJars = DiscoveredJars
  { discoveredJars :: Map.Map LayerPath [JarObservation]
  }
  deriving (Eq, Ord, Show)

toJarObservations :: DiscoveredJars -> [JarObservation]
toJarObservations = mconcat . Map.elems . discoveredJars

instance FromJSON DiscoveredJars where
  parseJSON = withObject "JarInput" $ \o -> DiscoveredJars <$> o .: "discovered_jars"

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
analyzeContainerJars :: (Has Logger sig m, Has Exec sig m, Has (Lift IO) sig m, Has Diagnostics sig m) => Path Abs File -> m [JarObservation]
analyzeContainerJars imagePath = withMillhoneBinary $ \binaryPaths ->
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
          either fatal pure $ eitherDecode @DiscoveredJars cmdSuccessStdout

    pure . toJarObservations $ jarOutput
