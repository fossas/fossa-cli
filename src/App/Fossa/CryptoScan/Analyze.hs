module App.Fossa.CryptoScan.Analyze (
  analyzeWithCryptoScan,
  analyzeCryptoScanCBOM,
) where

import App.Fossa.CryptoScan.Types (CryptoScanResults (..))
import App.Fossa.EmbeddedBinary (BinaryPaths, toPath, withCryptoScanBinary)
import Control.Carrier.Diagnostics (Diagnostics, fatalText)
import Control.Effect.Lift (Has, Lift)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.String.Conversion (ToText (toText))
import Data.Text (Text)
import Effect.Exec (AllowErr (Never), Command (..), Exec, execThrow)
import Effect.Logger (Logger, logDebug, pretty)
import Path (Abs, Dir, Path)

-- | Run the cryptoscan binary on the given directory and return parsed results.
analyzeWithCryptoScan ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Exec sig m
  , Has Logger sig m
  ) =>
  Path Abs Dir ->
  m (Maybe CryptoScanResults)
analyzeWithCryptoScan rootDir = withCryptoScanBinary $ \bin -> do
  logDebug "Running cryptoscan binary"
  result <- execThrow rootDir (cryptoScanCommand bin rootDir)
  case Aeson.eitherDecode result of
    Left err -> do
      fatalText $ "Failed to parse cryptoscan output: " <> toText err
    Right findings -> do
      logDebug $ "Cryptoscan completed: " <> pretty (show (length (cryptoFindings findings))) <> " findings"
      pure $ Just findings

-- | Run the cryptoscan binary with CycloneDX output format and return raw JSON bytes.
-- The Rust binary produces a complete CycloneDX 1.7 BOM, so no Haskell-side conversion is needed.
analyzeCryptoScanCBOM ::
  ( Has Diagnostics sig m
  , Has (Lift IO) sig m
  , Has Exec sig m
  , Has Logger sig m
  ) =>
  Path Abs Dir ->
  m (Maybe BL.ByteString)
analyzeCryptoScanCBOM rootDir = withCryptoScanBinary $ \bin -> do
  logDebug "Running cryptoscan binary (CycloneDX output)"
  result <- execThrow rootDir (cryptoScanCycloneDxCommand bin rootDir)
  logDebug "Cryptoscan CycloneDX output generated"
  pure $ Just result

cryptoScanCommand :: BinaryPaths -> Path Abs Dir -> Command
cryptoScanCommand = mkCryptoScanCommand "json"

cryptoScanCycloneDxCommand :: BinaryPaths -> Path Abs Dir -> Command
cryptoScanCycloneDxCommand = mkCryptoScanCommand "cyclonedx"

mkCryptoScanCommand :: Text -> BinaryPaths -> Path Abs Dir -> Command
mkCryptoScanCommand format bin rootDir =
  Command
    { cmdName = toText $ toPath bin
    , cmdArgs = ["--path", toText rootDir, "--ecosystem", "auto", "--format", format]
    , cmdAllowErr = Never
    , cmdEnvVars = mempty
    }
