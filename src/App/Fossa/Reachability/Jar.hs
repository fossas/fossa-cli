{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.Reachability.Jar (
  callGraphFromJar,
)
where

import App.Fossa.Reachability.Types (ContentRef (ContentRaw), ParsedJar (..))
import Control.Effect.Diagnostics (
  Diagnostics,
  ToDiagnostic,
  recover,
  renderDiagnostic,
  warnOnErr,
 )
import Control.Effect.Exception (Lift, bracket)
import Control.Effect.Lift (sendIO)
import Data.ByteString qualified as BS
import Data.FileEmbed.Extra (embedFile')
import Data.String.Conversion (toText)
import Effect.Exec (AllowErr (Never), Command (..), Exec, Has, execThrow)
import Effect.Logger (Doc, pretty)
import Path (Abs, File, Path, fromAbsDir, parent, toFilePath)
import Path.IO (createTempDir, getTempDir, removeDirRecur)
import System.FilePath qualified as FP

newtype CallGraphJarParser = CallGraphJarParser {jar :: BS.ByteString}
  deriving (Eq, Ord, Show)

parserJar :: CallGraphJarParser
parserJar = CallGraphJarParser{jar = $(embedFile' "scripts/parser.jar")}

withUnpackedPlugin ::
  ( Has (Lift IO) sig m
  ) =>
  CallGraphJarParser ->
  (FP.FilePath -> m a) ->
  m a
withUnpackedPlugin plugin act =
  bracket
    (sendIO (getTempDir >>= \tmp -> createTempDir tmp "fossa-reachability-jar"))
    (sendIO . removeDirRecur)
    go
  where
    go tmpDir = do
      let pluginJarFilepath = fromAbsDir tmpDir FP.</> "callgraph.jar"
      sendIO (BS.writeFile pluginJarFilepath $ jar plugin)
      act pluginJarFilepath

jarPraseCmd :: FilePath -> Path Abs File -> Command
jarPraseCmd plugin target = Command "java" ["-jar", toText plugin, toText $ toFilePath target] Never

callGraphFromJar ::
  ( Has Exec sig m
  , Has Diagnostics sig m
  , Has (Lift IO) sig m
  ) =>
  Path Abs File ->
  m (Maybe ParsedJar)
callGraphFromJar jar = recover . warnOnErr (FailedToParseJar jar) $
  withUnpackedPlugin parserJar $ \filepath -> do
    content <- execThrow (parent jar) (jarPraseCmd filepath jar)
    pure . ParsedJar jar . ContentRaw $ content

newtype FailedToParseJar = FailedToParseJar (Path Abs File)

instance ToDiagnostic FailedToParseJar where
  renderDiagnostic :: FailedToParseJar -> Doc ann
  renderDiagnostic (FailedToParseJar jar) =
    pretty $ "Could not parse jar, so skipping: " <> show jar
