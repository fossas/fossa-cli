{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.Reachability.Jar (
  callGraphFromJar,
  callGraphFromJars,
  isValidJar,
) where

import App.Fossa.Reachability.Types (CallGraphAnalysis (..), ContentRef (ContentRaw), ParsedJar (..))
import Control.Effect.Diagnostics (
  Diagnostics,
  ToDiagnostic,
  context,
  recover,
  renderDiagnostic,
  warnOnErr,
 )
import Control.Effect.Exception (Lift, bracket)
import Control.Effect.Lift (sendIO)
import Data.ByteString qualified as BS
import Data.Error (createErrataWithHeaderOnly)
import Data.FileEmbed.Extra (embedFile')
import Data.Maybe (catMaybes)
import Data.String.Conversion (toText)
import Data.Text (isSuffixOf)
import Effect.Exec (AllowErr (Never), Command (..), Exec, Has, execThrow)
import Effect.Logger (Logger)
import Effect.ReadFS (ReadFS, doesFileExist)
import Errata (Errata)
import Path (Abs, File, Path, fileExtension, fromAbsDir, parent, toFilePath)
import Path.IO (createTempDir, getTempDir, removeDirRecur)
import System.FilePath (takeFileName)
import System.FilePath qualified as FP

newtype CallGraphJarParser = CallGraphJarParser {jar :: BS.ByteString}
  deriving (Eq, Ord, Show)

-- This jar is from: https://github.com/fossas/jar-callgraph/pull/58
execJar :: CallGraphJarParser
execJar = CallGraphJarParser{jar = $(embedFile' "scripts/jar-callgraph-1.0.2.jar")}

withUnpackedPlugin ::
  (Has (Lift IO) sig m) =>
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
      let pluginJarFilepath = fromAbsDir tmpDir FP.</> "jar-callgraph.jar"
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
  withUnpackedPlugin execJar $ \filepath -> do
    content <- execThrow (parent jar) (jarPraseCmd filepath jar)
    pure . ParsedJar jar . ContentRaw $ content

newtype FailedToParseJar = FailedToParseJar (Path Abs File)

instance ToDiagnostic FailedToParseJar where
  renderDiagnostic :: FailedToParseJar -> Errata
  renderDiagnostic (FailedToParseJar jar) =
    createErrataWithHeaderOnly $ "Could not read from jar, so skipping: " <> toText (show jar)

-- | Like @mavenJarCallGraph@, but used when the list of JARs to parse is already available
-- and works for any Java project.
callGraphFromJars ::
  ( Has Logger sig m
  , Has Diagnostics sig m
  , Has Exec sig m
  , Has (Lift IO) sig m
  ) =>
  [Path Abs File] ->
  m CallGraphAnalysis
callGraphFromJars jars = context ("build call graph from " <> toText (show jars)) $ do
  parsedJars <- traverse callGraphFromJar jars
  pure $ JarAnalysis (catMaybes parsedJars)

-- True if jar exist, and is not likely test jar, otherwise False
isValidJar :: (Has ReadFS sig m) => Path Abs File -> m Bool
isValidJar file = do
  exists <- doesFileExist file
  pure $
    exists
      && fileExtension file == Just ".jar"
      -- In maven and java ecosystem, test jars have -test suffix by convention
      && not (isSuffixOf "-test.jar" $ toText . takeFileName . toFilePath $ file)
