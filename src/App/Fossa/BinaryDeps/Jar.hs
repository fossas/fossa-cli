{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.BinaryDeps.Jar (resolveJar) where

import Control.Algebra (Has)
import Control.Carrier.Diagnostics (
  Diagnostics,
  ToDiagnostic (renderDiagnostic),
  context,
  errCtx,
  fromMaybeText,
  recover,
  warnOnErr,
  (<||>),
 )
import Control.Carrier.Finally (runFinally)
import Control.Effect.Lift (Lift)
import Control.Monad (join, when)
import Data.List (isSuffixOf, sortOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.String.Conversion (ToString (toString), ToText (toText))
import Data.Text (Text)
import Data.Text qualified as Text
import Discovery.Archive (extractZip, withArchive)
import Discovery.Walk (WalkStep (WalkContinue, WalkSkipAll), findFileNamed, walk')
import Effect.Logger (Logger, logDebug, pretty)
import Effect.ReadFS (ReadFS, readContentsText, readContentsXML)
import Errata (Errata (..))
import GHC.Base ((<|>))
import Path (Abs, Dir, File, Path, filename, mkRelDir, mkRelFile, (</>))
import Path.Extra (renderRelative, tryMakeRelative)
import Srclib.Types (SourceUserDefDep (..))
import Strategy.Maven.Pom.PomFile (
  MavenCoordinate (..),
  Pom (..),
  RawPom,
  pomLicenseName,
  validatePom,
 )

data JarMetadata = JarMetadata
  { jarName :: Text
  , jarVersion :: Text
  , jarLicense :: Text
  }

-- | Implement JAR resolution using a similar method to Ant analysis in CLIv1.
-- The overall idea is to:
--   1. Extract the JAR to a temporary directory (it's a zip!)
--   2. Search inside for a file named `pom.xml`; if there are multiple pick the one with the shortest path.
--      If a representative pom.xml was found, parse it and return metadata derived from it.
--   3. Attempt to open `META-INF/MANIFEST.MF`, parse it, and return metadata derived from it.
resolveJar :: (Has (Lift IO) sig m, Has Diagnostics sig m, Has Logger sig m, Has ReadFS sig m) => Path Abs Dir -> Path Abs File -> m (Maybe SourceUserDefDep)
resolveJar _ file | not $ fileHasSuffix file [".jar", ".aar"] = pure Nothing
resolveJar root file = do
  let fileDescription = toText file
  logDebug $ "Inferring metadata from " <> pretty fileDescription
  result <- recover
    . warnOnErr (FailedToResolveJar file)
    . errCtx (FailedToResolveJarCtx file)
    . context ("Infer metadata from " <> fileDescription)
    . runFinally
    $ withArchive extractZip file
    $ \dir -> tacticPom dir <||> tacticMetaInf dir
  pure $ fmap (toUserDefDep root file) (join result)

newtype FailedToResolveJar = FailedToResolveJar (Path Abs File)
instance ToDiagnostic FailedToResolveJar where
  renderDiagnostic (FailedToResolveJar path) = do
    let header = "Could not infer jar metadata (license, jar name, and version) from " <> toText path
    Errata (Just header) [] Nothing

newtype FailedToResolveJarCtx = FailedToResolveJarCtx (Path Abs File)
instance ToDiagnostic FailedToResolveJarCtx where
  renderDiagnostic (FailedToResolveJarCtx path) = do
    let header = "Ensure " <> toText path <> " is a valid jar or aar file"
    Errata (Just header) [] Nothing

tacticMetaInf :: (Has (Lift IO) sig m, Has Diagnostics sig m, Has Logger sig m, Has ReadFS sig m) => Path Abs Dir -> m JarMetadata
tacticMetaInf archive = context ("Parse " <> toText metaInfPath) $ do
  content <- readContentsText metaInfPath
  logDebug $ "Parsing META-INF manifest: " <> pretty (renderRelative archive metaInfPath)
  metaInfManifestToMeta $ parseMetaInfManifest content
  where
    metaInfPath = archive </> $(mkRelDir "META-INF") </> $(mkRelFile "MANIFEST.MF")

parseMetaInfManifest :: Text -> Map Text Text
parseMetaInfManifest t = Map.fromList . map strip' . filter' $ map (Text.breakOn ":") (Text.lines t)
  where
    null' (a, b) = any Text.null [a, b]
    strip' (a, b) = (Text.strip a, Text.strip $ Text.drop 1 b)
    filter' = filter (not . null')

metaInfManifestToMeta :: Has Diagnostics sig m => Map Text Text -> m JarMetadata
metaInfManifestToMeta manifest =
  JarMetadata
    <$> fromMaybeText "Missing bundle name" (Map.lookup "Bundle-SymbolicName" manifest <|> Map.lookup "Implementation-Title" manifest)
    <*> fromMaybeText "Missing implementation version" (Map.lookup "Implementation-Version" manifest)
    <*> pure "" -- Don't attempt to use Bundle-License; it's a URL and we don't parse it on the backend

tacticPom :: (Has (Lift IO) sig m, Has Diagnostics sig m, Has Logger sig m, Has ReadFS sig m) => Path Abs Dir -> m JarMetadata
tacticPom archive = context ("Parse representative pom.xml in " <> toText archive) $ do
  poms <- context "Find pom.xml files" $ walk' (collectFilesNamed "pom.xml") (archive </> $(mkRelDir "META-INF"))
  when (length poms > 1) $
    logDebug $
      "Found multiple pom.xml files: " <> pretty (Text.intercalate "; " $ map (renderRelative archive) poms)

  pom <- fromMaybeText "No pom.xml files found" $ choosePom poms
  logDebug $ "Chose representative pom.xml: " <> pretty (renderRelative archive pom)
  parsePom pom

choosePom :: [Path Abs File] -> Maybe (Path Abs File)
choosePom = listToMaybe . sortOn (length . toString)

parsePom :: (Has (Lift IO) sig m, Has Diagnostics sig m, Has ReadFS sig m) => Path Abs File -> m JarMetadata
parsePom file = context ("Parse pom file: " <> toText file) $ do
  (result :: RawPom) <- readContentsXML file
  validated <- fromMaybeText "Invalid format" $ validatePom result
  pure $ pomToMeta validated

pomToMeta :: Pom -> JarMetadata
pomToMeta Pom{..} = do
  let name = (coordGroup pomCoord) <> ":" <> (coordArtifact pomCoord)
  let license = Text.intercalate "\n" $ mapMaybe pomLicenseName pomLicenses
  JarMetadata name (coordVersion pomCoord) license

collectFilesNamed :: Applicative f => String -> Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> f ([Path Abs File], WalkStep)
collectFilesNamed name _ _ files = case findFileNamed name files of
  Just f -> pure ([f], WalkSkipAll)
  Nothing -> pure ([], WalkContinue)

fileHasSuffix :: Path a File -> [String] -> Bool
fileHasSuffix file = any (\suffix -> suffix `isSuffixOf` toString (filename file))

toUserDefDep :: Path Abs Dir -> Path Abs File -> JarMetadata -> SourceUserDefDep
toUserDefDep root file JarMetadata{..} = do
  let rel = tryMakeRelative root file
  SourceUserDefDep (toText rel) jarVersion jarLicense (Just jarName) Nothing (Just rel)
