{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module App.Fossa.BinaryDeps.Whl (resolveWhl) where

import Control.Algebra (Has)
import Control.Carrier.Diagnostics
  ( Diagnostics,
    ToDiagnostic (renderDiagnostic),
    context,
    errCtx,
    fromMaybeText,
    recover,
    warnOnErr,
  )
import Control.Carrier.Finally (runFinally)
import Control.Effect.Lift (Lift)
import Control.Monad (join)
import Data.List (find, isSuffixOf)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.String.Conversion (ToString (toString), ToText (toText))
import Data.Text (Text, isInfixOf)
import Data.Text qualified as Text
import Discovery.Archive (extractZip, withArchive)
import Effect.Logger (Logger, logDebug, pretty, viaShow)
import Effect.ReadFS (ReadFS, listDir, readContentsText)
import Errata (Errata (..))
import Path (Abs, Dir, File, Path, dirname, filename, mkRelFile, (</>))
import Path.Extra (renderRelative, tryMakeRelative)
import Srclib.Types (SourceUserDefDep (..), BinaryDiscoveredDep (..))
import DepTypes (DepType(PipType))

data WhlMetadata = WhlMetadata
  { whlName :: Text,
    whlVersion :: Text,
    whlLicense :: Text
  }

-- | Implement .whl resolution using a similar method to Ant analysis in CLIv1.
-- The overall idea is to:
--   1. Extract the whl to a temporary directory (it's a zip!)
--   2. Search inside for a file named `*.dist-info/METADATA` parse it and return metadata derived from it.
resolveWhl :: (Has (Lift IO) sig m, Has Diagnostics sig m, Has Logger sig m, Has ReadFS sig m) => Path Abs Dir -> Path Abs File -> m (Maybe BinaryDiscoveredDep)
resolveWhl _ file | not $ fileHasSuffix file [".whl"] = pure Nothing
resolveWhl root file = do
  let fileDescription = toText file
  logDebug $ "Inferring metadata from " <> pretty fileDescription
  result <- recover
    . warnOnErr (FailedToResolveWhl file)
    . errCtx (FailedToResolveWhlCtx file)
    . context ("Infer metadata from " <> fileDescription)
    . runFinally
    $ withArchive extractZip file
    $ \dir -> tacticMetadata dir
  pure $ fmap (toBinaryDiscoveredDep root file) (join result)

newtype FailedToResolveWhl = FailedToResolveWhl (Path Abs File)

instance ToDiagnostic FailedToResolveWhl where
  renderDiagnostic (FailedToResolveWhl path) = do
    let header = "Could not infer whl metadata (license, whl name, and version) from " <> toText path
    Errata (Just header) [] Nothing

newtype FailedToResolveWhlCtx = FailedToResolveWhlCtx (Path Abs File)

instance ToDiagnostic FailedToResolveWhlCtx where
  renderDiagnostic (FailedToResolveWhlCtx path) = do
    let header = "Ensure " <> toText path <> " is a valid whl file"
    Errata (Just header) [] Nothing

tacticMetadata :: (Has (Lift IO) sig m, Has Diagnostics sig m, Has Logger sig m, Has ReadFS sig m) => Path Abs Dir -> m WhlMetadata
tacticMetadata archive = context ("Parse metadata for " <> toText archive) $ do
  distInfoDir <- findDistInfoFolder archive
  let metadataPath = distInfoDir </> $(mkRelFile "METADATA")
  do
    content <- readContentsText metadataPath
    logDebug $ "Parsing METADATA file: " <> pretty (renderRelative archive metadataPath)
    metadataToWhlMeta $ parseMetadata content

findDistInfoFolder :: (Has ReadFS sig m, Has Diagnostics sig m, Has Logger sig m) => Path Abs Dir -> m (Path Abs Dir)
findDistInfoFolder archive = do
  (dirs, _) <- listDir archive
  logDebug $ "Listing folders in archive " <> viaShow (map (toText . dirname) dirs)
  logDebug $ "Listing folder matching in archive " <> viaShow (map (isInfixOf ".dist-info" . (toText . dirname)) dirs)
  fromMaybeText "Could not find *.dist-info folder in whl archive" $
    find (isInfixOf ".dist-info" . toText . dirname) dirs

parseMetadata :: Text -> Map Text Text
parseMetadata t = Map.fromList . map strip' . filter' $ map (Text.breakOn ":") (Text.lines t)
  where
    null' (a, b) = any Text.null [a, b]
    strip' (a, b) = (Text.strip a, Text.strip $ Text.drop 1 b)
    filter' = filter (not . null')

metadataToWhlMeta :: (Has Diagnostics sig m) => Map Text Text -> m WhlMetadata
metadataToWhlMeta manifest =
  WhlMetadata
    <$> fromMaybeText "Missing whl name" (Map.lookup "Name" manifest)
    <*> fromMaybeText "Missing whl version" (Map.lookup "Version" manifest)
    <*> fromMaybeText "Missing whl license" (Map.lookup "License-Expression" manifest)

fileHasSuffix :: Path a File -> [String] -> Bool
fileHasSuffix file = any (\suffix -> suffix `isSuffixOf` toString (filename file))

toBinaryDiscoveredDep :: Path Abs Dir -> Path Abs File -> WhlMetadata -> BinaryDiscoveredDep
toBinaryDiscoveredDep root file WhlMetadata {..} = do
  let rel = tryMakeRelative root file
  LocatorDep (PipType, SourceUserDefDep whlName whlVersion whlLicense (Just whlName) Nothing (Just rel))
