{-# LANGUAGE RecordWildCards #-}

{- HLINT ignore "Redundant id" -}

module App.Fossa.BinaryDeps.Nupkg (resolveNupkg) where

import Control.Algebra (Has)
import Control.Applicative (optional)
import Control.Carrier.Diagnostics (
  Diagnostics,
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
import Data.Maybe (fromMaybe)
import Data.String.Conversion (ToString (toString), ToText (toText))
import Data.Text (Text)
import DepTypes (DepType (NuGetType))
import Discovery.Archive (extractZip, withArchive)
import Effect.Logger (Logger, logDebug, pretty, viaShow)
import Effect.ReadFS (ReadFS, listDir, readContentsXML)
import Errata (Errata (..))
import Parse.XML (FromXML (..), child)
import Path (Abs, Dir, File, Path, filename)
import Path.Extra (renderRelative, tryMakeRelative)
import Srclib.Types (BinaryDiscoveredDep (..), SourceUserDefDep (..))
import Prelude hiding (id)

data NupkgMetadata = NupkgMetadata
  { nupkgName :: Text
  , nupkgVersion :: Text
  , nupkgLicense :: Text
  }

-- | Implement .nupkg resolution using a similar method to Ant analysis in CLIv1.
-- The overall idea is to:
--   1. Extract the nupkg to a temporary directory (it's a zip!)
--   2. Search inside for a file named `*.nuspec` parse it and return metadata derived from it.
resolveNupkg :: (Has (Lift IO) sig m, Has Diagnostics sig m, Has Logger sig m, Has ReadFS sig m) => Path Abs Dir -> Path Abs File -> m (Maybe BinaryDiscoveredDep)
resolveNupkg _ file | not $ fileHasSuffix file [".nupkg"] = pure Nothing
resolveNupkg root file = do
  let fileDescription = toText file
  logDebug $ "Inferring metadata from " <> pretty fileDescription
  result <- recover
    . warnOnErr (FailedToResolveNupkg file)
    . errCtx (FailedToResolveNupkgCtx file)
    . context ("Infer metadata from " <> fileDescription)
    . runFinally
    $ withArchive extractZip file
    $ \dir -> tacticNuspec dir
  pure $ fmap (toBinaryDiscoveredDep root file) (join result)

newtype FailedToResolveNupkg = FailedToResolveNupkg (Path Abs File)

instance ToDiagnostic FailedToResolveNupkg where
  renderDiagnostic (FailedToResolveNupkg path) = do
    let header = "Could not infer nupkg metadata (license, nupkg name, and version) from " <> toText path
    Errata (Just header) [] Nothing

newtype FailedToResolveNupkgCtx = FailedToResolveNupkgCtx (Path Abs File)

instance FromXML Nuspec where
  parseElement el = do
    metadata <- child "metadata" el
    Nuspec
      <$> optional (child "license" metadata)
      <*> optional (child "id" metadata)
      <*> optional (child "version" metadata)

data Nuspec = Nuspec
  { license :: Maybe Text
  , id :: Maybe Text
  , version :: Maybe Text
  }
  deriving (Eq, Ord, Show)

instance ToDiagnostic FailedToResolveNupkgCtx where
  renderDiagnostic (FailedToResolveNupkgCtx path) = do
    let header = "Ensure " <> toText path <> " is a valid nupkg file"
    Errata (Just header) [] Nothing

tacticNuspec :: (Has (Lift IO) sig m, Has Diagnostics sig m, Has Logger sig m, Has ReadFS sig m) => Path Abs Dir -> m NupkgMetadata
tacticNuspec archive = context ("Parse metadata for " <> toText archive) $ do
  nuspecPath <- findNuspecFile archive
  do
    nuspec <- readContentsXML @Nuspec nuspecPath
    logDebug $ "Parsing Nuspec file: " <> pretty (renderRelative archive nuspecPath)
    nuspecToNupkgMeta nuspec

findNuspecFile :: (Has ReadFS sig m, Has Diagnostics sig m, Has Logger sig m) => Path Abs Dir -> m (Path Abs File)
findNuspecFile archive = do
  (_, files) <- listDir archive
  logDebug $ "Listing files in archive " <> viaShow (map filename files)
  logDebug $ "Listing files in archive " <> viaShow (map (\f -> fileHasSuffix (filename f) [".nuspec"]) files)
  fromMaybeText "Could not find nuspec file in nupkg archive" $
    find (\f -> fileHasSuffix (filename f) [".nuspec"]) files

nuspecToNupkgMeta :: (Has Diagnostics sig m) => Nuspec -> m NupkgMetadata
nuspecToNupkgMeta nuspec =
  NupkgMetadata
    <$> fromMaybeText "Missing nupkg name" (id nuspec)
    <*> fromMaybeText "Missing nupkg version" (version nuspec)
    <*> fromMaybeText "Missing nupkg license" (Just (fromMaybe "" (license nuspec)))

fileHasSuffix :: Path a File -> [String] -> Bool
fileHasSuffix file = any (\suffix -> suffix `isSuffixOf` toString (filename file))

toBinaryDiscoveredDep :: Path Abs Dir -> Path Abs File -> NupkgMetadata -> BinaryDiscoveredDep
toBinaryDiscoveredDep root file NupkgMetadata{..} = do
  let rel = tryMakeRelative root file
  LocatorDep (NuGetType, SourceUserDefDep nupkgName nupkgVersion nupkgLicense (Just nupkgName) Nothing (Just rel))
