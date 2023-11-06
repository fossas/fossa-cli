{-# LANGUAGE RecordWildCards #-}

module Control.Carrier.FossaApiClient.Internal.LicenseScanning (
  getSignedFirstPartyScanUrl,
  getSignedLicenseScanUrl,
  finalizeLicenseScan,
  uploadLicenseScanResult,
  uploadFirstPartyScanResult,
  uploadPathDependencyScanResult,
  finalizePathDependencyScan,
) where

import Control.Algebra (Has)
import Control.Carrier.FossaApiClient.Internal.FossaAPIV1 qualified as API
import Control.Effect.Debug (Debug)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.FossaApiClient (PackageRevision (..))
import Control.Effect.Lift (Lift)
import Control.Effect.Reader (Reader, ask)
import Control.Monad (void)
import Data.List.NonEmpty qualified as NE
import Fossa.API.Types (ApiOpts, ArchiveComponents, SignedURL, PathDependencyUpload)
import Srclib.Types (FullSourceUnit, LicenseSourceUnit, Locator)
import App.Types (FullFileUploads, ProjectRevision)

getSignedFirstPartyScanUrl ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Debug sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  PackageRevision ->
  m SignedURL
getSignedFirstPartyScanUrl PackageRevision{..} = do
  apiOpts <- ask
  API.getSignedFirstPartyScanURL apiOpts packageVersion packageName

getSignedLicenseScanUrl ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Debug sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  PackageRevision ->
  Bool ->
  m SignedURL
getSignedLicenseScanUrl PackageRevision{..} isPathDependency = do
  apiOpts <- ask
  API.getSignedLicenseScanURL apiOpts packageVersion packageName isPathDependency

finalizeLicenseScan ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Debug sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  ArchiveComponents ->
  m ()
finalizeLicenseScan components = do
  apiOpts <- ask
  void $ API.licenseScanFinalize apiOpts components

uploadLicenseScanResult ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  ) =>
  SignedURL ->
  LicenseSourceUnit ->
  m ()
uploadLicenseScanResult signedUrl licenseSourceUnit = do
  void $ API.licenseScanResultUpload signedUrl licenseSourceUnit

uploadFirstPartyScanResult ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  ) =>
  SignedURL ->
  NE.NonEmpty FullSourceUnit ->
  m ()
uploadFirstPartyScanResult signedUrl fullSourceUnits = do
  void $ API.firstPartyScanResultUpload signedUrl fullSourceUnits

uploadPathDependencyScanResult ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Debug sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  PackageRevision ->
  ProjectRevision ->
  FullFileUploads -> 
  m PathDependencyUpload
uploadPathDependencyScanResult PackageRevision{..} projectRevision fullFileUpload = do
  apiOpts <- ask
  API.getUploadURLForPathDependency apiOpts packageName packageVersion projectRevision fullFileUpload

finalizePathDependencyScan :: 
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Debug sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  [Locator] ->
  Bool -> 
  m ()
finalizePathDependencyScan locators forceRebuild = do
  apiOpts <- ask
  void $ API.finalizePathDependencyScan apiOpts locators forceRebuild