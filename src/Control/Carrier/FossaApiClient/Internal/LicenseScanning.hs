{-# LANGUAGE RecordWildCards #-}

module Control.Carrier.FossaApiClient.Internal.LicenseScanning (
  getSignedFirstPartyScanUrl,
  getSignedLicenseScanUrl,
  finalizeLicenseScan,
  uploadLicenseScanResult,
  uploadFirstPartyScanResult,
  uploadPathDependencyScanResult,
  finalizePathDependencyScan,
  alreadyAnalyzedPathRevision,
) where

import App.Types (FileUpload, ProjectRevision)
import Control.Algebra (Has)
import Control.Carrier.FossaApiClient.Internal.FossaAPIV1 qualified as API
import Control.Effect.FossaApiClient (PackageRevision (..))
import Control.Effect.Reader (Reader, ask)
import Control.Monad (void)
import Data.List.NonEmpty qualified as NE
import Fossa.API.Types (AnalyzedPathDependenciesResp (analyzedPathDeps), AnalyzedPathDependency, ApiOpts, ArchiveComponents, PathDependencyUpload, SignedURL)
import Srclib.Types (FullSourceUnit, LicenseSourceUnit, Locator)

getSignedFirstPartyScanUrl ::
  ( API.APIClientEffs sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  PackageRevision ->
  m SignedURL
getSignedFirstPartyScanUrl PackageRevision{..} = do
  apiOpts <- ask
  API.getSignedFirstPartyScanURL apiOpts packageVersion packageName

getSignedLicenseScanUrl ::
  ( API.APIClientEffs sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  PackageRevision ->
  m SignedURL
getSignedLicenseScanUrl PackageRevision{..} = do
  apiOpts <- ask
  API.getSignedLicenseScanURL apiOpts packageVersion packageName

finalizeLicenseScan ::
  ( API.APIClientEffs sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  ArchiveComponents ->
  m ()
finalizeLicenseScan components = do
  apiOpts <- ask
  void $ API.licenseScanFinalize apiOpts components

uploadLicenseScanResult ::
  (API.APIClientEffs sig m) =>
  SignedURL ->
  LicenseSourceUnit ->
  m ()
uploadLicenseScanResult signedUrl licenseSourceUnit = do
  void $ API.licenseScanResultUpload signedUrl licenseSourceUnit

uploadFirstPartyScanResult ::
  (API.APIClientEffs sig m) =>
  SignedURL ->
  NE.NonEmpty FullSourceUnit ->
  m ()
uploadFirstPartyScanResult signedUrl fullSourceUnits = do
  void $ API.firstPartyScanResultUpload signedUrl fullSourceUnits

uploadPathDependencyScanResult ::
  ( API.APIClientEffs sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  PackageRevision ->
  ProjectRevision ->
  FileUpload ->
  m PathDependencyUpload
uploadPathDependencyScanResult PackageRevision{..} projectRevision fullFileUpload = do
  apiOpts <- ask
  API.getUploadURLForPathDependency apiOpts packageName packageVersion projectRevision fullFileUpload

finalizePathDependencyScan ::
  ( API.APIClientEffs sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  [Locator] ->
  Bool ->
  m ()
finalizePathDependencyScan locators forceRebuild = do
  apiOpts <- ask
  void $ API.finalizePathDependencyScan apiOpts locators forceRebuild

alreadyAnalyzedPathRevision ::
  ( API.APIClientEffs sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  ProjectRevision ->
  m [AnalyzedPathDependency]
alreadyAnalyzedPathRevision projectRevision = do
  apiOpts <- ask
  analyzedPathDeps <$> API.alreadyAnalyzedPathRevision apiOpts projectRevision
