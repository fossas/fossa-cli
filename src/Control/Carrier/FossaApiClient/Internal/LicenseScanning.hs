{-# LANGUAGE RecordWildCards #-}

module Control.Carrier.FossaApiClient.Internal.LicenseScanning (
  getSignedLicenseScanUrl,
  finalizeLicenseScan,
  uploadLicenseScanResult,
  uploadFirstPartyScanResult,
) where

import Control.Algebra (Has)
import Control.Carrier.FossaApiClient.Internal.FossaAPIV1 qualified as API
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.FossaApiClient (PackageRevision (..))
import Control.Effect.Lift (Lift)
import Control.Effect.Reader (Reader, ask)
import Control.Monad (void)
import Fossa.API.Types (ApiOpts, ArchiveComponents, SignedURL)
import Srclib.Types (LicenseSourceUnit, FullSourceUnit)
import qualified Data.List.NonEmpty as NE

getSignedLicenseScanUrl ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  PackageRevision ->
  m SignedURL
getSignedLicenseScanUrl PackageRevision{..} = do
  apiOpts <- ask
  API.getSignedLicenseScanURL apiOpts packageVersion packageName

finalizeLicenseScan ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
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
