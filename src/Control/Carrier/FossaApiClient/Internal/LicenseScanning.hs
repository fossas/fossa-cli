{-# LANGUAGE RecordWildCards #-}

module Control.Carrier.FossaApiClient.Internal.LicenseScanning (
  getSignedLicenseScanUrl,
  finalizeLicenseScan,
  uploadLicenseScanResult,
) where

import App.Fossa.FossaAPIV1 qualified as API
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.FossaApiClient (PackageRevision (..))
import Control.Effect.Lift (Lift)
import Control.Effect.Reader (Reader, ask)
import Control.Monad (void)
import Fossa.API.Types (ApiOpts, ArchiveComponents, SignedURL)
import Srclib.Types (LicenseSourceUnit)

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
