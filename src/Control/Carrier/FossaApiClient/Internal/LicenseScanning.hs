{-# LANGUAGE RecordWildCards #-}
module Control.Carrier.FossaApiClient.Internal.LicenseScanning (
  getSignedLicenseScanUrl, finalizeLicenseScan, uploadLicenseScanResult
  ) where

import Control.Effect.FossaApiClient (PackageRevision (..))
import Fossa.API.Types (SignedURL, ApiOpts, ArchiveComponents)
import Control.Effect.Reader (Reader, ask)
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift (Lift)
import Srclib.Types (LicenseSourceUnit)
import qualified App.Fossa.FossaAPIV1 as API
import Control.Monad (void)


getSignedLicenseScanUrl ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  PackageRevision -> m SignedURL
getSignedLicenseScanUrl PackageRevision{..} = do
  apiOpts <- ask
  API.getSignedLicenseScanURL apiOpts packageVersion packageName

finalizeLicenseScan ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has (Reader ApiOpts) sig m
  ) =>
  ArchiveComponents -> m ()
finalizeLicenseScan components = do
  apiOpts <- ask
  void $ API.licenseScanFinalize apiOpts components

uploadLicenseScanResult ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  ) =>
  SignedURL -> LicenseSourceUnit -> m ()
uploadLicenseScanResult signedUrl licenseSourceUnit = do
  void $ API.licenseScanResultUpload signedUrl licenseSourceUnit
