module Test.MockApiExpectations (
  expectGetApiOpts,
  expectGetOrganization,
  expectGetSignedUrl,
) where

import Control.Algebra (Has)
import Control.Effect.FossaApiClient qualified as API
import Test.Fixtures qualified as Fixtures
import Test.MockApi (MockApi, alwaysReturns)

expectGetApiOpts :: Has MockApi sig m => m ()
expectGetApiOpts =
  API.GetApiOpts `alwaysReturns` Fixtures.apiOpts

expectGetOrganization :: Has MockApi sig m => m ()
expectGetOrganization = API.GetOrganization `alwaysReturns` Fixtures.organization

expectGetSignedUrl :: Has MockApi sig m => API.PackageRevision -> m ()
expectGetSignedUrl packageRevision = API.GetSignedLicenseScanUrl packageRevision `alwaysReturns` Fixtures.signedUrl
