module Test.MockApiExpectations (
  expectGetApiOpts,
  expectGetOrganization,
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
