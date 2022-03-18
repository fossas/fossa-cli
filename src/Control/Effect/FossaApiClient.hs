module Control.Effect.FossaApiClient (
  FossaApiClientF (..),
  FossaApiClient,
  getApiOpts,
  getOrganization,
) where

import Control.Algebra (Has)
import Control.Carrier.Simple (Simple, sendSimple)
import Fossa.API.Types (Organization, ApiOpts)

data FossaApiClientF a where
  GetOrganization :: FossaApiClientF Organization
  GetApiOpts :: FossaApiClientF ApiOpts 

type FossaApiClient = Simple FossaApiClientF

-- | Fetches the organization associated with the current API token
getOrganization :: (Has FossaApiClient sig m) => m Organization
getOrganization = sendSimple GetOrganization

-- | Returns the API options currently in scope.
-- The API options contain a lot of information required to build URLs.
getApiOpts :: (Has FossaApiClient sig m) => m ApiOpts
getApiOpts = sendSimple GetApiOpts
