module Control.Effect.FossaApiClient (
  FossaApiClientF (..),
  FossaApiClient,
  getAttribution,
) where

import App.Fossa.Report.Attribution (Attribution)
import App.Types (ProjectRevision)
import Control.Algebra (Has)
import Control.Carrier.Simple (Simple, sendSimple)

data FossaApiClientF a where
  GetAttribution :: ProjectRevision -> FossaApiClientF Attribution

type FossaApiClient = Simple FossaApiClientF

getAttribution :: (Has FossaApiClient sig m) => ProjectRevision -> m Attribution
getAttribution = sendSimple . GetAttribution
