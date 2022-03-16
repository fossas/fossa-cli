module Control.Effect.FossaApiClient (
  FossaApiClientF (..),
  FossaApiClient,
  getProject,
) where

import App.Types ( ProjectRevision )
import Control.Algebra
import Control.Carrier.Simple
import Fossa.API.Types ( Project )

data FossaApiClientF a where
  GetProject :: ProjectRevision -> FossaApiClientF Project

type FossaApiClient = Simple FossaApiClientF

getProject :: (Has FossaApiClient sig m) => ProjectRevision -> m Project
getProject = sendSimple . GetProject 
