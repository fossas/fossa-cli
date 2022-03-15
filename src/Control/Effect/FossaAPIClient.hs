module Control.Effect.FossaAPIClient (
  FossaAPIClientF(..),
  FossaAPIClient,
  getProject
) where

import App.Types
import Fossa.API.Types
import Control.Algebra
import Control.Carrier.Simple

data FossaAPIClientF a where
  GetProject :: ApiOpts -> ProjectRevision -> FossaAPIClientF Project

type FossaAPIClient = Simple FossaAPIClientF

getProject :: (Has FossaAPIClient sig m) => ApiOpts -> ProjectRevision -> m Project
getProject opts = sendSimple . GetProject opts
