module Effect.FossaAPI (
  Project(..),
  FossaAPIF(..),
  FossaAPI,
  getProject
) where

import Data.Text (Text)
import App.Types
import Fossa.API.Types
import Control.Algebra
import Control.Carrier.Simple
import Data.Aeson

data Project = Project
  { projectId :: Text
  , projectTitle :: Text
  , projectIsMonorepo :: Bool
  }
  deriving (Eq, Ord, Show)

instance FromJSON Project where
  parseJSON = withObject "Project" $ \obj ->
    Project <$> obj .: "id"
      <*> obj .: "title"
      <*> obj .: "isMonorepo"


data FossaAPIF a where
  GetProject :: ApiOpts -> ProjectRevision -> FossaAPIF Project

type FossaAPI = Simple FossaAPIF

getProject :: (Has FossaAPI sig m) => ApiOpts -> ProjectRevision -> m Project
getProject opts = sendSimple . GetProject opts
