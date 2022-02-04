{-# LANGUAGE DataKinds #-}

module App.Fossa.VPS.Scan.Core (
  createLocator,
  Locator (..),
) where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Data.String.Conversion (toText)
import Data.Text (Text)
import Prelude

data SherlockInfo = SherlockInfo
  { sherlockUrl :: Text
  , sherlockClientToken :: Text
  , sherlockClientId :: Text
  , sherlockOrgId :: Int
  }
  deriving (Eq, Ord, Show)

instance FromJSON SherlockInfo where
  parseJSON = withObject "SherlockInfo" $ \obj -> do
    auth <- obj .: "auth"
    SherlockInfo <$> obj .: "url" <*> auth .: "clientToken" <*> auth .: "clientId" <*> obj .: "orgId"

newtype Locator = Locator {unLocator :: Text}

createLocator :: Text -> Int -> Locator
createLocator projectName organizationId = Locator $ "custom+" <> toText (show organizationId) <> "/" <> projectName
