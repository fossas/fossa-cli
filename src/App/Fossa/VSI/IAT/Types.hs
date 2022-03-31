{-# LANGUAGE RecordWildCards #-}

module App.Fossa.VSI.IAT.Types (
  UserDefinedAssertionMeta (..),
  UserDep (..),
  toUserDep,
  renderUserDep,
) where

import App.Fossa.VSI.Types qualified as VSI
import Data.Aeson (FromJSON (parseJSON), ToJSON (toEncoding), defaultOptions, genericToEncoding, withObject, (.:))
import Data.Text (Text)
import GHC.Generics (Generic)

-- | UserDep is the minimal set of data required to lookup a UserDefinedAssertionMeta in FOSSA.
data UserDep = UserDep
  { userDepName :: Text
  , userDepVersion :: Text
  }
  deriving (Eq, Ord, Show)

renderUserDep :: UserDep -> Text
renderUserDep UserDep{..} = VSI.userDefinedFetcher <> "+" <> userDepName <> "$" <> userDepVersion

-- | User provided data to assert a binary via IAT.
data UserDefinedAssertionMeta = UserDefinedAssertionMeta
  { assertedName :: Text
  , assertedVersion :: Text
  , assertedLicense :: Text
  , assertedDescription :: Maybe Text
  , assertedUrl :: Maybe Text
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON UserDefinedAssertionMeta where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON UserDefinedAssertionMeta where
  parseJSON = withObject "UserDefinedAssertionMetadata" $ \obj -> do
    UserDefinedAssertionMeta
      <$> obj .: "name"
      <*> obj .: "version"
      <*> obj .: "license"
      <*> obj .: "description"
      <*> obj .: "url"

toUserDep :: VSI.Locator -> UserDep
toUserDep VSI.Locator{..} = UserDep locatorProject locatorRevision
