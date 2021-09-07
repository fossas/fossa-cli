{-# LANGUAGE RecordWildCards #-}

module App.Fossa.VSI.IAT.Types (
  Fingerprint (..),
  UserDefinedAssertionMeta (..),
  UserDep (..),
  toUserDep,
  renderUserDep,
) where

import App.Fossa.VSI.Types qualified as VSI
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), withObject, (.:))
import Data.Text (Text)

-- | Fingerprint uniquely idenfies a file, derived from its content.
-- Fingerprints are backed by base16 representations of underlying data.
newtype Fingerprint = Fingerprint {unFingerprint :: Text}

instance ToJSON Fingerprint where
  toJSON = toJSON . unFingerprint

-- | UserDep is the minimal set of data required to lookup a UserDefinedAssertionMeta in FOSSA.
data UserDep = UserDep
  { userDepName :: Text
  , userDepVersion :: Text
  }

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
