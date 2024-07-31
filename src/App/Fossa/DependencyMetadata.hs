module App.Fossa.DependencyMetadata (
  DependencyMetadata (..),
) where

import Data.Aeson (FromJSON (..), withObject, (.:?))
import Data.Aeson.Extra (forbidMembers)
import Data.Text (Text)

data DependencyMetadata = DependencyMetadata
  { depDescription :: Maybe Text
  , depHomepage :: Maybe Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON DependencyMetadata where
  parseJSON = withObject "metadata" $ \obj ->
    DependencyMetadata
      <$> obj .:? "description"
      <*> obj .:? "homepage"
      <* forbidMembers "metadata" ["url"] obj
