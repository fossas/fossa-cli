module App.Fossa.VendoredDependency (
  VendoredDependency (..),
) where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import Data.Aeson.Extra (TextLike (unTextLike), forbidMembers)
import Data.Functor.Extra ((<$$>))
import Data.Text (Text)

data VendoredDependency = VendoredDependency
  { vendoredName :: Text
  , vendoredPath :: Text
  , vendoredVersion :: Maybe Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON VendoredDependency where
  parseJSON = withObject "VendoredDependency" $ \obj ->
    VendoredDependency <$> obj .: "name"
      <*> obj .: "path"
      <*> (unTextLike <$$> obj .:? "version")
      <* forbidMembers "vendored dependencies" ["type", "license", "url", "description"] obj
