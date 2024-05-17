module App.Fossa.Config.SBOM.Common (
  ImageText (..),
  imageTextArg,
) where

import Data.Aeson (ToJSON (toEncoding), defaultOptions, genericToEncoding)
import Data.Text (Text)
import GHC.Generics (Generic)
import Options.Applicative (Parser, argument, metavar, str)
import Style (applyFossaStyle, stringToHelpDoc)

newtype ImageText = ImageText
  { unImageText :: Text
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ImageText where
  toEncoding = genericToEncoding defaultOptions

imageTextArg :: Parser ImageText
imageTextArg = ImageText <$> argument str (applyFossaStyle <> metavar "IMAGE" <> stringToHelpDoc "The image to scan")
