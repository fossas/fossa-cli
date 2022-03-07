module App.Fossa.Config.Container.Common (
  ImageText (..),
  imageTextArg,
) where

import Data.Aeson (ToJSON (toEncoding), defaultOptions, genericToEncoding)
import Data.Text (Text)
import GHC.Generics (Generic)
import Options.Applicative (Parser, argument, help, metavar, str)

newtype ImageText = ImageText
  { unImageText :: Text
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON ImageText where
  toEncoding = genericToEncoding defaultOptions

imageTextArg :: Parser ImageText
imageTextArg = ImageText <$> argument str (metavar "IMAGE" <> help "The image to scan")
