module App.Fossa.Config.Container.Common (
  ImageText (..),
  imageTextArg,
) where

import Data.Text (Text)
import Options.Applicative (Parser, argument, help, metavar, str)

newtype ImageText = ImageText
  { unImageText :: Text
  }
  deriving (Eq, Ord, Show)

imageTextArg :: Parser ImageText
imageTextArg = ImageText <$> argument str (metavar "IMAGE" <> help "The image to scan")
