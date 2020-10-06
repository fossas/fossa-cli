module App.OptionExtensions (uriOption, jsonOption) where

import qualified Data.Text as T
import Options.Applicative
import Text.URI (URI, mkURI)
import Data.Aeson
import Data.String

uriOption :: Mod OptionFields URI -> Parser URI
uriOption = option parseUri
  where
    parseUri :: ReadM URI
    parseUri = maybeReader (mkURI . T.pack)

jsonOption :: FromJSON a => Mod OptionFields a -> Parser a
jsonOption = option (eitherReader (eitherDecode . fromString))
