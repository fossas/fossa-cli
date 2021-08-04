module App.OptionExtensions (uriOption, jsonOption) where

import Data.Aeson
import Data.String
import Data.String.Conversion (toText)
import Options.Applicative
import Text.URI (URI, mkURI)

uriOption :: Mod OptionFields URI -> Parser URI
uriOption = option parseUri
  where
    parseUri :: ReadM URI
    parseUri = maybeReader (mkURI . toText)

jsonOption :: FromJSON a => Mod OptionFields a -> Parser a
jsonOption = option (eitherReader (eitherDecode . fromString))
