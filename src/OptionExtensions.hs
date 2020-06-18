module OptionExtensions (uriOption) where

import Options.Applicative
import Prologue
import qualified Data.Text as T
import Text.URI (URI, mkURI)

uriOption :: Mod OptionFields URI -> Parser URI
uriOption = option parseUri
  where
    parseUri :: ReadM URI
    parseUri = maybeReader (mkURI . T.pack)
