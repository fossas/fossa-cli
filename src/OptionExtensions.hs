module OptionExtensions
  (urlOption)
where

import Prologue
import Options.Applicative
import qualified Data.Text as T
import Network.HTTP.Req (Url, useURI)
import Text.URI (mkURI)

urlOption :: Mod OptionFields (Url scheme) -> Parser (Url scheme)
urlOption = option parseUrl
  where
    parseUrl :: ReadM (Url scheme)
    parseUrl = maybeReader (\s -> mkURI (T.pack s) >>= useURI >>=
                              \case
                                 Left (url,_) -> pure $ coerce url
                                 Right (url,_) -> pure $ coerce url)