module OptionExtensions
  (urlOption, UrlOption(..))
where

import Prologue
import Options.Applicative
import qualified Data.Text as T
import Network.HTTP.Req (Url, useURI, Scheme( Https ), Option)
import Text.URI (mkURI)

data UrlOption = UrlOption
  { urlOptionUrl :: Url 'Https
  , urlOptionOptions :: Option 'Https
  }
  deriving Generic

urlOption :: Mod OptionFields UrlOption -> Parser UrlOption
urlOption = option parseUrl
  where
    parseUrl :: ReadM UrlOption
    parseUrl = maybeReader (\s -> mkURI (T.pack s) >>= useURI >>=
                              \case
                                 Left (url, opts) -> pure (UrlOption (coerce url) opts)
                                 Right (url, opts) -> pure (UrlOption (coerce url) opts))
