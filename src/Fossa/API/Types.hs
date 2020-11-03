{-# LANGUAGE DataKinds #-}

module Fossa.API.Types
  ( ApiKey (..),
    ApiOpts (..),
    useApiOpts,
  )
where

import Control.Effect.Diagnostics
import Data.Coerce (coerce)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Req
import Text.URI (URI, render)
import qualified Unsafe.Coerce as Unsafe

newtype ApiKey = ApiKey {unApiKey :: Text}
  deriving (Eq, Ord, Show)

data ApiOpts = ApiOpts
  { apiOptsUri :: URI,
    apiOptsApiKey :: ApiKey
  }
  deriving (Eq, Ord, Show)

-- | parse a URI for use as a base Url, along with some default options (auth, port, ...)
useApiOpts :: Has Diagnostics sig m => ApiOpts -> m (Url 'Https, Option 'Https)
useApiOpts opts = case useURI (apiOptsUri opts) of
  Nothing -> fatalText ("Invalid URL: " <> render (apiOptsUri opts))
  -- Url is "type role Url nominal" in the scheme (Http/Https), so we have to
  -- unsafeCoerce @Url 'Http@ into @Url 'Https@. Options isn't nominal in the
  -- scheme, so we can coerce as usual.
  Just (Left (url, options)) -> pure (Unsafe.unsafeCoerce url, coerce options <> authHeader (apiOptsApiKey opts))
  Just (Right (url, options)) -> pure (url, options <> authHeader (apiOptsApiKey opts))

authHeader :: ApiKey -> Option 'Https
authHeader key = header "Authorization" (encodeUtf8 ("Bearer " <> unApiKey key))
