module App.Util
( validateDir
, parseUri
) where

import Prologue

import App.Types
import Control.Carrier.Diagnostics
import qualified Path.IO as P
import System.Exit (die)
import Text.URI (URI, render)
import Network.HTTP.Req
import qualified Unsafe.Coerce as Unsafe

-- | Validate that a filepath points to a directory and the directory exists
validateDir :: FilePath -> IO BaseDir
validateDir dir = do
  absolute <- P.resolveDir' dir
  exists <- P.doesDirExist absolute

  unless exists (die $ "ERROR: Directory " <> show absolute <> " does not exist")
  pure $ BaseDir absolute

-- parse a URI for use as a (base) Url, along with some default Options (e.g., port)
parseUri :: Has Diagnostics sig m => URI -> m (Url 'Https, Option 'Https)
parseUri uri = case useURI uri of
  Nothing -> fatalText ("Invalid URL: " <> render uri)
  -- Url is "type role Url nominal" in the scheme (Http/Https), so we have to
  -- unsafeCoerce @Url 'Http@ into @Url 'Https@. Options isn't nominal in the
  -- scheme, so we can coerce as usual.
  Just (Left (url, options)) -> pure (Unsafe.unsafeCoerce url, coerce options)
  Just (Right (url, options)) -> pure (url, options)
