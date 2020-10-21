{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module App.Fossa.VPS.Types
( FilterExpressions(..)
, DepsTarget(..)
, DepsDependency(..)
, runHTTP
, encodeFilterExpressions
, HTTP(..)
, HTTPRequestFailed(..)
, VPSOpts (..)
, PartialVPSOpts (..)
, FossaOpts (..)
, NinjaGraphOpts (..)
) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Carrier.Diagnostics
import Control.Effect.Lift (Lift, sendIO)
import Text.URI (URI)
import Data.Text (Text)
import Data.Aeson
import Network.HTTP.Req
import Data.Text.Prettyprint.Doc (viaShow)
import qualified Data.ByteString.Lazy as BSL
import Data.Text.Encoding (decodeUtf8)

newtype FilterExpressions = FilterExpressions { unFilterExpressions :: [Text] }

encodeFilterExpressions :: FilterExpressions -> Text
encodeFilterExpressions filters = decodeUtf8 $ BSL.toStrict $ encode (unFilterExpressions filters)

instance FromJSON FilterExpressions where
  parseJSON = withObject "CoreFilterExpressions" $ \obj -> FilterExpressions <$> obj .: "filters"

instance ToJSON FilterExpressions where
  toJSON (FilterExpressions filters) = object ["filters" .= filters]

-- FIXME: replace these with non-CLI types
-- VPSOpts in particular is used as a God type, and is very unwieldy in the merged CLI form.
data FossaOpts = FossaOpts
  { fossaUrl :: URI
  , fossaApiKey :: Text
  }

data PartialVPSOpts
  = PartialVPSOpts
    { fossaOpts :: FossaOpts,
      partSkipIprScan :: Bool,
      partFileFilter :: FilterExpressions
    }

data VPSOpts = VPSOpts
  { fossa :: FossaOpts
  , vpsProjectName :: Text
  , userProvidedRevision :: Maybe Text  -- FIXME: Since we can now infer a revision, we should rename this field.
  , skipIprScan :: Bool
  , fileFilter :: FilterExpressions
  }
-- end FIXME

data DepsTarget = DepsTarget
  { targetPath :: Text
  , targetDependencies :: [DepsDependency]
  , targetInputs :: [DepsDependency]
  , targetComponentName :: Maybe Text
  } deriving (Eq, Ord, Show)

instance ToJSON DepsTarget where
  toJSON DepsTarget{..} = object
    [ "path" .= targetPath
    , "dependencies" .= targetDependencies
    , "inputs" .= targetInputs
    , "componentName" .=  targetComponentName
    ]

data DepsDependency = DepsDependency
  { dependencyPath :: Text
  , dependencyComponentName :: Maybe Text
  , hasDependencies :: Bool
  } deriving (Eq, Ord, Show)

instance ToJSON DepsDependency where
  toJSON DepsDependency{..} = object
    [ "path" .= dependencyPath
    , "componentName" .= dependencyComponentName
    , "hasDependencies" .= hasDependencies
    ]

data NinjaGraphOpts = NinjaGraphOpts
  { ninjaFossaOpts :: FossaOpts
  , ninjaGraphNinjaPath :: Maybe FilePath
  , lunchTarget :: Maybe Text
  , scanId :: Text
  , ninjaProjectName :: Text
  , buildName :: Text
  }

newtype HTTP m a = HTTP {unHTTP :: m a}
  deriving (Functor, Applicative, Monad, Algebra sig)

instance Has (Lift IO) sig m => MonadIO (HTTP m) where
  liftIO = sendIO

newtype HTTPRequestFailed = HTTPRequestFailed HttpException deriving (Show)

instance ToDiagnostic HTTPRequestFailed where
  renderDiagnostic (HTTPRequestFailed exc) = "An HTTP request failed: " <> viaShow exc

instance (Has (Lift IO) sig m, Has Diagnostics sig m) => MonadHttp (HTTP m) where
  handleHttpException = HTTP . fatal . HTTPRequestFailed

runHTTP :: HTTP m a -> m a
runHTTP = unHTTP
