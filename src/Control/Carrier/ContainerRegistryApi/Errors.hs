module Control.Carrier.ContainerRegistryApi.Errors (
  ContainerRegistryApiErrorKind (..),
  ContainerRegistryApiErrorBody (..),
  UnknownApiError (..),
  FailedToParseAuthChallenge (..),
  errCodeToErrKind,
) where

import Data.Aeson (FromJSON (parseJSON), withObject, withText, (.:))
import Data.String.Conversion (toString)
import Data.Text (Text)
import Diag.Diagnostic (ToDiagnostic (..), renderDiagnostic)
import Network.HTTP.Types (Status (statusCode))
import Network.URI (URI)
import Prettyprinter (indent, line, pretty, vsep)

-- | OCI Registry Error Body.
-- Refer to: https://github.com/opencontainers/distribution-spec/blob/main/spec.md#error-codes
newtype ContainerRegistryApiErrorBody = ContainerRegistryApiErrorBody [ContainerRegistryApiError]
  deriving (Show, Eq, Ord)

data ContainerRegistryApiError = ContainerRegistryApiError ContainerRegistryApiErrorKind Text
  deriving (Show, Eq, Ord)

-- | OCI Registry API Error Kind.
--
-- Refer to:
--  * https://github.com/opencontainers/distribution-spec/blob/main/spec.md#error-codes
--  * https://github.com/distribution/distribution/blob/5cb406d511b7b9163bff9b6439072e4892e5ae3b/docs/spec/api.md#errors
data ContainerRegistryApiErrorKind
  = BlobUnknownError
  | ManifestUnknown
  | NameInvalid
  | NameUnknown
  | Unauthorized
  | Denied
  | Unsupported
  | OtherError
  deriving (Eq, Ord)

instance Show ContainerRegistryApiErrorKind where
  show BlobUnknownError = "BLOB_UNKNOWN"
  show ManifestUnknown = "MANIFEST_UNKNOWN"
  show NameInvalid = "NAME_INVALID"
  show NameUnknown = "NAME_UNKNOWN"
  show Unauthorized = "UNAUTHORIZED"
  show Denied = "DENIED"
  show Unsupported = "UNSUPPORTED"
  show OtherError = "OTHER_UNKNOWN_ERROR"

instance FromJSON ContainerRegistryApiErrorKind where
  parseJSON = withText "ErrorCode" $ \errCode -> pure $ errCodeToErrKind . toString $ errCode

errCodeToErrKind :: String -> ContainerRegistryApiErrorKind
errCodeToErrKind errorKind | show BlobUnknownError == errorKind = BlobUnknownError
errCodeToErrKind errorKind | show ManifestUnknown == errorKind = ManifestUnknown
errCodeToErrKind errorKind | show NameInvalid == errorKind = NameInvalid
errCodeToErrKind errorKind | show NameUnknown == errorKind = NameUnknown
errCodeToErrKind errorKind | show Unauthorized == errorKind = Unauthorized
errCodeToErrKind errorKind | show Denied == errorKind = Denied
errCodeToErrKind errorKind | show Unsupported == errorKind = Unsupported
errCodeToErrKind _ = OtherError

instance FromJSON ContainerRegistryApiErrorBody where
  parseJSON = withObject "ErrorEntry" $ \o ->
    ContainerRegistryApiErrorBody <$> o .: "errors"

instance FromJSON ContainerRegistryApiError where
  parseJSON = withObject "ErrorEntry" $ \o ->
    ContainerRegistryApiError <$> o .: "code" <*> o .: "message"

instance ToDiagnostic (URI, ContainerRegistryApiErrorBody) where
  renderDiagnostic (uri, ContainerRegistryApiErrorBody errs) =
    vsep
      [ pretty $ "Caught API Error From: " <> show uri
      , line
      , "API Errors:"
      , line
      , indent 4 $ vsep $ map renderDiagnostic errs
      ]

instance ToDiagnostic ContainerRegistryApiError where
  renderDiagnostic (ContainerRegistryApiError errKind msg) =
    vsep
      [ pretty $ "Error Code: " <> (show errKind)
      , pretty $ "Error Message: " <> msg
      , "-"
      ]

-- * Other Errors

data UnknownApiError = UnknownApiError URI Status

instance ToDiagnostic UnknownApiError where
  renderDiagnostic (UnknownApiError uri status) =
    vsep
      [ "Caught Unexpected Error From: "
      , indent 4 $ pretty $ "(" <> show (statusCode status) <> ") " <> show uri
      ]

newtype FailedToParseAuthChallenge = FailedToParseAuthChallenge Text

instance ToDiagnostic FailedToParseAuthChallenge where
  renderDiagnostic (FailedToParseAuthChallenge err) =
    vsep
      [ "Failed to parse authorization challenge: "
      , line
      , indent 4 $ pretty err
      ]
