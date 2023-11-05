{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Carrier.FossaApiClient.Internal.FossaAPIV1 (
  uploadAnalysis,
  uploadAnalysisWithFirstPartyLicenses,
  uploadContributors,
  uploadNativeContainerScan,
  mkMetadataOpts,
  fossaReq,
  getLatestBuild,
  getIssues,
  getOrganization,
  getAttribution,
  getAnalyzedRevisions,
  getRevisionDependencyCacheStatus,
  getSignedFirstPartyScanURL,
  getSignedLicenseScanURL,
  getSignedURL,
  getProject,
  archiveUpload,
  archiveBuildUpload,
  assertUserDefinedBinaries,
  assertRevisionBinaries,
  licenseScanFinalize,
  licenseScanResultUpload,
  resolveUserDefinedBinary,
  resolveProjectDependencies,
  vsiCreateScan,
  vsiAddFilesToScan,
  vsiCompleteScan,
  vsiScanAnalysisStatus,
  vsiDownloadInferences,
  renderLocatorUrl,
  getEndpointVersion,
  firstPartyScanResultUpload,
  getUploadURLForPathDependency,
  finalizePathDependencyScan,
) where

import App.Docs (fossaSslCertDocsUrl)
import App.Fossa.Config.Report
import App.Fossa.Config.Test (DiffRevision (DiffRevision))
import App.Fossa.Report.Attribution qualified as Attr
import App.Fossa.VSI.Fingerprint (Fingerprint, Raw)
import App.Fossa.VSI.Fingerprint qualified as Fingerprint
import App.Fossa.VSI.IAT.Types qualified as IAT
import App.Fossa.VSI.Types qualified as VSI
import App.Fossa.VendoredDependency (VendoredDependency (..), vendoredDepToLocator)
import App.Support (
  FossaEnvironment (FossaEnvironmentCloud),
  fossaEnvironment,
  reportCliBugErrorMsg,
  reportDefectMsg,
  reportDefectWithDebugBundle,
  reportFossaBugErrorMsg,
  reportNetworkErrorMsg,
  reportTransientErrorMsg,
  requestReportIfPersists,
 )
import App.Types (
  FullFileUploads (FullFileUploads),
  Policy (..),
  ProjectMetadata (..),
  ProjectRevision (..),
  ReleaseGroupMetadata (releaseGroupName, releaseGroupRelease),
  fullFileUploadsToCliLicenseScanType,
 )
import App.Version (versionNumber)
import Codec.Compression.GZip qualified as GZIP
import Container.Errors (EndpointDoesNotSupportNativeContainerScan (EndpointDoesNotSupportNativeContainerScan))
import Container.Types qualified as NativeContainer
import Control.Algebra (Algebra, Has, type (:+:))
import Control.Carrier.Empty.Maybe (Empty, EmptyC, runEmpty)
import Control.Effect.Debug (Debug, debugLog)
import Control.Effect.Diagnostics (Diagnostics, ToDiagnostic (..), context, fatal, fatalText, fromMaybeText)
import Control.Effect.Empty (empty)
import Control.Effect.Lift (Lift, sendIO)
import Control.Exception (Exception (displayException), SomeException)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (
  FromJSON (parseJSON),
  KeyValue ((.=)),
  ToJSON (toJSON),
  decodeStrict,
  encode,
  object,
  withObject,
  (.:),
 )
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C
import Data.ByteString.Lazy (ByteString)
import Data.Data (Proxy (Proxy))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.String.Conversion (decodeUtf8, toStrict, toString, toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64, Word8)
import Effect.Logger (
  AnsiStyle,
  Doc,
  Pretty (pretty),
  indent,
  newlinePreceding,
  newlineTrailing,
  vsep,
  (<+>),
 )
import Fossa.API.Types (
  ApiOpts,
  Archive,
  ArchiveComponents (ArchiveComponents),
  Build,
  Contributors,
  Issues,
  OrgId,
  Organization (Organization, orgRequiresFullFileUploads, orgSupportsIssueDiffs, orgSupportsNativeContainerScan, organizationId),
  Project,
  RevisionDependencyCache,
  SignedURL (signedURL),
  UploadResponse,
  useApiOpts, PathDependencyUpload,
 )
import Network.HTTP.Client (responseStatus)
import Network.HTTP.Client qualified as C
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Req (
  GET (GET),
  HttpBody (..),
  HttpException (..),
  LbsResponse,
  MonadHttp (..),
  NoReqBody (NoReqBody),
  Option,
  POST (POST),
  PUT (PUT),
  ReqBodyBs (ReqBodyBs),
  ReqBodyFile (ReqBodyFile),
  ReqBodyJson (ReqBodyJson),
  Scheme (Https),
  Url,
  bsResponse,
  httpMethodName,
  ignoreResponse,
  jsonResponse,
  lbsResponse,
  renderUrl,
  reqCb,
  responseBody,
  responseTimeout,
  useURI,
  (/:),
  (=:),
 )
import Network.HTTP.Req qualified as Req
import Network.HTTP.Req.Extra (httpConfigRetryTimeouts)
import Network.HTTP.Types (statusCode)
import Network.HTTP.Types qualified as HTTP
import Parse.XML (FromXML (..), child, parseXML, xmlErrorPretty)
import Path (File, Path, Rel, toFilePath)
import Prettyprinter (viaShow)
import Srclib.Types (
  FullSourceUnit,
  LicenseSourceUnit,
  Locator (..),
  SourceUnit,
  renderLocator,
 )
import System.FilePath (pathSeparator, splitDirectories)
import Text.URI qualified as URI

-- | Represents error emitted via FOSSA instance.
-- This data-shape corresponds to 'PublicFacingError' type in backend,
-- used for rendering errors from API endpoints, as well as FOSSA UI.
data FossaPublicFacingError = FossaPublicFacingError
  { fpeMessage :: Text
  , fpeUuid :: Text
  }
  deriving (Show, Eq, Ord)

instance FromJSON FossaPublicFacingError where
  parseJSON = withObject "FossaPublicFacingError" $ \v ->
    FossaPublicFacingError
      <$> v
        .: "message"
      <*> v
        .: "uuid"

newtype FossaReq m a = FossaReq {unFossaReq :: m a}
  deriving (Functor, Applicative, Monad, Algebra sig)

instance Has (Lift IO) sig m => MonadIO (FossaReq m) where
  liftIO = sendIO

instance (Has (Lift IO) sig m, Has Diagnostics sig m) => MonadHttp (FossaReq m) where
  getHttpConfig = pure httpConfigRetryTimeouts
  handleHttpException = FossaReq . fatal . mangleError

newtype FossaReqAllow401 m a = FossaReqAllow401 {unFossaReqAllow401 :: EmptyC m a}
  deriving (Functor, Applicative, Monad, Algebra (Empty :+: sig))

instance Has (Lift IO) sig m => MonadIO (FossaReqAllow401 m) where
  liftIO = sendIO

instance (Has (Lift IO) sig m, Has Diagnostics sig m) => MonadHttp (FossaReqAllow401 m) where
  getHttpConfig = pure httpConfigRetryTimeouts
  handleHttpException = FossaReqAllow401 . allow401
    where
      allow401 :: HttpException -> EmptyC m a
      allow401 err = maybe empty fatal (allow401' err)

data GetAnalyzedRevisionsBody = GetAnalyzedRevisionsBody
  { getAnalyzedRevisionsBodyLocators :: NonEmpty Text
  , getAnalyzedRevisionsBodyFullFileUploads :: Bool
  }
  deriving (Eq, Ord, Show)

instance ToJSON GetAnalyzedRevisionsBody where
  toJSON GetAnalyzedRevisionsBody{..} =
    object
      [ "locators" .= getAnalyzedRevisionsBodyLocators
      , "fullFileUploads" .= getAnalyzedRevisionsBodyFullFileUploads
      ]

fossaReq :: FossaReq m a -> m a
fossaReq = unFossaReq

fossaReqAllow401 :: FossaReqAllow401 m a -> EmptyC m a
fossaReqAllow401 = unFossaReqAllow401

-- allow401 is implemented due to the FOSSA API returning 401 status codes when we attempt to queue a build
-- that already exists. This function prevents us from erroring.
allow401' :: HttpException -> Maybe FossaError
allow401' err = case err of
  VanillaHttpException (HTTP.HttpExceptionRequest _ (HTTP.StatusCodeException resp _)) ->
    case HTTP.responseStatus resp of
      HTTP.Status 401 _ -> Nothing
      _ -> Just $ mangleError err
  _ -> Just $ mangleError err

-- Don't send any version if one doesn't exist
cliVersion :: Text
cliVersion = fromMaybe "" versionNumber

uploadUrl :: Url scheme -> Url scheme
uploadUrl baseurl = baseurl /: "api" /: "builds" /: "custom"

uploadWithFirstPartyUrl :: Url scheme -> Url scheme
uploadWithFirstPartyUrl baseurl = baseurl /: "api" /: "builds" /: "custom_with_first_party"

-- | This renders an organization + locator into a path piece for the fossa API
renderLocatorUrl :: OrgId -> Locator -> Text
renderLocatorUrl orgId Locator{..} =
  locatorFetcher <> "+" <> toText (show orgId) <> "/" <> normalizeGitProjectName locatorProject <> "$" <> fromMaybe "" locatorRevision

-- | The fossa backend treats http git locators in a specific way for the issues and builds endpoints.
-- This normalizes a project name to conform to what the API expects
normalizeGitProjectName :: Text -> Text
normalizeGitProjectName project
  | "http" `Text.isPrefixOf` project = dropPrefix "http://" . dropPrefix "https://" . dropSuffix ".git" $ project
  | otherwise = project
  where
    -- like Text.stripPrefix, but with a non-Maybe result (defaults to the original text)
    dropPrefix :: Text -> Text -> Text
    dropPrefix pre txt = fromMaybe txt (Text.stripPrefix pre txt)

    -- like Text.stripSuffix, but with a non-Maybe result (defaults to the original text)
    dropSuffix :: Text -> Text -> Text
    dropSuffix suf txt = fromMaybe txt (Text.stripSuffix suf txt)

responseTimeoutSeconds :: Int -> Option scheme
responseTimeoutSeconds sec = responseTimeout $ sec * 1_000_000

data FossaError
  = JsonDeserializeError String
  | InternalException SomeException
  | BackendPublicFacingError FossaPublicFacingError
  | InvalidUrlError String String
  | StatusCodeError HTTP.Request (HTTP.Response ())
  | TooManyRedirectsError [(HTTP.Request, HTTP.Response ByteString)]
  | OverlongHeadersError HTTP.Request
  | ResponseTimeoutError HTTP.Request
  | ConnectionTimeoutError HTTP.Request
  | ConnectionFailureError HTTP.Request SomeException
  | InvalidStatusLineError HTTP.Request String
  | InvalidResponseHeaderError HTTP.Request String
  | InvalidRequestHeaderError HTTP.Request String
  | ProxyConnectError HTTP.Request String Int HTTP.Status
  | NoResponseDataError HTTP.Request
  | TlsNotSupportedError HTTP.Request
  | WrongRequestBodyStreamSizeError HTTP.Request Word64 Word64
  | ResponseBodyTooShortError HTTP.Request Word64 Word64
  | InvalidChunkHeadersError HTTP.Request
  | IncompleteHeadersError HTTP.Request
  | InvalidDestinationHostError HTTP.Request
  | HttpZlibError HTTP.Request String
  | InvalidProxyEnvironmentVariableError HTTP.Request Text Text
  | ConnectionClosedError HTTP.Request
  | InvalidProxySettingsError HTTP.Request Text
  deriving (Show)

instance ToDiagnostic FossaError where
  renderDiagnostic = \case
    InternalException exception ->
      vsep
        [ "A socket-level error occurred when accessing the FOSSA API:"
        , ""
        , indent 4 $ pretty . displayException $ exception
        , ""
        , "These errors are usually related to TLS issues or the host being unreachable."
        , "For troubleshooting steps with TLS issues, please refer to:"
        , indent 4 $ pretty ("- " <> fossaSslCertDocsUrl)
        , ""
        , reportDefectMsg
        ]
    JsonDeserializeError err -> "An error occurred when deserializing a response from the FOSSA API:" <+> pretty err
    BackendPublicFacingError pfe ->
      vsep
        [ "The FOSSA endpoint reported an error:"
        , ""
        , indent 4 $ pretty . fpeMessage $ pfe
        , ""
        , "Error UUID from API:"
        , ""
        , indent 4 $ pretty . fpeUuid $ pfe
        , ""
        , reportDefectMsg
        , "Please include the error UUID in your request."
        ]
    InvalidUrlError url reason ->
      vsep
        [ "The URL provided is invalid."
        , ""
        , indent 4 $ "Provided:" <+> pretty url
        , indent 6 $ "Reason:" <+> pretty reason
        , ""
        , reportDefectMsg
        ]
    StatusCodeError ereq eres ->
      case statusCode $ responseStatus eres of
        403 ->
          vsep
            [ "The endpoint returned status code 403."
            , ""
            , "Typically, this status code indicates an authentication problem with the API."
            , "However, FOSSA reports invalid API keys using a different mechanism;"
            , "this likely means that some other service on your network intercepted the request"
            , "and reported this status code. This might be a proxy or some other network appliance."
            , ""
            , indent 4 $ "Request:" <+> renderRequest ereq
            , indent 4 $ "Response:" <+> renderResponse eres
            , ""
            , reportNetworkErrorMsg
            ]
        other ->
          vsep
            [ "The FOSSA endpoint returned an unexpected status code: " <> viaShow other
            , ""
            , "While HTTP responses typically come from the FOSSA API,"
            , "it's also possible that some other device on the network sent this response."
            , ""
            , "For a list of HTTP status codes and what they typically mean, see:"
            , "https://developer.mozilla.org/en-US/docs/Web/HTTP/Status."
            , ""
            , indent 4 $ "Request:" <+> renderRequest ereq
            , indent 4 $ "Response:" <+> renderResponse eres
            , ""
            , reportTransientErrorMsg
            ]
    TooManyRedirectsError txns ->
      newlineTrailing
        "Too many redirects were encountered when communicating with the FOSSA endpoint."
        <> "Network request log:"
        <> vsep (fmap (\(ereq, eres) -> indent 4 $ renderRequestResponse ereq eres) txns)
        <> newlinePreceding reportNetworkErrorMsg
    OverlongHeadersError ereq ->
      vsep
        [ "The HTTP headers provided by the server were too long."
        , ""
        , indent 4 "Request:" <+> renderRequest ereq
        , ""
        , reportNetworkErrorMsg
        ]
    ResponseTimeoutError ereq ->
      vsep
        [ "A connection to the FOSSA endpoint was established, but the service took too long to respond."
        , ""
        , indent 4 "Request:" <+> renderRequest ereq
        , ""
        , reportNetworkErrorMsg
        ]
    ConnectionTimeoutError ereq ->
      vsep
        [ "The request to the FOSSA endpoint took too long to send."
        , ""
        , "This typically means that the CLI is being asked to send too much data"
        , "with the current network speed (for example, uploading large archives),"
        , "although this can also be a transient error caused by congested"
        , "networking conditions between the CLI and the FOSSA API."
        , ""
        , "To reduce the likelihood of this error, ensure that only data you really"
        , "need FOSSA to scan is being uploaded."
        , ""
        , indent 4 "Request:" <+> renderRequest ereq
        , ""
        , requestReportIfPersists
        ]
    ConnectionFailureError ereq err ->
      vsep
        [ "Connecting to the FOSSA endpoint failed:"
        , ""
        , indent 4 "Request:" <+> renderRequest ereq
        , indent 4 "Error:" <+> pretty (displayException err)
        , ""
        , reportNetworkErrorMsg
        ]
    InvalidStatusLineError ereq status ->
      vsep
        [ "The FOSSA endpoint returned a status that could not be parsed:"
        , ""
        , indent 4 "Request:" <+> renderRequest ereq
        , indent 4 "Status:" <+> pretty status
        , ""
        , reportFossaBugErrorMsg $ fossaEnvironment ereq
        ]
    InvalidResponseHeaderError ereq header ->
      vsep
        [ "The FOSSA endpoint returned a header which could not be parsed:"
        , ""
        , indent 4 "Request:" <+> renderRequest ereq
        , indent 4 "Header:" <+> pretty header
        , ""
        , reportFossaBugErrorMsg $ fossaEnvironment ereq
        ]
    InvalidRequestHeaderError ereq header ->
      vsep
        [ "The FOSSA CLI provided a header which was not HTTP compliant:"
        , ""
        , indent 4 "Request:" <+> renderRequest ereq
        , indent 4 "Header:" <+> pretty header
        , ""
        , reportCliBugErrorMsg
        ]
    ProxyConnectError ereq host port status ->
      vsep
        [ "The proxy specified for FOSSA to use returned an unexpected status code."
        , ""
        , indent 4 "Request:" <+> renderRequest ereq
        , indent 4 "Proxy:"
        , indent 6 "Server:" <+> pretty host <+> ":" <> pretty port
        , indent 6 "Status:" <+> pretty (HTTP.statusCode status)
        , ""
        , reportNetworkErrorMsg
        ]
    NoResponseDataError ereq ->
      vsep
        [ "The connection to the FOSSA endpoint was closed without a response."
        , ""
        , indent 4 "Request:" <+> renderRequest ereq
        , ""
        , reportFossaBugErrorMsg $ fossaEnvironment ereq
        ]
    TlsNotSupportedError ereq ->
      if fossaEnvironment ereq == FossaEnvironmentCloud
        then
          vsep
            [ "The FOSSA endpoint reported that it does not support TLS connections."
            , "This request is connecting to FOSSA's cloud environment, which only supports TLS connections."
            , ""
            , indent 4 "Request:" <+> renderRequest ereq
            , ""
            , reportNetworkErrorMsg
            ]
        else
          vsep
            [ "The FOSSA endpoint reported that it does not support TLS connections."
            , "This request is not connecting to FOSSA's cloud environment, so this is up to the FOSSA administrators in your organization."
            , ""
            , indent 4 "Request:" <+> renderRequest ereq
            , ""
            , "Try again with an `http://` URL."
            , "The FOSSA endpoint URL may be specified in `.fossa.yml` or with the `-e` or `--endpoint` arguments."
            , ""
            , reportDefectWithDebugBundle
            ]
    WrongRequestBodyStreamSizeError ereq expect got ->
      vsep
        [ "The FOSSA CLI did not provide a request body with the correct length."
        , ""
        , indent 4 "Request:" <+> renderRequest ereq
        , indent 6 "Expected size (bytes):" <+> pretty (show expect)
        , indent 6 "Provided size (bytes):" <+> pretty (show got)
        , ""
        , reportCliBugErrorMsg
        ]
    ResponseBodyTooShortError ereq expect got ->
      vsep
        [ "The FOSSA endpoint provided a response body that was too short."
        , ""
        , indent 4 "Request:" <+> renderRequest ereq
        , indent 6 "Expected size (bytes):" <+> pretty (show expect)
        , indent 6 "Provided size (bytes):" <+> pretty (show got)
        , ""
        , reportFossaBugErrorMsg $ fossaEnvironment ereq
        ]
    InvalidChunkHeadersError ereq ->
      vsep
        [ "The FOSSA endpoint provided a chunked response but it had invalid headers."
        , ""
        , indent 4 "Request:" <+> renderRequest ereq
        , ""
        , reportFossaBugErrorMsg $ fossaEnvironment ereq
        ]
    IncompleteHeadersError ereq ->
      vsep
        [ "The FOSSA endpoint returned an incomplete set of headers."
        , ""
        , indent 4 "Request:" <+> renderRequest ereq
        , ""
        , reportFossaBugErrorMsg $ fossaEnvironment ereq
        ]
    InvalidDestinationHostError ereq ->
      vsep
        [ "The host provided as the FOSSA CLI endpoint is invalid."
        , ""
        , indent 4 "Request:" <+> renderRequest ereq
        , ""
        , reportDefectMsg
        ]
    HttpZlibError ereq msg ->
      vsep
        [ "The FOSSA endpoint provided a response body that was unable to decompress."
        , ""
        , indent 4 "Request:" <+> renderRequest ereq
        , indent 6 "Decompression error:" <+> pretty msg
        , ""
        , reportNetworkErrorMsg
        ]
    InvalidProxyEnvironmentVariableError ereq var val ->
      vsep
        [ "A provided environment variable used to configure the proxy connection is invalid."
        , ""
        , indent 4 "Request:" <+> renderRequest ereq
        , indent 4 "Environment variable:"
        , indent 6 "Name:" <+> pretty var
        , indent 6 "Value:" <+> pretty val
        , ""
        , reportDefectMsg
        ]
    ConnectionClosedError ereq ->
      vsep
        [ "FOSSA CLI attempted to reuse a connection that was closed."
        , ""
        , indent 4 "Request:" <+> renderRequest ereq
        , ""
        , reportCliBugErrorMsg
        ]
    InvalidProxySettingsError ereq msg ->
      vsep
        [ "The proxy settings provided were not valid."
        , ""
        , indent 4 "Request:" <+> renderRequest ereq
        , indent 4 "Proxy settings:" <+> pretty msg
        , ""
        , reportDefectMsg
        ]

containerUploadUrl :: Url scheme -> Url scheme
containerUploadUrl baseurl = baseurl /: "api" /: "container" /: "upload"

uploadNativeContainerScan ::
  (Has (Lift IO) sig m, Has Debug sig m, Has Diagnostics sig m) =>
  ApiOpts ->
  ProjectRevision ->
  ProjectMetadata ->
  NativeContainer.ContainerScan ->
  m UploadResponse
uploadNativeContainerScan apiOpts ProjectRevision{..} metadata scan = fossaReq $ do
  supportsNativeScan <- orgSupportsNativeContainerScan <$> getOrganization apiOpts
  -- Sanity Check!
  if not supportsNativeScan
    then fatal EndpointDoesNotSupportNativeContainerScan
    else do
      (baseUrl, baseOpts) <- useApiOpts apiOpts
      let locator = renderLocator $ Locator "custom" projectName (Just projectRevision)
          opts =
            "locator"
              =: locator
              <> "cliVersion"
                =: cliVersion
              <> "managedBuild"
                =: True
              <> maybe mempty ("branch" =:) projectBranch
              <> "scanType"
                =: ("native" :: Text)
              <> mkMetadataOpts metadata projectName
      resp <- req POST (containerUploadUrl baseUrl) (ReqBodyJson scan) jsonResponse (baseOpts <> opts)
      pure $ responseBody resp

-- | Replacement for @Data.HTTP.Req.req@ that additionally logs information about a request in a debug bundle.
req ::
  forall method body sig m scheme b.
  ( Req.HttpBodyAllowed (Req.AllowsBody method) (Req.ProvidesBody body)
  , MonadHttp m
  , Has Diagnostics sig m
  , Has Debug sig m
  , Req.HttpMethod method
  , HttpBody body
  , Req.HttpResponse b
  ) =>
  method ->
  Url scheme ->
  body ->
  Proxy b ->
  Option scheme ->
  m b
req method url body resp scheme = context "Calling FOSSA API" $
  do
    debugLog $ decodeUtf8 (httpMethodName (Proxy :: Proxy method)) <> " " <> renderUrl url
    Req.req method url body resp scheme

uploadAnalysis ::
  (Has (Lift IO) sig m, Has Diagnostics sig m, Has Debug sig m) =>
  ApiOpts ->
  ProjectRevision ->
  ProjectMetadata ->
  NE.NonEmpty SourceUnit ->
  m UploadResponse
uploadAnalysis apiOpts ProjectRevision{..} metadata sourceUnits = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts

  let opts =
        "locator"
          =: renderLocator (Locator "custom" projectName (Just projectRevision))
          <> "cliVersion"
            =: cliVersion
          <> "managedBuild"
            =: True
          <> mkMetadataOpts metadata projectName
          -- Don't include branch if it doesn't exist, core may not handle empty string properly.
          <> maybe mempty ("branch" =:) projectBranch
  resp <- req POST (uploadUrl baseUrl) (ReqBodyJson $ NE.toList sourceUnits) jsonResponse (baseOpts <> opts)
  pure (responseBody resp)

uploadAnalysisWithFirstPartyLicenses ::
  (Has (Lift IO) sig m, Has Debug sig m, Has Diagnostics sig m) =>
  ApiOpts ->
  ProjectRevision ->
  ProjectMetadata ->
  FullFileUploads ->
  m UploadResponse
uploadAnalysisWithFirstPartyLicenses apiOpts ProjectRevision{..} metadata fullFileUploads = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts

  let opts =
        "locator"
          =: renderLocator (Locator "custom" projectName (Just projectRevision))
          <> "cliVersion"
            =: cliVersion
          <> "managedBuild"
            =: True
          <> "cliLicenseScanType"
            =: (fullFileUploadsToCliLicenseScanType fullFileUploads)
          <> mkMetadataOpts metadata projectName
          -- Don't include branch if it doesn't exist, core may not handle empty string properly.
          <> maybe mempty ("branch" =:) projectBranch
  resp <- req POST (uploadWithFirstPartyUrl baseUrl) (NoReqBody) jsonResponse (baseOpts <> opts)
  pure (responseBody resp)

mkMetadataOpts :: ProjectMetadata -> Text -> Option scheme
mkMetadataOpts ProjectMetadata{..} projectName = mconcat totalOptions
  where
    title = Just $ fromMaybe projectName projectTitle
    maybeOptions =
      [ ("projectURL" =:) <$> projectUrl
      , ("jiraProjectKey" =:) <$> projectJiraKey
      , ("link" =:) <$> projectLink
      , ("team" =:) <$> projectTeam
      , policyOpt <$> projectPolicy
      , ("releaseGroup" =:) . releaseGroupName <$> projectReleaseGroup
      , ("releaseGroupRelease" =:) . releaseGroupRelease <$> projectReleaseGroup
      , ("title" =:) <$> title
      ]

    policyOpt (PolicyName n) = ("policy" =: n)
    policyOpt (PolicyId i) = ("policyId" =: i)

    labelOptions = map ("labels[]" =:) projectLabel
    totalOptions = catMaybes maybeOptions ++ labelOptions

mangleError :: HttpException -> FossaError
mangleError err = case errWithoutSensitiveInfo of
  JsonHttpException msg -> JsonDeserializeError msg
  VanillaHttpException he -> case he of
    HTTP.HttpExceptionRequest ereq hec -> case hec of
      HTTP.StatusCodeException res respBody -> case decodeStrict respBody of
        Just pfe -> BackendPublicFacingError pfe
        Nothing -> StatusCodeError ereq res
      HTTP.TooManyRedirects res -> TooManyRedirectsError $ fmap (ereq,) res
      HTTP.OverlongHeaders -> OverlongHeadersError ereq
      HTTP.ResponseTimeout -> ResponseTimeoutError ereq
      HTTP.ConnectionTimeout -> ConnectionTimeoutError ereq
      HTTP.ConnectionFailure se -> ConnectionFailureError ereq se
      HTTP.InvalidStatusLine bs -> InvalidStatusLineError ereq $ show bs
      HTTP.InvalidHeader bs -> InvalidResponseHeaderError ereq $ show bs
      HTTP.InvalidRequestHeader bs -> InvalidRequestHeaderError ereq $ show bs
      HTTP.InternalException se -> InternalException se
      HTTP.ProxyConnectException host port status -> ProxyConnectError ereq (show host) port status
      HTTP.NoResponseDataReceived -> NoResponseDataError ereq
      HTTP.TlsNotSupported -> TlsNotSupportedError ereq
      HTTP.WrongRequestBodyStreamSize expected actual -> WrongRequestBodyStreamSizeError ereq expected actual
      HTTP.ResponseBodyTooShort expected actual -> ResponseBodyTooShortError ereq expected actual
      HTTP.InvalidChunkHeaders -> InvalidChunkHeadersError ereq
      HTTP.IncompleteHeaders -> IncompleteHeadersError ereq
      HTTP.InvalidDestinationHost _ -> InvalidDestinationHostError ereq
      HTTP.HttpZlibException ze -> HttpZlibError ereq $ show ze
      HTTP.InvalidProxyEnvironmentVariable var value -> InvalidProxyEnvironmentVariableError ereq var value
      HTTP.ConnectionClosed -> ConnectionClosedError ereq
      HTTP.InvalidProxySettings txt -> InvalidProxySettingsError ereq txt
    HTTP.InvalidUrlException url str -> InvalidUrlError url str
  where
    errWithoutSensitiveInfo :: HttpException
    errWithoutSensitiveInfo = redactSensitiveInfoFromException err

-- Render the request and the response.
renderRequestResponse :: HTTP.Request -> HTTP.Response a -> Doc AnsiStyle
renderRequestResponse request response = renderRequest request <> " -> " <> renderResponse response

-- Render response details for humans to read.
-- Does not render bodies.
-- TODO: Possibly render it 'viaShow' in debug mode for more info.
renderResponse :: HTTP.Response a -> Doc AnsiStyle
renderResponse = pretty . show . HTTP.statusCode . HTTP.responseStatus

-- Render request details for humans to read.
-- TODO: Possibly render it 'viaShow' in debug mode for more info.
renderRequest :: HTTP.Request -> Doc AnsiStyle
renderRequest req' =
  pretty $
    decodeUtf8 (HTTP.method req')
      <> " "
      <> renderHttpHttps
      <> decodeUtf8 (HTTP.host req')
      <> renderNonDefaultPort
      <> decodeUtf8 (HTTP.path req')
      <> decodeUtf8 (HTTP.queryString req')
  where
    renderHttpHttps :: String
    renderHttpHttps = (if HTTP.secure req' then "https" else "http") <> "://"

    renderNonDefaultPort :: String
    renderNonDefaultPort =
      if (HTTP.port req') /= defaultPort
        then ":" <> show (HTTP.port req')
        else ""

    defaultPort :: Int
    defaultPort = if HTTP.secure req' then 443 else 80

redactSensitiveInfoFromException :: HttpException -> HttpException
redactSensitiveInfoFromException (VanillaHttpException (HTTP.HttpExceptionRequest r (HTTP.StatusCodeException resp respBody))) =
  VanillaHttpException (HTTP.HttpExceptionRequest r (HTTP.StatusCodeException respWithoutCookies respBody))
  where
    respWithoutCookies :: HTTP.Response ()
    respWithoutCookies =
      resp
        { HTTP.responseHeaders = map redactCookieHeader $ HTTP.responseHeaders resp -- Don't expose `Set-Cookie`
        , HTTP.responseCookieJar = HTTP.createCookieJar [] -- Don't expose any cookies
        }

    redactCookieHeader :: HTTP.Header -> HTTP.Header
    redactCookieHeader ("Set-Cookie", _) = ("Set-Cookie", "<REDACTED>")
    redactCookieHeader h = h
redactSensitiveInfoFromException ex = ex

-----

projectEndpoint :: Url scheme -> OrgId -> Locator -> Url scheme
projectEndpoint baseurl orgid locator = baseurl /: "api" /: "cli" /: renderLocatorUrl orgid locator /: "project"

getProject ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  , Has Debug sig m
  ) =>
  ApiOpts ->
  ProjectRevision ->
  m Project
getProject apiopts ProjectRevision{..} = fossaReq $ do
  (baseurl, baseopts) <- useApiOpts apiopts

  orgid <- organizationId <$> getOrganization apiopts

  let endpoint = projectEndpoint baseurl orgid $ Locator "custom" projectName Nothing

  responseBody <$> req GET endpoint NoReqBody jsonResponse baseopts

-----

getAnalyzedRevisionsEndpoint :: Url 'Https -> Url 'Https
getAnalyzedRevisionsEndpoint baseurl = baseurl /: "api" /: "cli" /: "analyzedRevisions"

-- | getAnalyzedRevisions makes a request to Core with a list of locators that we are considering scanning
--   Core will respond with a list of locators that have already been analyzed
getAnalyzedRevisions ::
  (Has (Lift IO) sig m, Has Debug sig m, Has Diagnostics sig m) =>
  ApiOpts ->
  NonEmpty VendoredDependency ->
  m [Text]
getAnalyzedRevisions apiOpts vDeps = fossaReq $ do
  Organization{organizationId = orgId, orgRequiresFullFileUploads = fullFileUploads} <- getOrganization apiOpts
  (baseUrl, baseOpts) <- useApiOpts apiOpts
  let locatorBody = GetAnalyzedRevisionsBody (NE.map (renderLocatorUrl orgId . vendoredDepToLocator) vDeps) fullFileUploads
  responseBody <$> req POST (getAnalyzedRevisionsEndpoint baseUrl) (ReqBodyJson locatorBody) jsonResponse baseOpts

-----

buildsEndpoint :: Url 'Https -> OrgId -> Locator -> Url 'Https
buildsEndpoint baseurl orgId locator = baseurl /: "api" /: "cli" /: renderLocatorUrl orgId locator /: "latest_build"

getLatestBuild ::
  (Has (Lift IO) sig m, Has Debug sig m, Has Diagnostics sig m) =>
  ApiOpts ->
  ProjectRevision ->
  m Build
getLatestBuild apiOpts ProjectRevision{..} = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts

  orgId <- organizationId <$> getOrganization apiOpts

  response <- req GET (buildsEndpoint baseUrl orgId (Locator "custom" projectName (Just projectRevision))) NoReqBody jsonResponse baseOpts
  pure (responseBody response)

----

dependencyCacheReadyEndpoint :: Url 'Https -> OrgId -> Locator -> Url 'Https
dependencyCacheReadyEndpoint baseurl orgId locator = baseurl /: "api" /: "cli" /: renderLocatorUrl orgId locator /: "dependencies-cache" /: "status"

getRevisionDependencyCacheStatus ::
  (Has (Lift IO) sig m, Has Debug sig m, Has Diagnostics sig m) =>
  ApiOpts ->
  ProjectRevision ->
  m RevisionDependencyCache
getRevisionDependencyCacheStatus apiOpts ProjectRevision{..} = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts
  orgId <- organizationId <$> getOrganization apiOpts
  response <- req GET (dependencyCacheReadyEndpoint baseUrl orgId (Locator "custom" projectName (Just projectRevision))) NoReqBody jsonResponse baseOpts
  pure (responseBody response)

---------- Archive build queueing. This Endpoint ensures that after an archive is uploaded, it is scanned.

archiveBuildURL :: Url 'Https -> Url 'Https
archiveBuildURL baseUrl = baseUrl /: "api" /: "components" /: "build"

archiveBuildUpload ::
  (Has (Lift IO) sig m, Has Debug sig m, Has Diagnostics sig m) =>
  ApiOpts ->
  Archive ->
  m (Maybe C.ByteString)
archiveBuildUpload apiOpts archive = runEmpty $
  fossaReqAllow401 $ do
    (baseUrl, baseOpts) <- useApiOpts apiOpts

    let opts = "dependency" =: True <> "rawLicenseScan" =: True
    -- The API route expects an array of archives, but doesn't properly handle multiple archives so we upload
    -- an array of a single archive.
    -- forceRebuild and fullFiles are ignored by this endpoint. They are only used by the licenseScanFinalize endpoint.
    let archiveProjects = ArchiveComponents [archive] False $ FullFileUploads False
    -- The response appears to either be "Created" for new builds, or an error message for existing builds.
    -- Making the actual return value of "Created" essentially worthless.
    resp <-
      context "Queuing a build for all archive uploads" $
        req POST (archiveBuildURL baseUrl) (ReqBodyJson archiveProjects) bsResponse (baseOpts <> opts)
    pure (responseBody resp)

---------- license-scan build queueing. This Endpoint ensures that after a license-scan is uploaded, it is scanned.

licenseScanFinalizeUrl :: Url 'Https -> Url 'Https
licenseScanFinalizeUrl baseUrl = baseUrl /: "api" /: "license_scan" /: "finalize"

licenseScanFinalize ::
  (Has (Lift IO) sig m, Has Debug sig m, Has Diagnostics sig m) =>
  ApiOpts ->
  ArchiveComponents ->
  Bool -> 
  m (Maybe ())
licenseScanFinalize apiOpts archiveProjects isPathDependency = runEmpty $
  fossaReqAllow401 $ do
    (baseUrl, baseOpts) <- useApiOpts apiOpts

    let opts = "dependency" =: True <> "rawLicenseScan" =: True <> "isPathDependency" =: isPathDependency

    _ <-
      context "Queuing a build for all license scan uploads" $
        req POST (licenseScanFinalizeUrl baseUrl) (ReqBodyJson archiveProjects) ignoreResponse (baseOpts <> opts)
    pure ()

---------- The signed URL endpoint returns a URL endpoint that can be used to directly upload to an S3 bucket.

signedURLEndpoint :: Url 'Https -> Url 'Https
signedURLEndpoint baseUrl = baseUrl /: "api" /: "components" /: "signed_url"

getSignedURL ::
  (Has (Lift IO) sig m, Has Debug sig m, Has Diagnostics sig m) =>
  ApiOpts ->
  Text ->
  Text ->
  m SignedURL
getSignedURL apiOpts revision packageName = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts

  let opts = "packageSpec" =: packageName <> "revision" =: revision

  response <-
    context ("Retrieving a signed S3 URL for " <> packageName) $
      req GET (signedURLEndpoint baseUrl) NoReqBody jsonResponse (baseOpts <> opts)
  pure (responseBody response)

---------- The signed First-Party Scan URL endpoint returns a URL endpoint that can be used to directly upload the results of a first-party scan to an S3 bucket.

signedFirstPartyScanURLEndpoint :: Url 'Https -> Url 'Https
signedFirstPartyScanURLEndpoint baseUrl = baseUrl /: "api" /: "first_party_scan" /: "signed_url"

getSignedFirstPartyScanURL ::
  (Has (Lift IO) sig m, Has Debug sig m, Has Diagnostics sig m) =>
  ApiOpts ->
  Text ->
  Text ->
  m SignedURL
getSignedFirstPartyScanURL apiOpts revision packageName = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts

  let opts = "packageSpec" =: packageName <> "revision" =: revision

  response <-
    context ("Retrieving a signed S3 URL for license scan results of " <> packageName) $
      req GET (signedFirstPartyScanURLEndpoint baseUrl) NoReqBody jsonResponse (baseOpts <> opts)
  pure (responseBody response)

---------- The signed License Scan URL endpoint returns a URL endpoint that can be used to directly upload the results of a license scan to an S3 bucket.

signedLicenseScanURLEndpoint :: Url 'Https -> Url 'Https
signedLicenseScanURLEndpoint baseUrl = baseUrl /: "api" /: "license_scan" /: "signed_url"

getSignedLicenseScanURL ::
  (Has (Lift IO) sig m, Has Debug sig m, Has Diagnostics sig m) =>
  ApiOpts ->
  Text ->
  Text ->
  Bool -> 
  m SignedURL
getSignedLicenseScanURL apiOpts revision packageName isPathDependency = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts

  let opts = "packageSpec" =: packageName <> "revision" =: revision <> "isPathDependency" =: isPathDependency

  response <-
    context ("Retrieving a signed S3 URL for license scan results of " <> packageName) $
      req GET (signedLicenseScanURLEndpoint baseUrl) NoReqBody jsonResponse (baseOpts <> opts)
  pure (responseBody response)

---------- The archive upload function uploads the file it is given directly to the signed URL it is provided.

archiveUpload ::
  (Has (Lift IO) sig m, Has Diagnostics sig m) =>
  SignedURL ->
  FilePath ->
  m ByteString
archiveUpload signedArcURI arcFile = fossaReq $ do
  let arcURL = URI.mkURI $ signedURL signedArcURI

  uri <- fromMaybeText ("Invalid URL: " <> signedURL signedArcURI) arcURL
  validatedURI <- fromMaybeText ("Invalid URI: " <> toText (show uri)) (useURI uri)

  response <- context ("Uploading project archive to " <> signedURL signedArcURI) $ case validatedURI of
    Left (url, options) -> uploadArchiveRequest url options
    Right (url, options) -> uploadArchiveRequest url options
  pure (responseBody response)
  where
    uploadArchiveRequest url options = reqCb PUT url (ReqBodyFile arcFile) lbsResponse options (pure . requestEncoder)

---------- The license scan result upload function uploads the JSON license result directly to the signed URL it is provided.

licenseScanResultUpload ::
  (Has (Lift IO) sig m, Has Diagnostics sig m) =>
  SignedURL ->
  LicenseSourceUnit ->
  m LbsResponse
licenseScanResultUpload signedUploadURI licenseScanResult = fossaReq $ do
  let uploadURL = URI.mkURI $ signedURL signedUploadURI

  uri <- fromMaybeText ("Invalid URL: " <> signedURL signedUploadURI) uploadURL
  validatedURI <- fromMaybeText ("Invalid URI: " <> toText (show uri)) (useURI uri)

  context ("Uploading license scan result to " <> signedURL signedUploadURI) $ case validatedURI of
    Left (httpUrl, httpOptions) -> uploadArchiveRequest httpUrl httpOptions
    Right (httpsUrl, httpsOptions) -> uploadArchiveRequest httpsUrl httpsOptions
  where
    zippedLicenseResult :: BS.ByteString
    zippedLicenseResult = toStrict $ GZIP.compress $ encode licenseScanResult
    -- We send gzipped json, so we can't use req's JSON utilities.
    uploadArchiveRequest :: (MonadHttp m) => Url scheme -> Option scheme -> m LbsResponse
    uploadArchiveRequest url options = reqCb PUT url (ReqBodyBs zippedLicenseResult) lbsResponse options (pure . requestEncoder)

---------- The first-party scan result upload function uploads the JSON license result directly to the signed URL it is provided.

firstPartyScanResultUpload ::
  (Has (Lift IO) sig m, Has Diagnostics sig m) =>
  SignedURL ->
  NE.NonEmpty FullSourceUnit ->
  m LbsResponse
firstPartyScanResultUpload signedUploadURI firstPartyScanResult = fossaReq $ do
  let uploadURL = URI.mkURI $ signedURL signedUploadURI

  uri <- fromMaybeText ("Invalid URL: " <> signedURL signedUploadURI) uploadURL
  validatedURI <- fromMaybeText ("Invalid URI: " <> toText (show uri)) (useURI uri)

  context ("Uploading first-party scan result to " <> signedURL signedUploadURI) $ case validatedURI of
    Left (httpUrl, httpOptions) -> uploadArchiveRequest httpUrl httpOptions
    Right (httpsUrl, httpsOptions) -> uploadArchiveRequest httpsUrl httpsOptions
  where
    zippedLicenseResult :: BS.ByteString
    zippedLicenseResult = toStrict $ GZIP.compress $ encode firstPartyScanResult
    -- We send gzipped json, so we can't use req's JSON utilities.
    uploadArchiveRequest :: (MonadHttp m) => Url scheme -> Option scheme -> m LbsResponse
    uploadArchiveRequest url options = reqCb PUT url (ReqBodyBs zippedLicenseResult) lbsResponse options (pure . requestEncoder)

-- requestEncoder properly encodes the Request path.
-- The default encoding logic does not encode "+" ot "$" characters which makes AWS very angry.
-- This is accomplished by passing "True" to "Http.urlEncode" to signify that we want to encode more characters.
requestEncoder :: C.Request -> C.Request
requestEncoder r = r{C.path = encoder (C.path r)}

encoder :: BS.ByteString -> BS.ByteString
encoder path = BS.singleton slashWord8 <> joined
  where
    split :: [BS.ByteString]
    split = BS.split slashWord8 path
    filtered :: [BS.ByteString]
    filtered = filter (/= BS.empty) split
    encoded :: [BS.ByteString]
    encoded = map (HTTP.urlEncode True) filtered
    joined :: BS.ByteString
    joined = BS.intercalate (BS.singleton slashWord8) encoded

slashWord8 :: Word8
slashWord8 = fromIntegral $ fromEnum '/'

----------

issuesEndpoint :: Url 'Https -> OrgId -> Locator -> Url 'Https
issuesEndpoint baseUrl orgId locator = baseUrl /: "api" /: "cli" /: renderLocatorUrl orgId locator /: "issues"

getIssues ::
  (Has (Lift IO) sig m, Has Debug sig m, Has Diagnostics sig m) =>
  ApiOpts ->
  ProjectRevision ->
  Maybe DiffRevision ->
  m Issues
getIssues apiOpts ProjectRevision{..} diffRevision = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts
  org <- getOrganization apiOpts

  opts <- case (diffRevision, orgSupportsIssueDiffs org) of
    (Just (DiffRevision diffRev), True) -> pure (baseOpts <> "diffRevision" =: diffRev)
    (Just _, False) -> fatal EndpointDoesNotSupportIssueDiffing
    (Nothing, _) -> pure baseOpts

  response <-
    req
      GET
      (issuesEndpoint baseUrl (organizationId org) (Locator "custom" projectName (Just projectRevision)))
      NoReqBody
      jsonResponse
      opts

  pure (responseBody response)

data EndpointDoesNotSupportIssueDiffing = EndpointDoesNotSupportIssueDiffing

instance ToDiagnostic EndpointDoesNotSupportIssueDiffing where
  renderDiagnostic (EndpointDoesNotSupportIssueDiffing) =
    vsep
      [ "Provided endpoint does not support issue diffing."
      , ""
      , "If this instance of FOSSA is on-premise, it likely needs to be updated."
      ]

---------------

attributionEndpoint :: Url 'Https -> OrgId -> Locator -> ReportOutputFormat -> Url 'Https
attributionEndpoint baseurl orgId locator format = appendSegment format $ baseurl /: "api" /: "revisions" /: renderLocatorUrl orgId locator /: "attribution"
  where
    appendSegment :: ReportOutputFormat -> Url a -> Url a
    appendSegment fmt input =
      case fmt of
        ReportJson -> input /: "json"
        ReportMarkdown -> input /: "full" /: "MD"
        ReportSpdx -> input /: "full" /: "spdx"
        ReportSpdxJSON -> input /: "full" /: "SPDX_JSON"
        ReportCycloneDXJSON -> input /: "full" /: "CYCLONEDX_JSON"
        ReportCycloneDXXML -> input /: "full" /: "CYCLONEDX_XML"
        ReportPlainText -> input /: "full" /: "TXT"
        ReportHTML -> input /: "full" /: "HTML"
        ReportCSV -> input /: "full" /: "CSV"

getAttributionJson ::
  (Has (Lift IO) sig m, Has Debug sig m, Has Diagnostics sig m) =>
  ApiOpts ->
  ProjectRevision ->
  m Attr.Attribution
getAttributionJson apiOpts ProjectRevision{..} = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts
  let packageDownloadUrl :: String
      packageDownloadUrl = "PackageDownloadUrl"
      opts =
        baseOpts
          <> "includeDeepDependencies"
            =: True
          <> "includeHashAndVersionData"
            =: True
          <> "dependencyInfoOptions[]"
            =: packageDownloadUrl
  orgId <- organizationId <$> getOrganization apiOpts
  response <- req GET (attributionEndpoint baseUrl orgId (Locator "custom" projectName (Just projectRevision)) ReportJson) NoReqBody jsonResponse opts
  pure (responseBody response)

getAttribution ::
  (Has (Lift IO) sig m, Has Debug sig m, Has Diagnostics sig m) =>
  ApiOpts ->
  ProjectRevision ->
  ReportOutputFormat ->
  m Text
getAttribution apiOpts revision ReportJson = fossaReq $ do
  jsonValue <- getAttributionJson apiOpts revision
  pure . decodeUtf8 $ Aeson.encode jsonValue
getAttribution apiOpts ProjectRevision{..} format = fossaReq $ do
  (baseUrl, opts) <- useApiOpts apiOpts

  orgId <- organizationId <$> getOrganization apiOpts
  response <- req GET (attributionEndpoint baseUrl orgId (Locator "custom" projectName (Just projectRevision)) format) NoReqBody bsResponse opts
  pure (decodeUtf8 $ responseBody response)

----------

organizationEndpoint :: Url scheme -> Url scheme
organizationEndpoint baseurl = baseurl /: "api" /: "cli" /: "organization"

getOrganization :: (Has (Lift IO) sig m, Has Debug sig m, Has Diagnostics sig m) => ApiOpts -> m Organization
getOrganization apiOpts = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts
  responseBody <$> req GET (organizationEndpoint baseUrl) NoReqBody jsonResponse baseOpts

----------

contributorsEndpoint :: Url scheme -> Url scheme
contributorsEndpoint baseurl = baseurl /: "api" /: "contributors"

uploadContributors :: (Has (Lift IO) sig m, Has Debug sig m, Has Diagnostics sig m) => ApiOpts -> Text -> Contributors -> m ()
uploadContributors apiOpts locator contributors = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts

  let opts = baseOpts <> "locator" =: locator

  _ <- req POST (contributorsEndpoint baseUrl) (ReqBodyJson contributors) ignoreResponse opts
  pure ()

----------

-- | Core expects an object for each fingerprint.
-- This type allows us to convert a fingerprint to the required object.
newtype FingerprintAssertion = FingerprintAssertion
  { fingerprintRaw :: Fingerprint Raw
  }

instance ToJSON FingerprintAssertion where
  toJSON FingerprintAssertion{..} =
    object
      [ "fingerprint_sha_256" .= fingerprintRaw
      ]

-- | The set of necessary data to describe a user defined binary assertion to the FOSSA API.
data UserDefinedAssertionBody = UserDefinedAssertionBody
  { bodyName :: Text
  , bodyVersion :: Text
  , bodyLicense :: Text
  , bodyDescription :: Maybe Text
  , bodyUrl :: Maybe Text
  , bodyFingerprints :: [FingerprintAssertion]
  }

instance ToJSON UserDefinedAssertionBody where
  toJSON UserDefinedAssertionBody{..} =
    object
      [ "name" .= bodyName
      , "version" .= bodyVersion
      , "license" .= bodyLicense
      , "description" .= bodyDescription
      , "url" .= bodyUrl
      , "fingerprints" .= bodyFingerprints
      ]

assertUserDefinedBinariesEndpoint :: Url scheme -> Url scheme
assertUserDefinedBinariesEndpoint baseurl = baseurl /: "api" /: "iat" /: "binary"

assertUserDefinedBinaries :: (Has (Lift IO) sig m, Has Debug sig m, Has Diagnostics sig m) => ApiOpts -> IAT.UserDefinedAssertionMeta -> [Fingerprint Raw] -> m ()
assertUserDefinedBinaries apiOpts IAT.UserDefinedAssertionMeta{..} fingerprints = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts
  let body = UserDefinedAssertionBody assertedName assertedVersion assertedLicense assertedDescription assertedUrl (FingerprintAssertion <$> fingerprints)
  _ <- req POST (assertUserDefinedBinariesEndpoint baseUrl) (ReqBodyJson body) ignoreResponse baseOpts
  pure ()

assertRevisionBinariesEndpoint :: Url scheme -> Locator -> Url scheme
assertRevisionBinariesEndpoint baseurl locator = baseurl /: "api" /: "iat" /: "binary" /: renderLocator locator

assertRevisionBinaries :: (Has (Lift IO) sig m, Has Debug sig m, Has Diagnostics sig m) => ApiOpts -> Locator -> [Fingerprint Raw] -> m ()
assertRevisionBinaries apiOpts locator fingerprints = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts
  let body = FingerprintAssertion <$> fingerprints
  _ <- req POST (assertRevisionBinariesEndpoint baseUrl locator) (ReqBodyJson body) ignoreResponse baseOpts
  pure ()

resolveUserDefinedBinaryEndpoint :: Url scheme -> IAT.UserDep -> Url scheme
resolveUserDefinedBinaryEndpoint baseurl dep = baseurl /: "api" /: "iat" /: "resolve" /: "user-defined" /: IAT.renderUserDep dep

resolveUserDefinedBinary :: (Has (Lift IO) sig m, Has Debug sig m, Has Diagnostics sig m) => ApiOpts -> IAT.UserDep -> m IAT.UserDefinedAssertionMeta
resolveUserDefinedBinary apiOpts dep = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts
  responseBody <$> req GET (resolveUserDefinedBinaryEndpoint baseUrl dep) NoReqBody jsonResponse baseOpts

-- | The revision dependencies endpoint contains a lot of information we don't need. This intermediate type allows us to throw it away.
newtype ResolvedDependency = ResolvedDependency {unwrapResolvedDependency :: VSI.Locator}

instance FromJSON ResolvedDependency where
  parseJSON = withObject "ResolvedProjectDependencies" $ \obj -> do
    ResolvedDependency <$> obj .: "loc"

resolveProjectDependenciesEndpoint :: Url scheme -> VSI.Locator -> Url scheme
resolveProjectDependenciesEndpoint baseurl locator = baseurl /: "api" /: "revisions" /: VSI.renderLocator locator /: "dependencies"

resolveProjectDependencies :: (Has (Lift IO) sig m, Has Debug sig m, Has Diagnostics sig m) => ApiOpts -> VSI.Locator -> m [VSI.Locator]
resolveProjectDependencies apiOpts locator = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts

  let opts = baseOpts <> "include_ignored" =: False
  (dependencies :: [ResolvedDependency]) <- responseBody <$> req GET (resolveProjectDependenciesEndpoint baseUrl locator) NoReqBody jsonResponse opts

  pure $ map unwrapResolvedDependency dependencies

----------

baseVsiUrl :: Url scheme -> Url scheme
baseVsiUrl baseurl = baseurl /: "api" /: "proxy" /: "sherlock"

-- | This body option allows us to use a JSON object as the request body.
-- Just wrap a data type that is an instance of 'ToJSON' type class and you are done:
-- it will be converted to JSON and inserted as request body.
--
-- This type is a replacement for @HTTP.ReqBodyJson@, because some services aren't tolerant
-- of the charset declaration appended to 'Content-Type' by that body provider.
-- Specifically, the VSI backend wants explicitly only an 'application/json' value.
--
-- This body option sets the @Content-Type@ header to @\"application/json\"@ value.
newtype ReqBodyJsonCompat a = ReqBodyJsonCompat a

instance ToJSON a => HttpBody (ReqBodyJsonCompat a) where
  getRequestBody (ReqBodyJsonCompat a) = HTTP.RequestBodyLBS (encode a)
  getRequestContentType _ = pure "application/json"

data VSICreateScanRequestBody = VSICreateScanRequestBody
  { vsiCreateScanRequestBodyOrgID :: OrgId
  , vsiCreateScanRequestBodyProjectID :: Text
  , vsiCreateScanRequestBodyRevisionID :: Text
  }

instance ToJSON VSICreateScanRequestBody where
  toJSON VSICreateScanRequestBody{..} =
    object
      [ "OrganizationID" .= vsiCreateScanRequestBodyOrgID
      , "ProjectID" .= vsiCreateScanRequestBodyProjectID
      , "RevisionID" .= vsiCreateScanRequestBodyRevisionID
      ]

newtype VSICreateScanResponseBody = VSICreateScanResponseBody {unVSICreateScanResponseBody :: VSI.ScanID}

instance FromJSON VSICreateScanResponseBody where
  parseJSON = withObject "VSICreateScanResponseBody" $ \obj -> VSICreateScanResponseBody <$> obj .: "ScanID"

vsiCreateScanEndpoint :: Url scheme -> Url scheme
vsiCreateScanEndpoint baseurl = baseVsiUrl baseurl /: "scans"

vsiCreateScan :: (Has (Lift IO) sig m, Has Debug sig m, Has Diagnostics sig m) => ApiOpts -> ProjectRevision -> m VSI.ScanID
vsiCreateScan apiOpts ProjectRevision{..} = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts

  orgId <- organizationId <$> getOrganization apiOpts
  let projectID = renderLocator $ Locator "custom" projectName Nothing
  let reqBody = VSICreateScanRequestBody orgId projectRevision projectID

  body <- responseBody <$> req POST (vsiCreateScanEndpoint baseUrl) (ReqBodyJsonCompat reqBody) jsonResponse baseOpts
  pure $ unVSICreateScanResponseBody body

newtype VSIAddFilesToScanRequestBody = VSIAddFilesToScanRequestBody {vsiAddFilesToScanRequestBodyFiles :: Map FilePath Fingerprint.Combined}

instance ToJSON VSIAddFilesToScanRequestBody where
  toJSON VSIAddFilesToScanRequestBody{..} = object ["ScanData" .= vsiAddFilesToScanRequestBodyFiles]

vsiAddFilesToScanEndpoint :: Url scheme -> VSI.ScanID -> Url scheme
vsiAddFilesToScanEndpoint baseurl (VSI.ScanID scanID) = baseVsiUrl baseurl /: "scans" /: scanID /: "files"

vsiAddFilesToScan :: (Has (Lift IO) sig m, Has Debug sig m, Has Diagnostics sig m) => ApiOpts -> VSI.ScanID -> Map (Path Rel File) Fingerprint.Combined -> m ()
vsiAddFilesToScan apiOpts scanID files = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts

  -- This is in the API layer because it's an implementation detail of the API.
  -- The API expects all file paths to be normalized to forward slashes.
  let normalized = Map.fromList . normalizeFiles $ Map.toList files
  let body = VSIAddFilesToScanRequestBody normalized
  _ <- req POST (vsiAddFilesToScanEndpoint baseUrl scanID) (ReqBodyJsonCompat body) ignoreResponse baseOpts

  pure ()
  where
    normalizeFiles :: [(Path Rel File, Fingerprint.Combined)] -> [(FilePath, Fingerprint.Combined)]
    normalizeFiles = map normalizeFile

    normalizeFile :: (Path Rel File, Fingerprint.Combined) -> (FilePath, Fingerprint.Combined)
    normalizeFile (path, fp) = (normalizePath path, fp)

    normalizePath :: Path Rel File -> FilePath
    normalizePath input | pathSeparator == '/' = toFilePath input
    normalizePath input = toString . Text.intercalate "/" $ toText <$> splitDirectories (toFilePath input)

-- | The 'vsiCompleteScanFilePath' is an absolute path denoting what portion of the scan should be considered complete.
-- In this path structure, '/' means "the root of the scan".
-- Technically this should really be a @Path Abs Dir@, because that's what it represents in the backend.
-- However @$(mkAbsDir "/")@ fails in Windows builds, and is the only thing we ever actually pass in, so leaving it @Text@.
newtype VSICompleteScanRequestBody = VSICompleteScanRequestBody {vsiCompleteScanFilePath :: Text}

instance ToJSON VSICompleteScanRequestBody where
  toJSON VSICompleteScanRequestBody{..} = object ["FilePath" .= toText vsiCompleteScanFilePath]

vsiCompleteScanEndpoint :: Url scheme -> VSI.ScanID -> Url scheme
vsiCompleteScanEndpoint baseurl (VSI.ScanID scanID) = baseVsiUrl baseurl /: "scans" /: scanID /: "complete"

vsiCompleteScan :: (Has (Lift IO) sig m, Has Debug sig m, Has Diagnostics sig m) => ApiOpts -> VSI.ScanID -> m ()
vsiCompleteScan apiOpts scanID = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts

  -- Completing the scan can take a fair amount of time for very large scans.
  -- For a project the size of Chromium, for example, it could take a minute or two (the server does a lot of work to mark a scan complete today).
  -- This timeout value wasn't chosen for any specific reason other than "it's unlikely we'll ever hit this for projects of the size we envision people scanning".
  let opts = baseOpts <> responseTimeoutSeconds 600

  -- Indicate that the entire scan is complete.
  let body = VSICompleteScanRequestBody "/"
  _ <- req PUT (vsiCompleteScanEndpoint baseUrl scanID) (ReqBodyJsonCompat body) ignoreResponse opts
  pure ()

newtype VSIScanAnalysisStatusBody = VSIScanAnalysisStatusBody {unVSIScanAnalysisStatusBody :: VSI.AnalysisStatus}

instance FromJSON VSIScanAnalysisStatusBody where
  parseJSON = withObject "VSIScanAnalysisStatusBody" $ \obj -> do
    status <- obj .: "Status"
    pure . VSIScanAnalysisStatusBody $ VSI.parseAnalysisStatus status

vsiScanAnalysisStatusEndpoint :: Url scheme -> VSI.ScanID -> Url scheme
vsiScanAnalysisStatusEndpoint baseurl (VSI.ScanID scanID) = baseVsiUrl baseurl /: "scans" /: scanID /: "status" /: "analysis"

vsiScanAnalysisStatus :: (Has (Lift IO) sig m, Has Debug sig m, Has Diagnostics sig m) => ApiOpts -> VSI.ScanID -> m VSI.AnalysisStatus
vsiScanAnalysisStatus apiOpts scanID = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts
  body <- responseBody <$> req GET (vsiScanAnalysisStatusEndpoint baseUrl scanID) NoReqBody jsonResponse baseOpts
  pure $ unVSIScanAnalysisStatusBody body

vsiDownloadInferencesEndpoint :: Url scheme -> VSI.ScanID -> Url scheme
vsiDownloadInferencesEndpoint baseurl (VSI.ScanID scanID) = baseVsiUrl baseurl /: "scans" /: scanID /: "inferences"

vsiDownloadInferences :: (Has (Lift IO) sig m, Has Debug sig m, Has Diagnostics sig m) => ApiOpts -> VSI.ScanID -> m VSI.VsiExportedInferencesBody
vsiDownloadInferences apiOpts scanID = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts
  responseBody <$> req GET (vsiDownloadInferencesEndpoint baseUrl scanID) NoReqBody jsonResponse baseOpts

endpointAppManifest :: Url scheme -> Url scheme
endpointAppManifest baseurl = baseurl /: "rest" /: "applinks" /: "*" /: "manifest"

newtype AppManifest = AppManifest {endpointAppVersion :: Text} deriving (Show, Eq, Ord)

instance FromXML AppManifest where
  parseElement el = AppManifest <$> child "version" el

getEndpointVersion :: (Has (Lift IO) sig m, Has Debug sig m, Has Diagnostics sig m) => ApiOpts -> m Text
getEndpointVersion apiOpts = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts
  body <- responseBody <$> req GET (endpointAppManifest baseUrl) NoReqBody bsResponse baseOpts
  case parseXML (decodeUtf8 body) of
    Left err -> fatalText (xmlErrorPretty err)
    Right (appManifest :: AppManifest) -> pure $ endpointAppVersion appManifest


---- Path Dependency

signedLicenseScanPathDependencyURLEndpoint :: Url 'Https -> Url 'Https
signedLicenseScanPathDependencyURLEndpoint baseUrl = baseUrl /: "api" /: "cli" /: "path_dependency" /: "upload"

getUploadURLForPathDependency ::
  (Has (Lift IO) sig m, Has Debug sig m, Has Diagnostics sig m) =>
  ApiOpts ->
  Text ->
  Text ->
  m PathDependencyUpload
getUploadURLForPathDependency apiOpts revision path = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts

  let opts = "path" =: path <> "version" =: revision

  response <-
    context ("Retrieving a signed S3 URL for license scan results of " <> path) $
      req GET (signedLicenseScanPathDependencyURLEndpoint baseUrl) NoReqBody jsonResponse (baseOpts <> opts)
  pure (responseBody response)

pathDependencyFinalizeUrl :: Url 'Https -> Url 'Https
pathDependencyFinalizeUrl baseUrl = baseUrl /: "api" /: "cli" /: "path_dependency" /: "finalize"

finalizePathDependencyScan ::
  (Has (Lift IO) sig m, Has Debug sig m, Has Diagnostics sig m) =>
  ApiOpts ->
  [Locator] ->
  Bool ->
  m (Maybe ())
finalizePathDependencyScan apiOpts locators forceRebuild = runEmpty $
  fossaReqAllow401 $ do
    (baseUrl, baseOpts) <- useApiOpts apiOpts
    _ <-
      context "Queuing a build for all license scan uploads" $
        req POST (pathDependencyFinalizeUrl baseUrl) (ReqBodyJson locators) ignoreResponse (baseOpts)
    pure ()
