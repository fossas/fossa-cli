{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module App.Fossa.FossaAPIV1 (
  uploadAnalysis,
  uploadContributors,
  uploadContainerScan,
  mkMetadataOpts,
  fossaReq,
  getLatestBuild,
  getIssues,
  getOrganization,
  getAttribution,
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
) where

import App.Docs (fossaSslCertDocsUrl)
import App.Fossa.Config.Report
import App.Fossa.Container.Scan (ContainerScan (..))
import App.Fossa.Report.Attribution qualified as Attr
import App.Fossa.VSI.Fingerprint (Fingerprint, Raw)
import App.Fossa.VSI.Fingerprint qualified as Fingerprint
import App.Fossa.VSI.IAT.Types qualified as IAT
import App.Fossa.VSI.Types qualified as VSI
import App.Support (reportDefectMsg)
import App.Types (
  ProjectMetadata (..),
  ProjectRevision (..),
  ReleaseGroupMetadata (releaseGroupName, releaseGroupRelease),
 )
import App.Version (versionNumber)
import Codec.Compression.GZip qualified as GZIP
import Control.Algebra (Algebra, Has, type (:+:))
import Control.Carrier.Empty.Maybe (Empty, EmptyC, runEmpty)
import Control.Effect.Diagnostics (Diagnostics, ToDiagnostic (..), context, fatal, fromMaybeText)
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
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Maybe (catMaybes, fromMaybe)
import Data.String.Conversion (decodeUtf8, toStrict, toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word8)
import Effect.Logger (
  Pretty (pretty),
  indent,
  viaShow,
  vsep,
 )
import Fossa.API.Types (
  ApiOpts,
  Archive,
  ArchiveComponents (ArchiveComponents),
  Build,
  Contributors,
  Issues,
  Organization (organizationId),
  Project,
  SignedURL (signedURL),
  UploadResponse,
  useApiOpts,
 )
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
  ignoreResponse,
  jsonResponse,
  lbsResponse,
  req,
  reqCb,
  responseBody,
  responseTimeout,
  useURI,
  (/:),
  (=:),
 )
import Network.HTTP.Req.Extra (httpConfigRetryTimeouts)
import Network.HTTP.Types qualified as HTTP
import Path (File, Path, Rel)
import Srclib.Types (
  LicenseSourceUnit,
  Locator (..),
  SourceUnit,
  parseLocator,
  renderLocator,
 )
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
      <$> v .: "message"
      <*> v .: "uuid"

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

-- | This renders an organization + locator into a path piece for the fossa API
renderLocatorUrl :: Int -> Locator -> Text
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
  | OtherError HttpException
  | BackendPublicFacingError FossaPublicFacingError
  deriving (Show)

instance ToDiagnostic FossaError where
  renderDiagnostic = \case
    InternalException exception ->
      vsep
        [ "An error occurred when accessing the FOSSA API."
        , ""
        , indent 4 $ pretty . displayException $ exception
        , ""
        , "If the exception is related to certificate, please refer to:"
        , indent 4 $ pretty ("- " <> fossaSslCertDocsUrl)
        , ""
        , reportDefectMsg
        ]
    JsonDeserializeError err -> "An error occurred when deserializing a response from the FOSSA API: " <> pretty err
    OtherError err -> "An unknown error occurred when accessing the FOSSA API: " <> viaShow err
    BackendPublicFacingError pfe ->
      vsep
        [ "An error occurred when accessing the FOSSA API."
        , ""
        , "Error message from API:"
        , ""
        , indent 4 $ pretty . fpeMessage $ pfe
        , ""
        , "Error UUID from API:"
        , ""
        , indent 4 $ pretty . fpeUuid $ pfe
        , ""
        , reportDefectMsg
        , "Please include Error UUID in your request."
        ]

containerUploadUrl :: Url scheme -> Url scheme
containerUploadUrl baseurl = baseurl /: "api" /: "container" /: "upload"

uploadContainerScan ::
  (Has (Lift IO) sig m, Has Diagnostics sig m) =>
  ApiOpts ->
  ProjectRevision ->
  ProjectMetadata ->
  ContainerScan ->
  m UploadResponse
uploadContainerScan apiOpts ProjectRevision{..} metadata scan = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts
  let locator = renderLocator $ Locator "custom" projectName (Just projectRevision)
      opts =
        "locator" =: locator
          <> "cliVersion" =: cliVersion
          <> "managedBuild" =: True
          <> maybe mempty ("branch" =:) projectBranch
          <> mkMetadataOpts metadata projectName
  resp <- req POST (containerUploadUrl baseUrl) (ReqBodyJson scan) jsonResponse (baseOpts <> opts)
  pure $ responseBody resp

uploadAnalysis ::
  (Has (Lift IO) sig m, Has Diagnostics sig m) =>
  ApiOpts ->
  ProjectRevision ->
  ProjectMetadata ->
  NE.NonEmpty SourceUnit ->
  m UploadResponse
uploadAnalysis apiOpts ProjectRevision{..} metadata sourceUnits = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts

  let opts =
        "locator" =: renderLocator (Locator "custom" projectName (Just projectRevision))
          <> "cliVersion" =: cliVersion
          <> "managedBuild" =: True
          <> mkMetadataOpts metadata projectName
          -- Don't include branch if it doesn't exist, core may not handle empty string properly.
          <> maybe mempty ("branch" =:) projectBranch
  resp <- req POST (uploadUrl baseUrl) (ReqBodyJson $ NE.toList sourceUnits) jsonResponse (baseOpts <> opts)
  pure (responseBody resp)

mkMetadataOpts :: ProjectMetadata -> Text -> Option scheme
mkMetadataOpts ProjectMetadata{..} projectName = mconcat $ catMaybes maybes
  where
    title = Just $ fromMaybe projectName projectTitle
    maybes =
      [ ("projectURL" =:) <$> projectUrl
      , ("jiraProjectKey" =:) <$> projectJiraKey
      , ("link" =:) <$> projectLink
      , ("team" =:) <$> projectTeam
      , ("policy" =:) <$> projectPolicy
      , ("releaseGroup" =:) . releaseGroupName <$> projectReleaseGroup
      , ("releaseGroupRelease" =:) . releaseGroupRelease <$> projectReleaseGroup
      , ("title" =:) <$> title
      ]

mangleError :: HttpException -> FossaError
mangleError err = case err of
  VanillaHttpException (HTTP.HttpExceptionRequest _ (HTTP.InternalException e)) -> InternalException e
  VanillaHttpException (HTTP.HttpExceptionRequest _ (HTTP.StatusCodeException _ respBody)) ->
    case decodeStrict respBody of
      Just pfe -> BackendPublicFacingError pfe
      Nothing -> OtherError err
  JsonHttpException msg -> JsonDeserializeError msg
  _ -> OtherError err

-----

projectEndpoint :: Url scheme -> Int -> Locator -> Url scheme
projectEndpoint baseurl orgid locator = baseurl /: "api" /: "cli" /: renderLocatorUrl orgid locator /: "project"

getProject ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
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

buildsEndpoint :: Url 'Https -> Int -> Locator -> Url 'Https
buildsEndpoint baseurl orgId locator = baseurl /: "api" /: "cli" /: renderLocatorUrl orgId locator /: "latest_build"

getLatestBuild ::
  (Has (Lift IO) sig m, Has Diagnostics sig m) =>
  ApiOpts ->
  ProjectRevision ->
  m Build
getLatestBuild apiOpts ProjectRevision{..} = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts

  orgId <- organizationId <$> getOrganization apiOpts

  response <- req GET (buildsEndpoint baseUrl orgId (Locator "custom" projectName (Just projectRevision))) NoReqBody jsonResponse baseOpts
  pure (responseBody response)

---------- Archive build queueing. This Endpoint ensures that after an archive is uploaded, it is scanned.

archiveBuildURL :: Url 'Https -> Url 'Https
archiveBuildURL baseUrl = baseUrl /: "api" /: "components" /: "build"

archiveBuildUpload ::
  (Has (Lift IO) sig m, Has Diagnostics sig m) =>
  ApiOpts ->
  Archive ->
  m (Maybe C.ByteString)
archiveBuildUpload apiOpts archive = runEmpty $
  fossaReqAllow401 $ do
    (baseUrl, baseOpts) <- useApiOpts apiOpts

    let opts = "dependency" =: True <> "rawLicenseScan" =: True
    -- The API route expects an array of archives, but doesn't properly handle multiple archives so we upload
    -- an array of a single archive.
    let archiveProjects = ArchiveComponents [archive]
    -- The response appears to either be "Created" for new builds, or an error message for existing builds.
    -- Making the actual return value of "Created" essentially worthless.
    resp <-
      context "Queuing a build for all archive uploads" $
        req POST (archiveBuildURL baseUrl) (ReqBodyJson archiveProjects) bsResponse (baseOpts <> opts)
    pure (responseBody resp)

---------- license-scan build queueing. This Endpoint ensures that after a license-scan is uploaded, it is scanned.

licenseScanFinalizeUrl :: Url 'Https -> Url 'Https
licenseScanFinalizeUrl baseUrl = baseUrl /: "api" /: "license_scan" /: "finalize"

-- TODO: /license_scan/finalize just returns a 201 if there's a success. No need to parse the body
licenseScanFinalize ::
  (Has (Lift IO) sig m, Has Diagnostics sig m) =>
  ApiOpts ->
  ArchiveComponents ->
  m (Maybe ())
licenseScanFinalize apiOpts archiveProjects = runEmpty $
  fossaReqAllow401 $ do
    (baseUrl, baseOpts) <- useApiOpts apiOpts

    let opts = "dependency" =: True <> "rawLicenseScan" =: True

    _ <-
      context "Queuing a build for all license scan uploads" $
        req POST (licenseScanFinalizeUrl baseUrl) (ReqBodyJson archiveProjects) ignoreResponse (baseOpts <> opts)
    pure ()

---------- The signed URL endpoint returns a URL endpoint that can be used to directly upload to an S3 bucket.

signedURLEndpoint :: Url 'Https -> Url 'Https
signedURLEndpoint baseUrl = baseUrl /: "api" /: "components" /: "signed_url"

getSignedURL ::
  (Has (Lift IO) sig m, Has Diagnostics sig m) =>
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

---------- The signed License Scan URL endpoint returns a URL endpoint that can be used to directly upload the results of a license scan to an S3 bucket.

signedLicenseScanURLEndpoint :: Url 'Https -> Url 'Https
signedLicenseScanURLEndpoint baseUrl = baseUrl /: "api" /: "license_scan" /: "signed_url"

getSignedLicenseScanURL ::
  (Has (Lift IO) sig m, Has Diagnostics sig m) =>
  ApiOpts ->
  Text ->
  Text ->
  m SignedURL
getSignedLicenseScanURL apiOpts revision packageName = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts

  let opts = "packageSpec" =: packageName <> "revision" =: revision

  response <-
    context ("Retrieving a signed S3 URL for license scan results of " <> packageName) $
      req GET (signedLicenseScanURLEndpoint baseUrl) NoReqBody jsonResponse (baseOpts <> opts)
  pure (responseBody response)

---------- The archive upload function uploads the file it is given directly to the signed URL it is provided.

archiveUpload ::
  (Has (Lift IO) sig m, Has Diagnostics sig m) =>
  SignedURL ->
  FilePath ->
  m LbsResponse
archiveUpload signedArcURI arcFile = fossaReq $ do
  let arcURL = URI.mkURI $ signedURL signedArcURI

  uri <- fromMaybeText ("Invalid URL: " <> signedURL signedArcURI) arcURL
  validatedURI <- fromMaybeText ("Invalid URI: " <> toText (show uri)) (useURI uri)

  context ("Uploading project archive to " <> signedURL signedArcURI) $ case validatedURI of
    Left (url, options) -> uploadArchiveRequest url options
    Right (url, options) -> uploadArchiveRequest url options
  where
    uploadArchiveRequest url options = reqCb PUT url (ReqBodyFile arcFile) lbsResponse options (pure . requestEncoder)

---------- The license scan result upload function uploads the JSON license result directly to the signed URL it is provided.

licenseScanResultUpload ::
  (Has (Lift IO) sig m, Has Diagnostics sig m) =>
  SignedURL ->
  LicenseSourceUnit ->
  m LbsResponse
licenseScanResultUpload signedArcURI licenseScanResult = fossaReq $ do
  let arcURL = URI.mkURI $ signedURL signedArcURI

  uri <- fromMaybeText ("Invalid URL: " <> signedURL signedArcURI) arcURL
  validatedURI <- fromMaybeText ("Invalid URI: " <> toText (show uri)) (useURI uri)

  context ("Uploading license scan result to " <> signedURL signedArcURI) $ case validatedURI of
    Left (httpUrl, httpOptions) -> uploadArchiveRequest httpUrl httpOptions
    Right (httpsUrl, httpsOptions) -> uploadArchiveRequest httpsUrl httpsOptions
  where
    zippedLicenseResult :: BS.ByteString
    zippedLicenseResult = toStrict $ GZIP.compress $ encode licenseScanResult
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

issuesEndpoint :: Url 'Https -> Int -> Locator -> Url 'Https
issuesEndpoint baseUrl orgId locator = baseUrl /: "api" /: "cli" /: renderLocatorUrl orgId locator /: "issues"

getIssues ::
  (Has (Lift IO) sig m, Has Diagnostics sig m) =>
  ApiOpts ->
  ProjectRevision ->
  m Issues
getIssues apiOpts ProjectRevision{..} = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts

  orgId <- organizationId <$> getOrganization apiOpts
  response <- req GET (issuesEndpoint baseUrl orgId (Locator "custom" projectName (Just projectRevision))) NoReqBody jsonResponse baseOpts
  pure (responseBody response)

---------------

attributionEndpoint :: Url 'Https -> Int -> Locator -> ReportOutputFormat -> Url 'Https
attributionEndpoint baseurl orgId locator format = appendSegment format $ baseurl /: "api" /: "revisions" /: renderLocatorUrl orgId locator /: "attribution"
  where
    appendSegment :: ReportOutputFormat -> Url a -> Url a
    appendSegment ReportJson input = input /: "json"
    appendSegment ReportMarkdown input = input /: "full" /: "MD"
    appendSegment ReportSpdx input = input /: "full" /: "spdx"

getAttributionJson ::
  (Has (Lift IO) sig m, Has Diagnostics sig m) =>
  ApiOpts ->
  ProjectRevision ->
  m Attr.Attribution
getAttributionJson apiOpts ProjectRevision{..} = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts

  let opts =
        baseOpts
          <> "includeDeepDependencies" =: True
          <> "includeHashAndVersionData" =: True
          <> "includeDownloadUrl" =: True
  orgId <- organizationId <$> getOrganization apiOpts
  response <- req GET (attributionEndpoint baseUrl orgId (Locator "custom" projectName (Just projectRevision)) ReportJson) NoReqBody jsonResponse opts
  pure (responseBody response)

getAttribution ::
  (Has (Lift IO) sig m, Has Diagnostics sig m) =>
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

getOrganization :: (Has (Lift IO) sig m, Has Diagnostics sig m) => ApiOpts -> m Organization
getOrganization apiOpts = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts
  responseBody <$> req GET (organizationEndpoint baseUrl) NoReqBody jsonResponse baseOpts

----------

contributorsEndpoint :: Url scheme -> Url scheme
contributorsEndpoint baseurl = baseurl /: "api" /: "contributors"

uploadContributors :: (Has (Lift IO) sig m, Has Diagnostics sig m) => ApiOpts -> Text -> Contributors -> m ()
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

assertUserDefinedBinaries :: (Has (Lift IO) sig m, Has Diagnostics sig m) => ApiOpts -> IAT.UserDefinedAssertionMeta -> [Fingerprint Raw] -> m ()
assertUserDefinedBinaries apiOpts IAT.UserDefinedAssertionMeta{..} fingerprints = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts
  let body = UserDefinedAssertionBody assertedName assertedVersion assertedLicense assertedDescription assertedUrl (FingerprintAssertion <$> fingerprints)
  _ <- req POST (assertUserDefinedBinariesEndpoint baseUrl) (ReqBodyJson body) ignoreResponse baseOpts
  pure ()

assertRevisionBinariesEndpoint :: Url scheme -> Locator -> Url scheme
assertRevisionBinariesEndpoint baseurl locator = baseurl /: "api" /: "iat" /: "binary" /: renderLocator locator

assertRevisionBinaries :: (Has (Lift IO) sig m, Has Diagnostics sig m) => ApiOpts -> Locator -> [Fingerprint Raw] -> m ()
assertRevisionBinaries apiOpts locator fingerprints = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts
  let body = FingerprintAssertion <$> fingerprints
  _ <- req POST (assertRevisionBinariesEndpoint baseUrl locator) (ReqBodyJson body) ignoreResponse baseOpts
  pure ()

resolveUserDefinedBinaryEndpoint :: Url scheme -> IAT.UserDep -> Url scheme
resolveUserDefinedBinaryEndpoint baseurl dep = baseurl /: "api" /: "iat" /: "resolve" /: "user-defined" /: IAT.renderUserDep dep

resolveUserDefinedBinary :: (Has (Lift IO) sig m, Has Diagnostics sig m) => ApiOpts -> IAT.UserDep -> m IAT.UserDefinedAssertionMeta
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

resolveProjectDependencies :: (Has (Lift IO) sig m, Has Diagnostics sig m) => ApiOpts -> VSI.Locator -> m [VSI.Locator]
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
  { vsiCreateScanRequestBodyOrgID :: Int
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

vsiCreateScan :: (Has (Lift IO) sig m, Has Diagnostics sig m) => ApiOpts -> ProjectRevision -> m VSI.ScanID
vsiCreateScan apiOpts ProjectRevision{..} = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts

  orgId <- organizationId <$> getOrganization apiOpts
  let projectID = renderLocator $ Locator "custom" projectName Nothing
  let reqBody = VSICreateScanRequestBody orgId projectRevision projectID

  body <- responseBody <$> req POST (vsiCreateScanEndpoint baseUrl) (ReqBodyJsonCompat reqBody) jsonResponse baseOpts
  pure $ unVSICreateScanResponseBody body

newtype VSIAddFilesToScanRequestBody = VSIAddFilesToScanRequestBody {vsiAddFilesToScanRequestBodyFiles :: Map (Path Rel File) Fingerprint.Combined}

instance ToJSON VSIAddFilesToScanRequestBody where
  toJSON VSIAddFilesToScanRequestBody{..} = object ["ScanData" .= vsiAddFilesToScanRequestBodyFiles]

vsiAddFilesToScanEndpoint :: Url scheme -> VSI.ScanID -> Url scheme
vsiAddFilesToScanEndpoint baseurl (VSI.ScanID scanID) = baseVsiUrl baseurl /: "scans" /: scanID /: "files"

vsiAddFilesToScan :: (Has (Lift IO) sig m, Has Diagnostics sig m) => ApiOpts -> VSI.ScanID -> Map (Path Rel File) Fingerprint.Combined -> m ()
vsiAddFilesToScan apiOpts scanID files = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts

  let body = VSIAddFilesToScanRequestBody files
  _ <- req POST (vsiAddFilesToScanEndpoint baseUrl scanID) (ReqBodyJsonCompat body) ignoreResponse baseOpts

  pure ()

-- | The 'vsiCompleteScanFilePath' is an absolute path denoting what portion of the scan should be considered complete.
-- In this path structure, '/' means "the root of the scan".
-- Technically this should really be a @Path Abs Dir@, because that's what it represents in the backend.
-- However @$(mkAbsDir "/")@ fails in Windows builds, and is the only thing we ever actually pass in, so leaving it @Text@.
newtype VSICompleteScanRequestBody = VSICompleteScanRequestBody {vsiCompleteScanFilePath :: Text}

instance ToJSON VSICompleteScanRequestBody where
  toJSON VSICompleteScanRequestBody{..} = object ["FilePath" .= toText vsiCompleteScanFilePath]

vsiCompleteScanEndpoint :: Url scheme -> VSI.ScanID -> Url scheme
vsiCompleteScanEndpoint baseurl (VSI.ScanID scanID) = baseVsiUrl baseurl /: "scans" /: scanID /: "complete"

vsiCompleteScan :: (Has (Lift IO) sig m, Has Diagnostics sig m) => ApiOpts -> VSI.ScanID -> m ()
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

vsiScanAnalysisStatus :: (Has (Lift IO) sig m, Has Diagnostics sig m) => ApiOpts -> VSI.ScanID -> m VSI.AnalysisStatus
vsiScanAnalysisStatus apiOpts scanID = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts
  body <- responseBody <$> req GET (vsiScanAnalysisStatusEndpoint baseUrl scanID) NoReqBody jsonResponse baseOpts
  pure $ unVSIScanAnalysisStatusBody body

newtype VSIExportedInferencesBody = VSIExportedInferencesBody {unVSIExportedInferencesBody :: [Locator]}

instance FromJSON VSIExportedInferencesBody where
  parseJSON = withObject "VSIExportedInferencesBody" $ \obj -> do
    plainLocators <- obj .: "locators"
    pure . VSIExportedInferencesBody $ fmap parseLocator plainLocators

vsiDownloadInferencesEndpoint :: Url scheme -> VSI.ScanID -> Url scheme
vsiDownloadInferencesEndpoint baseurl (VSI.ScanID scanID) = baseVsiUrl baseurl /: "scans" /: scanID /: "inferences" /: "locator"

vsiDownloadInferences :: (Has (Lift IO) sig m, Has Diagnostics sig m) => ApiOpts -> VSI.ScanID -> m [Locator]
vsiDownloadInferences apiOpts scanID = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts
  body <- responseBody <$> req GET (vsiDownloadInferencesEndpoint baseUrl scanID) NoReqBody jsonResponse baseOpts
  pure $ unVSIExportedInferencesBody body
