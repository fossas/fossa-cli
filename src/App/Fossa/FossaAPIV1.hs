{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module App.Fossa.FossaAPIV1 (
  uploadAnalysis,
  uploadContributors,
  uploadContainerScan,
  UploadResponse (..),
  mkMetadataOpts,
  FossaError (..),
  FossaReq (..),
  Contributors (..),
  fossaReq,
  getLatestBuild,
  Build (..),
  BuildTask (..),
  BuildStatus (..),
  getIssues,
  Organization (..),
  Project (..),
  getOrganization,
  getAttribution,
  getAttributionRaw,
  getSignedURL,
  getProject,
  archiveUpload,
  archiveBuildUpload,
  assertUserDefinedBinaries,
  assertRevisionBinaries,
  resolveUserDefinedBinary,
  resolveProjectDependencies,
) where

import App.Fossa.Container (ContainerScan (..))
import App.Fossa.Report.Attribution qualified as Attr
import App.Fossa.VSI.IAT.Types qualified as IAT
import App.Fossa.VSI.Types qualified as VSI
import App.Types
import App.Version (versionNumber)
import Control.Carrier.Empty.Maybe (Empty, EmptyC, runEmpty)
import Control.Effect.Diagnostics (Diagnostics, ToDiagnostic (..), context, fatal, fromMaybeText)
import Control.Effect.Empty (empty)
import Control.Effect.Lift (Lift, sendIO)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Maybe (catMaybes, fromMaybe)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word8)
import Effect.Logger
import Fossa.API.Types (ApiOpts, ArchiveComponents, Issues, SignedURL, signedURL, useApiOpts)
import Network.HTTP.Client qualified as C
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Req
import Network.HTTP.Req.Extra (httpConfigRetryTimeouts)
import Network.HTTP.Types qualified as HTTP
import Srclib.Types
import Text.URI (URI)
import Text.URI qualified as URI
import Prelude

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

data UploadResponse = UploadResponse
  { uploadLocator :: Text
  , uploadError :: Maybe Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON UploadResponse where
  parseJSON = withObject "UploadResponse" $ \obj ->
    UploadResponse <$> obj .: "locator"
      <*> obj .:? "error"

data FossaError
  = InvalidProjectOrRevision HttpException
  | NoPermission HttpException
  | JsonDeserializeError String
  | OtherError HttpException
  | BadURI URI
  deriving (Show)

instance ToDiagnostic FossaError where
  renderDiagnostic = \case
    InvalidProjectOrRevision _ -> "Response from FOSSA API: invalid project or revision"
    NoPermission _ -> "Response from FOSSA API: no permission"
    JsonDeserializeError err -> "An error occurred when deserializing a response from the FOSSA API: " <> pretty err
    OtherError err -> "An unknown error occurred when accessing the FOSSA API: " <> viaShow err
    BadURI uri -> "Invalid FOSSA URL: " <> pretty (URI.render uri)

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
  VanillaHttpException (HTTP.HttpExceptionRequest _ (HTTP.StatusCodeException resp _)) ->
    case HTTP.responseStatus resp of
      HTTP.Status 404 _ -> InvalidProjectOrRevision err
      HTTP.Status 403 _ -> NoPermission err
      _ -> OtherError err
  JsonHttpException msg -> JsonDeserializeError msg
  _ -> OtherError err

-----

projectEndpoint :: Url scheme -> Int -> Locator -> Url scheme
projectEndpoint baseurl orgid locator = baseurl /: "api" /: "cli" /: renderLocatorUrl orgid locator /: "project"

data Project = Project
  { projectId :: Text
  , projectTitle :: Text
  , projectIsMonorepo :: Bool
  }
  deriving (Eq, Ord, Show)

instance FromJSON Project where
  parseJSON = withObject "Project" $ \obj ->
    Project <$> obj .: "id"
      <*> obj .: "title"
      <*> obj .: "isMonorepo"

getProject ::
  ( Has (Lift IO) sig m
  , Has Diagnostics sig m
  ) =>
  ApiOpts ->
  ProjectRevision ->
  m Project
getProject apiopts ProjectRevision{..} = fossaReq $ do
  (baseurl, baseopts) <- useApiOpts apiopts

  Organization orgid _ <- getOrganization apiopts

  let endpoint = projectEndpoint baseurl orgid $ Locator "custom" projectName Nothing

  responseBody <$> req GET endpoint NoReqBody jsonResponse baseopts

-----

buildsEndpoint :: Url 'Https -> Int -> Locator -> Url 'Https
buildsEndpoint baseurl orgId locator = baseurl /: "api" /: "cli" /: renderLocatorUrl orgId locator /: "latest_build"

data BuildStatus
  = StatusSucceeded
  | StatusFailed
  | StatusCreated
  | StatusAssigned
  | StatusRunning
  | StatusUnknown Text
  deriving (Eq, Ord, Show)

data Build = Build
  { buildId :: Int
  , buildError :: Maybe Text
  , buildTask :: BuildTask
  }
  deriving (Eq, Ord, Show)

newtype BuildTask = BuildTask
  { buildTaskStatus :: BuildStatus
  }
  deriving (Eq, Ord, Show)

instance FromJSON Build where
  parseJSON = withObject "Build" $ \obj ->
    Build <$> obj .: "id"
      <*> obj .:? "error"
      <*> obj .: "task"

instance FromJSON BuildTask where
  parseJSON = withObject "BuildTask" $ \obj ->
    BuildTask <$> obj .: "status"

instance FromJSON BuildStatus where
  parseJSON = withText "BuildStatus" $ \case
    "SUCCEEDED" -> pure StatusSucceeded
    "FAILED" -> pure StatusFailed
    "CREATED" -> pure StatusCreated
    "ASSIGNED" -> pure StatusAssigned
    "RUNNING" -> pure StatusRunning
    other -> pure $ StatusUnknown other

getLatestBuild ::
  (Has (Lift IO) sig m, Has Diagnostics sig m) =>
  ApiOpts ->
  ProjectRevision ->
  m Build
getLatestBuild apiOpts ProjectRevision{..} = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts

  Organization orgId _ <- getOrganization apiOpts

  response <- req GET (buildsEndpoint baseUrl orgId (Locator "custom" projectName (Just projectRevision))) NoReqBody jsonResponse baseOpts
  pure (responseBody response)

---------- Archive build queueing. This Endpoint ensures that after an archive is uploaded, it is scanned.

archiveBuildURL :: Url 'Https -> Url 'Https
archiveBuildURL baseUrl = baseUrl /: "api" /: "components" /: "build"

archiveBuildUpload ::
  (Has (Lift IO) sig m, Has Diagnostics sig m) =>
  ApiOpts ->
  ArchiveComponents ->
  m (Maybe C.ByteString)
archiveBuildUpload apiOpts archiveProjects = runEmpty $
  fossaReqAllow401 $ do
    (baseUrl, baseOpts) <- useApiOpts apiOpts

    let opts = "dependency" =: True <> "rawLicenseScan" =: True

    -- The response appears to either be "Created" for new builds, or an error message for existing builds.
    -- Making the actual return value of "Created" essentially worthless.
    resp <-
      context "Queuing a build for an archive project" $
        req POST (archiveBuildURL baseUrl) (ReqBodyJson archiveProjects) bsResponse (baseOpts <> opts)
    pure (responseBody resp)

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
    context "Retrieving a signed S3 URL" $
      req GET (signedURLEndpoint baseUrl) NoReqBody jsonResponse (baseOpts <> opts)
  pure (responseBody response)

---------- The archive upload function uploads the file it is given directly to the signed URL it is provided.

archiveUpload ::
  (Has (Lift IO) sig m, Has Diagnostics sig m) =>
  SignedURL ->
  FilePath ->
  m ()
archiveUpload signedArcURI arcFile = fossaReq $ do
  let arcURL = URI.mkURI $ signedURL signedArcURI

  uri <- fromMaybeText ("Invalid URL: " <> signedURL signedArcURI) arcURL
  validatedURI <- fromMaybeText ("Invalid URI: " <> toText (show uri)) (useURI uri)

  _ <- context "Uploading project archive" $ case validatedURI of
    Left (url, options) -> uploadArchiveRequest url options
    Right (url, options) -> uploadArchiveRequest url options

  pure ()
  where
    uploadArchiveRequest url options = reqCb PUT url (ReqBodyFile arcFile) lbsResponse options (pure . requestEncoder)

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

  Organization orgId _ <- getOrganization apiOpts
  response <- req GET (issuesEndpoint baseUrl orgId (Locator "custom" projectName (Just projectRevision))) NoReqBody jsonResponse baseOpts
  pure (responseBody response)

---------------

attributionEndpoint :: Url 'Https -> Int -> Locator -> Url 'Https
attributionEndpoint baseurl orgId locator = baseurl /: "api" /: "revisions" /: renderLocatorUrl orgId locator /: "attribution" /: "json"

getAttribution ::
  (Has (Lift IO) sig m, Has Diagnostics sig m) =>
  ApiOpts ->
  ProjectRevision ->
  m Attr.Attribution
getAttribution apiOpts ProjectRevision{..} = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts

  let opts =
        baseOpts
          <> "includeDeepDependencies" =: True
          <> "includeHashAndVersionData" =: True
          <> "includeDownloadUrl" =: True
  Organization orgId _ <- getOrganization apiOpts
  response <- req GET (attributionEndpoint baseUrl orgId (Locator "custom" projectName (Just projectRevision))) NoReqBody jsonResponse opts
  pure (responseBody response)

getAttributionRaw ::
  (Has (Lift IO) sig m, Has Diagnostics sig m) =>
  ApiOpts ->
  ProjectRevision ->
  m Value
getAttributionRaw apiOpts ProjectRevision{..} = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts

  let opts =
        baseOpts
          <> "includeDeepDependencies" =: True
          <> "includeHashAndVersionData" =: True
          <> "includeDownloadUrl" =: True
  Organization orgId _ <- getOrganization apiOpts
  response <- req GET (attributionEndpoint baseUrl orgId (Locator "custom" projectName (Just projectRevision))) NoReqBody jsonResponse opts
  pure (responseBody response)

----------

data Organization = Organization
  { organizationId :: Int
  , orgUsesSAML :: Bool
  }
  deriving (Eq, Ord, Show)

instance FromJSON Organization where
  parseJSON = withObject "Organization" $ \obj ->
    Organization <$> obj .: "organizationId"
      <*> obj .:? "usesSAML" .!= False

organizationEndpoint :: Url scheme -> Url scheme
organizationEndpoint baseurl = baseurl /: "api" /: "cli" /: "organization"

getOrganization :: (Has (Lift IO) sig m, Has Diagnostics sig m) => ApiOpts -> m Organization
getOrganization apiOpts = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts
  responseBody <$> req GET (organizationEndpoint baseUrl) NoReqBody jsonResponse baseOpts

----------

newtype Contributors = Contributors
  {unContributors :: Map Text Text}
  deriving (Eq, Ord, Show, ToJSON)

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
newtype FingerprintSet = FingerprintSet
  { fingerprintRaw :: IAT.Fingerprint
  }

instance ToJSON FingerprintSet where
  toJSON FingerprintSet{..} =
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
  , bodyFingerprints :: [FingerprintSet]
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

assertUserDefinedBinaries :: (Has (Lift IO) sig m, Has Diagnostics sig m) => ApiOpts -> IAT.UserDefinedAssertionMeta -> [IAT.Fingerprint] -> m ()
assertUserDefinedBinaries apiOpts IAT.UserDefinedAssertionMeta{..} fingerprints = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts
  let body = UserDefinedAssertionBody assertedName assertedVersion assertedLicense assertedDescription assertedUrl (FingerprintSet <$> fingerprints)
  _ <- req POST (assertUserDefinedBinariesEndpoint baseUrl) (ReqBodyJson body) ignoreResponse baseOpts
  pure ()

assertRevisionBinariesEndpoint :: Url scheme -> Locator -> Url scheme
assertRevisionBinariesEndpoint baseurl locator = baseurl /: "api" /: "iat" /: "binary" /: renderLocator locator

assertRevisionBinaries :: (Has (Lift IO) sig m, Has Diagnostics sig m) => ApiOpts -> Locator -> [IAT.Fingerprint] -> m ()
assertRevisionBinaries apiOpts locator fingerprints = fossaReq $ do
  (baseUrl, baseOpts) <- useApiOpts apiOpts
  let body = FingerprintSet <$> fingerprints
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
