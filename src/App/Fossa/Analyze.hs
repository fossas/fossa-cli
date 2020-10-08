{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module App.Fossa.Analyze
  ( analyzeMain
  , ScanDestination(..)
  ) where

import App.Fossa.Analyze.Project (BestStrategy(..), Project(..), mkProjects)
import App.Fossa.FossaAPIV1 (ProjectMetadata, UploadResponse (..), uploadAnalysis, uploadContributors)
import App.Fossa.ProjectInference (inferProject, mergeOverride)
import App.Types
import qualified Control.Carrier.Diagnostics as Diag
import Control.Carrier.Error.Either
import Control.Carrier.Finally
import Control.Carrier.Output.IO
import Control.Carrier.TaskPool
import Control.Concurrent
import Control.Effect.Lift (Lift, sendIO)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Effect.Exec
import Effect.Logger
import Effect.ReadFS
import Network.HTTP.Types (urlEncode)
import Path
import qualified Srclib.Converter as Srclib
import Srclib.Types (Locator (..), parseLocator)
import qualified Strategy.Archive as Archive
import qualified Strategy.Cargo as Cargo
import qualified Strategy.Carthage as Carthage
import qualified Strategy.Clojure as Clojure
import qualified Strategy.Cocoapods.Podfile as Podfile
import qualified Strategy.Cocoapods.PodfileLock as PodfileLock
import qualified Strategy.Composer as Composer
import qualified Strategy.Erlang.Rebar3Tree as Rebar3Tree
import qualified Strategy.Go.GlideLock as GlideLock
import qualified Strategy.Go.GoList as GoList
import qualified Strategy.Go.Gomod as Gomod
import qualified Strategy.Go.GopkgLock as GopkgLock
import qualified Strategy.Go.GopkgToml as GopkgToml
import qualified Strategy.Googlesource.RepoManifest as RepoManifest
import qualified Strategy.Gradle as Gradle
import qualified Strategy.Haskell.Cabal as Cabal
import qualified Strategy.Haskell.Stack as Stack
import qualified Strategy.Maven.PluginStrategy as MavenPlugin
import qualified Strategy.Maven.Pom as MavenPom
import qualified Strategy.Node.NpmList as NpmList
import qualified Strategy.Node.NpmLock as NpmLock
import qualified Strategy.Node.PackageJson as PackageJson
import qualified Strategy.Node.YarnLock as YarnLock
import qualified Strategy.NuGet.Nuspec as Nuspec
import qualified Strategy.NuGet.PackageReference as PackageReference
import qualified Strategy.NuGet.PackagesConfig as PackagesConfig
import qualified Strategy.NuGet.Paket as Paket
import qualified Strategy.NuGet.ProjectAssetsJson as ProjectAssetsJson
import qualified Strategy.NuGet.ProjectJson as ProjectJson
import qualified Strategy.Python.Pipenv as Pipenv
import qualified Strategy.Python.ReqTxt as ReqTxt
import qualified Strategy.Python.SetupPy as SetupPy
import qualified Strategy.RPM as RPM
import qualified Strategy.Ruby.BundleShow as BundleShow
import qualified Strategy.Ruby.GemfileLock as GemfileLock
import qualified Strategy.Scala as Scala
import Text.URI (URI)
import qualified Text.URI as URI
import Types
import VCS.Git (fetchGitContributors)

data ScanDestination
  = UploadScan URI ApiKey ProjectMetadata -- ^ upload to fossa with provided api key and base url
  | OutputStdout

analyzeMain :: BaseDir -> Severity -> ScanDestination -> OverrideProject -> Bool -> IO ()
analyzeMain basedir logSeverity destination project unpackArchives = withLogger logSeverity $
  analyze basedir destination project unpackArchives

analyze ::
  ( Has (Lift IO) sig m
  , Has Logger sig m
  , MonadIO m
  )
  => BaseDir
  -> ScanDestination
  -> OverrideProject
  -> Bool -- ^ whether to unpack archives
  -> m ()
analyze basedir destination override unpackArchives = runFinally $ do
  capabilities <- sendIO getNumCapabilities

  (closures,(failures,())) <- runOutput @ProjectClosure . runOutput @ProjectFailure . runExecIO . runReadFSIO $
    withTaskPool capabilities updateProgress $
      if unpackArchives
        then discoverWithArchives $ unBaseDir basedir
        else discover $ unBaseDir basedir

  traverse_ (logDebug . Diag.renderFailureBundle . projectFailureCause) failures

  logSticky ""

  let projects = mkProjects closures
      result = buildResult projects failures

  traverse_ (logInfo . ("Found " <>) . pretty . BestStrategy) projects

  case destination of
    OutputStdout -> logStdout $ pretty (decodeUtf8 (Aeson.encode result))
    UploadScan baseurl apiKey metadata -> do
      revision <- mergeOverride override <$> inferProject (unBaseDir basedir)

      logInfo ""
      logInfo ("Using project name: `" <> pretty (projectName revision) <> "`")
      logInfo ("Using revision: `" <> pretty (projectRevision revision) <> "`")
      let branchText = fromMaybe "No branch (detached HEAD)" $ projectBranch revision
      logInfo ("Using branch: `" <> pretty branchText <> "`")

      uploadResult <- Diag.runDiagnostics $ uploadAnalysis basedir baseurl apiKey revision metadata projects
      case uploadResult of
        Left failure -> logError (Diag.renderFailureBundle failure)
        Right success -> do
          let resp = Diag.resultValue success
          logInfo $ vsep
            [ "============================================================"
            , ""
            , "    View FOSSA Report:"
            , "    " <> pretty (fossaProjectUrl baseurl (uploadLocator resp) revision)
            , ""
            , "============================================================"
            ]
          traverse_ (\err -> logError $ "FOSSA error: " <> viaShow err) (uploadError resp)

          contribResult <- Diag.runDiagnostics $ runExecIO $ tryUploadContributors (unBaseDir basedir) baseurl apiKey $ uploadLocator resp
          case contribResult of
            Left failure -> logDebug (Diag.renderFailureBundle failure)
            Right _ -> pure ()

tryUploadContributors ::
  ( Has Diag.Diagnostics sig m,
    Has Exec sig m,
    Has (Lift IO) sig m
  ) =>
  Path x Dir ->
  URI ->
  ApiKey ->
  -- | Locator
  Text ->
  m ()
tryUploadContributors baseDir baseUrl apiKey locator = do
  contributors <- fetchGitContributors baseDir
  uploadContributors baseUrl apiKey locator contributors

-- This url can have a two forms (Core may allow more, but we don't care here):
--    https://<fossa host>/projects/<project>/
--    https://<fossa host>/projects/<project>/refs/branch/<branch>/<revision>
fossaProjectUrl :: URI -> Text -> ProjectRevision -> Text
fossaProjectUrl baseUrl rawLocator revision = URI.render baseUrl <> "projects/" <> encodedProject <> buildSelector
  where
    Locator{locatorFetcher, locatorProject, locatorRevision} = parseLocator rawLocator

    underBS :: (ByteString -> ByteString) -> Text -> Text
    underBS f = TE.decodeUtf8 . f . TE.encodeUtf8
    urlEncode' = underBS (urlEncode True)

    encodedProject = urlEncode' (locatorFetcher <> "+" <> locatorProject)
    encodedRevision = urlEncode' (fromMaybe "" locatorRevision)
    -- | buildSelector is empty string unless we have a real branch to work with.
    buildSelector = fromMaybe "" $ do
      branch <- projectBranch revision
      Just $ "/refs/branch/" <> urlEncode' branch <> "/" <> encodedRevision

buildResult :: [Project] -> [ProjectFailure] -> Aeson.Value
buildResult projects failures = Aeson.object
  [ "projects" .= projects
  , "failures" .= map renderFailure failures
  , "sourceUnits" .= map Srclib.toSourceUnit projects
  ]

renderFailure :: ProjectFailure -> Aeson.Value
renderFailure failure = Aeson.object
  [ "name" .= projectFailureName failure
  , "cause" .= show (Diag.renderFailureBundle (projectFailureCause failure))
  ]

discover :: HasDiscover sig m => Path Abs Dir -> m ()
discover dir = traverse_ (forkTask . apply dir) discoverFuncs

discoverWithArchives :: HasDiscover sig m => Path Abs Dir -> m ()
discoverWithArchives dir = traverse_ (forkTask . apply dir) (Archive.discover discoverWithArchives : discoverFuncs)

apply :: a -> (a -> b) -> b
apply x f = f x

discoverFuncs :: HasDiscover sig m => [Path Abs Dir -> m ()]
discoverFuncs =
  [ Rebar3Tree.discover

  , GoList.discover
  , Gomod.discover
  , GopkgToml.discover
  , GopkgLock.discover
  , GlideLock.discover

  , Gradle.discover

  , MavenPlugin.discover
  , MavenPom.discover

  , PackageJson.discover
  , NpmLock.discover
  , NpmList.discover
  , YarnLock.discover

  , PackagesConfig.discover
  , PackageReference.discover
  , ProjectAssetsJson.discover
  , ProjectJson.discover
  , Nuspec.discover
  , Paket.discover

  , Pipenv.discover
  , SetupPy.discover
  , ReqTxt.discover

  , RepoManifest.discover

  , BundleShow.discover
  , GemfileLock.discover

  , Carthage.discover

  , Podfile.discover
  , PodfileLock.discover

  , Composer.discover

  , Clojure.discover
  
  , Cargo.discover

  , RPM.discover

  , Scala.discover

  , Cabal.discover

  , Stack.discover
  ]

updateProgress :: Has Logger sig m => Progress -> m ()
updateProgress Progress{..} =
  logSticky ( "[ "
            <> annotate (color Cyan) (pretty pQueued)
            <> " Waiting / "
            <> annotate (color Yellow) (pretty pRunning)
            <> " Running / "
            <> annotate (color Green) (pretty pCompleted)
            <> " Completed"
            <> " ]" )
