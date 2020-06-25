
module App.Fossa.Analyze
  ( analyzeMain
  , ScanDestination(..)
  ) where

import Prologue

import Control.Carrier.Error.Either
import Control.Effect.Lift (Lift)
import qualified Control.Carrier.Diagnostics as Diag
import Control.Carrier.Output.IO
import Control.Concurrent
import Path.IO

import App.Fossa.CliTypes
import App.Fossa.FossaAPIV1 (ProjectMetadata, uploadAnalysis, UploadResponse(..))
import App.Fossa.Analyze.Project (Project, mkProjects)
import App.Fossa.ProjectInference (mergeOverride, inferProject)
import Control.Carrier.TaskPool
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Effect.Logger
import Network.HTTP.Types (urlEncode)
import qualified Srclib.Converter as Srclib
import Srclib.Types (Locator(..), parseLocator)
import qualified Strategy.Cargo as Cargo
import qualified Strategy.Carthage as Carthage
import qualified Strategy.Clojure as Clojure
import qualified Strategy.Cocoapods.Podfile as Podfile
import qualified Strategy.Cocoapods.PodfileLock as PodfileLock
import qualified Strategy.Erlang.Rebar3Tree as Rebar3Tree
import qualified Strategy.Go.GoList as GoList
import qualified Strategy.Go.Gomod as Gomod
import qualified Strategy.Go.GopkgLock as GopkgLock
import qualified Strategy.Go.GopkgToml as GopkgToml
import qualified Strategy.Go.GlideLock as GlideLock
import qualified Strategy.Googlesource.RepoManifest as RepoManifest
import qualified Strategy.Gradle as Gradle
import qualified Strategy.Maven.Pom as MavenPom
import qualified Strategy.Maven.PluginStrategy as MavenPlugin
import qualified Strategy.Node.NpmList as NpmList
import qualified Strategy.Node.NpmLock as NpmLock
import qualified Strategy.Node.PackageJson as PackageJson
import qualified Strategy.Node.YarnLock as YarnLock
import qualified Strategy.NuGet.PackagesConfig as PackagesConfig
import qualified Strategy.NuGet.PackageReference as PackageReference
import qualified Strategy.NuGet.ProjectAssetsJson as ProjectAssetsJson
import qualified Strategy.NuGet.ProjectJson as ProjectJson
import qualified Strategy.NuGet.Nuspec as Nuspec
import qualified Strategy.Python.Pipenv as Pipenv
import qualified Strategy.Python.ReqTxt as ReqTxt
import qualified Strategy.Python.SetupPy as SetupPy
import qualified Strategy.RPM as RPM
import qualified Strategy.Ruby.BundleShow as BundleShow
import qualified Strategy.Ruby.GemfileLock as GemfileLock
import Text.URI (URI)
import qualified Text.URI as URI
import Types
import qualified Data.Text.Encoding as TE

data ScanDestination
  = UploadScan URI ApiKey ProjectMetadata -- ^ upload to fossa with provided api key and base url
  | OutputStdout
  deriving (Generic)
 
analyzeMain :: Severity -> ScanDestination -> OverrideProject -> IO ()
analyzeMain logSeverity destination project = do
  basedir <- getCurrentDir
  withLogger logSeverity $ analyze basedir destination project

analyze ::
  ( Has (Lift IO) sig m
  , Has Logger sig m
  , MonadIO m
  )
  => Path Abs Dir
  -> ScanDestination
  -> OverrideProject
  -> m ()
analyze basedir destination override = do
  capabilities <- liftIO getNumCapabilities

  (closures,(failures,())) <- runOutput @ProjectClosure $ runOutput @ProjectFailure $
    withTaskPool capabilities updateProgress (traverse_ ($ basedir) discoverFuncs)

  traverse_ (logDebug . Diag.renderFailureBundle . projectFailureCause) failures

  logSticky ""

  let projects = mkProjects closures
      result = buildResult projects failures
 
  case destination of
    OutputStdout -> logStdout $ pretty (decodeUtf8 (encode result))
    UploadScan baseurl apiKey metadata -> do
      revision <- mergeOverride override <$> inferProject basedir

      logInfo ""
      logInfo ("Using project name: `" <> pretty (projectName revision) <> "`")
      logInfo ("Using revision: `" <> pretty (projectRevision revision) <> "`")
      logInfo ("Using branch: `" <> pretty (projectBranch revision) <> "`")

      uploadResult <- Diag.runDiagnostics $ uploadAnalysis baseurl apiKey revision metadata projects
      case uploadResult of
        Left failure -> logError (Diag.renderFailureBundle failure)
        Right success -> do
          let resp = Diag.resultValue success
          logInfo $ vsep
            [ "============================================================"
            , ""
            , "    View FOSSA Report:"
            , "    " <> pretty (fossaProjectUrl baseurl (uploadLocator resp) (projectBranch revision))
            , ""
            , "============================================================"
            ]
          traverse_ (\err -> logError $ "FOSSA error: " <> viaShow err) (uploadError resp)

fossaProjectUrl :: URI -> Text -> Text -> Text
fossaProjectUrl baseUrl rawLocator branch = URI.render baseUrl <> "projects/" <> encodedProject <> "/refs/branch/" <> branch <> "/" <> encodedRevision
  where
    Locator{locatorFetcher, locatorProject, locatorRevision} = parseLocator rawLocator

    underBS :: (ByteString -> ByteString) -> Text -> Text
    underBS f = TE.decodeUtf8 . f . TE.encodeUtf8

    encodedProject = underBS (urlEncode True) (locatorFetcher <> "+" <> locatorProject)
    encodedRevision = underBS (urlEncode True) (fromMaybe "" locatorRevision)

buildResult :: [Project] -> [ProjectFailure] -> Value
buildResult projects failures = object
  [ "projects" .= projects
  , "failures" .= map renderFailure failures
  , "sourceUnits" .= fromMaybe [] (traverse Srclib.toSourceUnit projects)
  ]

renderFailure :: ProjectFailure -> Value
renderFailure failure = object
  [ "name" .= projectFailureName failure
  , "cause" .= show (Diag.renderFailureBundle (projectFailureCause failure))
  ]

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

  , Pipenv.discover
  , SetupPy.discover
  , ReqTxt.discover

  , RepoManifest.discover

  , BundleShow.discover
  , GemfileLock.discover

  , Carthage.discover

  , Podfile.discover
  , PodfileLock.discover

  , Clojure.discover
  
  , Cargo.discover

  , RPM.discover
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
