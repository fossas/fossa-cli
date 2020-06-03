
module App.Fossa.Analyze
  ( analyzeMain
  , ScanDestination(..)
  ) where

import Prologue

import Control.Carrier.Error.Either
import Control.Effect.Exception as Exc
import Control.Carrier.Output.IO
import Control.Concurrent
import Path.IO

import App.Fossa.FossaAPIV1 (ProjectRevision(..), ProjectMetadata, fossaReq, uploadAnalysis, FossaError(..), UploadResponse(..))
import App.Fossa.Analyze.Project (Project, mkProjects)
import App.Fossa.ProjectInference (InferredProject(..), inferProject)
import Control.Carrier.TaskPool
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Effect.Exec (ExecErr(..))
import Effect.Logger
import Effect.ReadFS (ReadFSErr(..))
import qualified Srclib.Converter as Srclib
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
import qualified Strategy.Ruby.BundleShow as BundleShow
import qualified Strategy.Ruby.GemfileLock as GemfileLock
import Types
import OptionExtensions

data ScanDestination
  = UploadScan UrlOption Text ProjectMetadata -- ^ upload to fossa with provided api key and base url
  | OutputStdout
  deriving (Generic)
 
analyzeMain :: Severity -> ScanDestination -> Maybe Text -> Maybe Text -> IO ()
analyzeMain logSeverity destination name revision = do
  basedir <- getCurrentDir
  withLogger logSeverity $ analyze basedir destination name revision

analyze ::
  ( Has (Lift IO) sig m
  , Has Logger sig m
  , MonadIO m
  )
  => Path Abs Dir
  -> ScanDestination
  -> Maybe Text -- ^ cli override for name
  -> Maybe Text -- ^ cli override for revision
  -> m ()
analyze basedir destination overrideName overrideRevision = do
  capabilities <- liftIO getNumCapabilities

  (closures,(failures,())) <- runOutput @ProjectClosure $ runOutput @ProjectFailure $
    withTaskPool capabilities updateProgress (traverse_ ($ basedir) discoverFuncs)

  logSticky ""

  let projects = mkProjects closures
      result = buildResult projects failures
 
  case destination of
    OutputStdout -> logStdout $ pretty (decodeUtf8 (encode result))
    UploadScan baseurl apiKey metadata -> do
      inferred <- inferProject basedir
      let revision = ProjectRevision
            (fromMaybe (inferredName inferred) overrideName)
            (fromMaybe (inferredRevision inferred) overrideRevision)

      logInfo ""
      logInfo ("Using project name: `" <> pretty (projectName revision) <> "`")
      logInfo ("Using revision: `" <> pretty (projectRevision revision) <> "`")
     
      maybeResp <- fossaReq $ uploadAnalysis baseurl apiKey revision metadata projects
      case maybeResp of
        Left (InvalidProjectOrRevision _) -> logError "FOSSA error: Invalid project or revision"
        Left (NoPermission _) -> logError "FOSSA error: No permission to upload"
        Left (JsonDeserializeError msg) -> logError $ "FOSSA error: Couldn't deserialize API response: " <> pretty msg
        Left (OtherError exc) -> do
          logError "Error when uploading to FOSSA:"
          logError (viaShow exc)
        Right resp -> do
          logInfo $ "FOSSA locator: " <> viaShow (uploadLocator resp)
          traverse_ (\err -> logError $ "FOSSA error: " <> viaShow err) (uploadError resp)

buildResult :: [Project] -> [ProjectFailure] -> Value
buildResult projects failures = object
  [ "projects" .= projects
  , "failures" .= map renderFailure failures
  , "sourceUnits" .= fromMaybe [] (traverse Srclib.toSourceUnit projects)
  ]

renderFailure :: ProjectFailure -> Value
renderFailure failure = object
  [ "name" .= projectFailureName failure
  , "cause" .= renderCause (projectFailureCause failure)
  ]

renderCause :: SomeException -> Value
renderCause e = fromMaybe renderSomeException $
      renderReadFSErr <$> fromException e
  <|> renderExecErr   <$> fromException e
  where
  renderSomeException = object
    [ "type" .= ("unknown" :: Text)
    , "err"  .= show e
    ]

  renderReadFSErr :: ReadFSErr -> Value
  renderReadFSErr = \case
    FileReadError path err -> object
      [ "type" .= ("file_read_error" :: Text)
      , "path" .= path
      , "err"  .= err
      ]
    FileParseError path err -> object
      [ "type" .= ("file_parse_error" :: Text)
      , "path" .= path
      , "err"  .= err
      ]
    ResolveError base path err -> object
      [ "type" .= ("file_resolve_error" :: Text)
      , "base" .= base
      , "path" .= path
      , "err"  .= err
      ]

  renderExecErr :: ExecErr -> Value
  renderExecErr = \case
    CommandFailed cmd outerr -> object
      [ "type"   .= ("command_execution_error" :: Text)
      , "cmd"    .= cmd
      , "stderr" .= outerr
      ]
    CommandParseError cmd err -> object
      [ "type" .= ("command_parse_error" :: Text)
      , "cmd"  .= cmd
      , "err"  .= err
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
