
module App.Scan
  ( scanMain
  , ScanCmdOpts(..)
  ) where

import Prologue

import Control.Carrier.Error.Either
import Control.Effect.Exception as Exc
import Control.Carrier.Output.IO
import Control.Concurrent
import Path.IO
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout, stderr)
import System.Exit (die)

import App.Scan.FossaV1 (uploadAnalysis, FossaError(..), UploadResponse(..))
import App.Scan.Project (Project, mkProjects)
import App.Scan.ProjectInference (InferredProject(..), inferProject)
import Control.Carrier.TaskPool
import Control.Carrier.Threaded
import qualified Data.ByteString.Lazy as BL
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Effect.Exec (ExecErr(..))
import Effect.Logger
import Effect.ReadFS (ReadFSErr(..))
import qualified Srclib.Converter as Srclib
import qualified Strategy.Carthage as Carthage
import qualified Strategy.Cocoapods.Podfile as Podfile
import qualified Strategy.Cocoapods.PodfileLock as PodfileLock
import qualified Strategy.Go.GoList as GoList
import qualified Strategy.Go.Gomod as Gomod
import qualified Strategy.Go.GopkgLock as GopkgLock
import qualified Strategy.Go.GopkgToml as GopkgToml
import qualified Strategy.Go.GlideLock as GlideLock
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

data ScanCmdOpts = ScanCmdOpts
  { cmdBasedir :: FilePath
  , cmdDebug   :: Bool
  , cmdOutFile :: Maybe FilePath
  , cmdApiKey  :: Maybe Text
  } deriving (Eq, Ord, Show, Generic)

scanMain :: ScanCmdOpts -> IO ()
scanMain ScanCmdOpts{..} = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  basedir <- validateDir cmdBasedir

  scan basedir cmdOutFile cmdApiKey
    & withLogger (bool SevInfo SevDebug cmdDebug)
    & runThreaded

validateDir :: FilePath -> IO (Path Abs Dir)
validateDir dir = do
  absolute <- resolveDir' dir
  exists <- doesDirExist absolute

  unless exists (die $ "ERROR: Directory " <> show absolute <> " does not exist")

  pure absolute

scan ::
  ( Has (Lift IO) sig m
  , Has Logger sig m
  , Has Threaded sig m
  , MonadIO m
  , Effect sig
  )
  => Path Abs Dir
  -> Maybe FilePath
  -> Maybe Text -- ^ API key for fossa core. when present, we'll upload results
  -> m ()
scan basedir outFile fossaApiKey = do
  setCurrentDir basedir
  capabilities <- liftIO getNumCapabilities

  (closures,(failures,())) <- runOutput @ProjectClosure $ runOutput @ProjectFailure $
    withTaskPool capabilities updateProgress (traverse_ ($ basedir) discoverFuncs)

  logSticky "[ Combining Analyses ]"

  let projects = mkProjects closures
      result = buildResult projects failures
  liftIO $ case outFile of
    Nothing -> BL.putStr (encode result)
    Just path -> liftIO (encodeFile path result)

  inferred <- inferProject basedir
  logInfo ""
  logInfo ("Inferred project name: `" <> pretty (inferredName inferred) <> "`")
  logInfo ("Inferred revision: `" <> pretty (inferredRevision inferred) <> "`")

  logSticky ""

  for_ fossaApiKey $ \key -> do
    maybeResp <- liftIO $ uploadAnalysis key (inferredName inferred) (inferredRevision inferred) projects
    case maybeResp of
      Left InvalidProjectOrRevision -> logError "FOSSA error: Invalid project or revision"
      Left NoPermission -> logError "FOSSA error: No permission to upload"
      Left (JsonDeserializeError msg) -> logError $ "FOSSA error: Couldn't deserialize API response: " <> pretty msg
      Left (OtherError exc) -> do
        logError "FOSSA error: other unknown error. See debug log for details"
        logDebug (viaShow exc)
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
  [ GoList.discover
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

  , BundleShow.discover
  , GemfileLock.discover

  , Carthage.discover

  , Podfile.discover
  , PodfileLock.discover
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
