{-# LANGUAGE RecordWildCards #-}

module App.Fossa.VPS.Scan.RunWiggins (
  execWiggins,
  execWigginsJson,
  generateWigginsScanOpts,
  generateWigginsAOSPNoticeOpts,
  generateVSIStandaloneOpts,
  generateWigginsMonorepoOpts,
  WigginsOpts (..),
  ScanType (..),
) where

import App.Fossa.EmbeddedBinary
import App.Fossa.VPS.Types (
  FilterExpressions (FilterExpressions),
  NinjaFilePaths (unNinjaFilePaths),
  NinjaScanID (unNinjaScanID),
  encodeFilterExpressions,
 )
import App.Types
import Control.Carrier.Error.Either
import Control.Effect.Diagnostics
import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Effect.Exec
import Effect.Logger
import Fossa.API.Types
import Path
import Text.URI

data ScanType = ScanType
  { followSymlinks :: Bool
  , scanSkipIpr :: Bool
  , scanLicenseOnly :: Bool
  }

data WigginsOpts = WigginsOpts
  { scanDir :: Path Abs Dir
  , spectrometerArgs :: [Text]
  }

generateWigginsScanOpts :: Path Abs Dir -> Severity -> ProjectRevision -> ScanType -> FilterExpressions -> ApiOpts -> ProjectMetadata -> WigginsOpts
generateWigginsScanOpts scanDir logSeverity projectRevision scanType fileFilters apiOpts metadata =
  WigginsOpts scanDir $ generateSpectrometerScanArgs logSeverity projectRevision scanType fileFilters apiOpts metadata

generateWigginsAOSPNoticeOpts :: Path Abs Dir -> Severity -> ApiOpts -> ProjectRevision -> NinjaScanID -> NinjaFilePaths -> WigginsOpts
generateWigginsAOSPNoticeOpts scanDir logSeverity apiOpts projectRevision ninjaScanId ninjaInputFiles =
  WigginsOpts scanDir $ generateSpectrometerAOSPNoticeArgs logSeverity apiOpts projectRevision ninjaScanId ninjaInputFiles

generateVSIStandaloneOpts :: Path Abs Dir -> ApiOpts -> WigginsOpts
generateVSIStandaloneOpts scanDir apiOpts = WigginsOpts scanDir $ generateVSIStandaloneArgs apiOpts

generateWigginsMonorepoOpts :: Path Abs Dir -> MonorepoAnalysisOpts -> Severity -> ProjectRevision -> ApiOpts -> ProjectMetadata -> WigginsOpts
generateWigginsMonorepoOpts scanDir monorepoAnalysisOpts logSeverity projectRevision apiOpts metadata =
  WigginsOpts scanDir $ generateMonorepoArgs monorepoAnalysisOpts logSeverity projectRevision apiOpts metadata

generateSpectrometerAOSPNoticeArgs :: Severity -> ApiOpts -> ProjectRevision -> NinjaScanID -> NinjaFilePaths -> [Text]
generateSpectrometerAOSPNoticeArgs logSeverity ApiOpts{..} ProjectRevision{..} ninjaScanId ninjaInputFiles =
  ["aosp-notice-files"]
    ++ optBool "-debug" (logSeverity == SevDebug)
    ++ optMaybeText "-endpoint" (render <$> apiOptsUri)
    ++ ["-fossa-api-key", unApiKey apiOptsApiKey]
    ++ ["-scan-id", unNinjaScanID ninjaScanId]
    ++ ["-name", projectName]
    ++ ["."]
    ++ (T.pack . toFilePath <$> unNinjaFilePaths ninjaInputFiles)

generateVSIStandaloneArgs :: ApiOpts -> [Text]
generateVSIStandaloneArgs ApiOpts{..} =
  "vsi-direct" :
  optMaybeText "-endpoint" (render <$> apiOptsUri)
    ++ ["-fossa-api-key", unApiKey apiOptsApiKey]
    ++ ["."]

generateMonorepoArgs :: MonorepoAnalysisOpts -> Severity -> ProjectRevision -> ApiOpts -> ProjectMetadata -> [Text]
generateMonorepoArgs MonorepoAnalysisOpts{..} logSeverity ProjectRevision{..} ApiOpts{..} ProjectMetadata{..} =
  "monorepo" :
  optMaybeText "-endpoint" (render <$> apiOptsUri)
    ++ ["-fossa-api-key", unApiKey apiOptsApiKey]
    ++ ["-project", projectName, "-revision", projectRevision]
    ++ optMaybeText "-jira-project-key" projectJiraKey
    ++ optMaybeText "-link" projectLink
    ++ optMaybeText "-policy" projectPolicy
    ++ optMaybeText "-project-url" projectUrl
    ++ optMaybeText "-team" projectTeam
    ++ optMaybeText "-title" projectTitle
    ++ optMaybeText "-branch" projectBranch
    ++ optBool "-debug" (logSeverity == SevDebug)
    ++ optMaybeText "-type" monorepoAnalysisType
    ++ ["."]

generateSpectrometerScanArgs :: Severity -> ProjectRevision -> ScanType -> FilterExpressions -> ApiOpts -> ProjectMetadata -> [Text]
generateSpectrometerScanArgs logSeverity ProjectRevision{..} ScanType{..} fileFilters ApiOpts{..} ProjectMetadata{..} =
  "analyze" :
  optMaybeText "-endpoint" (render <$> apiOptsUri)
    ++ ["-fossa-api-key", unApiKey apiOptsApiKey]
    ++ ["-name", projectName, "-revision", projectRevision]
    ++ optMaybeText "-jira-project-key" projectJiraKey
    ++ optMaybeText "-link" projectLink
    ++ optMaybeText "-policy" projectPolicy
    ++ optMaybeText "-project-url" projectUrl
    ++ optMaybeText "-team" projectTeam
    ++ optMaybeText "-title" projectTitle
    ++ optBool "-follow" followSymlinks
    ++ optBool "-license-only" scanLicenseOnly
    ++ optBool "-skip-ipr-scan" scanSkipIpr
    ++ optBool "-debug" (logSeverity == SevDebug)
    ++ optFilterExpressions fileFilters
    ++ ["."]

optFilterExpressions :: FilterExpressions -> [Text]
optFilterExpressions (FilterExpressions []) = []
optFilterExpressions expressions = ["-i", encodeFilterExpressions expressions]

optBool :: Text -> Bool -> [Text]
optBool flag True = [flag]
optBool _ False = []

optMaybeText :: Text -> Maybe Text -> [Text]
optMaybeText _ Nothing = []
optMaybeText flag (Just value) = [flag, value]

execWiggins :: (Has Exec sig m, Has Diagnostics sig m) => BinaryPaths -> WigginsOpts -> m Text
execWiggins binaryPaths opts = decodeUtf8 . BL.toStrict <$> execThrow (scanDir opts) (wigginsCommand binaryPaths opts)

execWigginsJson :: (FromJSON a, Has Exec sig m, Has Diagnostics sig m) => WigginsOpts -> BinaryPaths -> m a
execWigginsJson opts binaryPaths = execJson (scanDir opts) (wigginsCommand binaryPaths opts)

wigginsCommand :: BinaryPaths -> WigginsOpts -> Command
wigginsCommand bin WigginsOpts{..} = do
  Command
    { cmdName = T.pack $ fromAbsFile $ toExecutablePath bin
    , cmdArgs = spectrometerArgs
    , cmdAllowErr = Never
    }
