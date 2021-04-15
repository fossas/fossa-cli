{-# LANGUAGE RecordWildCards #-}

module App.Fossa.VPS.Scan.RunWiggins
  ( execWiggins
  , generateWigginsScanOpts
  , generateWigginsAOSPNoticeOpts
  , WigginsOpts(..)
  , ScanType(..)
  )
where

import App.Fossa.VPS.Types
    ( FilterExpressions (FilterExpressions),
      NinjaFilePaths (unNinjaFilePaths),
      NinjaScanID (unNinjaScanID),
      encodeFilterExpressions )
import App.Fossa.EmbeddedBinary
import Control.Carrier.Error.Either
import Control.Effect.Diagnostics
import Data.Text (Text)
import qualified Data.Text as T
import Effect.Exec
import Path
import Effect.Logger
import Fossa.API.Types
import App.Types
import Text.URI
import qualified Data.ByteString.Lazy as BL
import Data.Text.Encoding ( decodeUtf8 )

data ScanType = ScanType
  { scanSkipIpr :: Bool
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

generateSpectrometerScanArgs :: Severity -> ProjectRevision -> ScanType -> FilterExpressions -> ApiOpts -> ProjectMetadata -> [Text]
generateSpectrometerScanArgs logSeverity ProjectRevision{..} ScanType{..} fileFilters ApiOpts{..} ProjectMetadata{..} =
    "analyze"
      : optMaybeText "-endpoint" (render <$> apiOptsUri)
      ++ ["-fossa-api-key", unApiKey apiOptsApiKey]
      ++ ["-name", projectName, "-revision", projectRevision]
      ++ optMaybeText "-jira-project-key" projectJiraKey
      ++ optMaybeText "-link" projectLink
      ++ optMaybeText "-policy" projectPolicy
      ++ optMaybeText "-project-url" projectUrl
      ++ optMaybeText "-team" projectTeam
      ++ optMaybeText "-title" projectTitle
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

wigginsCommand :: BinaryPaths -> WigginsOpts -> Command
wigginsCommand bin WigginsOpts{..} = do
  Command
    { cmdName = T.pack $ fromAbsFile $ toExecutablePath bin,
      cmdArgs = spectrometerArgs,
      cmdAllowErr = Never
    }
