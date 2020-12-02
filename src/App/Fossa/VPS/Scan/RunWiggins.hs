{-# LANGUAGE RecordWildCards #-}

module App.Fossa.VPS.Scan.RunWiggins
  ( execWiggins
  , generateWigginsOpts
  , WigginsOpts(..)
  , ScanType(..)
  )
where

import App.Fossa.VPS.Types
import App.Fossa.VPS.EmbeddedBinary
import Control.Carrier.Error.Either
import Control.Effect.Diagnostics
import Data.Functor (void)
import Data.Text (Text)
import qualified Data.Text as T
import Effect.Exec
import Path
import Effect.Logger
import Fossa.API.Types
import App.Types
import Text.URI

data ScanType = ScanType
  { scanSkipIpr :: Bool
  , scanLicenseOnly :: Bool
  }

data WigginsOpts = WigginsOpts
  { scanDir :: Path Abs Dir
  , spectrometerArgs :: [Text]
  }

generateWigginsOpts :: Path Abs Dir -> Severity -> ProjectRevision -> ScanType -> FilterExpressions -> ApiOpts -> ProjectMetadata -> WigginsOpts
generateWigginsOpts scanDir logSeverity projectRevision scanType fileFilters apiOpts metadata =
  WigginsOpts scanDir (generateSpectrometerArgs logSeverity projectRevision scanType fileFilters apiOpts metadata)

generateSpectrometerArgs :: Severity -> ProjectRevision -> ScanType -> FilterExpressions -> ApiOpts -> ProjectMetadata -> [Text]
generateSpectrometerArgs logSeverity ProjectRevision{..} ScanType{..} fileFilters ApiOpts{..} ProjectMetadata{..} =
    "analyze"
      : ["-endpoint", render apiOptsUri, "-fossa-api-key", unApiKey apiOptsApiKey]
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

execWiggins :: (Has Exec sig m, Has Diagnostics sig m) => BinaryPaths -> WigginsOpts -> m ()
execWiggins binaryPaths opts = void $ execThrow (scanDir opts) (wigginsCommand binaryPaths opts)

wigginsCommand :: BinaryPaths -> WigginsOpts -> Command
wigginsCommand BinaryPaths{..} WigginsOpts{..} = do
  Command
    { cmdName = T.pack $ fromAbsFile wigginsBinaryPath,
      cmdArgs = spectrometerArgs,
      cmdAllowErr = Never
    }
