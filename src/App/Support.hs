module App.Support (
  supportUrl,
  statusPageUrl,
  fossaSaasHost,
  fossaEnvironment,
  reportFossaBugErrorMsg,
  reportCliBugErrorMsg,
  reportTransientErrorMsg,
  reportNetworkErrorMsg,
  reportDefectMsg,
  reportDefectWithFileMsg,
  reportDefectWithDebugBundle,
  requestDebugBundle,
  requestReportIfPersists,
  requestReportIfPersistsWithDebugBundle,
  FossaEnvironment (..),
) where

import Data.String.Conversion (decodeUtf8)
import Data.Text (Text, isInfixOf)
import Network.HTTP.Client qualified as HTTP
import Prettyprinter (Doc, Pretty (pretty), indent, vsep)

data FossaEnvironment = FossaEnvironmentCloud | FossaEnvironmentOnprem deriving (Eq)

supportUrl :: Text
supportUrl = "https://support.fossa.com"

statusPageUrl :: Text
statusPageUrl = "https://status.fossa.com/"

fossaSaasHost :: Text
fossaSaasHost = "fossa.com"

-- | Return the FOSSA environment used in the request.
fossaEnvironment :: HTTP.Request -> FossaEnvironment
fossaEnvironment req' =
  if fossaSaasHost `isInfixOf` decodeUtf8 (HTTP.host req')
    then FossaEnvironmentCloud
    else FossaEnvironmentOnprem

-- | Standard call to action for reporting defects.
reportDefectMsg :: Doc ann
reportDefectMsg = pretty $ "If you believe this to be a defect, please report a bug to FOSSA support at " <> supportUrl

-- | Standard call to action for reporting defects, but also ask for the named file.
reportDefectWithFileMsg :: String -> Doc ann
reportDefectWithFileMsg filepath =
  vsep
    [ pretty $ "If you believe this to be a defect, please report a bug to FOSSA support at " <> supportUrl <> ", with a copy of:"
    , indent 2 (pretty filepath)
    ]

-- | Standard call to action for reporting defects, but also ask for the debug bundle.
reportDefectWithDebugBundle :: Doc ann
reportDefectWithDebugBundle = withDebugBundle reportDefectMsg

-- | Request a report if the issue persists, but also ask for the debug bundle
requestReportIfPersistsWithDebugBundle :: Doc ann
requestReportIfPersistsWithDebugBundle = withDebugBundle requestReportIfPersists

-- | A request for the debug bundle, along with instructions on how to generate it.
requestDebugBundle :: Doc ann
requestDebugBundle =
  vsep
    [ "In your bug report, please include FOSSA's debug bundle file: fossa.debug.json.gz."
    , "You can generate debug bundle by using `--debug` flag, for example: `fossa analyze --debug`"
    ]

-- | For networking errors, explain that networking errors are often transient or caused by local configuration.
reportNetworkErrorMsg :: Doc ann
reportNetworkErrorMsg =
  vsep
    [ "This is a networking error."
    , ""
    , "Networking errors are typically caused by actual network failure or a network appliance"
    , "(e.g. a firewall) between the FOSSA CLI and the FOSSA backend."
    , "This means that often such errors are transient, or are caused by local network configuration."
    , ""
    , "Trying again in a few minutes may resolve this issue."
    ]

-- | For errors which almost definitely are a bug in the FOSSA CLI.
reportCliBugErrorMsg :: Doc ann
reportCliBugErrorMsg = "This is likely a bug in the FOSSA CLI."

-- | For errors which almost definitely are a bug in FOSSA.
reportFossaBugErrorMsg :: FossaEnvironment -> Doc ann
reportFossaBugErrorMsg FossaEnvironmentCloud =
  vsep
    [ "This is likely a bug in FOSSA, although it is also possible that this is caused by network failure"
    , "or a network appliance (e.g. a firewall) between FOSSA CLI and the FOSSA endpoint."
    , ""
    , "FOSSA may already be aware of this issue, in which case this may be transient."
    , "For current status, see the FOSSA status page at " <> pretty statusPageUrl
    , ""
    , "Trying again in a few minutes may resolve this issue."
    ]
reportFossaBugErrorMsg FossaEnvironmentOnprem =
  withDebugBundle $
    vsep
      [ "This is likely a bug in FOSSA, although it is also possible that this is caused by network failure"
      , "or a network appliance (e.g. a firewall) between FOSSA CLI and the FOSSA endpoint."
      , ""
      , "Trying again in a few minutes may resolve this issue."
      ]

-- | For temporary errors, explain that the error is transient and to wait a bit to try again.
-- If this is a networking error, consider 'reportNetworkErrorMsg' instead.
reportTransientErrorMsg :: Doc ann
reportTransientErrorMsg = "This error is often transient, so trying again in a few minutes may resolve the issue."

-- | Request a report if the issue persists.
requestReportIfPersists :: Doc ann
requestReportIfPersists = "If this issue persists, please contact FOSSA support at " <> pretty supportUrl

withDebugBundle :: Doc ann -> Doc ann
withDebugBundle msg =
  vsep
    [ msg
    , requestDebugBundle
    ]
