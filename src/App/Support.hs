module App.Support (
  supportUrl,
  statusPageUrl,
  reportFossaBugErrorMsg,
  reportTransientErrorMsg,
  reportNetworkErrorMsg,
  reportDefectMsg,
  reportDefectWithFileMsg,
  reportDefectWithDebugBundle,
  requestDebugBundle,
) where

import Data.Text (Text)
import Prettyprinter (Doc, Pretty (pretty), indent, vsep)

supportUrl :: Text
supportUrl = "https://support.fossa.com/hc/en-us"

statusPageUrl :: Text
statusPageUrl = "https://status.fossa.com/"

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

-- | A request for the debug bundle, along with instructions on how to generate it.
requestDebugBundle :: Doc ann
requestDebugBundle =
  vsep
    [ "In your bug report, please include FOSSA's debug bundle file: fossa.debug.json.gz."
    , ""
    , "You can generate debug bundle by using `--debug` flag, for example:"
    , indent 2 "fossa analyze --debug"
    ]

-- | For networking errors, explain that networking errors are often transient or caused by local configuration.
-- Contains a call to action for a bug report if the issue persists.
reportNetworkErrorMsg :: Doc ann
reportNetworkErrorMsg =
  withDebugBundle $
    vsep
      [ "This is a networking error."
      , ""
      , "Networking errors are typically caused by a computer (for example, a router or firewall) between the FOSSA CLI and the FOSSA backend."
      , "This means that often such errors are transient, or are caused by local network configuration."
      , ""
      , "Trying again in a few minutes may resolve this issue."
      , "If this issue persists, please report a bug to FOSSA support at " <> pretty supportUrl
      ]

-- | For errors which almost definitely are a bug in FOSSA.
reportFossaBugErrorMsg :: Doc ann
reportFossaBugErrorMsg =
  withDebugBundle $
    vsep
      [ "This is likely a bug in FOSSA."
      , "It is also possible that a router or firewall between FOSSA CLI and the FOSSA service is causing this."
      , ""
      , "FOSSA may already be aware of this error, in which case this may be transient."
      , "For current status, see the FOSSA status page at " <> pretty statusPageUrl
      , ""
      , "Trying again in a few minutes may resolve this issue."
      , "If this issue persists, please report a bug to FOSSA support at " <> pretty supportUrl
      ]

-- | For temporary errors, explain that the error is transient and to wait a bit to try again.
-- Contains a call to action for a bug report if the issue persists.
-- If this is a networking error, consider 'reportNetworkErrorMsg' instead.
reportTransientErrorMsg :: Doc ann
reportTransientErrorMsg =
  withDebugBundle $
    vsep
      [ "This error is often transient, so trying again in a few minutes may resolve the issue."
      , ""
      , "If this issue persists, please report a bug to FOSSA support at " <> pretty supportUrl
      ]

withDebugBundle :: Doc ann -> Doc ann
withDebugBundle msg =
  vsep
    [ msg
    , ""
    , requestDebugBundle
    ]
