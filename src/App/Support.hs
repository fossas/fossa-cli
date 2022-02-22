module App.Support (supportUrl, reportDefectMsg, reportDefectWithFileMsg, reportDefectWithDebugBundle) where

import Data.Text (Text)
import Prettyprinter (Doc, Pretty (pretty), indent, vsep)

supportUrl :: Text
supportUrl = "https://support.fossa.com/hc/en-us"

reportDefectMsg :: Doc ann
reportDefectMsg = pretty $ "If you believe this to be a defect, please report a bug to FOSSA support at " <> supportUrl

reportDefectWithFileMsg :: String -> Doc ann
reportDefectWithFileMsg filepath =
  vsep
    [ pretty $ "If you believe this to be a defect, please report a bug to FOSSA support at " <> supportUrl <> ", with a copy of:"
    , indent 2 (pretty filepath)
    ]

reportDefectWithDebugBundle :: Doc ann
reportDefectWithDebugBundle =
  vsep
    [ reportDefectMsg
    , ""
    , "In your bug report, please include fossa's debug bundle file: fossa.debug.json.gz."
    , ""
    , "You can generate debug bundle by using `--debug` flag, for example:"
    , indent 2 "fossa analyze --debug"
    ]
