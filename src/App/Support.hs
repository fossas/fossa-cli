module App.Support (supportUrl, reportDefectMsg, reportDefectWithFileMsg) where

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
