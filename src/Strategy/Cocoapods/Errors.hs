module Strategy.Cocoapods.Errors (
  MissingPodLockFile (..),
  refPodDocUrl,
) where

import App.Docs (platformDocUrl)
import Data.Text (Text)
import Diag.Diagnostic (ToDiagnostic (..))
import Prettyprinter (Pretty (pretty), indent, vsep)

refPodDocUrl :: Text
refPodDocUrl = platformDocUrl "ios/cocoapods.md"

data MissingPodLockFile = MissingPodLockFile

instance ToDiagnostic MissingPodLockFile where
  renderDiagnostic (MissingPodLockFile) =
    vsep
      [ "We could not perform analysis using Podfile.lock."
      , ""
      , "Ensure a valid Podfile.lock file exists prior to using fossa-cli."
      , ""
      , "Refer to:"
      , indent 2 $ pretty $ "- " <> refPodDocUrl
      ]
