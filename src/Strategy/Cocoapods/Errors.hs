module Strategy.Cocoapods.Errors (
  MissingPodLockFile (..),
  refPodDocUrl,
) where

import App.Docs (platformDocUrl)
import Data.Text (Text)
import Diag.Diagnostic (ToDiagnostic (..))
import Errata (Errata (..))

refPodDocUrl :: Text
refPodDocUrl = platformDocUrl "ios/cocoapods.md"

data MissingPodLockFile
  = MissingPodLockFileCtx
  | MissingPodLockFileHelp

instance ToDiagnostic MissingPodLockFile where
  renderDiagnostic MissingPodLockFileCtx = do
    let header = "Could not perform analysis using Podfile.lock"
    Errata (Just header) [] Nothing
  renderDiagnostic MissingPodLockFileHelp = do
    let header = "Ensure a valid Podfile.lock file exists prior to using fossa-cli"
    Errata (Just header) [] Nothing
