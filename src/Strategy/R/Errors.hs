module Strategy.R.Errors (
  MissingRenvLockFile (..),
  MissingDescriptionFile (..),
  VersionConstraintsIgnored (..),
  rEnvLockFileDocUrl,
  rEnvLockFileGenerateDocUrl,
) where

import Data.Text (Text)
import Diag.Diagnostic (ToDiagnostic, renderDiagnostic)
import Errata (Errata (..))

rEnvLockFileDocUrl :: Text
rEnvLockFileDocUrl = "https://rstudio.github.io/renv/"

rEnvLockFileGenerateDocUrl :: Text
rEnvLockFileGenerateDocUrl = "https://rstudio.github.io/renv/reference/snapshot.html"

data MissingDescriptionFile = MissingDescriptionFile
instance ToDiagnostic MissingDescriptionFile where
  renderDiagnostic (MissingDescriptionFile) = do
    let header = "Provide DESCRIPTION file in same path as `renv.lock`, so FOSSA CLI can infer direct dependencies."
    Errata (Just header) [] Nothing

data VersionConstraintsIgnored = VersionConstraintsIgnored
instance ToDiagnostic VersionConstraintsIgnored where
  renderDiagnostic (VersionConstraintsIgnored) = do
    let header = "Version constraints (if specified) in the DESCRIPTION file will be ignored. Please use renv to create renv.lock file, so package versions as pinned, can be analyzed."
    Errata (Just header) [] Nothing

data MissingRenvLockFile
  = MissingRenvLockFileCtx
  | MissingRenvLockFileHelp
instance ToDiagnostic MissingRenvLockFile where
  renderDiagnostic MissingRenvLockFileCtx = do
    let header = "Could not perform lockfile analysis for your r project"
    Errata (Just header) [] Nothing
  renderDiagnostic MissingRenvLockFileHelp = do
    let header = "Ensure valid lockfile exist and is readable prior to running fossa. If you are using renv, you will likely need to run: renv::snapshot() to create renv.lock"
    Errata (Just header) [] Nothing
