module Strategy.R.Errors (
  MissingRenvLockFile (..),
  VersionConstraintsIgnored (..),
  rEnvLockFileDocUrl,
  rEnvLockFileGenerateDocUrl,
) where

import Data.Text (Text)
import Diag.Diagnostic (ToDiagnostic, renderDiagnostic)
import Prettyprinter (Pretty (pretty), indent, vsep)

rEnvLockFileDocUrl :: Text
rEnvLockFileDocUrl = "https://rstudio.github.io/renv/"

rEnvLockFileGenerateDocUrl :: Text
rEnvLockFileGenerateDocUrl = "https://rstudio.github.io/renv/reference/snapshot.html"

data VersionConstraintsIgnored = VersionConstraintsIgnored
instance ToDiagnostic VersionConstraintsIgnored where
  renderDiagnostic (VersionConstraintsIgnored) =
    vsep
      [ "Version constraints (if specified) in the DESCRIPTION file will be ignored."
      , "Please use renv to create renv.lock file, so package versions as pinned, can be analyzed."
      ]

data MissingRenvLockFile = MissingRenvLockFile
instance ToDiagnostic MissingRenvLockFile where
  renderDiagnostic (MissingRenvLockFile) =
    vsep
      [ "We could not perform lockfile analysis for your r project."
      , ""
      , indent 2 $
          vsep
            [ "Ensure valid lockfile exist and is readable prior to running fossa."
            , "If you are using renv, you will likely need to run: renv::snapshot() to create renv.lock"
            ]
      , ""
      , "Refer to:"
      , indent 2 $
          vsep
            [ pretty $ "- " <> rEnvLockFileDocUrl
            , pretty $ "- " <> rEnvLockFileGenerateDocUrl
            ]
      ]
