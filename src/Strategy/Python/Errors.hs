module Strategy.Python.Errors (
  MissingPoetryLockFile (..),
  PipenvCmdFailed (..),

  -- * docs
  commitPoetryLockToVCS,
) where

import Data.Text (Text)
import Diag.Diagnostic (ToDiagnostic, renderDiagnostic)
import Effect.Exec (Command, renderCommand)
import Path (Abs, File, Path)
import Prettyprinter (Pretty (pretty), indent, viaShow, vsep)

commitPoetryLockToVCS :: Text
commitPoetryLockToVCS = "https://python-poetry.org/docs/basic-usage/#commit-your-poetrylock-file-to-version-control"

newtype PipenvCmdFailed = PipenvCmdFailed Command
instance ToDiagnostic PipenvCmdFailed where
  renderDiagnostic (PipenvCmdFailed cmd) =
    vsep
      [ pretty $ "We could not perform pipenv graph analysis using, command:" <> renderCommand cmd
      , ""
      , indent 2 $
          vsep
            [ "Ensure pipenv executable is in your PATH."
            ]
      ]

newtype MissingPoetryLockFile = MissingPoetryLockFile (Path Abs File)
instance ToDiagnostic MissingPoetryLockFile where
  renderDiagnostic (MissingPoetryLockFile path) =
    vsep
      [ "We could not perform poetry.lock analysis for: " <> viaShow path
      , ""
      , indent 2 $
          vsep
            [ "Ensure valid poetry.lock exists and is readable."
            ]
      , ""
      , "Refer to:"
      , indent 2 $ pretty $ "- " <> commitPoetryLockToVCS
      ]
