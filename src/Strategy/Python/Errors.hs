module Strategy.Python.Errors (
  MissingPoetryLockFile (..),
  PipenvCmdFailed (..),

  -- * docs
  commitPoetryLockToVCS,
) where

import Data.String.Conversion (toText)
import Data.Text (Text)
import Diag.Diagnostic (ToDiagnostic, renderDiagnostic)
import Effect.Exec (Command, renderCommand)
import Errata (Errata (..))
import Path (Abs, File, Path)

commitPoetryLockToVCS :: Text
commitPoetryLockToVCS = "https://python-poetry.org/docs/basic-usage/#commit-your-poetrylock-file-to-version-control"

data PipenvCmdFailed
  = PipenvCmdFailedCtx Command
  | PipenvCmdFailedHelp
instance ToDiagnostic PipenvCmdFailed where
  renderDiagnostic (PipenvCmdFailedCtx cmd) = do
    let header = "We could not perform pipenv graph analysis using, command:" <> renderCommand cmd
    Errata (Just header) [] Nothing
  renderDiagnostic PipenvCmdFailedHelp = do
    let header = "Ensure pipenv executable is in your PATH"
    Errata (Just header) [] Nothing

data MissingPoetryLockFile
  = MissingPoetryLockFileCtx (Path Abs File)
  | MissingPoetryLockFileHelp
instance ToDiagnostic MissingPoetryLockFile where
  renderDiagnostic (MissingPoetryLockFileCtx path) = do
    let header = "We could not perform poetry.lock analysis for: " <> toText path
    Errata (Just header) [] Nothing
  renderDiagnostic MissingPoetryLockFileHelp = do
    let header = "Ensure valid poetry.lock exists and is readable."
    Errata (Just header) [] Nothing
