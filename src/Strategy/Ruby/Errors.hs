module Strategy.Ruby.Errors (
  BundlerMissingLockFile (..),

  -- * Reference docs
  bundlerLockFileRationaleUrl,
  rubyFossaDocUrl,
) where

import App.Docs (strategyLangDocUrl)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Diag.Diagnostic (ToDiagnostic, renderDiagnostic)
import Errata (Errata (..))
import Path

bundlerLockFileRationaleUrl :: Text
bundlerLockFileRationaleUrl = "https://bundler.io/rationale.html#sharing-your-application-with-other-developers"

rubyFossaDocUrl :: Text
rubyFossaDocUrl = strategyLangDocUrl "ruby/ruby.md"

data BundlerMissingLockFile
  = BundlerMissingLockFileCtx (Path Abs File)
  | BundlerMissingLockFileHelp

instance ToDiagnostic BundlerMissingLockFile where
  renderDiagnostic (BundlerMissingLockFileCtx path) = do
    let header = "We could not perform Gemfile.lock analysis for Gemfile: " <> toText (show path)
    Errata (Just header) [] Nothing
  renderDiagnostic BundlerMissingLockFileHelp = do
    let header = "Ensure valid Gemfile.lock exists, and is readable by user. If you are using bundler, run `bundler install` to generate Gemfile.lock."
    Errata (Just header) [] Nothing
