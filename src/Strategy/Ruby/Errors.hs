module Strategy.Ruby.Errors (
  BundlerMissingLockFile (..),

  -- * Reference docs
  bundlerLockFileRationaleUrl,
  rubyFossaDocUrl,
) where

import App.Docs (strategyLangDocUrl)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Diag.Diagnostic (DiagnosticInfo (..), ToDiagnostic, renderDiagnostic)
import Path

bundlerLockFileRationaleUrl :: Text
bundlerLockFileRationaleUrl = "https://bundler.io/rationale.html#sharing-your-application-with-other-developers"

rubyFossaDocUrl :: Text
rubyFossaDocUrl = strategyLangDocUrl "ruby/ruby.md"

newtype BundlerMissingLockFile = BundlerMissingLockFile (Path Abs File)
instance ToDiagnostic BundlerMissingLockFile where
  renderDiagnostic (BundlerMissingLockFile path) = do
    let ctx = "We could not perform Gemfile.lock analysis for Gemfile: " <> toText (show path)
        help = "Ensure valid Gemfile.lock exists, and is readable by user. If you are using bundler, run `bundler install` to generate Gemfile.lock."
        documentationReferences = [rubyFossaDocUrl, bundlerLockFileRationaleUrl]
    DiagnosticInfo Nothing Nothing (Just documentationReferences) Nothing (Just help) (Just ctx) Nothing
