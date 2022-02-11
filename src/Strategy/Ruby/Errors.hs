module Strategy.Ruby.Errors (
  BundlerMissingLockFile (..),
  RubyMissingEdges (..),
  RubyMissingDepClassification (..),

  -- * Reference docs
  bundlerLockFileRationaleUrl,
  rubyFossaDocUrl,
) where

import App.Docs (strategyLangDocUrl)
import Data.Text (Text)
import Diag.Diagnostic (ToDiagnostic, renderDiagnostic)
import Path
import Prettyprinter (Pretty (pretty), indent, viaShow, vsep)

bundlerLockFileRationaleUrl :: Text
bundlerLockFileRationaleUrl = "https://bundler.io/rationale.html#sharing-your-application-with-other-developers"

rubyFossaDocUrl :: Text
rubyFossaDocUrl = strategyLangDocUrl "ruby/ruby.md"

newtype BundlerMissingLockFile = BundlerMissingLockFile (Path Abs File)
instance ToDiagnostic BundlerMissingLockFile where
  renderDiagnostic (BundlerMissingLockFile path) =
    vsep
      [ "We could not find or parse Gemfile.lock corresponding to Gemfile: " <> viaShow path
      , ""
      , "Ensure your ruby project is built, and has lockfile prior to running fossa."
      , "If you are using bundler, you can perform: `bundler install` to generate Gemfile.lock."
      , ""
      , "Refer to:"
      , indent 2 $ pretty $ "- " <> bundlerLockFileRationaleUrl
      , indent 2 $ pretty $ "- " <> rubyFossaDocUrl
      ]

data RubyMissingEdges = RubyMissingEdges
instance ToDiagnostic RubyMissingEdges where
  renderDiagnostic (RubyMissingEdges) =
    "Could not infer edges between dependencies."

data RubyMissingDepClassification = RubyMissingDepClassification
instance ToDiagnostic RubyMissingDepClassification where
  renderDiagnostic (RubyMissingDepClassification) =
    "Could not classify dependencies between direct or deep. All dependencies will be classified as direct."
