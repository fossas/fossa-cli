module Strategy.Ruby.Errors (
  BundlerMissingLockFile (..),

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
      [ "We could not perform Gemfile.lock analysis for Gemfile: " <> viaShow path
      , ""
      , indent 2 $
          vsep
            [ "Ensure valid Gemfile.lock exists, and is readable by user."
            , "If you are using bundler, you can perform: `bundler install` to generate Gemfile.lock."
            ]
      , ""
      , "Refer to:"
      , indent 2 $ pretty $ "- " <> bundlerLockFileRationaleUrl
      , indent 2 $ pretty $ "- " <> rubyFossaDocUrl
      ]
