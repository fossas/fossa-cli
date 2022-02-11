module Strategy.Go.Errors (
  GoModStaticAnalysisLimitation (..),
  GoModTransitivesNotFilled (..),
  refGoModDocUrl,
) where

import App.Docs (strategyLangDocUrl)
import Data.List.NonEmpty (fromList)
import Data.Text (Text)
import Diag.Diagnostic (ActionableDiagCtx (..), ToDiagnostic (..), renderActionable)

refGoModDocUrl :: Text
refGoModDocUrl = strategyLangDocUrl "golang/gomodules.md"

data GoModStaticAnalysisLimitation = GoModStaticAnalysisLimitation
data GoModTransitivesNotFilled = GoModTransitivesNotFilled

instance ToDiagnostic GoModTransitivesNotFilled where
  renderDiagnostic (GoModTransitivesNotFilled) =
    renderActionable $
      ActionableDiagCtx
        { description = "Deep dependencies and edges between them will not be reported."
        , documentationLinks = fromList [refGoModDocUrl]
        , troubleshootingSteps =
            Just $
              fromList
                [ "Ensure go executable is in your PATH."
                , "Ensure go project is built."
                , "Ensure go mod graph, go list -m -json all command can be performed for your project."
                ]
        }

instance ToDiagnostic GoModStaticAnalysisLimitation where
  renderDiagnostic (GoModStaticAnalysisLimitation) =
    renderActionable $
      ActionableDiagCtx
        { description = "Suboptimal static analyzer used for golang module analysis. Deep dependencies and edges will not be reported."
        , documentationLinks = fromList [refGoModDocUrl]
        , troubleshootingSteps =
            Just $
              fromList
                [ "Ensure go executable is in your PATH."
                , "Ensure go project is built."
                , "Ensure go mod graph, go list -m -json all command can be performed for your project."
                ]
        }
