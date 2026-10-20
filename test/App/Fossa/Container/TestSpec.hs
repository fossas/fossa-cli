module App.Fossa.Container.TestSpec (
  spec,
) where

import App.Fossa.Config.Container (TestOutputFormat (TestOutputJson, TestOutputPretty))
import App.Fossa.Container.Test (reportIssues)
import Control.Effect.Diagnostics (errorBoundary)
import Data.String.Conversion (toString)
import Diag.Result (Result (Failure, Success), renderFailure)
import Effect.Logger (renderIt)
import Fossa.API.Types (Issues (issuesCount, issuesIssues))
import Test.Effect (expectationFailure', it', shouldContain', shouldNotContain')
import Test.Fixtures qualified as Fixtures
import Test.Hspec (Spec, describe)

spec :: Spec
spec =
  describe "reportIssues" $ do
    it' "passes when no issues are found" $
      reportIssues TestOutputPretty Fixtures.emptyIssues

    -- Failing by calling 'System.Exit.exitFailure' from inside the effect stack
    -- used to be reported as `An exception occurred:ExitFailure 1`, because the
    -- subcommand runner catches every synchronous exception as a diagnostic.
    it' "fails through diagnostics when issues are found" $ do
      result <- errorBoundary $ reportIssues TestOutputPretty Fixtures.issuesAvailable
      expectIssuesFailure (issuesCount Fixtures.issuesAvailable) result

    it' "fails through diagnostics when issues are found but not listed (push-only API key)" $ do
      let issues = Fixtures.issuesAvailable{issuesIssues = []}
      result <- errorBoundary $ reportIssues TestOutputJson issues
      expectIssuesFailure (issuesCount issues) result
  where
    expectIssuesFailure count = \case
      Success _ _ -> expectationFailure' "expected `fossa container test` to fail when issues are found"
      Failure ws eg -> do
        let rendered = toString . renderIt $ renderFailure ws eg "An issue occurred"
        rendered `shouldContain'` ("The scan has revealed issues. Number of issues found: " <> show count)
        rendered `shouldNotContain'` "An exception occurred"
