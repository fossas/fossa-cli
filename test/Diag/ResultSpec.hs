module Diag.ResultSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as Text
import Diag.Diagnostic (ToDiagnostic (renderDiagnostic))
import Diag.Result (
  EmittedWarn (WarnOnErrGroup),
  ErrGroup (ErrGroup),
  ErrWithStack (ErrWithStack),
  SomeErr (SomeErr),
  SomeWarn (SomeWarn),
  Stack (Stack),
  renderFailure,
  renderSuccess,
 )
import Effect.Logger (renderIt)
import Errata (Errata (Errata))
import Prettyprinter (Doc, unAnnotate)
import Prettyprinter.Render.Terminal (AnsiStyle)
import Test.Hspec (Spec, describe, it, shouldBe)

newtype TestMsg = TestMsg Text

instance ToDiagnostic TestMsg where
  renderDiagnostic (TestMsg t) = Errata (Just t) [] Nothing

-- Renders without ANSI color annotations, the way the raw logger does.
renderPlain :: Doc AnsiStyle -> Text
renderPlain = renderIt . unAnnotate

-- Counts lines that consist of exactly the traceback header. A traceback
-- header glued onto the end of an error message (the regression this spec
-- covers) does not count.
tracebackHeaderCount :: Text -> Int
tracebackHeaderCount rendered = length . filter (== "Traceback:") . map Text.strip $ Text.lines rendered

spec :: Spec
spec = describe "renderErrs" $ do
  let errA = ErrWithStack (Stack ["step one"]) (SomeErr (TestMsg "first error"))
      errB = ErrWithStack (Stack ["step two"]) (SomeErr (TestMsg "second error"))
      errs = NE.fromList [errA, errB]

  it "renders each error's traceback header on its own line in failures" $ do
    let rendered = renderPlain $ renderFailure [] (ErrGroup [] [] [] [] [] errs) "An issue occurred"
    Text.isInfixOf "second errorTraceback:" rendered `shouldBe` False
    tracebackHeaderCount rendered `shouldBe` 2

  it "renders each error's traceback header on its own line in warnings" $ do
    let warned = WarnOnErrGroup (NE.fromList [SomeWarn (TestMsg "some warning")]) [] [] [] [] errs
        rendered = maybe "" renderPlain (renderSuccess [warned] "An issue occurred")
    Text.isInfixOf "second errorTraceback:" rendered `shouldBe` False
    tracebackHeaderCount rendered `shouldBe` 2
