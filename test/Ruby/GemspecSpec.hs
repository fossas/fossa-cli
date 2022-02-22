module Ruby.GemspecSpec (spec) where

import Strategy.Ruby.Gemspec (rubyString)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (ParseErrorBundle, Parsec, runParser)

-- I'm not sure about these helpers. Get guidance on whether they help readability.
shouldParse :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
shouldParse parser = runParser parser ""

to :: (Show a1, Show a2, Eq a2) => Either a1 a2 -> a2 -> IO ()
to parsed expected = either (fail . show) (`shouldBe` expected) parsed

stringParseTest :: Spec
stringParseTest =
  describe "Ruby string parsing test" $ do
    it "Parses string enclosed in \"" $
      rubyString `shouldParse` "\"Hello\"" `to` "Hello"
    it "Parses string enclosed in \'" $
      rubyString `shouldParse` "\'Hello\'" `to` "Hello"

spec :: Spec
spec = stringParseTest
