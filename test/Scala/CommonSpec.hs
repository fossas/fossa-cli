{-# LANGUAGE QuasiQuotes #-}

module Scala.CommonSpec (
  spec,
) where

import Data.Text (Text)
import Strategy.Scala.Common (removeLogPrefixes)
import Test.Hspec (
  Spec,
  describe,
  it,
  shouldBe,
 )
import Text.RawString.QQ (r)

stdoutFromSbt :: Text
stdoutFromSbt =
  [r|[info] truth
[info] is
[warn] out
[error] there!
|]

spec :: Spec
spec = do
  describe "removeLogPrefixes" $ do
    it "should parse sbt artifact" $ do
      removeLogPrefixes "pineapples!\n" `shouldBe` "pineapples!\n"
      removeLogPrefixes "[info] pineapples!\n" `shouldBe` "pineapples!\n"
      removeLogPrefixes "[warn] pineapples!\n" `shouldBe` "pineapples!\n"
      removeLogPrefixes "[error] pineapples!\n" `shouldBe` "pineapples!\n"
      removeLogPrefixes "[debug] pineapples!\n" `shouldBe` "pineapples!\n"
      removeLogPrefixes "[success] pineapples!\n" `shouldBe` "pineapples!\n"
      removeLogPrefixes "[trace] pineapples!\n" `shouldBe` "pineapples!\n"
      removeLogPrefixes stdoutFromSbt `shouldBe` "truth\nis\nout\nthere!\n"
