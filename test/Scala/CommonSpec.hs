{-# LANGUAGE QuasiQuotes #-}

module Scala.CommonSpec (
  spec,
) where

import Data.Set qualified as Set
import Data.Text (Text)
import DepTypes (
  DepEnvironment (EnvProduction),
  DepType (MavenType),
  Dependency (Dependency),
  VerConstraint (CEq),
 )
import Graphing (deep, edges)
import Strategy.Scala.Common (removeLogPrefixes, withoutStdLibs)
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

  describe "withoutStdLibs" $ do
    it "should remove standard library for scala" $ do
      let graph = edges [(akkaStream, scalaLib), (akkaStream, scala3Lib)]
      withoutStdLibs graph `shouldBe` deep akkaStream

akkaStream :: Dependency
akkaStream = mkDep "com.typesafe.akka:akka-stream_3" "2.6.19"

scalaLib :: Dependency
scalaLib = mkDep "org.scala-lang:scala-library" "2.0.0"

scala3Lib :: Dependency
scala3Lib = mkDep "org.scala-lang:scala3-library_3" "3.1.1"

mkDep :: Text -> Text -> Dependency
mkDep name version =
  Dependency
    MavenType
    name
    (Just $ CEq version)
    mempty
    (Set.singleton EnvProduction)
    mempty
