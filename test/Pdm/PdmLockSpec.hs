{-# LANGUAGE QuasiQuotes #-}

module Pdm.PdmLockSpec (
  spec,
) where

import Data.Text (Text)
import Strategy.Python.PDM.PdmLock (PdmLock (..), PdmLockPackage (..), pdmLockCodec)
import Strategy.Python.Util (Req (..))
import Test.Hspec (
  Spec,
  describe,
  it,
  shouldBe,
 )
import Text.RawString.QQ (r)
import Toml qualified

mkReq :: Text -> Req
mkReq name = NameReq name Nothing Nothing Nothing

spec :: Spec
spec = do
  describe "lockfile" $ do
    it "should parse empty lock file" $ do
      let expected = PdmLock mempty
      Toml.decode pdmLockCodec lockNoContent `shouldBe` Right expected

    it "should parse lock file with one entry" $ do
      let expected = PdmLock [PdmLockPackage "blinker" "1.6.2" Nothing Nothing Nothing Nothing Nothing]
      Toml.decode pdmLockCodec lockOneEntry `shouldBe` Right expected

    it "should parse lock file with multiple entry" $ do
      let expected =
            PdmLock
              [ PdmLockPackage "blinker" "1.6.2" Nothing Nothing Nothing Nothing (Just [mkReq "b"])
              , PdmLockPackage "b" "1.0.0" Nothing Nothing Nothing Nothing Nothing
              ]
      Toml.decode pdmLockCodec lockMultipleEntry `shouldBe` Right expected

    it "should parse lock file, with git deps" $ do
      let expected =
            PdmLock
              [ PdmLockPackage
                  "pip"
                  "22.0"
                  (Just "https://github.com/pypa/pip.git")
                  (Just "1742af7bdc0b4a883a35ad69da6dcaefe0f21978")
                  Nothing
                  Nothing
                  Nothing
              ]
      Toml.decode pdmLockCodec lockWithGitEntry `shouldBe` Right expected

    it "should parse lock file, with filepath deps" $ do
      let expected =
            PdmLock
              [ PdmLockPackage
                  "flake8"
                  "6.0.0"
                  Nothing
                  Nothing
                  (Just "./subpackage/flake8-6.0.0-py2.py3-none-any.whl")
                  Nothing
                  (Just [mkReq "mccabe"])
              ]
      Toml.decode pdmLockCodec lockWithFilePathEntry `shouldBe` Right expected

    it "should parse lock file, with url deps" $ do
      let expected =
            PdmLock
              [ PdmLockPackage
                  "en-core-web-trf"
                  "3.5.0"
                  Nothing
                  Nothing
                  Nothing
                  (Just "https://github.com/explosion/spacy-models/releases/download/en_core_web_trf-3.5.0/en_core_web_trf-3.5.0-py3-none-any.whl")
                  Nothing
              ]
      Toml.decode pdmLockCodec lockWithFileUrlEntry `shouldBe` Right expected

lockNoContent :: Text
lockNoContent = ""

lockOneEntry :: Text
lockOneEntry =
  [r|
[[package]]
name = "blinker"
version = "1.6.2"
|]

lockMultipleEntry :: Text
lockMultipleEntry =
  [r|
[[package]]
name = "blinker"
version = "1.6.2"
dependencies = [
    "b",
]

[[package]]
name = "b"
version = "1.0.0"
|]

lockWithGitEntry :: Text
lockWithGitEntry =
  [r|
[[package]]
name = "pip"
version = "22.0"
requires_python = ">=3.7"
git = "https://github.com/pypa/pip.git"
ref = "22.0"
revision = "1742af7bdc0b4a883a35ad69da6dcaefe0f21978"
summary = "The PyPA recommended tool for installing Python packages."
|]

lockWithFilePathEntry :: Text
lockWithFilePathEntry =
  [r|
[[package]]
name = "flake8"
version = "6.0.0"
requires_python = ">=3.8.1"
path = "./subpackage/flake8-6.0.0-py2.py3-none-any.whl"
summary = "the modular source code checker: pep8 pyflakes and co"
dependencies = [
    "mccabe",
]
|]

lockWithFileUrlEntry :: Text
lockWithFileUrlEntry =
  [r|
[[package]]
name = "en-core-web-trf"
version = "3.5.0"
url = "https://github.com/explosion/spacy-models/releases/download/en_core_web_trf-3.5.0/en_core_web_trf-3.5.0-py3-none-any.whl"
|]
