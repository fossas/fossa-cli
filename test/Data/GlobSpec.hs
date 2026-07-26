module Data.GlobSpec (spec) where

import Data.Glob (unGlob, unsafeGlobAbs, unsafeGlobRel)
import Data.Glob qualified as Glob
import Data.List (isInfixOf, isPrefixOf)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = describe "normalize" $ do
  it "collapses a leading ./ segment" $
    Glob.normalize (unsafeGlobAbs "/root/./packages/*/package.json")
      `shouldBe` Glob.normalize (unsafeGlobAbs "/root/packages/*/package.json")

  it "collapses redundant // separators" $
    Glob.normalize (unsafeGlobAbs "/root//packages/*/package.json")
      `shouldBe` Glob.normalize (unsafeGlobAbs "/root/packages/*/package.json")

  it "leaves ** globs intact" $
    unGlob (Glob.normalize (unsafeGlobRel "packages/**/package.json"))
      `shouldSatisfy` ("**" `isInfixOf`)

  it "leaves pnpm ! negation intact" $
    unGlob (Glob.normalize (unsafeGlobRel "!packages/test/package.json"))
      `shouldSatisfy` ("!" `isPrefixOf`)

  it "leaves .. segments intact" $
    unGlob (Glob.normalize (unsafeGlobRel "../sibling/package.json"))
      `shouldSatisfy` (".." `isInfixOf`)
