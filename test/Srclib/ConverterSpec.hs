module Srclib.ConverterSpec (spec) where

import DepTypes (
  VerConstraint (..),
 )
import Srclib.Converter (verConstraintToRevision)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "verConstraintToRevision" $ do
    it "should use the version named by an exact constraint" $ do
      verConstraintToRevision (CEq "1.2.3") `shouldBe` Just "1.2.3"

    it "should use the version named by a compatible-release constraint" $ do
      verConstraintToRevision (CCompatible "1.2") `shouldBe` Just "1.2"

    it "should have no revision for a URI or an exclusion" $ do
      verConstraintToRevision (CURI "https://example.com/pkg.tar.gz") `shouldBe` Nothing
      verConstraintToRevision (CNot "1.5") `shouldBe` Nothing

    it "should fall back to a lone bound when that is all there is" $ do
      verConstraintToRevision (CGreaterOrEq "1.0") `shouldBe` Just "1.0"
      verConstraintToRevision (CGreater "1.0") `shouldBe` Just "1.0"
      verConstraintToRevision (CLessOrEq "2.0") `shouldBe` Just "2.0"
      verConstraintToRevision (CLess "2.0") `shouldBe` Just "2.0"

    -- The bug this ranking exists to fix: a range reported a different version
    -- depending on which bound the author happened to write first, and an
    -- upper bound written first won. `cryptography<60.0.0, >=46.0.3` reported
    -- 60.0.0 --- a version the range excludes, and one that does not exist.
    it "should prefer the lower bound of a range regardless of bound order" $ do
      let lowerFirst = CAnd (CGreaterOrEq "46.0.3") (CLess "60.0.0")
          upperFirst = CAnd (CLess "60.0.0") (CGreaterOrEq "46.0.3")
      verConstraintToRevision lowerFirst `shouldBe` Just "46.0.3"
      verConstraintToRevision upperFirst `shouldBe` Just "46.0.3"
      verConstraintToRevision upperFirst `shouldBe` verConstraintToRevision lowerFirst

    it "should prefer an exact version over any bound, in either position" $ do
      verConstraintToRevision (CAnd (CGreaterOrEq "1.0") (CEq "1.5")) `shouldBe` Just "1.5"
      verConstraintToRevision (CAnd (CEq "1.5") (CGreaterOrEq "1.0")) `shouldBe` Just "1.5"

    it "should prefer a bound the range admits over one it excludes" $ do
      -- >1.0 excludes 1.0 but <=2.0 admits 2.0, so 2.0 is the only candidate
      -- that actually satisfies the constraint.
      verConstraintToRevision (CAnd (CGreater "1.0") (CLessOrEq "2.0")) `shouldBe` Just "2.0"
      verConstraintToRevision (CAnd (CLessOrEq "2.0") (CGreater "1.0")) `shouldBe` Just "2.0"

    it "should skip constraints that have no revision to offer" $ do
      let withExclusion = CAnd (CLess "2.0") (CAnd (CNot "1.5") (CGreaterOrEq "1.0"))
      verConstraintToRevision withExclusion `shouldBe` Just "1.0"

    it "should be order-independent for a disjunction too" $ do
      let leftFirst = COr (CLess "2.0") (CGreaterOrEq "3.0")
          rightFirst = COr (CGreaterOrEq "3.0") (CLess "2.0")
      verConstraintToRevision leftFirst `shouldBe` verConstraintToRevision rightFirst

    it "should keep the first of two equally good candidates" $ do
      verConstraintToRevision (COr (CEq "6.1") (CEq "6.2")) `shouldBe` Just "6.1"
