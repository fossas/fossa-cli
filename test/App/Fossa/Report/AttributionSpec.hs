module App.Fossa.Report.AttributionSpec (
  spec,
) where

import App.Fossa.Report.Attribution
import Control.Applicative (liftA2)
import Data.Aeson
import Data.Map.Strict (Map)
import Data.Text (Text)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog

defaultRange :: Range Int
defaultRange = Range.linear 0 50

genLicense :: Gen License
genLicense =
  License . LicenseName
    <$> arbitraryText
    <*> Gen.maybe (LicenseContents <$> arbitraryText)

genProject :: Gen Project
genProject = Project <$> arbitraryText <*> arbitraryText

genDependency :: Gen Dependency
genDependency =
  Dependency
    <$> arbitraryText
    <*> arbitraryText
    <*> Gen.maybe arbitraryText
    <*> Gen.maybe Gen.bool
    <*> Gen.maybe arbitraryText
    <*> Gen.list defaultRange arbitraryText
    <*> Gen.maybe arbitraryText
    <*> Gen.maybe (Gen.list defaultRange genLicense)
    <*> Gen.list defaultRange genLicense
    <*> Gen.maybe arbitraryText
    <*> Gen.list defaultRange arbitraryText
    <*> Gen.list defaultRange arbitraryText
    <*> Gen.maybe arbitraryText
    <*> arbitraryText

genAttribution :: Gen Attribution
genAttribution =
  Attribution
    <$> genProject
    <*> Gen.list defaultRange genDependency
    <*> Gen.list defaultRange genDependency
    <*> genLicenseMap
    <*> Gen.maybe genCopyrightMap

tuplify :: Monad m => m a -> m b -> m (a, b)
tuplify = liftA2 (,)

genLicenseMap :: Gen (Map LicenseName LicenseContents)
genLicenseMap = do
  let genName = LicenseName <$> arbitraryText
  let genContents = LicenseContents <$> arbitraryText
  Gen.map defaultRange $ tuplify genName genContents

genCopyrightMap :: Gen (Map LicenseName [CopyrightText])
genCopyrightMap = do
  let genName = LicenseName <$> arbitraryText
  let genCopyright = CopyrightText <$> arbitraryText
  Gen.map defaultRange $ tuplify genName $ Gen.list defaultRange genCopyright

arbitraryText :: Gen Text
arbitraryText = Gen.text (Range.linear 3 25) Gen.unicodeAll

spec :: Spec
spec =
  describe "Attribution ToJSON/FromJSON instances" $
    modifyMaxSuccess (const 20) $
      it "are roundtrippable" $
        hedgehog $
          do
            attr <- forAll genAttribution
            roundtripJson attr

roundtripJson :: (MonadTest m, Show a, Eq a, ToJSON a, FromJSON a) => a -> m ()
roundtripJson a = tripping a toJSON fromJSON
