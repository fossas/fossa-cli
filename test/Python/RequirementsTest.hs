
module Python.RequirementsTest
  ( spec_parser
  ) where

import Prelude
import Test.Tasty.Hspec

import Data.Foldable
import Data.Text (Text)
import Text.Megaparsec

import Strategy.Python.Util (requirementParser)

spec_parser :: Spec
spec_parser = do
  describe "requirementParser" $ do
    it "can parse the edge case examples" $ do
      traverse_ (parseTest requirementParser) examples

examples :: [Text]
examples =
  [ "A"
  , "A.B-C_D"
  , "aa"
  , "name"
  , "name<=1"
  , "name>=3"
  , "name>=3,<2"
  , "name@http://foo.com"
  , "name [fred,bar] @ http://foo.com ; python_version=='2.7'"
  , "name[quux, strange];python_version<'2.7' and platform_version=='2'"
  , "name; os_name=='a' or os_name=='b'"
  , "name; os_name=='a' and os_name=='b' or os_name=='c'"
  , "name; os_name=='a' and (os_name=='b' or os_name=='c')"
  , "name; os_name=='a' or os_name=='b' and os_name=='c'"
  , "name; (os_name=='a' or os_name=='b') and os_name=='c'"
  ]
