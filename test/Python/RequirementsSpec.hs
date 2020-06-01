module Python.RequirementsSpec
  ( spec,
  )
where

import Data.Foldable
import Data.Text (Text)
import Strategy.Python.Util (requirementParser)
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Prelude
import Test.Hspec

spec :: Spec
spec =
  describe "requirementParser"
    $ it "can parse the edge case examples"
    $ traverse_ (\input -> runParser requirementParser "" `shouldSucceedOn` input) examples

examples :: [Text]
examples =
  [ "A",
    "A.B-C_D",
    "aa",
    "name",
    "name<=1",
    "name>=3",
    "name>=3,<2",
    "name@http://foo.com",
    "name [fred,bar] @ http://foo.com ; python_version=='2.7'",
    "name[quux, strange];python_version<'2.7' and platform_version=='2'",
    "name; os_name=='a' or os_name=='b'",
    "name; os_name=='a' and os_name=='b' or os_name=='c'",
    "name; os_name=='a' and (os_name=='b' or os_name=='c')",
    "name; os_name=='a' or os_name=='b' and os_name=='c'",
    "name; (os_name=='a' or os_name=='b') and os_name=='c'"
  ]
