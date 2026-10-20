module Python.RequirementsSpec (
  spec,
) where

import Data.Foldable (traverse_)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import DepTypes
import GraphUtil (expectDeps)
import Strategy.Python.ReqTxt (requirementsTxtParser)
import Strategy.Python.Util (Marker (..), MarkerOp (..), Req (..), buildGraph, requirementParser)
import Test.Hspec qualified as T
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Prelude

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
  , "name; os_name=='a' and (os_name=='b' or os_name=='c' and os_name=='d')"
  , "name; (os_name=='b' or os_name=='c' and os_name=='d') and os_name=='a'"
  , "name; os_name=='a' or os_name=='b' and os_name=='c'"
  , "name; (os_name=='a' or os_name=='b') and os_name=='c'"
  ]

depOne :: Dependency
depOne =
  Dependency
    { dependencyType = PipType
    , dependencyName = "one"
    , dependencyVersion = Just (CEq "1.0")
    , dependencyLocations = []
    , dependencyEnvironments = mempty
    , dependencyTags = Map.empty
    }

depTwo :: Dependency
depTwo =
  Dependency
    { dependencyType = PipType
    , dependencyName = "two"
    , dependencyVersion = Just (CLessOrEq "2.0.0")
    , dependencyLocations = []
    , dependencyEnvironments = mempty
    , dependencyTags = Map.empty
    }

depThree :: Dependency
depThree =
  Dependency
    { dependencyType = PipType
    , dependencyName = "three"
    , dependencyVersion = Just (CEq "3.0.0")
    , dependencyLocations = []
    , dependencyEnvironments = mempty
    , dependencyTags = Map.empty
    }

depFour :: Dependency
depFour =
  Dependency
    { dependencyType = PipType
    , dependencyName = "four"
    , dependencyVersion = Just (CEq "4.0.0")
    , dependencyLocations = []
    , dependencyEnvironments = mempty
    , dependencyTags = Map.empty
    }

spec :: T.Spec
spec = do
  markerOperatorSpec
  T.describe "requirementParser" $
    T.it "can parse the edge case examples" $
      traverse_ (\input -> runParser requirementParser "" `shouldSucceedOn` input) examples

  requirementsTextFile <- T.runIO (TIO.readFile "test/Python/testdata/req.txt")
  T.describe "req file" $
    T.it "can parse" $
      case runParser requirementsTxtParser "" requirementsTextFile of
        Left r -> do
          T.expectationFailure $ "failed to parse: error:" ++ errorBundlePretty r
        Right res -> do
          let result = buildGraph Nothing res
          expectDeps [depOne, depTwo, depThree, depFour] result

markerOperatorSpec :: T.Spec
markerOperatorSpec =
  T.describe "requirementParser" $ do
    -- Fails until src/Strategy/Python/Util.hs:requirementParser is fixed: marker_op tries version_cmp first, whose leading whitespace consumes the space before `in`, so the PEP 508 `in` and `not in` operators never parse and the whole requirement is rejected. See the PR description.
    T.it "parses the PEP 508 in and not in marker operators" $ do
      parse requirementParser "" "name; sys_platform in \"win32\""
        `shouldParse` NameReq "name" Nothing Nothing (Just (MarkerExpr "sys_platform" MarkerIn "win32"))
      parse requirementParser "" "name; python_version not in \"2.6 2.7\""
        `shouldParse` NameReq "name" Nothing Nothing (Just (MarkerExpr "python_version" MarkerNotIn "2.6 2.7"))
