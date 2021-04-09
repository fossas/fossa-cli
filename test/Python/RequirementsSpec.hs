module Python.RequirementsSpec
  ( spec,
  )
where

import Data.Foldable ( traverse_ )
import Data.Text (Text)
import Strategy.Python.Util (requirementParser, buildGraph)
import Strategy.Python.ReqTxt (requirementsTxtParser)
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Prelude
import DepTypes
import qualified Data.Map.Strict as M
import qualified Data.Text.IO as TIO
import qualified Test.Hspec as T
import GraphUtil (expectDeps)

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

depOne :: Dependency
depOne = Dependency { dependencyType = PipType
                        , dependencyName = "one"
                        , dependencyVersion = Just (CEq "1.0")
                        , dependencyLocations = []
                        , dependencyEnvironments = []
                        , dependencyTags = M.empty
                        }

depTwo :: Dependency
depTwo = Dependency { dependencyType = PipType
                        , dependencyName = "two"
                        , dependencyVersion = Just (CLessOrEq "2.0.0")
                        , dependencyLocations = []
                        , dependencyEnvironments = []
                        , dependencyTags = M.empty
                        }

depThree :: Dependency
depThree = Dependency { dependencyType = PipType
                        , dependencyName = "three"
                        , dependencyVersion = Just (CEq "3.0.0")
                        , dependencyLocations = []
                        , dependencyEnvironments = []
                        , dependencyTags = M.empty
                        }

depFour :: Dependency
depFour = Dependency { dependencyType = PipType
                        , dependencyName = "four"
                        , dependencyVersion = Just (CEq "4.0.0")
                        , dependencyLocations = []
                        , dependencyEnvironments = []
                        , dependencyTags = M.empty
                        }

spec :: T.Spec
spec = do
  T.describe "requirementParser"
    $ T.it "can parse the edge case examples"
    $ traverse_ (\input -> runParser requirementParser "" `shouldSucceedOn` input) examples

  requirementsTextFile <- T.runIO (TIO.readFile "test/Python/testdata/req.txt")
  T.describe "req file" $
    T.it "can parse" $
      case runParser requirementsTxtParser "" requirementsTextFile of
        Left r -> do
          T.expectationFailure $ "failed to parse: error:" ++ show r
        Right res -> do
          let result = buildGraph res
          expectDeps [depOne, depTwo, depThree, depFour] result
