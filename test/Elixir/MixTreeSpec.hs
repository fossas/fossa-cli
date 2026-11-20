module Elixir.MixTreeSpec (
  spec,
) where

import Data.Foldable (for_)
import Data.Map.Strict qualified as Map
import Data.Text.IO qualified as TIO
import DepTypes (
  DepType (GitType, HexType),
  Dependency (Dependency),
  VerConstraint (
    CAnd,
    CCompatible,
    CEq,
    CGreater,
    CGreaterOrEq,
    CLess,
    CLessOrEq,
    CNot,
    COr
  ),
 )
import GraphUtil (expectDep, expectDeps, expectDirect, expectEdges)
import Strategy.Elixir.MixTree (
  DepSCM (..),
  MixDep (..),
  MixDepResolved (..),
  PackageName (..),
  buildGraph,
  mixDepsCmdOutputParser,
  mixTreeCmdOutputParser,
  parseConstraintExpr,
 )
import Text.Megaparsec

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Test.Hspec
import Test.Hspec.Megaparsec hiding (err)

parseMatch :: (Show a, Eq a) => Parsec Void Text a -> Text -> a -> Expectation
parseMatch parser input expected = parse parser "" input `shouldParse` expected

shouldParseInto :: Text -> VerConstraint -> Expectation
shouldParseInto = parseMatch parseConstraintExpr

depZero :: MixDep
depZero =
  MixDep
    { depName = PackageName "pkgX"
    , depVersion = Nothing
    , depSCM = Other "/Users/some/dir/"
    , subDeps =
        [ MixDep
            { depName = PackageName "pkgY"
            , depVersion = Just $ COr (CCompatible "1.5") (CCompatible "2.0")
            , depSCM = Hex
            , subDeps = []
            }
        ]
    }

depOne :: MixDep
depOne =
  MixDep
    { depName = PackageName "one"
    , depVersion = Nothing
    , depSCM = Git "https://github.com/dep/one.git" Nothing
    , subDeps =
        [ MixDep
            { depName = PackageName "one_one"
            , depVersion = Nothing
            , depSCM = Git "https://github.com/dep/one_one" $ Just "2.11.0"
            , subDeps = []
            }
        , MixDep
            { depName = PackageName "one_two"
            , depVersion = Nothing
            , depSCM = Git "https://github.com/dep/one_two" $ Just "1.8.0"
            , subDeps = []
            }
        ]
    }

depTwo :: MixDep
depTwo =
  MixDep
    { depName = PackageName "two"
    , depVersion = Just $ CCompatible "2.0"
    , depSCM = Hex
    , subDeps =
        [ MixDep
            { depName = PackageName "pkg"
            , depVersion = Just $ CCompatible "1.1"
            , depSCM = Hex
            , subDeps =
                [ MixDep
                    { depName = PackageName "pkgC"
                    , depVersion = Just $ CCompatible "1.5"
                    , depSCM = Hex
                    , subDeps =
                        []
                    }
                ]
            }
        , MixDep
            { depName = PackageName "pkgE"
            , depVersion = Just $ CCompatible "0.13.0"
            , depSCM = Hex
            , subDeps =
                [ MixDep
                    { depName = PackageName "makeup"
                    , depVersion = Just $ CCompatible "1.0"
                    , depSCM = Hex
                    , subDeps =
                        []
                    }
                ]
            }
        ]
    }

depThree :: MixDep
depThree =
  MixDep
    { depName = PackageName "pkg"
    , depVersion = Just $ CAnd (CGreaterOrEq "0.8.1") (CLess "3.0.0")
    , depSCM = Hex
    , subDeps =
        [ MixDep
            { depName = PackageName "pkgC"
            , depVersion = Just $ CCompatible "1.1"
            , depSCM = Hex
            , subDeps =
                []
            }
        , MixDep
            { depName = PackageName "pkgD"
            , depVersion = Just $ CCompatible "1.0"
            , depSCM = Hex
            , subDeps =
                []
            }
        ]
    }

spec :: Spec
spec = do
  mixDepsTreeContent <- runIO (TIO.readFile "test/Elixir/testdata/mix_deps.tree")
  mixDepsContent <- runIO (TIO.readFile "test/Elixir/testdata/mix_deps")

  describe "mix tree parser" $ do
    it "should parse mix deps.tree output" $ do
      case runParser mixTreeCmdOutputParser "" mixDepsTreeContent of
        Left failCode -> expectationFailure $ show failCode
        Right result -> result `shouldMatchList` [depZero, depOne, depTwo, depThree]

    -- A git dependency pinned to a reference with characters other than alphanumerics and dots
    -- stays in the tree together with everything under it. The source parser is shared with
    -- `mix deps`, and a line it cannot read is dropped from the tree instead of failing the command.
    it "should keep git dependencies whose reference has characters other than alphanumerics and dots" $
      for_ ["v1.4.5-rc.1", "release/2.0", "feature_branch"] $ \ref ->
        parseMatch
          mixTreeCmdOutputParser
          ( Text.unlines
              [ "example"
              , "`-- one (https://github.com/dep/one.git - " <> ref <> ")"
              , "    `-- two ~> 2.0 (Hex package)"
              ]
          )
          [ MixDep
              { depName = PackageName "one"
              , depVersion = Nothing
              , depSCM = Git "https://github.com/dep/one.git" (Just ref)
              , subDeps = [MixDep (PackageName "two") (Just $ CCompatible "2.0") Hex []]
              }
          ]

  describe "mix deps parser" $ do
    it "should parse mix deps output" $ do
      case runParser mixDepsCmdOutputParser "" mixDepsContent of
        Left failCode -> expectationFailure $ show failCode
        Right result ->
          result
            `shouldBe` Map.fromList
              [
                ( PackageName "pkgA"
                , MixDepResolved
                    { depResolvedName = PackageName "pkgA"
                    , depResolvedVersion = Just $ CEq "1.0.4"
                    , depResolvedSCM = Hex
                    , depResolvedRef = Just $ CEq "1.0.4"
                    }
                )
              ,
                ( PackageName "pkgB"
                , MixDepResolved
                    { depResolvedName = PackageName "pkgB"
                    , depResolvedVersion = Just $ CEq "2.9.0"
                    , depResolvedSCM = Git "https://github.com/some-url.git" (Nothing)
                    , depResolvedRef = Just $ CEq "2a08250"
                    }
                )
              ,
                ( PackageName "pkgX"
                , MixDepResolved
                    { depResolvedName = PackageName "pkgX"
                    , depResolvedVersion = Just $ CEq "1.0.3"
                    , depResolvedSCM = Other "/Users/some/dir/"
                    , depResolvedRef = Nothing
                    }
                )
              ,
                ( PackageName "pkgC"
                , MixDepResolved
                    { depResolvedName = PackageName "pkgC"
                    , depResolvedVersion = Just $ CEq "2.11.0"
                    , depResolvedSCM = Git "https://github.com/some-id/some" (Just "2.11.0")
                    , depResolvedRef = Just $ CEq "e9448e5"
                    }
                )
              ,
                ( PackageName "pkgD"
                , MixDepResolved
                    { depResolvedName = PackageName "pkgD"
                    , depResolvedVersion = Nothing
                    , depResolvedSCM = Hex
                    , depResolvedRef = Nothing
                    }
                )
              ,
                ( PackageName "privatepkg"
                , MixDepResolved
                    { depResolvedName = PackageName "privatepkg"
                    , depResolvedVersion = Just $ CEq "0.1.0"
                    , depResolvedSCM = Hex
                    , depResolvedRef = Just $ CEq "0.1.0"
                    }
                )
              ]

    -- `mix deps` output for git dependencies pinned to a reference with characters other than
    -- alphanumerics and dots parses in full: the reference is kept verbatim, and the package
    -- version and locked commit around it are read as usual.
    it "should parse git dependencies whose reference has characters other than alphanumerics and dots" $
      for_ ["v1.4.5-rc.1", "release/2.0", "feature_branch"] $ \ref ->
        parseMatch
          mixDepsCmdOutputParser
          ( Text.unlines
              [ "* pkgG (https://github.com/some-org/pkg_g.git - " <> ref <> ")"
              , "  the dependency is not available, run \"mix deps.get\""
              , "* pkgH 2.0.0 (https://github.com/some-org/pkg_h.git - " <> ref <> ") (mix)"
              , "  locked at 2a08250"
              , "  ok"
              ]
          )
          ( Map.fromList
              [
                ( PackageName "pkgG"
                , MixDepResolved
                    { depResolvedName = PackageName "pkgG"
                    , depResolvedVersion = Nothing
                    , depResolvedSCM = Git "https://github.com/some-org/pkg_g.git" (Just ref)
                    , depResolvedRef = Nothing
                    }
                )
              ,
                ( PackageName "pkgH"
                , MixDepResolved
                    { depResolvedName = PackageName "pkgH"
                    , depResolvedVersion = Just $ CEq "2.0.0"
                    , depResolvedSCM = Git "https://github.com/some-org/pkg_h.git" (Just ref)
                    , depResolvedRef = Just $ CEq "2a08250"
                    }
                )
              ]
          )

  describe "buildGraph" $ do
    it "should identify dependency type correctly" $ do
      expectDep
        (Dependency GitType "https://github.com/dep/one.git" Nothing ([]) mempty Map.empty)
        ( buildGraph
            [ MixDep
                { depName = PackageName "one"
                , depVersion = Nothing
                , depSCM = Git "https://github.com/dep/one.git" Nothing
                , subDeps = []
                }
            ]
            Map.empty
        )

      expectDep
        (Dependency HexType "pkgZ" Nothing ([]) mempty Map.empty)
        ( buildGraph
            [ MixDep
                { depName = PackageName "pkgZ"
                , depVersion = Nothing
                , depSCM = Hex
                , subDeps = []
                }
            ]
            Map.empty
        )

    it "should use git ref for version, when dependency is GitType" $ do
      expectDep
        (Dependency GitType "https://github.com/some-url.git" (Just $ CEq "2a08250") ([]) mempty Map.empty)
        ( buildGraph
            ( [ MixDep
                  { depName = PackageName "pkgY"
                  , depVersion = Nothing
                  , depSCM = Git "https://github.com/some-url.git" Nothing
                  , subDeps = []
                  }
              ]
            )
            ( Map.fromList
                [
                  ( PackageName "pkgY"
                  , MixDepResolved
                      { depResolvedName = PackageName "pkgY"
                      , depResolvedVersion = Nothing
                      , depResolvedSCM = Hex
                      , depResolvedRef = Just $ CEq "2a08250"
                      }
                  )
                ]
            )
        )

    it "should use locked ref for version, when locked ref exists" $ do
      expectDep
        (Dependency HexType "pkgX" (Just $ CEq "2.0.1") ([]) mempty Map.empty)
        ( buildGraph
            ( [ MixDep
                  { depName = PackageName "pkgX"
                  , depVersion = Just $ CCompatible "2.0"
                  , depSCM = Hex
                  , subDeps = []
                  }
              ]
            )
            ( Map.fromList
                [
                  ( PackageName "pkgX"
                  , MixDepResolved
                      { depResolvedName = PackageName "pkgX"
                      , depResolvedVersion = Nothing
                      , depResolvedSCM = Hex
                      , depResolvedRef = Just $ CEq "2.0.1"
                      }
                  )
                ]
            )
        )

    -- A compiled git dependency has both a package version and a locked commit. Its locator
    -- names a revision of the repository, so the commit is the one reported.
    it "should use locked commit over package version, when dependency is GitType" $ do
      expectDep
        (Dependency GitType "https://github.com/some-org/pkg_h.git" (Just $ CEq "2a08250") [] mempty Map.empty)
        ( buildGraph
            [ MixDep
                { depName = PackageName "pkgH"
                , depVersion = Nothing
                , depSCM = Git "https://github.com/some-org/pkg_h.git" (Just "v1.4.5-rc.1")
                , subDeps = []
                }
            ]
            ( Map.fromList
                [
                  ( PackageName "pkgH"
                  , MixDepResolved
                      { depResolvedName = PackageName "pkgH"
                      , depResolvedVersion = Just $ CEq "2.0.0"
                      , depResolvedSCM = Git "https://github.com/some-org/pkg_h.git" (Just "v1.4.5-rc.1")
                      , depResolvedRef = Just $ CEq "2a08250"
                      }
                  )
                ]
            )
        )

    it "should use version constraint for version, when locked ref or resolved version does not exists" $ do
      expectDep
        (Dependency HexType "pkgW" (Just $ CCompatible "2.0") ([]) mempty Map.empty)
        ( buildGraph
            ( [ MixDep
                  { depName = PackageName "pkgW"
                  , depVersion = Just $ CCompatible "2.0"
                  , depSCM = Hex
                  , subDeps = []
                  }
              ]
            )
            ( Map.fromList
                [
                  ( PackageName "pkgW"
                  , MixDepResolved
                      { depResolvedName = PackageName "pkgW"
                      , depResolvedVersion = Nothing
                      , depResolvedSCM = Hex
                      , depResolvedRef = Nothing
                      }
                  )
                ]
            )
        )

    it "should build graph correctly" $ do
      let graph =
            buildGraph
              [ MixDep
                  { depName = PackageName "pkgParentA"
                  , depVersion = Nothing
                  , depSCM = Hex
                  , subDeps =
                      [ MixDep
                          { depName = PackageName "pkgChildC"
                          , depVersion = Nothing
                          , depSCM = Hex
                          , subDeps = []
                          }
                      , MixDep
                          { depName = PackageName "pkgParentB"
                          , depVersion = Nothing
                          , depSCM = Hex
                          , subDeps = []
                          }
                      ]
                  }
              , MixDep
                  { depName = PackageName "pkgParentB"
                  , depVersion = Nothing
                  , depSCM = Hex
                  , subDeps =
                      [ MixDep
                          { depName = PackageName "pkgChildC"
                          , depVersion = Nothing
                          , depSCM = Hex
                          , subDeps = []
                          }
                      , MixDep
                          { depName = PackageName "pkgChildD"
                          , depVersion = Nothing
                          , depSCM = Hex
                          , subDeps = []
                          }
                      ]
                  }
              ]
              ( Map.fromList
                  [
                    ( PackageName "pkgParentA"
                    , MixDepResolved
                        { depResolvedName = PackageName "pkgParentA"
                        , depResolvedVersion = Just $ CEq "A"
                        , depResolvedSCM = Hex
                        , depResolvedRef = Nothing
                        }
                    )
                  ,
                    ( PackageName "pkgParentB"
                    , MixDepResolved
                        { depResolvedName = PackageName "pkgParentB"
                        , depResolvedVersion = Just $ CEq "B"
                        , depResolvedSCM = Hex
                        , depResolvedRef = Nothing
                        }
                    )
                  ,
                    ( PackageName "pkgChildC"
                    , MixDepResolved
                        { depResolvedName = PackageName "pkgChildC"
                        , depResolvedVersion = Just $ CEq "C"
                        , depResolvedSCM = Hex
                        , depResolvedRef = Nothing
                        }
                    )
                  ,
                    ( PackageName "pkgChildD"
                    , MixDepResolved
                        { depResolvedName = PackageName "pkgChildD"
                        , depResolvedVersion = Just $ CEq "D"
                        , depResolvedSCM = Hex
                        , depResolvedRef = Nothing
                        }
                    )
                  ]
              )

      let depA = Dependency HexType "pkgParentA" (Just $ CEq "A") [] mempty Map.empty
          depB = Dependency HexType "pkgParentB" (Just $ CEq "B") [] mempty Map.empty
          depC = Dependency HexType "pkgChildC" (Just $ CEq "C") [] mempty Map.empty
          depD = Dependency HexType "pkgChildD" (Just $ CEq "D") [] mempty Map.empty

          expectedDeps = [depA, depB, depC, depD]
          expectedDirect = [depA, depB]
          expectedEdges =
            [ (depA, depB)
            , (depA, depC)
            , (depB, depC)
            , (depB, depD)
            ]

      expectDeps expectedDeps graph
      expectEdges expectedEdges graph
      expectDirect expectedDirect graph

  describe "parseConstraintExpr" $ do
    it "should parse equality constraint" $
      do
        "1.1" `shouldParseInto` (CEq "1.1")
        "=1.1" `shouldParseInto` (CEq "1.1")

    it "should parse greater than or equal constraint" $
      do
        ">=2.1" `shouldParseInto` (CGreaterOrEq "2.1")

    it "should parse less than or equal constraint" $
      do
        "<=2.1" `shouldParseInto` (CLessOrEq "2.1")

    it "should parse greater than constraint" $
      do
        ">2.1" `shouldParseInto` (CGreater "2.1")

    it "should parse less than constraint" $
      do
        "<2.1" `shouldParseInto` (CLess "2.1")

    it "should parse not equal to constraint" $
      do
        "!=3.1" `shouldParseInto` (CNot "3.1")

    it "should parse wildcard constraint" $
      do
        "*" `shouldParseInto` (CEq "*")

    it "should parse (~>) constraints" $
      do
        "~>5.1" `shouldParseInto` (CCompatible "5.1")
        "~> 5.1" `shouldParseInto` (CCompatible "5.1")

    it "should parse pre version constraints" $
      do
        "2.2.0-dev" `shouldParseInto` (CEq "2.2.0-dev")
        "~> 2.2.0-dev" `shouldParseInto` (CCompatible "2.2.0-dev")
        "~>1.0.0-alpha.3" `shouldParseInto` (CCompatible "1.0.0-alpha.3")
        "1.0.0-alpha.3+20130417140000.amd64" `shouldParseInto` (CEq "1.0.0-alpha.3+20130417140000.amd64")

    context "when provided with multiple constraints" $ do
      it "should parse OR operator" $
        do
          "6.1 or 6.2" `shouldParseInto` (COr (CEq "6.1") (CEq "6.2"))

      it "should parse AND operator" $
        do
          ">=7.1 and <7.7" `shouldParseInto` (CAnd (CGreaterOrEq "7.1") (CLess "7.7"))

      it "should give precedence to the AND operator" $
        do
          ">=8 and 9.1.1 or 9.1.2 and <=8.9.9 " `shouldParseInto` CAnd (CAnd (CGreaterOrEq "8") (COr (CEq "9.1.1") (CEq "9.1.2"))) (CLessOrEq "8.9.9")

    context "when provided with irregular spacing or tabs" $ do
      it "should parse expressions" $
        do
          "  1.1" `shouldParseInto` (CEq "1.1")
          "==1.1  " `shouldParseInto` (CEq "1.1")
          "= 1.1" `shouldParseInto` (CEq "1.1")
          "6.1 and 6.2" `shouldParseInto` (CAnd (CEq "6.1") (CEq "6.2"))
          " 6.1 or 6.2   " `shouldParseInto` (COr (CEq "6.1") (CEq "6.2"))
          ">=7.1 and <7.7" `shouldParseInto` (CAnd (CGreaterOrEq "7.1") (CLess "7.7"))
          ">=7.1 and   < 7.7" `shouldParseInto` (CAnd (CGreaterOrEq "7.1") (CLess "7.7"))
          " >=7.1 and < 7.7" `shouldParseInto` (CAnd (CGreaterOrEq "7.1") (CLess "7.7"))
          "<7.7 and >=7.1" `shouldParseInto` (CAnd (CLess "7.7") (CGreaterOrEq "7.1"))
          "\t1.1" `shouldParseInto` (CEq "1.1")
          "<7.7 and\t>=7.1" `shouldParseInto` (CAnd (CLess "7.7") (CGreaterOrEq "7.1"))
