module Fortran.FpmTomlSpec (
  spec,
) where

import Data.Map (fromList)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import DepTypes (
  DepEnvironment (EnvDevelopment, EnvProduction),
  DepType (GitType),
  Dependency (Dependency),
  VerConstraint (CEq),
 )
import GraphUtil (expectDeps, expectEdges)
import Strategy.Fortran.FpmToml (
  FpmDependency (..),
  FpmGitDependency (..),
  FpmPathDependency (..),
  FpmToml (..),
  FpmTomlExecutables (..),
  buildGraph,
 )
import Test.Hspec (
  Spec,
  describe,
  it,
  runIO,
  shouldBe,
 )
import Toml qualified

mkPathFpmDep :: Text -> FpmDependency
mkPathFpmDep name = FpmPathDep $ FpmPathDependency name

mkGitDep :: Text -> Maybe Text -> Set.Set DepEnvironment -> Dependency
mkGitDep name version envs = Dependency GitType name (CEq <$> version) [] envs mempty

mkGitFpmDep :: FpmGitDependency
mkGitFpmDep = FpmGitDependency "git-url" Nothing Nothing Nothing

expectedFpmToml :: FpmToml
expectedFpmToml =
  FpmToml
    ( fromList
        [ ("my-utils", mkPathFpmDep "utils")
        , ("dep-head", FpmGitDep mkGitFpmDep)
        , ("dep-branch", FpmGitDep $ mkGitFpmDep{branch = Just "main"})
        , ("dep-tag", FpmGitDep $ mkGitFpmDep{tag = Just "v0.2.1"})
        , ("dep-rev", FpmGitDep $ mkGitFpmDep{rev = Just "2f5eaba"})
        ]
    )
    (fromList [("dep-dev", FpmGitDep mkGitFpmDep{url = "git-url-dev-dep", rev = Just "2f5eaba"})])
    ([FpmTomlExecutables $ fromList [("dep-exec", FpmGitDep mkGitFpmDep{url = "git-url-exec"})]])

spec :: Spec
spec = do
  content <- runIO (TIO.readFile "test/Fortran/testdata/fpm.toml")
  describe "fpmTomlCodec" $
    it "should parse fpm.toml file" $
      Toml.decode content
        `shouldBe` Toml.Success
          [ "11:33: unexpected key: branch in dependencies.dep-branch"
          , "13:30: unexpected key: rev in dependencies.dep-rev"
          , "12:30: unexpected key: tag in dependencies.dep-tag"
          , "16:38: unexpected key: rev in dev-dependencies.dep-dev"
          , "4:1: unexpected key: name in executable[0]"
          , "1:1: unexpected key: name in <top-level>"
          ]
          expectedFpmToml

  describe "buildGraph" $ do
    let graph = buildGraph expectedFpmToml
    let graphDeps =
          [ mkGitDep "git-url" Nothing $ Set.singleton EnvProduction
          , mkGitDep "git-url" (Just "main") $ Set.singleton EnvProduction
          , mkGitDep "git-url" (Just "v0.2.1") $ Set.singleton EnvProduction
          , mkGitDep "git-url" (Just "2f5eaba") $ Set.singleton EnvProduction
          , mkGitDep "git-url-dev-dep" (Just "2f5eaba") $ Set.singleton EnvDevelopment
          , mkGitDep "git-url-exec" (Nothing) $ Set.singleton EnvProduction
          ]

    it "should not have any edges" $
      expectEdges [] graph

    it "should have deps" $ do
      expectDeps graphDeps graph
