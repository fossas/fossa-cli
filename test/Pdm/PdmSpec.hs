{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Pdm.PdmSpec (
  spec,
) where

import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import DepTypes (
  DepEnvironment (..),
  DepType (GitType, PipType, URLType, UnresolvedPathType),
  Dependency (..),
  VerConstraint (..),
 )
import GraphUtil (expectDeps', expectDirect', expectEdges')
import Path (mkRelFile, (</>))
import Path.IO (getCurrentDir)
import Strategy.Python.PDM.Pdm (analyze)
import Test.Effect (it')
import Test.Hspec (
  Spec,
  describe,
  runIO,
 )

mkPipProdDep :: Text -> Dependency
mkPipProdDep nameAtVersion = mkDep PipType nameAtVersion (Just EnvProduction)

mkPipDevDep :: Text -> Dependency
mkPipDevDep nameAtVersion = mkDep PipType nameAtVersion (Just EnvDevelopment)

mkDep :: DepType -> Text -> Maybe DepEnvironment -> Dependency
mkDep dt nameAtVersion env = do
  let nameAndVersionSplit = Text.splitOn "@" nameAtVersion
      name = head nameAndVersionSplit
      version = last nameAndVersionSplit
  Dependency
    dt
    name
    (CEq <$> (Just version))
    mempty
    (maybe mempty Set.singleton env)
    mempty

spec :: Spec
spec = do
  currentDir <- runIO getCurrentDir
  let basicLockFile = currentDir </> $(mkRelFile "test/Pdm/testdata/basic/pdm.lock")
  let basicPyprojectFile = currentDir </> $(mkRelFile "test/Pdm/testdata/basic/pyproject.toml")
  let woLockfilePyprojectFile = currentDir </> $(mkRelFile "test/Pdm/testdata/basic/pyproject.toml")

  describe "basic graph" $ do
    it' "should graph accurately, when lockfile is NOT present" $ do
      let deps = [pipReq, pytestReq, requests, blackReq, spacyModelReq, flake8Req]

      graph <- analyze woLockfilePyprojectFile Nothing
      expectDeps' deps graph
      expectDirect' deps graph
      expectEdges' [] graph

    it' "should graph accurately, when lockfile is present" $ do
      --
      -- \$ pdm list --graph
      -- -
      -- > pip 22.0 [ required: Any ]
      -- > pytest 7.3.1 [ required: >=7.3.1 ]
      -- > ├── iniconfig 2.0.0 [ required: Any ]
      -- > ├── packaging 23.1 [ required: Any ]
      -- > └── pluggy 1.0.0 [ required: <2.0,>=0.12 ]
      -- > requests 2.25.1 [ required: ==2.25.1 ]
      -- > ├── certifi 2023.5.7 [ required: >=2017.4.17 ]
      -- > ├── greenlet 3.1.1 [ required: !=0.4.17 ]
      -- > ├── chardet 4.0.0 [ required: <5,>=3.0.2 ]
      -- > ├── idna 2.10 [ required: <3,>=2.5 ]
      -- > └── urllib3 1.26.16 [ required: <1.27,>=1.21.1 ]
      -- -
      -- NOTE:
      --
      --  `pip list --graph` command is platform dependent. In
      --   this example, colorama dependency is not shown in the graph,
      --   as it has [sys_platform == "win32"] requirement, and cmd
      --   was executed in macOs. However, for compliance analysis
      --   we want to be conservative, and we should include this in
      --   our analysis.
      graph <- analyze basicPyprojectFile (Just basicLockFile)
      expectDirect' [pip, pytest, requests] graph
      expectDeps'
        [ pip
        , pytest
        , colorama
        , iniconfig
        , packaging
        , pluggy
        , greenlet
        , requests
        , certifi
        , chardet
        , idna
        , urllib3
        ]
        graph
      expectEdges'
        [ (pytest, colorama)
        , (pytest, iniconfig)
        , (pytest, packaging)
        , (pytest, pluggy)
        , (requests, chardet)
        , (requests, greenlet)
        , (requests, certifi)
        , (requests, idna)
        , (requests, urllib3)
        ]
        graph

pip :: Dependency
pip =
  mkDep
    GitType
    "https://github.com/pypa/pip.git@1742af7bdc0b4a883a35ad69da6dcaefe0f21978"
    (Just EnvProduction)

pipReq :: Dependency
pipReq = Dependency URLType "git+https://github.com/pypa/pip.git%4022.0" Nothing mempty (Set.singleton EnvProduction) mempty

pytest :: Dependency
pytest = mkPipDevDep "pytest@7.3.1"

pytestReq :: Dependency
pytestReq = Dependency PipType "pytest" (Just $ CGreaterOrEq "7.3.1") mempty (Set.singleton EnvDevelopment) mempty

blackReq :: Dependency
blackReq = Dependency PipType "black" Nothing mempty (Set.singleton EnvDevelopment) mempty

spacyModelReq :: Dependency
spacyModelReq =
  Dependency
    URLType
    "https://github.com/explosion/spacy-models/releases/download/en_core_web_trf-3.5.0/en_core_web_trf-3.5.0-py3-none-any.whl"
    Nothing
    mempty
    (Set.singleton EnvProduction)
    mempty

flake8Req :: Dependency
flake8Req =
  Dependency
    UnresolvedPathType
    "flake8"
    Nothing
    mempty
    (Set.singleton EnvProduction)
    mempty

iniconfig :: Dependency
iniconfig = mkPipDevDep "iniconfig@2.0.0"

packaging :: Dependency
packaging = mkPipDevDep "packaging@23.1"

pluggy :: Dependency
pluggy = mkPipDevDep "pluggy@1.0.0"

requests :: Dependency
requests = mkPipProdDep "requests@2.25.1"

certifi :: Dependency
certifi = mkPipProdDep "certifi@2023.5.7"

chardet :: Dependency
chardet = mkPipProdDep "chardet@4.0.0"

idna :: Dependency
idna = mkPipProdDep "idna@2.10"

urllib3 :: Dependency
urllib3 = mkPipProdDep "urllib3@1.26.16"

colorama :: Dependency
colorama = mkPipDevDep "colorama@0.4.6"

greenlet :: Dependency
greenlet = mkPipDevDep "greenlet@3.1.1"
