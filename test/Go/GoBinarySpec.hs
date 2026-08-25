{-# LANGUAGE TemplateHaskell #-}

module Go.GoBinarySpec (spec) where

import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import DepTypes (
  DepType (GoType),
  Dependency (..),
  VerConstraint (CEq),
 )
import Path (mkAbsDir, mkAbsFile)
import Strategy.Go.GoBinary (
  DiscoveredGoBinary (..),
  GoBinaryProject (..),
  GoModule (..),
  goBinaryDependencies,
  goModuleToDependency,
  toProjects,
 )
import Test.Hspec (Spec, describe, it, shouldBe)

-- Output from @millhone analyze-go-binaries@ for a single Go binary.
millhoneOutput :: ByteString
millhoneOutput =
  "[{\"kind\":\"v1.discover.binary.go\",\"path\":\"/src/jni/arm64-v8a/libgojni.so\",\
  \\"go_version\":\"go1.25.6\",\
  \\"main_module\":{\"path\":\"example.com/sdk\",\"version\":\"(devel)\"},\
  \\"modules\":[\
  \{\"path\":\"github.com/google/uuid\",\"version\":\"v1.6.0\"},\
  \{\"path\":\"golang.org/x/sys\",\"version\":\"v0.0.0-20220715151400-c0bba94af5f8\"}\
  \]}]"

expectedBinary :: DiscoveredGoBinary
expectedBinary =
  DiscoveredGoBinary
    { goBinaryPath = "/src/jni/arm64-v8a/libgojni.so"
    , goBinaryGoVersion = "go1.25.6"
    , goBinaryMainModule = Just (GoModule "example.com/sdk" "(devel)")
    , goBinaryModules =
        [ GoModule "github.com/google/uuid" "v1.6.0"
        , GoModule "golang.org/x/sys" "v0.0.0-20220715151400-c0bba94af5f8"
        ]
    }

mkDep :: Text -> Text -> Dependency
mkDep name version =
  Dependency
    { dependencyType = GoType
    , dependencyName = name
    , dependencyVersion = Just $ CEq version
    , dependencyLocations = []
    , dependencyEnvironments = mempty
    , dependencyTags = mempty
    }

spec :: Spec
spec = do
  describe "millhone analyze-go-binaries output decoding" $
    it "decodes discovered go binaries" $
      eitherDecode millhoneOutput `shouldBe` Right [expectedBinary]

  describe "goModuleToDependency" $ do
    it "renders semantic versions with the v prefix" $
      goModuleToDependency (GoModule "github.com/google/uuid" "v1.6.0")
        `shouldBe` Just (mkDep "github.com/google/uuid" "v1.6.0")

    it "normalizes pseudo-versions to their commit hash" $
      goModuleToDependency (GoModule "golang.org/x/sys" "v0.0.0-20220715151400-c0bba94af5f8")
        `shouldBe` Just (mkDep "golang.org/x/sys" "c0bba94af5f8")

    it "drops (devel) and empty versions" $ do
      goModuleToDependency (GoModule "example.com/sdk" "(devel)") `shouldBe` Nothing
      goModuleToDependency (GoModule "example.com/sdk" "") `shouldBe` Nothing

  describe "goBinaryDependencies" $ do
    it "reports every dependency module, dropping an unversioned main module" $
      goBinaryDependencies expectedBinary
        `shouldBe` [ mkDep "github.com/google/uuid" "v1.6.0"
                   , mkDep "golang.org/x/sys" "c0bba94af5f8"
                   ]

    it "keeps a main module that carries a real version" $
      -- Binaries built via `go install module@version` record one.
      goBinaryDependencies
        expectedBinary{goBinaryMainModule = Just (GoModule "example.com/sdk" "v1.2.3")}
        `shouldBe` [ mkDep "github.com/google/uuid" "v1.6.0"
                   , mkDep "golang.org/x/sys" "c0bba94af5f8"
                   , mkDep "example.com/sdk" "v1.2.3"
                   ]

    it "reports nothing when no module carries a usable version" $
      goBinaryDependencies
        expectedBinary
          { goBinaryModules = []
          , goBinaryMainModule = Just (GoModule "example.com/sdk" "(devel)")
          }
        `shouldBe` []

  describe "toProjects" $ do
    -- Source units are named after their directory, so one project per binary
    -- would emit colliding units for a directory holding several Go binaries.
    it "groups binaries in one directory into a single project" $ do
      let toolA = $(mkAbsFile "/src/tools/toolA")
          toolB = $(mkAbsFile "/src/tools/toolB")
          binA = expectedBinary{goBinaryPath = "/src/tools/toolA"}
          binB =
            expectedBinary
              { goBinaryPath = "/src/tools/toolB"
              , goBinaryModules = [GoModule "github.com/urfave/cli/v3" "v3.3.3"]
              }
      toProjects [toolA, toolB] [binA, binB]
        `shouldBe` [ GoBinaryProject
                      { goBinaryProjectDir = $(mkAbsDir "/src/tools/")
                      , goBinaryProjectFiles = [toolA, toolB]
                      , goBinaryProjectDeps =
                          [ mkDep "github.com/google/uuid" "v1.6.0"
                          , mkDep "golang.org/x/sys" "c0bba94af5f8"
                          , mkDep "github.com/urfave/cli/v3" "v3.3.3"
                          ]
                      }
                   ]

    it "keeps binaries in different directories as separate projects" $ do
      let toolA = $(mkAbsFile "/src/tools/toolA")
          nested = $(mkAbsFile "/src/jni/libgojni.so")
          binA = expectedBinary{goBinaryPath = "/src/tools/toolA"}
          binNested = expectedBinary{goBinaryPath = "/src/jni/libgojni.so"}
      map goBinaryProjectDir (toProjects [toolA, nested] [binA, binNested])
        `shouldBe` [$(mkAbsDir "/src/jni/"), $(mkAbsDir "/src/tools/")]

    it "drops binaries with no usable dependency and paths millhone was not given" $ do
      let toolA = $(mkAbsFile "/src/tools/toolA")
          devel = expectedBinary{goBinaryPath = "/src/tools/toolA", goBinaryModules = [], goBinaryMainModule = Just (GoModule "example.com/sdk" "(devel)")}
          unknown = expectedBinary{goBinaryPath = "/somewhere/else"}
      toProjects [toolA] [devel] `shouldBe` []
      toProjects [toolA] [unknown] `shouldBe` []
