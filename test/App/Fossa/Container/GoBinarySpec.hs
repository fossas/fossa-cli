module App.Fossa.Container.GoBinarySpec (spec) where

import App.Fossa.Container.Sources.GoBinary (goBinariesToSourceUnits, goModuleToLocator)
import Container.Types (DiscoveredGoBinary (..), DiscoveredJars (..), GoModule (..))
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (ByteString)
import Data.Map qualified as Map
import Srclib.Types (Locator (..), SourceUnit (..), SourceUnitBuild (..))
import Test.Hspec (Spec, describe, it, shouldBe)

-- Millhone output containing one Go binary, as produced for a minimal repro image.
millhoneOutput :: ByteString
millhoneOutput =
  "{\"discovered_jars\":{\"blobs/sha256/aaa\":[]},\
  \\"discovered_go_binaries\":{\"blobs/sha256/aaa\":[\
  \{\"kind\":\"v1.discover.binary.go\",\"path\":\"app\",\"go_version\":\"go1.25.6\",\
  \\"main_module\":{\"path\":\"example.com/repro-app\",\"version\":\"(devel)\"},\
  \\"modules\":[\
  \{\"path\":\"github.com/google/uuid\",\"version\":\"v1.6.0\"},\
  \{\"path\":\"golang.org/x/sys\",\"version\":\"v0.0.0-20220715151400-c0bba94af5f8\"}\
  \]}]}}"

-- Output from a millhone predating Go binary analysis.
legacyMillhoneOutput :: ByteString
legacyMillhoneOutput = "{\"discovered_jars\":{\"blobs/sha256/aaa\":[]}}"

expectedBinary :: DiscoveredGoBinary
expectedBinary =
  DiscoveredGoBinary
    { goBinaryPath = "app"
    , goBinaryGoVersion = "go1.25.6"
    , goBinaryMainModule = Just (GoModule "example.com/repro-app" "(devel)")
    , goBinaryModules =
        [ GoModule "github.com/google/uuid" "v1.6.0"
        , GoModule "golang.org/x/sys" "v0.0.0-20220715151400-c0bba94af5f8"
        ]
    }

spec :: Spec
spec = do
  describe "millhone output decoding" $ do
    it "decodes discovered go binaries" $
      (Map.elems . discoveredGoBinaries <$> eitherDecode millhoneOutput)
        `shouldBe` Right [[expectedBinary]]

    it "decodes legacy output without go binaries" $
      (discoveredGoBinaries <$> eitherDecode legacyMillhoneOutput)
        `shouldBe` Right Map.empty

  describe "goModuleToLocator" $ do
    it "renders semantic versions with the v prefix" $
      goModuleToLocator (GoModule "github.com/google/uuid" "v1.6.0")
        `shouldBe` Just (Locator "go" "github.com/google/uuid" (Just "v1.6.0"))

    it "normalizes pseudo-versions to their commit hash" $
      goModuleToLocator (GoModule "golang.org/x/sys" "v0.0.0-20220715151400-c0bba94af5f8")
        `shouldBe` Just (Locator "go" "golang.org/x/sys" (Just "c0bba94af5f8"))

    it "drops (devel) and empty versions" $ do
      goModuleToLocator (GoModule "example.com/app" "(devel)") `shouldBe` Nothing
      goModuleToLocator (GoModule "example.com/app" "") `shouldBe` Nothing

    it "strips build metadata from +incompatible versions" $
      goModuleToLocator (GoModule "example.com/legacy/big" "v2.0.0+incompatible")
        `shouldBe` Just (Locator "go" "example.com/legacy/big" (Just "v2.0.0"))

  describe "goBinariesToSourceUnits" $ do
    it "converts a discovered binary into a gobinary source unit" $ do
      let units = goBinariesToSourceUnits [expectedBinary]
      map sourceUnitType units `shouldBe` ["gobinary"]
      map sourceUnitName units `shouldBe` ["app"]
      (buildImports <$> (sourceUnitBuild =<< safeHead units))
        `shouldBe` Just
          [ Locator "go" "github.com/google/uuid" (Just "v1.6.0")
          , Locator "go" "golang.org/x/sys" (Just "c0bba94af5f8")
          ]

    it "emits no unit when no usable versions exist" $ do
      let develOnly = expectedBinary{goBinaryModules = [], goBinaryMainModule = Just (GoModule "example.com/app" "(devel)")}
      goBinariesToSourceUnits [develOnly] `shouldBe` []

    it "includes a main module carrying a real version (go install case)" $ do
      let installed =
            expectedBinary
              { goBinaryMainModule = Just (GoModule "example.com/tool" "v1.2.3")
              , goBinaryModules = [GoModule "github.com/google/uuid" "v1.6.0"]
              }
          units = goBinariesToSourceUnits [installed]
      (buildImports <$> (sourceUnitBuild =<< safeHead units))
        `shouldBe` Just
          [ Locator "go" "github.com/google/uuid" (Just "v1.6.0")
          , Locator "go" "example.com/tool" (Just "v1.2.3")
          ]

    it "deduplicates a module repeated across dep list and main module" $ do
      let dup = GoModule "github.com/google/uuid" "v1.6.0"
          binary = expectedBinary{goBinaryMainModule = Just dup, goBinaryModules = [dup, dup]}
          units = goBinariesToSourceUnits [binary]
      (buildImports <$> (sourceUnitBuild =<< safeHead units))
        `shouldBe` Just [Locator "go" "github.com/google/uuid" (Just "v1.6.0")]
  where
    safeHead :: [a] -> Maybe a
    safeHead (x : _) = Just x
    safeHead _ = Nothing
