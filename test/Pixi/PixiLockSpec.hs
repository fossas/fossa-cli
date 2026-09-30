{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Pixi.PixiLockSpec (
  spec,
) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import DepTypes (
  DepEnvironment (EnvDevelopment, EnvOther, EnvProduction, EnvTesting),
  DepType (CondaType, GitType, PipType),
  Dependency (..),
  VerConstraint (CEq),
 )
import GraphUtil (expectDeps')
import Path (relfile)
import Path.IO (makeAbsolute)
import Strategy.Conda.CondaEnvCreate (CondaEnvDep (..), parseCondaEnvDep)
import Strategy.Conda.Naming (condaDependencyName)
import Strategy.Pixi.PixiLock (
  CondaArtifact (..),
  PypiSource (PypiGit, PypiLocalPath, PypiRegistry),
  analyze,
  classifyPypiSource,
  environmentToDepEnvironment,
  parseCondaArtifactUrl,
 )
import Test.Effect (expectFatal', it')
import Test.Hspec
import Text.Megaparsec (parse)

mkDep :: DepType -> Text -> Maybe Text -> [DepEnvironment] -> Dependency
mkDep depType name version envs =
  Dependency
    { dependencyType = depType
    , dependencyName = name
    , dependencyVersion = CEq <$> version
    , dependencyLocations = []
    , dependencyEnvironments = Set.fromList envs
    , dependencyTags = Map.empty
    }

condaDep :: Text -> Text -> Text -> Text -> [DepEnvironment] -> Dependency
condaDep channel platform name version =
  mkDep CondaType ("'" <> channel <> "':" <> platform <> ":" <> name) (Just version)

spec :: Spec
spec = do
  describe "parseCondaArtifactUrl" $ do
    it "reads channel, platform, name and version out of the filename" $
      parseCondaArtifactUrl "https://conda.anaconda.org/conda-forge/linux-64/zlib-1.3.1-hb9d3cd8_2.conda"
        `shouldBe` Just (CondaArtifact "conda-forge" "linux-64" "zlib" "1.3.1")

    it "keeps hyphens that belong to the package name" $
      -- Only the last two hyphen-separated fields are version and build, so a
      -- name like ld_impl_linux-64 has to survive the split intact.
      parseCondaArtifactUrl "https://conda.anaconda.org/conda-forge/linux-64/ld_impl_linux-64-2.40-hf3520f5_7.conda"
        `shouldBe` Just (CondaArtifact "conda-forge" "linux-64" "ld_impl_linux-64" "2.40")

    it "handles the .tar.bz2 artifact extension" $
      parseCondaArtifactUrl "https://conda.anaconda.org/conda-forge/linux-64/_openmp_mutex-4.5-2_gnu.tar.bz2"
        `shouldBe` Just (CondaArtifact "conda-forge" "linux-64" "_openmp_mutex" "4.5")

    it "reads the channel from a self-hosted mirror" $
      parseCondaArtifactUrl "https://prefix.dev/conda-forge/osx-arm64/libffi-3.4.2-h3422bc3_5.conda"
        `shouldBe` Just (CondaArtifact "conda-forge" "osx-arm64" "libffi" "3.4.2")

    it "keeps every channel segment of a LABELED channel" $
      -- conda-forge/label/broken is the channel; `broken` alone is a
      -- different channel that mostly does not exist.
      parseCondaArtifactUrl "https://conda.anaconda.org/conda-forge/label/broken/linux-64/zlib-1.3.1-hb9d3cd8_2.conda"
        `shouldBe` Just (CondaArtifact "conda-forge/label/broken" "linux-64" "zlib" "1.3.1")

    it "returns Nothing rather than a garbage name for an unparseable URL" $ do
      parseCondaArtifactUrl "https://example.com/nonsense" `shouldBe` Nothing
      parseCondaArtifactUrl "" `shouldBe` Nothing

  describe "pixi and conda agree on conda dependency names" $
    -- Both strategies emit CondaType deps. If they name the same package
    -- differently it lands in FOSSA twice, so the agreement is pinned here
    -- rather than left to two parsers drifting apart.
    it "produce the same dep name for the same package, labeled channels included" $ do
      let viaPixi url = do
            CondaArtifact{..} <- parseCondaArtifactUrl url
            pure $ condaDependencyName condaArtifactChannel condaArtifactPlatform condaArtifactName
          viaConda spec' = case parse parseCondaEnvDep "" spec' of
            Left _ -> Nothing
            Right CondaEnvDep{..} -> Just $ condaDependencyName channel platform name

      viaPixi "https://conda.anaconda.org/conda-forge/linux-64/zlib-1.3.1-hb9d3cd8_2.conda"
        `shouldBe` viaConda "conda-forge/linux-64::zlib==1.3.1=hb9d3cd8_2"
      viaPixi "https://conda.anaconda.org/conda-forge/label/broken/linux-64/zlib-1.3.1-hb9d3cd8_2.conda"
        `shouldBe` viaConda "conda-forge/label/broken/linux-64::zlib==1.3.1=hb9d3cd8_2"
      -- ...and that shared name is the one FOSSA resolves against.
      viaPixi "https://conda.anaconda.org/conda-forge/label/broken/linux-64/zlib-1.3.1-hb9d3cd8_2.conda"
        `shouldBe` Just "'conda-forge/label/broken':linux-64:zlib"

  describe "classifyPypiSource" $
    it "distinguishes registry, git and local sources" $ do
      classifyPypiSource "https://files.pythonhosted.org/packages/ab/cd/absl_py-2.1.0-py3-none-any.whl"
        `shouldBe` PypiRegistry
      classifyPypiSource "git+https://github.com/squidfunk/mike#2d4ad79"
        `shouldBe` PypiGit "https://github.com/squidfunk/mike#2d4ad79"
      -- pixi writes a bare `.` or `./` for the workspace package itself.
      classifyPypiSource "." `shouldBe` PypiLocalPath
      classifyPypiSource "./" `shouldBe` PypiLocalPath
      classifyPypiSource "file:///home/me/pkg" `shouldBe` PypiLocalPath

  describe "environmentToDepEnvironment" $
    it "maps default to production and keeps arbitrary names filterable" $ do
      environmentToDepEnvironment "default" `shouldBe` EnvProduction
      environmentToDepEnvironment "dev" `shouldBe` EnvDevelopment
      environmentToDepEnvironment "test" `shouldBe` EnvTesting
      environmentToDepEnvironment "lint" `shouldBe` EnvOther "lint"

  describe "analyze a v6 lockfile" $
    it' "reports conda and pypi packages, deduped across platforms and environments" $ do
      path <- makeAbsolute [relfile|test/Pixi/testdata/pixi-v6.lock|]
      graph <- analyze path

      expectDeps'
        [ -- Present under default/linux-64 and dev/linux-64: one dependency
          -- carrying both environments, not two dependencies.
          condaDep "conda-forge" "linux-64" "_openmp_mutex" "4.5" [EnvProduction, EnvDevelopment]
        , -- Present under default/linux-64 and lint/linux-64.
          condaDep "conda-forge" "linux-64" "ld_impl_linux-64" "2.40" [EnvProduction, EnvOther "lint"]
        , -- noarch, listed under both platforms of the default environment:
          -- the URL subdir makes it one dependency rather than two.
          condaDep "conda-forge" "noarch" "tzdata" "2024a" [EnvProduction]
        , condaDep "conda-forge" "osx-arm64" "libffi" "3.4.2" [EnvProduction]
        , -- The same wheel under two platforms collapses to one dep.
          mkDep PipType "absl-py" (Just "2.1.0") [EnvProduction]
        , mkDep GitType "https://github.com/laurentS/slowapi.git" (Just "a72bcc66597f620f04bf5be3676e40ed308d3a6a") [EnvDevelopment]
        ]
        graph

  describe "analyze a v5 lockfile" $
    it' "uses the explicit name and version fields v5 provides" $ do
      path <- makeAbsolute [relfile|test/Pixi/testdata/pixi-v5.lock|]
      graph <- analyze path

      expectDeps'
        [ condaDep "conda-forge" "linux-64" "_libgcc_mutex" "0.1" [EnvProduction]
        , condaDep "conda-forge" "linux-64" "binaryen" "118" [EnvProduction]
        ]
        graph

  describe "analyze a v7 lockfile" $
    it' "reads v7, whose extra top-level platforms block carries no dependencies" $ do
      path <- makeAbsolute [relfile|test/Pixi/testdata/pixi-v7.lock|]
      graph <- analyze path

      expectDeps'
        [ condaDep "conda-forge" "linux-64" "_openmp_mutex" "4.5" [EnvProduction]
        , condaDep "conda-forge" "linux-64" "actionlint" "1.7.12" [EnvProduction]
        , mkDep GitType "https://github.com/squidfunk/mike" (Just "2d4ad799442f4592db8ad53b179bfb33db8c69ac") [EnvProduction]
        ]
        graph

  describe "analyze a v5 lockfile with a local path package" $
    it' "skips only the path-sourced package and keeps every other one" $ do
      -- Trimmed from the Rattler v5 conda-lock fixture. The `requests` entry
      -- has `path:` and no `url:`; requiring `url:` failed the whole document
      -- with `key "url" not found`, so the project reported nothing at all.
      path <- makeAbsolute [relfile|test/Pixi/testdata/pixi-v5-local-path.lock|]
      graph <- analyze path

      expectDeps'
        [ condaDep "conda-forge" "osx-arm64" "bzip2" "1.0.8" [EnvProduction]
        , condaDep "conda-forge" "noarch" "tzdata" "2024a" [EnvProduction]
        , mkDep PipType "certifi" (Just "2024.2.2") [EnvProduction]
        , mkDep PipType "idna" (Just "3.7") [EnvProduction]
        ]
        graph

  describe "one bad entry never empties the graph" $
    it' "degrades to warnings and still reports the packages it could read" $ do
      -- The invariant the whole strategy rests on: reporting zero
      -- dependencies for a project that has them is indistinguishable from
      -- success, so no single entry may ever cost more than itself. Here an
      -- entry keyed on an unknown ecosystem, a conda URL with no parseable
      -- filename, and a nameless pypi entry all coexist with one good package.
      path <- makeAbsolute [relfile|test/Pixi/testdata/pixi-v6-odd-entries.lock|]
      graph <- analyze path

      expectDeps' [condaDep "conda-forge" "linux-64" "zlib" "1.3.1" [EnvProduction]] graph

  describe "analyze an unsupported lock version" $
    it' "fails loudly instead of reporting an empty graph" $ do
      -- Reporting zero dependencies for a project that plainly has them is the
      -- bug this strategy exists to fix, and it looks exactly like success.
      path <- makeAbsolute [relfile|test/Pixi/testdata/pixi-unsupported.lock|]
      expectFatal' $ analyze path
