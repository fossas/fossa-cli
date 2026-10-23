{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Pixi.PixiLockSpec (
  spec,
) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Text qualified as Text
import DepTypes (
  DepEnvironment (EnvDevelopment, EnvOther, EnvProduction, EnvTesting),
  DepType (CondaType, GitType, PipType, UnresolvedPathType),
  Dependency (..),
  VerConstraint (CEq),
 )
import Effect.ReadFS (readContentsToml)
import GraphUtil (expectDeps', expectDirect', expectEdge')
import Graphing qualified
import Path (Dir, Path, Rel, parent, reldir, relfile)
import Path.IO (makeAbsolute)
import Strategy.Conda.CondaEnvCreate (CondaEnvDep (..), parseCondaEnvDep)
import Strategy.Conda.Naming (condaDependencyName)
import Strategy.Pixi (PixiManifest (PixiToml), PixiProject (..), findProjects, getDeps)
import Strategy.Pixi.Manifest (PixiManifestDeps (PixiManifestDeps), PyprojectPixiDeps (PyprojectPixiDeps))
import Strategy.Pixi.PixiLock (
  CondaArtifact (..),
  PypiSource (PypiGit, PypiLocalPath, PypiRegistry),
  analyze,
  classifyPypiSource,
  environmentToDepEnvironment,
  outOfRangeVersionWarning,
  parseCondaArtifactUrl,
 )
import System.FilePath ((</>))
import Test.Effect (EffectStack, expectFatal', expectationFailure', it', shouldBe')
import Test.Hspec
import Text.Megaparsec (parse)
import Types (DependencyResults (dependencyGraph))

warns :: Text -> Maybe Text -> Bool
warns needle = maybe False (Text.isInfixOf needle)

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

withFixture :: Path Rel Dir -> (PixiProject -> EffectStack ()) -> EffectStack ()
withFixture dir act = do
  projects <- findProjects =<< makeAbsolute dir
  case projects of
    [project] -> act project
    _ -> expectationFailure' $ "expected one pixi project, got " <> show (length projects)

spec :: Spec
spec = do
  describe "parseCondaArtifactUrl" $ do
    it "reads channel, platform, name, and version from the URL" $
      parseCondaArtifactUrl "https://conda.anaconda.org/conda-forge/linux-64/zlib-1.3.1-hb9d3cd8_2.conda"
        `shouldBe` Just (CondaArtifact "conda-forge" "linux-64" "zlib" "1.3.1")

    it "keeps hyphens that belong to the package name" $
      parseCondaArtifactUrl "https://conda.anaconda.org/conda-forge/linux-64/ld_impl_linux-64-2.40-hf3520f5_7.conda"
        `shouldBe` Just (CondaArtifact "conda-forge" "linux-64" "ld_impl_linux-64" "2.40")

    it "handles the .tar.bz2 artifact extension" $
      parseCondaArtifactUrl "https://conda.anaconda.org/conda-forge/linux-64/_openmp_mutex-4.5-2_gnu.tar.bz2"
        `shouldBe` Just (CondaArtifact "conda-forge" "linux-64" "_openmp_mutex" "4.5")

    it "reads noarch packages" $
      parseCondaArtifactUrl "https://conda.anaconda.org/conda-forge/noarch/tzdata-2025b-h78e105d_0.conda"
        `shouldBe` Just (CondaArtifact "conda-forge" "noarch" "tzdata" "2025b")

    it "reads a channel in an S3 bucket" $
      parseCondaArtifactUrl "s3://my-s3-bucket/my-channel/linux-64/ld_impl_linux-64-2.40-hf3520f5_7.conda"
        `shouldBe` Just (CondaArtifact "my-channel" "linux-64" "ld_impl_linux-64" "2.40")

    it "reads the channel from a self-hosted mirror" $
      parseCondaArtifactUrl "https://prefix.dev/conda-forge/osx-arm64/libffi-3.4.2-h3422bc3_5.conda"
        `shouldBe` Just (CondaArtifact "conda-forge" "osx-arm64" "libffi" "3.4.2")

    it "keeps all segments of a labeled channel" $
      parseCondaArtifactUrl "https://conda.anaconda.org/conda-forge/label/broken/linux-64/zlib-1.3.1-hb9d3cd8_2.conda"
        `shouldBe` Just (CondaArtifact "conda-forge/label/broken" "linux-64" "zlib" "1.3.1")

    it "returns Nothing for an unparseable URL" $ do
      parseCondaArtifactUrl "https://example.com/nonsense" `shouldBe` Nothing
      parseCondaArtifactUrl "" `shouldBe` Nothing

  describe "conda dependency naming" $
    it "matches the conda strategy, including labeled channels" $ do
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
      viaPixi "https://conda.anaconda.org/conda-forge/label/broken/linux-64/zlib-1.3.1-hb9d3cd8_2.conda"
        `shouldBe` Just "'conda-forge/label/broken':linux-64:zlib"

  describe "classifyPypiSource" $
    it "distinguishes registry, git, and local sources" $ do
      classifyPypiSource "https://files.pythonhosted.org/packages/ab/cd/absl_py-2.1.0-py3-none-any.whl"
        `shouldBe` PypiRegistry
      classifyPypiSource "git+https://github.com/squidfunk/mike#2d4ad79"
        `shouldBe` PypiGit "https://github.com/squidfunk/mike#2d4ad79"
      -- pixi uses `.` or `./` for the workspace package.
      classifyPypiSource "." `shouldBe` PypiLocalPath "."
      classifyPypiSource "./" `shouldBe` PypiLocalPath "./"
      classifyPypiSource "file:///home/me/pkg" `shouldBe` PypiLocalPath "/home/me/pkg"
      classifyPypiSource "s3://bucket/simple/widget-1.0.0-py3-none-any.whl" `shouldBe` PypiRegistry

  describe "environmentToDepEnvironment" $
    it "maps conventional environments and preserves other names" $ do
      environmentToDepEnvironment "default" `shouldBe` EnvProduction
      environmentToDepEnvironment "dev" `shouldBe` EnvDevelopment
      environmentToDepEnvironment "test" `shouldBe` EnvTesting
      environmentToDepEnvironment "lint" `shouldBe` EnvOther "lint"

  describe "analyze a v6 lockfile" $
    it' "deduplicates packages across platforms and environments" $ do
      path <- makeAbsolute [relfile|test/Pixi/testdata/pixi-v6.lock|]
      graph <- analyze mempty path

      expectDeps'
        [ -- Shared by default and dev.
          condaDep "conda-forge" "linux-64" "_openmp_mutex" "4.5" [EnvProduction, EnvDevelopment]
        , -- Present under default/linux-64 and lint/linux-64.
          condaDep "conda-forge" "linux-64" "ld_impl_linux-64" "2.40" [EnvProduction, EnvOther "lint"]
        , -- Shared by both platforms.
          condaDep "conda-forge" "noarch" "tzdata" "2024a" [EnvProduction]
        , condaDep "conda-forge" "osx-arm64" "libffi" "3.4.2" [EnvProduction]
        , -- The same wheel on both platforms.
          mkDep PipType "absl-py" (Just "2.1.0") [EnvProduction]
        , mkDep GitType "https://github.com/laurentS/slowapi.git" (Just "a72bcc66597f620f04bf5be3676e40ed308d3a6a") [EnvDevelopment]
        ]
        graph

      -- Dropping the workspace package makes its requirement, absl-py, direct.
      expectDirect'
        [ condaDep "conda-forge" "linux-64" "_openmp_mutex" "4.5" [EnvProduction, EnvDevelopment]
        , condaDep "conda-forge" "linux-64" "ld_impl_linux-64" "2.40" [EnvProduction, EnvOther "lint"]
        , condaDep "conda-forge" "noarch" "tzdata" "2024a" [EnvProduction]
        , condaDep "conda-forge" "osx-arm64" "libffi" "3.4.2" [EnvProduction]
        , mkDep PipType "absl-py" (Just "2.1.0") [EnvProduction]
        , mkDep GitType "https://github.com/laurentS/slowapi.git" (Just "a72bcc66597f620f04bf5be3676e40ed308d3a6a") [EnvDevelopment]
        ]
        graph

  describe "analyze a v5 lockfile" $
    it' "uses the explicit name and version fields v5 provides" $ do
      path <- makeAbsolute [relfile|test/Pixi/testdata/pixi-v5.lock|]
      graph <- analyze mempty path

      expectDeps'
        [ condaDep "conda-forge" "linux-64" "_libgcc_mutex" "0.1" [EnvProduction]
        , condaDep "conda-forge" "linux-64" "binaryen" "118" [EnvProduction]
        ]
        graph

  describe "analyze a v7 lockfile" $
    it' "ignores platforms metadata and skips the workspace's own source package" $ do
      path <- makeAbsolute [relfile|test/Pixi/testdata/pixi-v7.lock|]
      graph <- analyze mempty path
      let deps =
            [ condaDep "conda-forge" "linux-64" "_openmp_mutex" "4.5" [EnvProduction]
            , -- The workspace's package depends on it.
              condaDep "conda-forge" "linux-64" "actionlint" "1.7.12" [EnvProduction]
            , mkDep GitType "https://github.com/squidfunk/mike" (Just "2d4ad799442f4592db8ad53b179bfb33db8c69ac") [EnvProduction]
            ]

      expectDeps' deps graph
      expectDirect' deps graph

  describe "analyze a v5 lockfile with a local path package" $
    it' "reports the local wheel as a path dependency" $ do
      path <- makeAbsolute [relfile|test/Pixi/testdata/pixi-v5-local-path.lock|]
      graph <- analyze mempty path

      expectDeps'
        [ condaDep "conda-forge" "osx-arm64" "bzip2" "1.0.8" [EnvProduction]
        , condaDep "conda-forge" "noarch" "tzdata" "2024a" [EnvProduction]
        , mkDep PipType "certifi" (Just "2024.2.2") [EnvProduction]
        , mkDep PipType "idna" (Just "3.7") [EnvProduction]
        , mkDep UnresolvedPathType "./links/requests-2.31.0-py3-none-any.whl" (Just "2.31.0") [EnvProduction]
        ]
        graph

  describe "analyze a lockfile with unreadable entries" $
    it' "reports the readable package" $ do
      path <- makeAbsolute [relfile|test/Pixi/testdata/pixi-v6-odd-entries.lock|]
      graph <- analyze mempty path

      expectDeps' [condaDep "conda-forge" "linux-64" "zlib" "1.3.1" [EnvProduction]] graph

  describe "analyze local path packages" $ do
    it' "reports conda and PyPI paths with their environments" $ do
      path <- makeAbsolute [relfile|test/Pixi/testdata/pixi-v6-path-deps.lock|]
      graph <- analyze mempty path

      expectDeps'
        [ condaDep "conda-forge" "linux-64" "zlib" "1.3.1" [EnvProduction]
        , -- A relative conda path.
          mkDep UnresolvedPathType "./vendor/openssl-3.2.1-hd590300_0.conda" Nothing [EnvProduction]
        , -- A path with enough segments to resemble an artifact URL.
          mkDep UnresolvedPathType "../shared/pkgs/openblas-0.3.26-pthreads_h413a1c8_0.conda" Nothing [EnvProduction]
        , -- file:// and the bare path are one package in two environments.
          mkDep UnresolvedPathType "/home/me/pkgs/widget" (Just "1.0.0") [EnvProduction, EnvDevelopment]
        , mkDep UnresolvedPathType "./local/gadget" (Just "0.2.0") [EnvDevelopment]
        ]
        graph

  describe "edges from depends and requires_dist" $ do
    let conda name version = condaDep "conda-forge" "linux-64" name version [EnvProduction]
        pypi name version = mkDep PipType name (Just version) [EnvProduction]
        edges' = makeAbsolute [relfile|test/Pixi/testdata/pixi-v6-edges.lock|] >>= analyze mempty

    it' "links conda packages within a subdir and through noarch" $ do
      graph <- edges'
      expectEdge' graph (conda "app" "1.0") (conda "libfoo" "1.2")
      -- Platform packages can depend on noarch packages.
      expectEdge' graph (conda "app" "1.0") (condaDep "conda-forge" "noarch" "tzdata" "2024a" [EnvProduction])
      -- noarch packages can also depend on platform packages.
      expectEdge' graph (condaDep "conda-forge" "noarch" "tzdata" "2024a" [EnvProduction]) (conda "libfoo" "1.2")

    it' "marks roots and all members of root cycles as direct" $ do
      graph <- edges'
      expectDirect'
        [ conda "app" "1.0"
        , -- Nothing reaches it: app's `libonlyosx` names another subdir.
          condaDep "conda-forge" "osx-arm64" "libonlyosx" "1.0" [EnvProduction]
        , -- A path dependency is a root like any other node.
          mkDep UnresolvedPathType "./libs/gadget" (Just "0.2.0") [EnvProduction]
        , -- Both members of an unreached cycle are direct.
          conda "cyc-a" "1.0"
        , conda "cyc-b" "1.0"
        ]
        graph

    it' "keeps edges from path dependencies" $ do
      graph <- edges'
      expectEdge' graph (mkDep UnresolvedPathType "./libs/gadget" (Just "0.2.0") [EnvProduction]) (pypi "widget" "1.0.0")

    it' "normalizes PyPI names and ignores environment markers" $ do
      graph <- edges'
      -- Requirement names use dots and underscores; locked names use hyphens.
      expectEdge' graph (pypi "widget" "1.0.0") (pypi "zope-interface" "6.0")
      expectEdge' graph (pypi "widget" "1.0.0") (pypi "charset-normalizer" "3.3.2")
      expectEdge' graph (pypi "widget" "1.0.0") (pypi "guarded" "0.1.0")

    it' "preserves both edges of a dependency cycle" $ do
      graph <- edges'
      expectEdge' graph (conda "cyc-a" "1.0") (conda "cyc-b" "1.0")
      expectEdge' graph (conda "cyc-b" "1.0") (conda "cyc-a" "1.0")

    it' "keeps locked packages when requirements are absent or on another platform" $ do
      graph <- edges'
      expectDeps'
        [ conda "app" "1.0"
        , conda "libfoo" "1.2"
        , condaDep "conda-forge" "noarch" "tzdata" "2024a" [EnvProduction]
        , conda "cyc-a" "1.0"
        , conda "cyc-b" "1.0"
        , condaDep "conda-forge" "osx-arm64" "libonlyosx" "1.0" [EnvProduction]
        , mkDep UnresolvedPathType "./libs/gadget" (Just "0.2.0") [EnvProduction]
        , pypi "widget" "1.0.0"
        , pypi "zope-interface" "6.0"
        , pypi "charset-normalizer" "3.3.2"
        , pypi "guarded" "0.1.0"
        ]
        graph

  describe "analyze an unknown lock version" $
    it' "reports packages in a recognized format" $ do
      -- This fixture uses the v7 shape with version 8.
      path <- makeAbsolute [relfile|test/Pixi/testdata/pixi-v8-future.lock|]
      graph <- analyze mempty path

      expectDeps'
        [ condaDep "conda-forge" "linux-64" "zlib" "1.3.1" [EnvProduction]
        , mkDep PipType "certifi" (Just "2024.2.2") [EnvProduction]
        ]
        graph

  describe "outOfRangeVersionWarning" $
    it "warns above and below the known range, and stays quiet inside it" $ do
      outOfRangeVersionWarning 8 `shouldSatisfy` warns "is newer than any version this fossa-cli knows about"
      outOfRangeVersionWarning 4 `shouldSatisfy` warns "is not officially supported!"
      outOfRangeVersionWarning 5 `shouldBe` Nothing
      outOfRangeVersionWarning 6 `shouldBe` Nothing
      outOfRangeVersionWarning 7 `shouldBe` Nothing

  describe "analyze a malformed lockfile with an unknown version" $
    it' "fails when environments is a list instead of a map" $ do
      path <- makeAbsolute [relfile|test/Pixi/testdata/pixi-v8-malformed.lock|]
      expectFatal' $ analyze mempty path

  describe "analyze a lockfile with only unreadable entries" $
    it' "fails rather than reporting an empty graph" $ do
      path <- makeAbsolute [relfile|test/Pixi/testdata/pixi-v6-unreadable-packages.lock|]
      expectFatal' $ analyze mempty path

  describe "analyze a lockfile with no packages" $
    it' "reports an empty graph instead of failing" $ do
      path <- makeAbsolute [relfile|test/Pixi/testdata/pixi-v6-no-packages.lock|]
      graph <- analyze mempty path

      expectDeps' [] graph

  describe "analyze a lockfile whose only package is the workspace itself" $
    it' "reports an empty graph instead of failing" $ do
      path <- makeAbsolute [relfile|test/Pixi/testdata/pixi-v6-workspace-only.lock|]
      graph <- analyze mempty path

      expectDeps' [] graph

  describe "declared dependencies" $
    it' "are direct, matching PyPI names in normalized form" $ do
      path <- makeAbsolute [relfile|test/Pixi/testdata/pixi-v6-edges.lock|]
      graph <- analyze (PixiManifestDeps (Set.singleton "LibFoo") (Map.singleton "Zope.Interface" mempty)) path

      expectDirect'
        [ condaDep "conda-forge" "linux-64" "app" "1.0" [EnvProduction]
        , condaDep "conda-forge" "osx-arm64" "libonlyosx" "1.0" [EnvProduction]
        , mkDep UnresolvedPathType "./libs/gadget" (Just "0.2.0") [EnvProduction]
        , condaDep "conda-forge" "linux-64" "cyc-a" "1.0" [EnvProduction]
        , condaDep "conda-forge" "linux-64" "cyc-b" "1.0" [EnvProduction]
        , -- app and tzdata depend on these, and widget on zope-interface.
          condaDep "conda-forge" "linux-64" "libfoo" "1.2" [EnvProduction]
        , mkDep PipType "zope-interface" (Just "6.0") [EnvProduction]
        ]
        graph

  describe "direct dependencies from pixi.toml" $ do
    let conda = condaDep "conda-forge" "linux-64"
        pypi name version = mkDep PipType name (Just version)
        both = [EnvProduction, EnvOther "dev-cpu"]
        devCpu = [EnvOther "dev-cpu"]
        inferred =
          [ conda "pytorch-cpu" "2.8.0" devCpu
          , -- Nothing in the default environment depends on its numpy.
            conda "numpy" "1.26.4" [EnvProduction]
          , pypi "pydantic-settings" "2.12.0" both
          , pypi "pytest-cov" "7.0.0" devCpu
          , pypi "pybigwig" "0.3.24" devCpu
          ]
        lockPath = makeAbsolute [relfile|test/Pixi/testdata/pixi-toml-manifest/pixi.lock|]

    it' "marks the packages it declares as direct, including ones only a feature declares" $
      withFixture [reldir|test/Pixi/testdata/pixi-toml-manifest|] $ \project -> do
        graph <- dependencyGraph <$> getDeps project

        expectDirect'
          ( inferred
              <> [ conda "python" "3.11.0" both
                 , conda "numpy" "2.3.4" devCpu
                 , pypi "pydantic" "2.12.4" both
                 , -- Only the test feature declares pytest, and pytest-cov depends on it.
                   pypi "pytest" "9.0.1" devCpu
                 ]
          )
          graph

    it' "infers direct dependencies without a manifest" $ do
      lock <- lockPath
      graph <- dependencyGraph <$> getDeps PixiProject{pixiDir = parent lock, pixiLock = lock, pixiManifest = Nothing}

      expectDirect' inferred graph

    it' "infers direct dependencies when the manifest can't be read" $ do
      lock <- lockPath
      manifest <- makeAbsolute [relfile|test/Pixi/testdata/pixi-unreadable-manifest.toml|]
      graph <- dependencyGraph <$> getDeps PixiProject{pixiDir = parent lock, pixiLock = lock, pixiManifest = Just (PixiToml manifest)}

      expectDirect' inferred graph

  describe "direct dependencies from pyproject.toml" $ do
    it' "declares [project] dependencies, dependency groups, [tool.pixi] tables, and python" $ do
      path <- makeAbsolute [relfile|test/Pixi/testdata/pyproject-manifest/pyproject.toml|]
      PyprojectPixiDeps declared <- readContentsToml path

      declared
        `shouldBe'` Just
          ( PixiManifestDeps
              (Set.singleton "python")
              (Map.fromList [(name, mempty) | name <- ["snakemake-interface-common", "throttler", "pyyaml", "ruff", "snakemake-executor-plugin-slurm", "pytest"]])
          )

    it' "marks the packages it declares as direct" $
      withFixture [reldir|test/Pixi/testdata/pyproject-manifest|] $ \project -> do
        graph <- dependencyGraph <$> getDeps project
        let pypi name version = mkDep PipType name (Just version)
            everywhere = [EnvProduction, EnvOther "quality", EnvTesting]

        expectDirect'
          [ -- pixi declares python for a pyproject.toml workspace.
            condaDep "conda-forge" "linux-64" "python" "3.14.6" everywhere
          , pypi "snakemake-interface-common" "1.23.0" everywhere
          , pypi "throttler" "1.2.3" everywhere
          , pypi "pyyaml" "6.0.3" everywhere
          , -- From the style dependency group.
            pypi "ruff" "0.10.0" [EnvOther "quality"]
          , pypi "pytest" "9.1.1" [EnvTesting]
          ]
          graph

  describe "edges within each environment and platform" $ do
    let conda = condaDep "conda-forge" "linux-64"
        pypi name version = mkDep PipType name (Just version)
        devCpu = [EnvOther "dev-cpu"]
        graph' = makeAbsolute [relfile|test/Pixi/testdata/pixi-toml-manifest/pixi.lock|] >>= analyze mempty

    it' "link a dependency to the build installed alongside it" $ do
      graph <- graph'
      expectEdge' graph (conda "pytorch" "2.8.0" devCpu) (conda "numpy" "2.3.4" devCpu)
      Graphing.hasEdge (conda "pytorch" "2.8.0" devCpu) (conda "numpy" "1.26.4" [EnvProduction]) graph `shouldBe'` False

    it' "follow a requirement gated on an extra only when that extra is installed" $ do
      graph <- graph'
      -- pytest-cov requires coverage[toml].
      expectEdge' graph (pypi "coverage" "7.11.3" devCpu) (pypi "tomli" "2.3.0" devCpu)
      -- Nothing installs pluggy's testing extra or pydantic-settings' toml extra.
      Graphing.hasEdge (pypi "pluggy" "1.6.0" devCpu) (pypi "pytest" "9.0.1" devCpu) graph `shouldBe'` False
      Graphing.hasEdge (pypi "pydantic-settings" "2.12.0" [EnvProduction, EnvOther "dev-cpu"]) (pypi "tomli" "2.3.0" devCpu) graph `shouldBe'` False

    it' "follow the extras a lockfile or manifest installs a package with" $ do
      path <- makeAbsolute [relfile|test/Pixi/testdata/pixi-v6-extras.lock|]
      let requests = mkDep PipType "requests" (Just "2.32.3") [EnvProduction]
          pysocks = mkDep PipType "pysocks" (Just "1.7.1") [EnvProduction]
          chardet = mkDep PipType "chardet" (Just "5.2.0") [EnvProduction]
      locked <- analyze mempty path
      expectEdge' locked requests pysocks
      Graphing.hasEdge requests chardet locked `shouldBe'` False

      declared <- analyze (PixiManifestDeps mempty (Map.singleton "requests" (Set.singleton "use_chardet_on_py3"))) path
      expectEdge' declared requests chardet

  describe "packages built from source" $ do
    let sdl2 platform = condaDep "conda-forge" platform "sdl2" "2.32.56" [EnvProduction]
        -- Its manifest is in the workspace, but its build source is a git repository.
        sdlExample = mkDep GitType "https://github.com/prefix-dev/pixi-build-testsuite" (Just "34d90eaf81fa042ace51ec27af28b1e487c79f32") [EnvProduction]
        myPackage = mkDep UnresolvedPathType "my-package-linux" (Just "1.0.0") [EnvProduction]
        -- A build source path is relative to the package's manifest.
        viewer = mkDep UnresolvedPathType (toText $ "packages/viewer" </> "../src") (Just "0.2.0") [EnvProduction]
        graph' = makeAbsolute [relfile|test/Pixi/testdata/pixi-v7-source-packages.lock|] >>= analyze mempty

    it' "are reported by where they're built from, once for all platforms" $ do
      graph <- graph'
      expectDeps' [sdl2 "linux-64", sdl2 "osx-arm64", sdlExample, myPackage, viewer] graph

    it' "link to their dependencies on each platform" $ do
      graph <- graph'
      expectEdge' graph sdlExample (sdl2 "linux-64")
      expectEdge' graph sdlExample (sdl2 "osx-arm64")
      expectEdge' graph myPackage sdlExample
