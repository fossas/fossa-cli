{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Pixi.PixiLockSpec (
  spec,
) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import DepTypes (
  DepEnvironment (EnvDevelopment, EnvOther, EnvProduction, EnvTesting),
  DepType (CondaType, GitType, PipType, UnresolvedPathType),
  Dependency (..),
  VerConstraint (CEq),
 )
import GraphUtil (expectDeps', expectDirect', expectEdge')
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
  outOfRangeVersionWarning,
  parseCondaArtifactUrl,
 )
import Test.Effect (expectFatal', it')
import Test.Hspec
import Text.Megaparsec (parse)

-- | A warning was emitted and it says what we meant it to say.
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
      classifyPypiSource "." `shouldBe` PypiLocalPath "."
      classifyPypiSource "./" `shouldBe` PypiLocalPath "./"
      -- The scheme comes off so both spellings name one package; the path is
      -- resolved against the project directory downstream, where a URL would
      -- not resolve at all.
      classifyPypiSource "file:///home/me/pkg" `shouldBe` PypiLocalPath "/home/me/pkg"
      -- Any other URL scheme is remote, the way rattler reads it.
      classifyPypiSource "s3://bucket/simple/widget-1.0.0-py3-none-any.whl" `shouldBe` PypiRegistry

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

      -- The workspace package itself, which pixi writes as a bare `./`, is
      -- the project, so it isn't reported, and absl-py, which it requires, is
      -- direct.
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
      graph <- analyze path

      expectDeps'
        [ condaDep "conda-forge" "linux-64" "_libgcc_mutex" "0.1" [EnvProduction]
        , condaDep "conda-forge" "linux-64" "binaryen" "118" [EnvProduction]
        ]
        graph

  describe "analyze a v7 lockfile" $
    it' "reads v7, skipping its platforms block and pixi-build source packages" $ do
      path <- makeAbsolute [relfile|test/Pixi/testdata/pixi-v7.lock|]
      graph <- analyze path

      expectDeps'
        [ condaDep "conda-forge" "linux-64" "_openmp_mutex" "4.5" [EnvProduction]
        , condaDep "conda-forge" "linux-64" "actionlint" "1.7.12" [EnvProduction]
        , mkDep GitType "https://github.com/squidfunk/mike" (Just "2d4ad799442f4592db8ad53b179bfb33db8c69ac") [EnvProduction]
        ]
        graph

  describe "analyze a v5 lockfile with a local path package" $
    it' "reports the path-sourced package instead of dropping it" $ do
      -- Trimmed from the Rattler v5 conda-lock fixture. The `requests` entry
      -- has `path:` and no `url:`; requiring `url:` failed the whole document
      -- with `key "url" not found`, so the project reported nothing at all.
      -- It was then warned-and-skipped, which under-reported the workspace;
      -- it is now handed to the path-dependency scanner.
      path <- makeAbsolute [relfile|test/Pixi/testdata/pixi-v5-local-path.lock|]
      graph <- analyze path

      expectDeps'
        [ condaDep "conda-forge" "osx-arm64" "bzip2" "1.0.8" [EnvProduction]
        , condaDep "conda-forge" "noarch" "tzdata" "2024a" [EnvProduction]
        , mkDep PipType "certifi" (Just "2024.2.2") [EnvProduction]
        , mkDep PipType "idna" (Just "3.7") [EnvProduction]
        , mkDep UnresolvedPathType "./links/requests-2.31.0-py3-none-any.whl" (Just "2.31.0") [EnvProduction]
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

  describe "analyze local path packages" $ do
    it' "reports conda and pypi paths alike, and keeps their environments" $ do
      path <- makeAbsolute [relfile|test/Pixi/testdata/pixi-v6-path-deps.lock|]
      graph <- analyze path

      expectDeps'
        [ condaDep "conda-forge" "linux-64" "zlib" "1.3.1" [EnvProduction]
        , -- Shallow conda path: parseCondaArtifactUrl returned Nothing, so
          -- this package used to disappear from the graph entirely.
          mkDep UnresolvedPathType "./vendor/openssl-3.2.1-hd590300_0.conda" Nothing [EnvProduction]
        , -- Deeper conda path: enough segments to parse, so this one used to
          -- be reported under the invented name 'shared':pkgs:openblas.
          mkDep UnresolvedPathType "../shared/pkgs/openblas-0.3.26-pthreads_h413a1c8_0.conda" Nothing [EnvProduction]
        , -- file:// and the bare path are one package in two environments.
          mkDep UnresolvedPathType "/home/me/pkgs/widget" (Just "1.0.0") [EnvProduction, EnvDevelopment]
        , mkDep UnresolvedPathType "./local/gadget" (Just "0.2.0") [EnvDevelopment]
        ]
        graph

  describe "edges from depends and requires_dist" $ do
    let conda name version = condaDep "conda-forge" "linux-64" name version [EnvProduction]
        pypi name version = mkDep PipType name (Just version) [EnvProduction]
        edges' = makeAbsolute [relfile|test/Pixi/testdata/pixi-v6-edges.lock|] >>= analyze

    it' "a conda depends entry naming a locked package is a real edge" $ do
      graph <- edges'
      expectEdge' graph (conda "app" "1.0") (conda "libfoo" "1.2")
      -- noarch is in scope from every subdir, so this one resolves too.
      expectEdge' graph (conda "app" "1.0") (condaDep "conda-forge" "noarch" "tzdata" "2024a" [EnvProduction])
      -- ...and a noarch package, which every platform shares, resolves in every subdir.
      expectEdge' graph (condaDep "conda-forge" "noarch" "tzdata" "2024a" [EnvProduction]) (conda "libfoo" "1.2")

    it' "a transitively-reached package is NOT direct" $ do
      -- The whole point: before edges existed every package was direct, so a
      -- lockfile of 600 packages claimed 600 direct dependencies.
      graph <- edges'
      expectDirect'
        [ conda "app" "1.0"
        , -- Nothing reaches it: app's `libonlyosx` names another subdir.
          condaDep "conda-forge" "osx-arm64" "libonlyosx" "1.0" [EnvProduction]
        , -- A path dependency is a root like any other node.
          mkDep UnresolvedPathType "./libs/gadget" (Just "0.2.0") [EnvProduction]
        , -- A cycle nothing else reaches: both members are direct, or the
          -- unreachable pair would be pruned from the results.
          conda "cyc-a" "1.0"
        , conda "cyc-b" "1.0"
        ]
        graph

    it' "a path dependency keeps its own edges" $ do
      graph <- edges'
      expectEdge' graph (mkDep UnresolvedPathType "./libs/gadget" (Just "0.2.0") [EnvProduction]) (pypi "widget" "1.0.0")

    it' "requires_dist resolves through PEP 503 normalization, markers and all" $ do
      graph <- edges'
      -- `zope.interface` and `charset_normalizer` are spelled with a dot and
      -- an underscore in requires_dist, and with hyphens in the locked set.
      expectEdge' graph (pypi "widget" "1.0.0") (pypi "zope-interface" "6.0")
      expectEdge' graph (pypi "widget" "1.0.0") (pypi "charset-normalizer" "3.3.2")
      -- Marker-guarded, and kept: a spurious edge beats a dropped dependency.
      expectEdge' graph (pypi "widget" "1.0.0") (pypi "guarded" "0.1.0")

    it' "a cycle doesn't hang the traversal" $ do
      graph <- edges'
      expectEdge' graph (conda "cyc-a" "1.0") (conda "cyc-b" "1.0")
      expectEdge' graph (conda "cyc-b" "1.0") (conda "cyc-a" "1.0")

    it' "an absent or out-of-subdir match-spec yields no edge and no crash" $ do
      -- `missing-pkg` is in no lockfile at all; `libonlyosx` is locked only
      -- for osx-arm64, and app is linux-64.
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

  describe "analyze a lock version newer than any we know" $
    it' "parses it anyway rather than reporting nothing" $ do
      -- The format has only ever been additive, so refusing an unseen version
      -- number costs the project its dependencies for no gain. v8 here is
      -- shaped like v7; it reports, and the warning is asserted separately
      -- below because the test effect stack discards warnings.
      path <- makeAbsolute [relfile|test/Pixi/testdata/pixi-v8-future.lock|]
      graph <- analyze path

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

  describe "analyze a malformed lockfile whose version is also out of range" $
    it' "still fails loudly: parsing an unseen version is not accepting anything" $ do
      -- `environments` is a list where a map belongs. Dropping the version
      -- gate must not drop the structural checks underneath it.
      path <- makeAbsolute [relfile|test/Pixi/testdata/pixi-v8-malformed.lock|]
      expectFatal' $ analyze path

  describe "analyze a lockfile whose every package entry is unreadable" $
    it' "fails rather than reporting an empty graph" $ do
      -- Entry-level parsing cannot fail by design, so a restructured
      -- `packages` shape surfaces here as every entry being skipped at once.
      -- Zero dependencies for a project that has them is indistinguishable
      -- from success, which is the one thing this strategy must never do.
      path <- makeAbsolute [relfile|test/Pixi/testdata/pixi-v6-unreadable-packages.lock|]
      expectFatal' $ analyze path

  describe "analyze a lockfile with no packages at all" $
    it' "reports an empty graph instead of failing" $ do
      -- A workspace with no dependencies is legitimate and graphs to nothing.
      -- The guard above keys off whether there were entries to read, so this
      -- has to pass -- otherwise it would be a false positive on valid input.
      path <- makeAbsolute [relfile|test/Pixi/testdata/pixi-v6-no-packages.lock|]
      graph <- analyze path

      expectDeps' [] graph

  describe "analyze a lockfile whose only package is the workspace itself" $
    it' "reports an empty graph instead of failing" $ do
      -- The workspace's own package is dropped as the project, not skipped as
      -- unreadable, so it doesn't count toward the guard above.
      path <- makeAbsolute [relfile|test/Pixi/testdata/pixi-v6-workspace-only.lock|]
      graph <- analyze path

      expectDeps' [] graph
