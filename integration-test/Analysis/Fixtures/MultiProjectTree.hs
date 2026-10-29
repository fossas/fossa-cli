{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

-- | A checked-in fixture tree of minimal, manifest-only ("static analysis",
-- no tool execution) projects nested in a single directory tree at varying
-- depths. Each manifest is derived from a real example project under
-- @example-projects@ and trimmed to exactly one real dependency.
--
-- The consumer (piece 2) materializes these files under a temp root and runs
-- the shared discovery walk over the result.
--
-- Subtree → reviewer scenario pinned:
--
--   * root @pom.xml@                              — scenario 1: a project at the tree root (Maven)
--   * @apps@ (@package.json@, @mix.exs@, @fpm.toml@) — scenario 2: 2-3 sibling projects in one subdir
--                                                     (Npm, Elixir, Fortran)
--   * @deep\/l1\/l2\/l3\/leaf\/project.clj@        — scenario 3: nested several levels deep, with
--                                                   empty directories (@l1@, @l2@, @l3@) between
--                                                   each level (created as empty dirs when the
--                                                   parent directories of the single leaf file
--                                                   are materialized) (Leiningen)
--   * @libs\/go-app@ and @services\/internal\/worker@ (@go.mod@) — scenario 4: two projects of the
--                                                   same ecosystem (Go) at different depths
--
--   The remaining subtrees add ecosystem coverage: @composer@ (Composer, needs
--   composer.lock to discover), @cocoapods@ (CocoaPods), @conda@ (Conda) and
--   @cabal@ (Cabal).
module Analysis.Fixtures.MultiProjectTree (
  multiProjectTree,
) where

import Data.Text (Text)
import Path (File, Path, Rel, relfile)

multiProjectTree :: [(Path Rel File, Text)]
multiProjectTree =
  [
    ( [relfile|pom.xml|]
    , "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
        <> "<project xmlns=\"http://maven.apache.org/POM/4.0.0\"\n"
        <> "  xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n"
        <> "  xsi:schemaLocation=\"http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd\">\n"
        <> "  <modelVersion>4.0.0</modelVersion>\n"
        <> "  <groupId>com.example.app</groupId>\n"
        <> "  <artifactId>example-artifact</artifactId>\n"
        <> "  <version>1.1</version>\n"
        <> "  <name>example-artifact</name>\n"
        <> "  <dependencies>\n"
        <> "    <dependency>\n"
        <> "      <groupId>joda-time</groupId>\n"
        <> "      <artifactId>joda-time</artifactId>\n"
        <> "      <version>2.10.14</version>\n"
        <> "    </dependency>\n"
        <> "  </dependencies>\n"
        <> "</project>\n"
    )
  ,
    ( [relfile|README.md|]
    , "# Multi-project fixture tree\n\n"
        <> "Minimal manifest-only projects for static-strategy discovery tests.\n"
    )
  , ([relfile|.gitignore|], "dist-newstyle/\ndeps/\n_build/\n")
  , -- Scenario 2: three sibling projects in one subdirectory (apps/).
    -- Npm (derived from example-projects/javascript/with-package-json).

    ( [relfile|apps/package.json|]
    , "{\n"
        <> "  \"name\": \"example-pkg\",\n"
        <> "  \"version\": \"1.0.0\",\n"
        <> "  \"license\": \"MIT\",\n"
        <> "  \"dependencies\": {\n"
        <> "    \"lodash\": \"^4.17.21\"\n"
        <> "  }\n"
        <> "}\n"
    )
  , -- Elixir (derived from example-projects/elixir/with-mix).

    ( [relfile|apps/mix.exs|]
    , "defmodule Example.MixProject do\n"
        <> "  use Mix.Project\n\n"
        <> "  def project do\n"
        <> "    [\n"
        <> "      app: :example,\n"
        <> "      version: \"0.1.0\",\n"
        <> "      elixir: \"~> 1.12\",\n"
        <> "      deps: deps()\n"
        <> "    ]\n"
        <> "  end\n\n"
        <> "  def application do\n"
        <> "    [extra_applications: [:logger]]\n"
        <> "  end\n\n"
        <> "  defp deps do\n"
        <> "    [\n"
        <> "      {:phoenix, \"~> 1.6\"}\n"
        <> "    ]\n"
        <> "  end\n"
        <> "end\n"
    )
  , -- Fortran (derived from example-projects/fortran/with-fpm).

    ( [relfile|apps/fpm.toml|]
    , "name = \"with-fpm\"\n"
        <> "version = \"0.1.0\"\n\n"
        <> "[dependencies]\n"
        <> "toml-f = { git = \"https://github.com/toml-f/toml-f\", tag = \"v0.2.1\" }\n"
    )
  , -- Scenario 4 (part a): a Go module at moderate depth. Go also appears again
    -- at a greater depth for the duplicate-type case (derived from
    -- example-projects/path-dependency).

    ( [relfile|libs/go-app/go.mod|]
    , "module example.com/app\n\n"
        <> "go 1.14\n\n"
        <> "require github.com/Masterminds/squirrel v1.4.0\n"
    )
  , -- Scenario 3: one project nested several levels deep, with empty
    -- directories (l1, l2, l3) between each level. Leiningen. Derived from
    -- example-projects/clojure/with-lein.

    ( [relfile|deep/l1/l2/l3/leaf/project.clj|]
    , "(defproject with-lein \"0.1.0\"\n"
        <> "  :description \"Example lein project\"\n"
        <> "  :license {:name \"EPL-2.0\"}\n"
        <> "  :dependencies [[org.clojure/clojure \"1.10.3\"]])\n"
    )
  , -- Scenario 4 (part b): a second Go module at a greater depth, so the same
    -- ecosystem (Go) appears at two different depths.

    ( [relfile|services/internal/worker/go.mod|]
    , "module example.com/services/worker\n\n"
        <> "go 1.14\n\n"
        <> "require github.com/Masterminds/squirrel v1.4.0\n"
    )
  , -- Composer. Discovery reads composer.lock, so both files are required.
    -- Derived from example-projects/php.

    ( [relfile|composer/composer.json|]
    , "{\n"
        <> "    \"name\": \"example/project\",\n"
        <> "    \"description\": \"example php project\",\n"
        <> "    \"require\": {\n"
        <> "        \"michelf/php-markdown\": \"^1.9\"\n"
        <> "    }\n"
        <> "}\n"
    )
  ,
    ( [relfile|composer/composer.lock|]
    , "{\n"
        <> "    \"packages\": [\n"
        <> "        {\n"
        <> "            \"name\": \"michelf/php-markdown\",\n"
        <> "            \"version\": \"1.9.0\",\n"
        <> "            \"type\": \"library\"\n"
        <> "        }\n"
        <> "    ],\n"
        <> "    \"packages-dev\": [],\n"
        <> "    \"content-hash\": \"03013a744e7b98b8e3a63a6f92399118\",\n"
        <> "    \"aliases\": [],\n"
        <> "    \"minimum-stability\": \"stable\",\n"
        <> "    \"prefer-stable\": true,\n"
        <> "    \"platform\": [],\n"
        <> "    \"platform-dev\": [],\n"
        <> "    \"plugin-api-version\": \"2.1.0\"\n"
        <> "}\n"
    )
  , -- CocoaPods. Derived from example-projects/cocoapods/simple-cocoapods-example.

    ( [relfile|cocoapods/Podfile|]
    , "platform :ios, '14.0'\n"
        <> "target 'ExampleApp' do\n"
        <> "  pod 'PureLayout'\n"
        <> "end\n"
    )
  ,
    ( [relfile|cocoapods/Podfile.lock|]
    , "PODS:\n"
        <> "  - PureLayout (3.1.9)\n"
        <> "DEPENDENCIES:\n"
        <> "  - PureLayout\n"
        <> "SPEC REPOS:\n"
        <> "  trunk:\n"
        <> "    - PureLayout\n"
        <> "SPEC CHECKSUMS:\n"
        <> "  PureLayout: 5fb5e5429519627d60d079ccb1eaa7265ce7cf88\n"
        <> "PODFILE CHECKSUM: c6a1f6e5b0b1c2d3e4f5a6b7c8d9e0f1a2b3c4d5\n"
        <> "COCOAPODS: 1.11.3\n"
    )
  , -- Conda. Derived from example-projects/conda/conda-forge-ex.

    ( [relfile|conda/environment.yml|]
    , "name: example-conda\n"
        <> "channels:\n"
        <> "  - conda-forge\n"
        <> "dependencies:\n"
        <> "  - numpy=1.23.3=py310h1b7c290_0\n"
        <> "prefix: /Users/example/anaconda3/envs/example-conda\n"
    )
  , -- Cabal. Derived from example-projects/fossa-deps/unarchived-vendored-dependencies.

    ( [relfile|cabal/example-cabal.cabal|]
    , "cabal-version:      2.4\n"
        <> "name:               example-cabal\n"
        <> "version:            0.1.0.0\n\n"
        <> "synopsis: An example cabal project\n\n"
        <> "license: MIT\n\n"
        <> "executable example-cabal\n"
        <> "    main-is:          Main.hs\n"
        <> "    build-depends:\n"
        <> "        base ^>=4.14.3.0,\n"
        <> "        haskell-say == 1.0.0.0\n"
        <> "    hs-source-dirs:   app\n"
        <> "    default-language: Haskell2010\n"
    )
  ]
