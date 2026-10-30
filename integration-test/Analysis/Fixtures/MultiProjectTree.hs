{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

-- | A checked-in fixture tree of minimal, manifest-only ("static analysis",
-- no tool execution) projects nested in a single directory tree at varying
-- depths. Each manifest is derived from a real example project under
-- @example-projects@ and trimmed to exactly one real dependency.
--
-- The consumer (Analysis.SharedWalkDiscoverySpec) materializes these files
-- under a temp root and runs the shared discovery walk over the result.
--
-- Subtree → reviewer scenario pinned:
--
--   * root @pom.xml@                       — scenario 1: project at the tree root (Maven)
--   * @apps@                               — scenario 2: sibling projects in one subdir (Npm, Elixir, Fortran)
--   * @apps\/build@                        — scenario 5: skip isolation. Fpm's visitor emits WalkSkipSome
--                                            ["build"] at @apps@, so it never descends and does not
--                                            report its fpm.toml project, yet Mix (which skips
--                                            "deps"/"_build", not "build") still discovers the mix.exs
--   * @deep\/l1\/l2\/l3\/leaf@              — scenario 3: nested several levels deep, with empty
--                                            directories between each level (Leiningen)
--   * @libs\/go-app@, @services\/internal\/worker@ — scenario 4: same ecosystem (Go) at different depths
--
-- Remaining subtrees add ecosystem coverage: @composer@, @cocoapods@, @conda@
-- and @cabal@.
module Analysis.Fixtures.MultiProjectTree (
  multiProjectTree,
) where

import Data.Text (Text)
import Path (File, Path, Rel, relfile)
import Text.RawString.QQ (r)

multiProjectTree :: [(Path Rel File, Text)]
multiProjectTree =
  [
    ( [relfile|pom.xml|]
    , [r|
<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0"
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
  xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
  <modelVersion>4.0.0</modelVersion>
  <groupId>com.example.app</groupId>
  <artifactId>example-artifact</artifactId>
  <version>1.1</version>
  <name>example-artifact</name>
  <dependencies>
    <dependency>
      <groupId>joda-time</groupId>
      <artifactId>joda-time</artifactId>
      <version>2.10.14</version>
    </dependency>
  </dependencies>
</project>
|]
    )
  ,
    ( [relfile|README.md|]
    , [r|
# Multi-project fixture tree

Minimal manifest-only projects for static-strategy discovery tests.
|]
    )
  ,
    ( [relfile|.gitignore|]
    , [r|
dist-newstyle/
deps/
_build/
|]
    )
  , -- Scenario 2: three sibling projects in one subdirectory (apps/).
    -- Npm (derived from example-projects/javascript/with-package-json).

    ( [relfile|apps/package.json|]
    , [r|
{
  "name": "example-pkg",
  "version": "1.0.0",
  "license": "MIT",
  "dependencies": {
    "lodash": "^4.17.21"
  }
}
|]
    )
  , -- Elixir (derived from example-projects/elixir/with-mix).

    ( [relfile|apps/mix.exs|]
    , [r|
defmodule Example.MixProject do
  use Mix.Project

  def project do
    [
      app: :example,
      version: "0.1.0",
      elixir: "~> 1.12",
      deps: deps()
    ]
  end

  def application do
    [extra_applications: [:logger]]
  end

  defp deps do
    [
      {:phoenix, "~> 1.6"}
    ]
  end
end
|]
    )
  , -- Fortran (derived from example-projects/fortran/with-fpm).

    ( [relfile|apps/fpm.toml|]
    , [r|
name = "with-fpm"
version = "0.1.0"

[dependencies]
toml-f = { git = "https://github.com/toml-f/toml-f", tag = "v0.2.1" }
|]
    )
  , -- Skip-isolation pin: Fpm's discoverVisitor emits WalkSkipSome
    -- ["build"] at apps/, so fpm never reports this project even though it
    -- holds its own fpm.toml; Mix (which skips "deps"/"_build", not
    -- "build") still discovers the mix.exs below.  Derived from
    -- example-projects/fortran/with-fpm and elixir/with-mix.

    ( [relfile|apps/build/fpm.toml|]
    , [r|
name = "nested-fpm"
version = "0.2.0"

[dependencies]
toml-f = { git = "https://github.com/toml-f/toml-f", tag = "v0.2.1" }
|]
    )
  ,
    ( [relfile|apps/build/mix.exs|]
    , [r|
defmodule Nested.MixProject do
  use Mix.Project

  def project do
    [
      app: :nested,
      version: "0.2.0",
      elixir: "~> 1.12",
      deps: deps()
    ]
  end

  def application do
    [extra_applications: [:logger]]
  end
|]
    )
  , -- Scenario 4 (part a): Go at moderate depth (part b repeats it at
    -- greater depth). Derived from example-projects/path-dependency.

    ( [relfile|libs/go-app/go.mod|]
    , [r|
module example.com/app

go 1.14

require github.com/Masterminds/squirrel v1.4.0
|]
    )
  , -- Scenario 3: one project nested several levels deep, with empty
    -- directories (l1, l2, l3) between each level. Leiningen. Derived from
    -- example-projects/clojure/with-lein.

    ( [relfile|deep/l1/l2/l3/leaf/project.clj|]
    , [r|
(defproject with-lein "0.1.0"
  :description "Example lein project"
  :license {:name "EPL-2.0"}
  :dependencies [[org.clojure/clojure "1.10.3"]])
|]
    )
  , -- Scenario 4 (part b): a second Go module at a greater depth, so the same
    -- ecosystem (Go) appears at two different depths.

    ( [relfile|services/internal/worker/go.mod|]
    , [r|
module example.com/services/worker

go 1.14

require github.com/Masterminds/squirrel v1.4.0
|]
    )
  , -- Composer. Discovery reads composer.lock, so both files are required.
    -- Derived from example-projects/php.

    ( [relfile|composer/composer.json|]
    , [r|
{
    "name": "example/project",
    "description": "example php project",
    "require": {
        "michelf/php-markdown": "^1.9"
    }
}
|]
    )
  ,
    ( [relfile|composer/composer.lock|]
    , [r|
{
    "packages": [
        {
            "name": "michelf/php-markdown",
            "version": "1.9.0",
            "type": "library"
        }
    ],
    "packages-dev": [],
    "content-hash": "03013a744e7b98b8e3a63a6f92399118",
    "aliases": [],
    "minimum-stability": "stable",
    "prefer-stable": true,
    "platform": [],
    "platform-dev": [],
    "plugin-api-version": "2.1.0"
}
|]
    )
  , -- CocoaPods. Derived from example-projects/cocoapods/simple-cocoapods-example.

    ( [relfile|cocoapods/Podfile|]
    , [r|
platform :ios, '14.0'
target 'ExampleApp' do
  pod 'PureLayout'
end
|]
    )
  ,
    ( [relfile|cocoapods/Podfile.lock|]
    , [r|
PODS:
  - PureLayout (3.1.9)
DEPENDENCIES:
  - PureLayout
SPEC REPOS:
  trunk:
    - PureLayout
SPEC CHECKSUMS:
  PureLayout: 5fb5e5429519627d60d079ccb1eaa7265ce7cf88
PODFILE CHECKSUM: c6a1f6e5b0b1c2d3e4f5a6b7c8d9e0f1a2b3c4d5
COCOAPODS: 1.11.3
|]
    )
  , -- Conda. Derived from example-projects/conda/conda-forge-ex.

    ( [relfile|conda/environment.yml|]
    , [r|
name: example-conda
channels:
  - conda-forge
dependencies:
  - numpy=1.23.3=py310h1b7c290_0
prefix: /Users/example/anaconda3/envs/example-conda
|]
    )
  , -- Cabal. Derived from example-projects/fossa-deps/unarchived-vendored-dependencies.

    ( [relfile|cabal/example-cabal.cabal|]
    , [r|
cabal-version:      2.4
name:               example-cabal
version:            0.1.0.0

synopsis: An example cabal project

license: MIT

executable example-cabal
    main-is:          Main.hs
    build-depends:
        base ^>=4.14.3.0,
        haskell-say == 1.0.0.0
    hs-source-dirs:   app
    default-language: Haskell2010
|]
    )
  ]
