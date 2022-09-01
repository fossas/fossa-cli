# Analysis Strategies

The CLI performs dependency analysis using a set of "strategies".

Strategies define how to identify user projects and how to determine the dependencies of each project. Each strategy corresponds to roughly one tool, language, or ecosystem.

<!-- omit in toc -->
## Table of contents

- [Anatomy of a strategy](#anatomy-of-a-strategy)
  - [Analysis targets](#analysis-targets)
  - [Discovery and analysis](#discovery-and-analysis)
  - [Tactics](#tactics)
- [Supported strategies](#supported-strategies)

## Anatomy of a strategy

### Analysis targets

When the CLI identifies user projects to analyze, it groups these projects into "analysis targets". The exact semantics of an analysis target depends on the language, but usually a target is a single module, package, library, or program. Usually, an analysis target is equivalent to whatever you would create a dependency manifest file for.

Analysis targets have exactly one type (which identifies which strategy is used to analyze the target) and produce exactly one dependency graph.

A single Analysis (i.e. a single run of the CLI) includes every analysis target discovered in the folder being analyzed.

### Discovery and analysis

Strategies usually have two phases:

1. A _discovery_ phase, to determine which user projects are in the folder being analyzed.
2. An _analysis_ phase, where dependencies of discovered user projects are analyzed.

_Discovery_ usually works by examining the file system for special files that indicate the presence of a project. For example, a `package.json` usually indicates the presence of an NPM project, or a `pom.xml` usually indicates the presence of a Maven project.

_Analysis_ usually works by running through a series of _tactics_ for each strategy. For example, to analyze the dependencies of an NPM project, we might try to parse `package-lock.json` or execute `npm ls`.

Each strategy defines its own logic for how it does discovery, and its own logic for which tactics it uses to analyze dependencies.

### Tactics

Strategies use one or more _tactics_ to analyze the dependencies or discovered projects.

Each strategy defines its own tactics. Strategies generally have multiple tactics, and will automatically select between tactics depending on what information is available.

Tactics tend to vary across two axes:

1. What _requirements_ must be fulfilled for the tactic to succeed?
2. What _structure_ does the tactic provide in its output?

The relevant questions for _requirements_ are usually:

1. Does this tactic perform dynamic or static analysis? Tactics that do dynamic analysis usually require a functioning build environment and a CI integration, while tactics that only do static analysis only require a copy of the source code.
2. What files are necessary? Some tactics require files such as lockfiles that are not always present by default.
3. What build environment is necessary? Some tactics require running after successful builds, or being able to install build plugins.

The relevant questions for _structure_ are usually:

1. Does this tactic provide all dependencies? Some tactics are only able to provide direct dependencies, installed dependencies, or some other subset of dependencies.
2. Does this tactic provide a graph or a list? Some tactics don't provide edge information between dependencies.
3. Does this tactic provide additional dependency metadata? Some tactics are able to tag dependencies with metadata, such as whether they're test dependencies.

Each strategy's documentation contains information about its tactics and how it chooses between them.

## Supported strategies

<!--
TODO: create a lookup table that categorizes these strategies by language or tool.
-->

The CLI supports the following strategies:

- [clojure](../references/strategies/languages/golang/golang.md) (lein)
- [dart](../references/strategies/languages/dart/dart.md) (pub)
- [elixir](../references/strategies/languages/elixir/elixir.md) (mix)
- [erlang](../references/strategies/languages/erlang/erlang.md) (rebar3)
- [fortran](../references/strategies/languages/fortran/fortran.md)
- [go](../references/strategies/languages/golang/golang.md) (gomodules, dep, glide)
- [gradle](../references/strategies/languages/gradle/gradle.md)
- [haskell](../references/strategies/languages/haskell/README.md) (cabal, stack)
- [iOS](../references/strategies/platforms/ios/ios.md) (Carthage, Cocoapods, Swift package manager)
- [maven](../references/strategies/languages/maven/maven.md)
- [nodejs](../references/strategies/languages/nodejs/nodejs.md) (yarn, npmcli)
- [php](../references/strategies/languages/php/composer.md) (composer)
- [python](../references/strategies/languages/python/python.md) (conda, pipenv, setuptools)
- [rpm](../references/strategies/system/rpm/rpm.md) (spec files)
- [ruby](../references/strategies/languages/ruby/ruby.md) (bundler)
- [rust](../references/strategies/languages/rust/rust.md) (cargo)
- [sbt](../references/strategies/languages/scala/sbt.md) (sbt)
- [.NET](../references/strategies/languages/dotnet/nuget.md) (NuGet, Paket)