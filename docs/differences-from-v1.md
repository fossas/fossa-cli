# Differences from FOSSA 1.x to 2.x

Upgrading from FOSSA CLI 1.x to 2.x is a major breaking change. For most users, this change should be seamless. For users who use less common flags, or who have heavily customized their CLI usage, you may need to update some scripts.

## Table of contents <!-- omit in toc -->

- [Differences from FOSSA 1.x to 2.x](#differences-from-fossa-1x-to-2x)
  - [What's new in FOSSA 2.x?](#whats-new-in-fossa-2x)
    - [New Build Manager Supports](#new-build-manager-supports)
    - [Automatic analysis target discovery](#automatic-analysis-target-discovery)
    - [New fossa-deps configuration support](#new-fossa-deps-configuration-support)
    - [Improved correctness](#improved-correctness)
    - [Improved debug logging](#improved-debug-logging)
    - ["Modules" are now "analysis targets"](#modules-are-now-analysis-targets)
  - [Breaking changes in FOSSA 2.x](#breaking-changes-in-fossa-2x)
    - [Build Managers](#build-managers)
    - [CLI commands](#cli-commands)
    - [Language Specific Changes](#language-specific-changes)
      - [Gradle](#gradle)
      - [Clojure](#clojure)
      - [Golang](#golang)
      - [Haskell](#haskell)
      - [Maven](#maven)
      - [Nodejs](#nodejs)
      - [Nuget](#nuget)
      - [Cocoapods](#cocoapods)
      - [Carthage](#carthage)
      - [Composer](#composer)
      - [Python](#python)
      - [Gem](#gem)
      - [Cargo](#cargo)
      - [Sbt](#sbt)
  - [How to upgrade to FOSSA 2.x](#how-to-upgrade-to-fossa-2x)
    - [Remove calls to `fossa init`](#remove-calls-to-fossa-init)
    - [Migrate your .fossa.yml file](#migrate-your-fossayml-file)
    - [Migrate "archive upload" targets](#migrate-archive-upload-targets)
    - [Getting help with your migration](#getting-help-with-your-migration)
    - [Migrate "fossa report" commands](#migrate-fossa-report-commands)
  - [Frequently Asked Questions](#frequently-asked-questions)
      - [Until when CLI v1 will be supported by FOSSA?](#until-when-cli-v1-will-be-supported-by-fossa)
      - [I'm getting a poor result with latest version compared 1.x.](#im-getting-a-poor-result-with-latest-version-compared-1x)
      - [How do I run only a specific analyzer for my project?](#how-do-i-run-only-a-specific-analyzer-for-my-project)
    - [How do I identify which version of CLI I have installed?](#how-do-i-identify-which-version-of-cli-i-have-installed)
      - [I'm running into an error - how do I debug?](#im-running-into-an-error---how-do-i-debug)

## What's new in FOSSA 2.x?

Our focus in CLI 2.x is making FOSSA integrations easier to deploy by reducing the amount of configuration needed by users. Our goal is to get as close to turnkey deployment on as many build systems and codebases as possible.

### New Build Manager Supports

FOSSA 2.x supports following new build managers and languages:

- [Swift Package Manager (for swift)](references/strategies/platforms/ios/swift.md)
- [Pub (for flutter and dart)](references/strategies/languages/dart/dart.md)
- [Poetry (for Python)](references/strategies/languages/python/python.md)
- [Mix (for Elixir)](references/strategies/languages/elixir/elixir.md)
- [Fortran Package Manager (for Fortran)](references/strategies/languages/fortran/fortran.md)

### Automatic analysis target discovery

FOSSA CLI 2.x now does automatic analysis target discovery when you run `fossa analyze`. This means that `fossa init` is no longer required. `fossa init` is now a no-op that emits a warning, and will be removed in a future release.

In 1.x, modules could be manually configured with "strategies" that specified _how_ a module should be analyzed. While `fossa init` attempted to choose the best strategy, manual intervention was sometimes required depending on the project's setup. 

In 2.x, the CLI now automatically selects the optimal strategy for analysis targets given the current environment (e.g. whether a build tool is available). This is possible because discovery and analysis are now one step, so we can check the suitability of analysis strategies while discovering targets.

### New fossa-deps configuration support

With [`fossa-deps.{yml,json}` file](features/manual-dependencies.md), 2.x supports:

- License scanning vendor dependencies 
<!-- markdown-link-check-disable-next-line -->
- Analyzing archives that are located at a specific web address (e.g. https://my-deps-source/v1.zip)
- Manually specifying dependency by it's name and license (e.g. my-custom-dep with MIT licence)
- Manually specifying dependency for analysis by its name and dependency type (e.g. pip dependency: request)

This is very useful when working with a package manager that is not supported, or when you have a custom and nonstandard dependency management solution. The FOSSA CLI will automatically read a fossa-deps.yml or a fossa-deps.json file in the root directory (usually the current working directory) when fossa analyze is run and parse dependencies from it. These dependencies will be added to the dependencies that are normally found when fossa analysis is run in the directory.

Please refer to [fossa-deps](features/manual-dependencies.md) documentation for more details.

### Improved correctness

Analysis strategies in CLI 2.x have substantive improvements in correctness and reliability across all language integrations. 2.x has much stronger compile-time correctness guarantees in its parsers, and uses build tool plugins instead of output parsing for target types where output parsing was brittle.

### Improved debug logging

CLI 2.x has much better debug logging, including a new feature called "replay logging" which allows developers to perfectly reproduce a bug report given a replay log. This is made possible by stronger compile-time guarantees that ensure all effects that occur during analysis are logged for replay.
### "Modules" are now "analysis targets"

In 1.x, the CLI was configured by "modules", which were entrypoints to user programs that the CLI should analyze. In 2.x, we've renamed "modules" to "analysis targets", and simplified how analysis targets are defined. We renamed analysis targets to avoid confusing naming conflicts with existing language-specific "modules". Analysis targets are no longer defined by multiple, confusingly similar fields (e.g. "Dir" vs. "Path"), and are now simple `(type, target)` tuples.

## Breaking changes in FOSSA 2.x

### Build Managers

FOSSA 2.x does not support following build managers:

- Bazel
- Debian
- Ant
- Bower
- Okbuck
- Buck

For these managers, you can write a custom integration by creating `fossa-deps.{yml,json}` file which is more representative of your project's dependencies usage.

### CLI commands

Following CLI commands are not supported with 2.x:

- `fossa analyze --config`
- `fossa analyze --server-scan`
- `fossa analyze --dev`
- `fossa analyze --options`
- `fossa test --config`
- `fossa test --suppress-issues`
- `fossa upload` (this is supplemented by `fossa-deps.{yml,json}` file now, refer to "Migrate archive upload targets" section)
- `fossa report licences` (supplemented by `fossa report attribution --json`)
- `fossa report dependencies`

### Language Specific Changes

#### Gradle
- 2.x uses a new plugin-based strategy to perform discovery and analysis, it analyses all resolvable Gradle configurations. 
- 2.x does not accept any options: `cmd`, `task`, `timeout`, `all-configurations`, `configuration`, `retries`, `online`, `all-submodules`, and any other option supported in 1.x for Gradle analysis.
- In 2.x, 
   - There is no timeout (analysis may run for a long time)
   - All resolvable configurations are analyzed
   - There are no retries (CLI will attempt to analyze the project once)
   - Specific command and Gradle tasks can not be used in the analysis
   - `$FOSSA_GRADLE_COMMAND` environment variable cannot be configured. 2.x uses Gradle executable in `$PATH`.
- Refer to [FOSSA 2.x gradle docs](references/strategies/languages/gradle/gradle.md) for more information for gradle.

#### Clojure 
- 2.x performs the `lein deps :tree` strategy by default. 
- 2.x does not support any options - `strategy`, and `lien` for Clojure analysis.
- Refer to [FOSSA 2.x clojure docs](references/strategies/languages/clojure/clojure.md) for more information on how 2.x performs analysis for clojure.

#### Golang 
- 2.x supports analysis of the Golang project for projects using gomod, godeps, and glide.
- 2.x does not support following options: `tags`, `all-tags,` `strategy`, `lockfile`, `manifest`, `modules-vendor`, `allow-unresolved`, `allow-unresolved-prefix`, `allow-nested-vendor`, `allow-deep-vendor`, `allow-external-vendor`, `allow-external-vendor-prefix`.
- Refer to [FOSSA 2.x golang docs](references/strategies/languages/golang/golang.md) for more information on how 2.x performs analysis for golang.

#### Haskell 
- 2.x will apply stack strategy when a project is discovered having stack.yaml file
- 2.x will apply cabal-install strategy for projects using cabal.project or file with .cabal extension
- 2.x does not support `strategy` option for Haskell analysis
- Refer to [FOSSA 2.x haskell docs](references/strategies/languages/haskell/README.md) for more information on how 2.x performs analysis for haskell.

#### Maven
- 2.x uses `mvn` command for analysis. `mvn` must be accessible in $PATH
- 2.x does not support any option: `strategy`, `cmd`, or `bin` for Maven analysis
- Refer to [FOSSA 2.x maven docs](references/strategies/languages/maven/maven.md) for more information on how 2.x performs analysis for maven.

#### Nodejs
- 2.x supports `yarn`, `npm`, and `package.json` for analysis.
- 2.x does not support the `strategy` option for Nodejs analysis. v2.x automatically selects the application strategy which yields the highest fidelity of dependency information.
- Refer to [FOSSA 2.x nodejs docs](references/strategies/languages/nodejs/npm.md) for more information on how 2.x performs analysis for nodejs.

#### Nuget
- 2.x does not support the `strategy` option for Nuget analysis. v2.x automatically selects the application strategy which yields the highest fidelity of dependency information.
- Refer to [FOSSA 2.x nuget docs](references/strategies/languages/dotnet/README.md) for more information on how 2.x performs analysis for nuget.

#### Cocoapods
- Refer to [FOSSA 2.x cocoapods docs](references/strategies/platforms/ios/cocoapods.md) for more information on how 2.x performs analysis for cocoapods.

#### Carthage
- Refer to [FOSSA 2.x carthage docs](references/strategies/platforms/ios/carthage.md) for more information on how 2.x performs analysis for carthage.

#### Composer
- Refer to [FOSSA 2.x composer docs](references/strategies/languages/php/composer.md) for more information on how 2.x performs analysis for composer.

#### Python
- 2.x automatically selects the application strategy which yields the highest fidelity of dependency information.
- 2.x uses attempts to infer requirements.txt for any file with prefix `req` in its name, and `txt` extension.
- 2.x does not support the `strategy` or `requirement` option for Python analysis. 
- Refer to [FOSSA 2.x python docs](references/strategies/languages/python/python.md) for more information on how 2.x performs analysis for python.

#### Gem
- 2.x attempts to use the `bundle show` command (`bundle` must be accessible from `$PATH`), and if it fails, it attempts to analyze dependencies from `Gemfile.lock` file. 
- 2.x does not support `strategy` or `gemfile-lock-path` option for Gem Analysis.
- Refer to [FOSSA 2.x gem docs](references/strategies/languages/ruby/ruby.md) for more information on how 2.x performs analysis for gem.

#### Cargo
- Refer to [FOSSA 2.x cargo docs](references/strategies/languages/rust/rust.md) for more information on how 2.x performs analysis for cargo.

#### Sbt
- 2.x requires `sbt` executable to be accessible from `$PATH`
- 2.x does not require a project to have a `net.virtual-void.sbt-dependency-graph` extension installed.
- Refer to [FOSSA 2.x scala docs](references/strategies/languages/scala/sbt.md) for more information on how 2.x performs analysis for sbt.

## How to upgrade to FOSSA 2.x

### Remove calls to `fossa init`

Since analysis targets are now automatically discovered during analysis, `fossa init` is no longer a valid command. `fossa init` is currently a no-op that emits a warning. It may be removed in a future release.

### Migrate your .fossa.yml file

We've made major breaking changes in the `.fossa.yml` file format for CLI 2.x to improve clarity. You'll need to migrate your 1.x `.fossa.yml` to the new 2.x format for their configurations to apply. `.fossa.yml` for 1.x will be ignored when running cli with version greater than 1.x. We determine whether a configuration file is compatible by examining its `version` field. 

- .fossa.yml with version field value of `1` and `2` are for 1.x.
- .fossa.yml with version field value of `3` are for 2.x.

For documentation on the new configuration file format, see [here](references/files/fossa-yml.md)

### Migrate "archive upload" targets

In 1.x, ["archive upload" modules](https://github.com/fossas/fossa-cli/blob/master/docs/integrations/archive.md#archive) were a special kind of manually specified module of type `archive` that uploaded source code to the FOSSA backend for license scanning. They were also known as "raw" modules or "license scan" modules.

In 2.x, archive uploads are no longer a special analysis target type. Instead, you can use our general support for [manually specified dependencies](./features/manual-dependencies.md) to specify local dependencies.

### Getting help with your migration

If you run into migration issues, you can get support by opening a ticket in this repository.

If you are integrating a private project and want to share more details, or if you're a FOSSA customer with priority support, you can also email support@fossa.com or file a ticket at support.fossa.com for assistance.

### Migrate "fossa report" commands

In 1.x, `fossa report` supported `fossa report dependencies`, `fossa report licenses` and `fossa report attribution`.

In 2.x, only `fossa report attribution --json` is reported. The information in the previously supported v1 commands is contained in the output from `attribution`.

The format for this report has also changed slightly. Until the end of April 2022, FOSSA supports a compatibilty script to convert the format of the report.
For more information, including usage information, see [FOSSAv1 report compatibility](references/subcommands/report.md#fossav1-report-compatibility) in the user guide.

## Frequently Asked Questions

#### Until when CLI v1 will be supported by FOSSA?

FOSSA 1.x CLI is available and can be used indefinitely. We intend to make 2.x the default target for our installation scripts (as previously described in our documentation) in July 2022. If you wish to continue using 1.x, please migrate to using `install-v1` scripts.

FOSSA will only patch 1.x for security fixes. Any feature and patch development work will occur in 2.x moving forth. 

#### I'm getting a poor result with latest version compared 1.x.

If you run into poor dependency results with 2.x, you can get support by opening a ticket in this repository.

If you are integrating a private project and want to share more details, or if you're a FOSSA customer with priority support, you can also email support@fossa.com or file a ticket at support.fossa.com for assistance.

#### How do I run only a specific analyzer for my project? 

You can configure to run specific analyzer or only analyze specific paths in `.fossa.yml` file. Please refer to documentation [here](references/files/fossa-yml.md)

### How do I identify which version of CLI I have installed?

You can identify your cli version by performing `fossa --version` command.

#### I'm running into an error - how do I debug?

You can add `--debug` argument to your fossa commands (e.g. `fossa analyze --debug`), this will emit debug logs to stdout, and create `fossa.debug.json` in working directory. 
