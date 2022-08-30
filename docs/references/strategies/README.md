# Supported Languages

<!-- add table here

- Analyzers
  - By language
  - By platform
    - Might have duplicates
      - Carthage is both "iOS" as platform and "Objective-C" as language
      - .NET is platform, C# is language
      - Conda is platform, Python is language
    - TODO: add scripting/linting to let us say "file at this folder location is the same as other file" and check that their contents are identical (e.g. so we can duplicate Carthage file under both iOS and Objective-C)
  - System deps
  - Others
    - Docker
-->

### clojure

- [leiningen](languages/clojure/leiningen.md)

### dart

- [pub](languages/dart/pub.md)

### erlang

- [rebar3](languages/erlang/erlang.md)

### elixir

- [mix](languages/elixir/mix.md)

### fortran

- [fortran](languages/fortran/fortran.md)

### golang

- [gomodules (`go mod`)](languages/golang/gomodules.md)
- [dep](languages/golang/godep.md)
- [glide](languages/golang/glide.md)

### haskell

- [cabal](languages/haskell/cabal.md)
- [stack](languages/haskell/stack.md)

### java

- [maven](languages/maven/maven.md)
- [gradle](languages/gradle/gradle.md)

### javascript/typescript

- [yarn](languages/nodejs/yarn.md)
- [npm](languages/nodejs/npm.md)
- [pnpm](languages/nodejs/pnpm.md)
### nim

- [Nimble](languages/nim/nimble.md)

### .NET

- [NuGet](languages/dotnet/nuget.md)
- [Paket](languages/dotnet/paket.md)

### objective-c

- [carthage](platforms/ios/carthage.md)
- [cocoapods](platforms/ios/cocoapods.md)

### perl

- [perl](languages/perl/perl.md)

### php

- [php](languages/php/composer.md)

### python

- [conda](platforms/conda/conda.md)
- [`requirements.txt`/`setup.py`](languages/python/python.md)
- [pipenv](languages/python/pipenv.md)

### ruby

- [bundler](languages/ruby/bundler.md)

### rust

- [cargo](languages/rust/cargo.md)

### scala

- [sbt](languages/scala/sbt.md)
- [gradle](languages/gradle/gradle.md)
- [maven](languages/maven/maven.md)

### swift

- [carthage](platforms/ios/carthage.md)
- [cocoapods](platforms/ios/cocoapods.md)
- [swiftPM](platforms/ios/swift.md)

## Strategies by type

Languages supported by FOSSA CLI can have multiple strategies for detecting dependencies, one primary strategy that yields ideal results and zero or more fallback strategies. Within this list of strategies, we have the concept of *static* and *dynamic* strategies. Static strategies parse files to find a dependency graph (example: parse a `package-lock.json` file). Dynamic strategies are required when analyzing package managers that do not offer complete lockfiles, such as Gradle or Golang. Dynamic strategies require a working build environment to operate in.

It is important to note that neither type of strategy has an inherent benefit when detecting dependencies. If a supported language does not have static and dynamic strategies, this does not mean it is less supported than a language that does.

> If the FOSSA CLI uses a fallback strategy and does not detect ideal results, you will see a "Warning" in the output when running `fossa analyze`.

\* - denotes the primary strategy (all other strategies are fallbacks)

| Language/Package Manager                                                                                                                        | Dynamic | Static |
| ----------------------------------------------------------------------------------------------------------------------------------------------- | ------- | ------ |
| [C#](https://github.com/fossas/fossa-cli/tree/master/docs/references/strategies/languages/dotnet)                                               | ✅\*    | ✅     |
| [Clojure (leiningen)](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/languages/clojure/clojure.md)                  | ✅\*    | ❌     |
| [Dart (pub)](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/languages/dart/dart.md)                                 | ✅\*    | ✅     |
| [Elixer (mix)](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/languages/elixir/elixir.md)                           | ✅\*    | ❌     |
| [Erlang (rebar3)](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/languages/erlang/erlang.md)                        | ✅\*    | ❌     |
| [Fortran](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/languages/fortran/fortran.md)                              | ❌      | ✅\*   |
| [Go (dep)](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/languages/golang/godep.md)                            | ❌      | ✅\*   |
| [Go (glide)](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/languages/golang/glide.md)                          | ❌      | ✅\*   |
| [Go (gomodules)](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/languages/golang/gomodules.md)                  | ✅\*    | ✅     |
| [Gradle](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/languages/gradle/gradle.md)                                 | ✅\*    | ❌     |
| [Haskell (cabal)](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/languages/haskell/cabal.md)                        | ✅\*    | ❌     |
| [Haskell (stack)](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/languages/haskell/stack.md)                        | ✅\*    | ❌     |
| [iOS (carthage)](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/platforms/ios/carthage.md)                          | ❌      | ✅\*   |
| [iOS (cocoapods)](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/platforms/ios/cocoapods.md)                        | ❌      | ✅\*   |
| [Maven](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/languages/maven/maven.md)                                    | ✅\*    | ✅     |
| [NodeJS (NPM/Yarn/pnpm)](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/languages/nodejs/nodejs.md)                 | ❌      | ✅\*   |
| [Perl](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/languages/perl/perl.md)                                       | ❌      | ✅\*   |
| [PHP (Composer)](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/languages/php/composer.md)                          | ❌      | ✅\*   |
| [Python (Conda)](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/languages/python/conda.md)                          | ✅\*    | ✅     |
| [Python (Pipenv)](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/languages/python/pipenv.md)                        | ✅\*    | ✅     |
| [Python (Poetry)](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/languages/python/poetry.md)                        | ❌      | ✅\*   |
| [Python (setup.py/requirements.txt)](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/languages/python/setuptools.md) | ❌      | ✅\*   |
| [Ruby (bundler)](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/languages/ruby/ruby.md)                             | ✅      | ✅\*   |
| [Rust (cargo)](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/languages/rust/rust.md)                               | ✅\*    | ❌     |
| [Scala (sbt)](https://github.com/fossas/fossa-cli/tree/master/docs/references/strategies/languages/scala)                                       | ✅\*    | ✅     |