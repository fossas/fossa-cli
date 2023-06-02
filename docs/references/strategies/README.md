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

### Strategy Type

Languages supported by FOSSA CLI can have multiple strategies for detecting dependencies, one primary strategy that yields ideal results and zero or more fallback strategies. Within this list of strategies, we have the concept of _static_ and _dynamic_ strategies. Static strategies parse files to find a dependency graph (example: parse a `package-lock.json` file). Dynamic strategies are required when analyzing package managers that do not offer complete lockfiles, such as Gradle or Go. Dynamic strategies require a working build environment to operate in.

It is important to note that neither type of strategy has an inherent benefit when detecting dependencies. If a supported language has only a static strategy, it does mean it is less supported than one that has a dynamic method.

### Dependency scope filtering

Dependencies can be used for a variety of reasons (different scopes) in a project, the most common scopes we refer to are "production" and "test" dependencies. "production" dependencies are shipped with the project whereas "test" dependencies are used solely for testing purposes (example: [JUnit](https://mvnrepository.com/artifact/junit/junit) in Maven is used to write unit tests). We do not show the differences between these scopes of dependencies in the FOSSA UI, but the FOSSA CLI will attempt to filter out non production dependencies from the dependency graph. The column "Scope filtering" shows a checkmark if language's primary strategy has the ability to filter out non production dependencies. More details about how we filter these scopes can be found in the docs for each language.

> If the FOSSA CLI is forced to utilize a fallback strategy, meaning it did not detect ideal results, a warning is emitted in the scan summary after running `fossa analyze`.

| Language/Package Manager                                                                                                                        | Dynamic   | Static    | Vendored Code | Primary Strategy | Scope Filtering |
| ----------------------------------------------------------------------------------------------------------------------------------------------- | --------- | --------- | ------------- | ---------------- | --------------- |
| [C#](https://github.com/fossas/fossa-cli/tree/master/docs/references/strategies/languages/dotnet)                                               | ❌        | ✅        | ❌            | Static           |                 |
| [C](https://github.com/fossas/fossa-cli/tree/master/docs/references/strategies/languages/c-cpp/c-cpp.md)                                        | :warning: | :warning: | ✅            | None             | ❌              |
| [C++](https://github.com/fossas/fossa-cli/tree/master/docs/references/strategies/languages/c-cpp/c-cpp.md)                                      | :warning: | :warning: | ✅            | None             | ❌              |
| [Clojure (leiningen)](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/languages/clojure/clojure.md)                  | ✅        | ❌        | ❌            | Dynamic          |                 |
| [Dart (pub)](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/languages/dart/dart.md)                                 | ✅        | ✅        | ❌            | Dynamic          |                 |
| [Elixer (mix)](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/languages/elixir/elixir.md)                           | ✅        | ❌        | ❌            | Dynamic          |                 |
| [Erlang (rebar3)](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/languages/erlang/erlang.md)                        | ✅        | ❌        | ❌            | Dynamic          |                 |
| [Fortran](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/languages/fortran/fortran.md)                              | ❌        | ✅        | ❌            | Static           |                 |
| [Go (dep)](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/languages/golang/godep.md)                                | ❌        | ✅        | ❌            | Static           |                 |
| [Go (glide)](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/languages/golang/glide.md)                              | ❌        | ✅        | ❌            | Static           |                 |
| [Go (gomodules)](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/languages/golang/gomodules.md)                      | ✅        | ✅        | ❌            | Dynamic          | ✅              |
| [Gradle](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/languages/gradle/gradle.md)                                 | ✅        | ❌        | ❌            | Dynamic          | ✅              |
| [Haskell (cabal)](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/languages/haskell/cabal.md)                        | ✅        | ❌        | ❌            | Dynamic          |                 |
| [Haskell (stack)](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/languages/haskell/stack.md)                        | ✅        | ❌        | ❌            | Dynamic          |                 |
| [iOS (carthage)](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/platforms/ios/carthage.md)                          | ❌        | ✅        | ❌            | Static           |                 |
| [iOS (cocoapods)](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/platforms/ios/cocoapods.md)                        | ❌        | ✅        | ❌            | Static           |                 |
| [iOS (swift)](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/platforms/ios/swift.md)                                | ❌        | ✅        | ❌            | Static           |                 |
| [Maven](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/languages/maven/maven.md)                                    | ✅        | ✅        | ❌            | Dynamic          |                 |
| [NodeJS (NPM/Yarn/pnpm)](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/languages/nodejs/nodejs.md)                 | ❌        | ✅        | ❌            | Static           |                 |
| [Perl](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/languages/perl/perl.md)                                       | ❌        | ✅        | ❌            | Static           |                 |
| [PHP (Composer)](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/languages/php/composer.md)                          | ❌        | ✅        | ❌            | Static           |                 |
| [Python (Conda)](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/languages/python/conda.md)                          | ✅        | ✅        | ❌            | Dynamic          |                 |
| [Python (Pipenv)](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/languages/python/pipenv.md)                        | ✅        | ✅        | ❌            | Dynamic          |                 |
| [Python (Poetry)](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/languages/python/poetry.md)                        | ❌        | ✅        | ❌            | Static           |                 |
| [Python (setup.py/requirements.txt)](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/languages/python/setuptools.md) | ❌        | ✅        | ❌            | Static           | ❌              |
| [R (renv)](./languages/r/renv.md)                                                                                                               | ❌        | ✅        | ❌            | Static           |                 |
| [Ruby (bundler)](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/languages/ruby/ruby.md)                             | ✅        | ✅        | ❌            | Static           |                 |
| [Rust (cargo)](https://github.com/fossas/fossa-cli/blob/master/docs/references/strategies/languages/rust/rust.md)                               | ✅        | ❌        | ❌            | Dynamic          |                 |
| [Scala (sbt)](https://github.com/fossas/fossa-cli/tree/master/docs/references/strategies/languages/scala)                                       | ✅        | ❌        | ❌            | Dynamic          |                 |

:warning:: Note that these strategies support _static_ and _dynamic_ detection differently than other strategies, and are not run by default.
Please make sure to check their linked documentation in the table above for more details.
