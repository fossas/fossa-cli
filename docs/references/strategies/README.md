# Supported Package Managers

Package managers supported by FOSSA CLI can have multiple strategies for detecting dependencies,
one primary strategy that yields ideal results and zero or more fallback strategies.

Within this list of strategies, we have the concept of _static_ and _dynamic_ strategies.
Static strategies parse files to find a dependency graph (example: parse a `package-lock.json` file).
Dynamic strategies are required when analyzing package managers that do not offer complete lockfiles, such as Gradle or Go.
Dynamic strategies require a working build environment to operate in.

It is important to note that neither type of strategy has an inherent benefit when detecting dependencies.
If a supported package manager has only a static or only a dynamic strategy, 
this does not necessarily mean it is less supported than a package manager that has both.

However, if a package manager does have multiple strategies,
the first one is _generally_ considered ideal.
Click through to the package manager details from this table for more information.

> If FOSSA CLI is forced to utilize a fallback strategy,
> meaning it did not detect ideal results,
> a warning is emitted in the scan summary after running `fossa analyze`.

This table is organized by language primarily; if a language has multiple supported package managers
they are listed in `(parenthesis)` after the name of the language. 
For example, `Go (gomodules)` means "The Go language, using the 'gomodules' package manager".

> When we refer to "fallback strategies", these are _within_ the overall strategy of a given package manager.
> In other words, `Go (gomodules)` is _not_ a "fallback strategy" of `Go (dep)`. They are two different package managers.
> To see more details (including the primary and fallback strategies) for a specific language & package manager,
> click the table entry.

| Language/Package Manager                                               | Dynamic            | Static             | Report Vulnerabilities | Primary Strategy |
|------------------------------------------------------------------------|--------------------|--------------------|------------------------|------------------|
| [C#](./languages/dotnet/README.md)                                     | :white_check_mark: | :white_check_mark: | :grey_question:        | Dynamic          |
| [Clojure (leiningen)](./languages/clojure/clojure.md)                  | :white_check_mark: | :x:                | :grey_question:        | Dynamic          |
| [Dart (pub)](./languages/dart/dart.md)                                 | :white_check_mark: | :white_check_mark: | :grey_question:        | Dynamic          |
| [Elixer (mix)](./languages/elixir/elixir.md)                           | :white_check_mark: | :x:                | :grey_question:        | Dynamic          |
| [Erlang (rebar3)](./languages/erlang/erlang.md)                        | :white_check_mark: | :x:                | :grey_question:        | Dynamic          |
| [Fortran](./languages/fortran/fortran.md)                              | :x:                | :white_check_mark: | :grey_question:        | Static           |
| [Go (dep)](./languages/golang/godep.md)                                | :x:                | :white_check_mark: | :grey_question:        | Static           |
| [Go (glide)](./languages/golang/glide.md)                              | :x:                | :white_check_mark: | :grey_question:        | Static           |
| [Go (gomodules)](./languages/golang/gomodules.md)                      | :white_check_mark: | :white_check_mark: | :grey_question:        | Dynamic          |
| [Gradle](./languages/gradle/gradle.md)                                 | :white_check_mark: | :x:                | :grey_question:        | Dynamic          |
| [Haskell (cabal)](./languages/haskell/cabal.md)                        | :white_check_mark: | :x:                | :grey_question:        | Dynamic          |
| [Haskell (stack)](./languages/haskell/stack.md)                        | :white_check_mark: | :x:                | :grey_question:        | Dynamic          |
| [iOS (carthage)](./platforms/ios/carthage.md)                          | :x:                | :white_check_mark: | :grey_question:        | Static           |
| [iOS (cocoapods)](./platforms/ios/cocoapods.md)                        | :x:                | :white_check_mark: | :grey_question:        | Static           |
| [iOS (swift)](./platforms/ios/swift.md)                                | :x:                | :white_check_mark: | :grey_question:        | Static           |
| [Maven](./languages/maven/maven.md)                                    | :white_check_mark: | :white_check_mark: | :grey_question:        | Dynamic          |
| [NodeJS (NPM/Yarn/pnpm)](./languages/nodejs/nodejs.md)                 | :x:                | :white_check_mark: | :grey_question:        | Static           |
| [Perl](./languages/perl/perl.md)                                       | :x:                | :white_check_mark: | :grey_question:        | Static           |
| [PHP (Composer)](./languages/php/composer.md)                          | :x:                | :white_check_mark: | :grey_question:        | Static           |
| [Python (Conda)](./languages/python/conda.md)                          | :white_check_mark: | :white_check_mark: | :grey_question:        | Dynamic          |
| [Python (Pdm)](./languages/python/pdm.md)                              | :x:                | :white_check_mark: | :grey_question:        | Static           |
| [Python (Pipenv)](./languages/python/pipenv.md)                        | :white_check_mark: | :white_check_mark: | :grey_question:        | Dynamic          |
| [Python (Poetry)](./languages/python/poetry.md)                        | :x:                | :white_check_mark: | :grey_question:        | Static           |
| [Python (setup.py/requirements.txt)](./languages/python/setuptools.md) | :x:                | :white_check_mark: | :grey_question:        | Static           |
| [R (renv)](./languages/r/renv.md)                                      | :x:                | :white_check_mark: | :grey_question:        | Static           |
| [Ruby (bundler)](./languages/ruby/ruby.md)                             | :white_check_mark: | :white_check_mark: | :grey_question:        | Static           |
| [Rust (cargo)](./languages/rust/rust.md)                               | :white_check_mark: | :x:                | :grey_question:        | Dynamic          |
| [Scala (sbt)](./languages/scala/sbt.md)                                | :white_check_mark: | :x:                | :grey_question:        | Dynamic          |

# Additional strategies

These are strategies that do not integrate with a package manager,
but instead work in some other method that is usually unique to the strategy.

For more information, click through to the documentation for the strategy.

| Strategy                                                                                                | Report Vulnerabilities | Strategy kind |
|---------------------------------------------------------------------------------------------------------|------------------------|---------------|
| [C](./languages/c-cpp/c-cpp.md)                                                                         | :white_check_mark:     | Manual        |
| [C++](./languages/c-cpp/c-cpp.md)                                                                       | :white_check_mark:     | Manual        |
| [`fossa-deps` (referenced-dependencies)](../../features/manual-dependencies.md#referenced-dependencies) | :white_check_mark:     | Static        |
| [`fossa-deps` (custom-dependencies)](../../features/manual-dependencies.md#custom-dependencies)         | :x:                    | Static        |
| [`fossa-deps` (remote-dependencies)](../../features/manual-dependencies.md#remote-dependencies)         | :x:                    | Static        |
| [`fossa-deps` (vendored-dependencies)](../../features/vendored-dependencies.md)                         | :x:                    | Static        |

> "Manual" strategies listed here indicate that this strategy does not run by default, only if a user opts in via command-line argument.
> This means that for any import method in which one might want to consider whether a strategy is "static" or "dynamic", 
> these are not included since they require a user to manually opt in to their use.

# Other import methods

FOSSA also supports other ways of scanning projects that have some other entrypoint than `fossa analyze`.
If these sound useful to you, follow these links for more information.

Note: The main area in which "dynamic" vs "static" strategies matter is in the context of using _Quick Import_, _Broker_, and _Container Scanning_.
These import methods utilize only "static" strategies, since they do not run in the context of your build environment.

As such, depending on the language and package manager, it may be extremely beneficial to use FOSSA CLI inside your CI instead:
Any package manager which does not provide a "static" strategy is generally unsupported in these environments,
and any package manager with a "dynamic" _primary_ strategy is better supported in CI.


| Tool                                                     | Summary                                                                                     | Strategies supported                                            |
|----------------------------------------------------------|---------------------------------------------------------------------------------------------|-----------------------------------------------------------------|
| [Container Scanning](../subcommands/container.md)        | Report system dependencies, and dependencies of detected projects, inside a container image | Any _static_ strategy                                           |
| [Quick Import](https://docs.fossa.com/docs/quick-import) | Scan your projects without configuring CI                                                   | Any _static_ strategy                                           |
| [Broker](https://github.com/fossas/broker)               | Scan your projects inside your firewall, without configuring CI or running FOSSA on premise | Any _static_ strategy                                           |
| [Yocto](https://github.com/fossas/meta-fossa)            | Report the licenses for your dependencies when building an OS with Yocto                    | `fossa-deps`: `vendored-dependencies` and `custom-dependencies` |
| [GitHub Action](https://github.com/fossas/fossa-action)  | Integrate with GitHub Actions instead of using FOSSA CLI directly                           | Any _static_ or _dynamic_ strategy                              |
