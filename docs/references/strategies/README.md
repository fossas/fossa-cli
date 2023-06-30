# Primer: Strategies in FOSSA CLI

_Already know about strategies and tactics, and the difference between static and dynamic analysis?_
_Jump to [supported package managers](#supported-package-managers)._

In FOSSA CLI, we refer to "support for a package manager" as a "strategy".
Different approaches to interacting with a given package manager are then referred to as "tactics".

For example, "Go (gomodules)" is a _strategy_ for parsing dependencies installed with the built in package manager for Go.
Then we further have multiple _tactics_ for different ways of parsing the information from that package manager.

More information about the tactics for a given strategy is listed in the reference for the strategy,
which can be viewed by clicking the name of the strategy in the table under [supported package managers](#supported-package-managers).

## Discovery and analysis

FOSSA CLI works in two phases: the "discovery" phase and the "analysis" phase.

The _discovery_ phase identifies a project (sometimes called a "target").
The _analysis_ phase then analyzes that project.

The reference for each strategy provides information on both of these phases,
and then describes the tactics used during the analysis phase.

## Primary and Fallback tactics

Each strategy has a _primary tactic_, which is the tactic that provides the best results.
Most strategies then also have _fallback_ tactics.

Which tactic is used generally depends on the project environment:
FOSSA CLI tries these tactics in order and uses the results of the first successful tactic.

If a strategy utilizes a fallback tactic, a warning is emitted in the scan summary.

## Static and Dynamic analysis

Tactics may interact with the package manager in different ways, which we call "static" and "dynamic".

When a tactic uses a _static_ interaction method, this means that FOSSA CLI can read information about the installed packages
without actually running the package manager.
We refer to these as "static tactics". A strategy composed entirely of static tactics is a "static strategy".

When a tactic uses a _dynamic_ interaction method, this means that FOSSA CLI must actually execute the package manager
in the local system in order to get information about the dependencies installed in the project.
We refer to these as "dynamic tactics". A strategy composed fully or partially of dynamic tactics is a "dynamic strategy".

In both kinds of analysis, the project generally must be in a valid state to build,
since otherwise the build tooling (in the case of a _dynamic_ tactic)
or FOSSA CLI (in the case of a _static_ tactic)
may encounter unexpected project configuration and be unable to continue.

Importantly, note that neither _static_ nor _dynamic_ tactics are inherently better,
and if a strategy has more tactics, or different kinds of tactics, than another language
that is not a relection on the relative level of support in FOSSA CLI for that strategy.

> **Manual analysis**
>
> There is another kind of analysis method, "manual".
> This indicates when the tactic and/or strategy is not automatically run by FOSSA CLI,
> and instead requires the user to opt-in via command line arguments.
>
> Strategies and tactics that use this analysis method are rare.

# Supported Package Managers

This table describes the strategies supported by FOSSA CLI.
Note that some languages have multiple strategies; this is the case when the language
has multiple supported package managers.

When a language has multiple supported package managers they are listed in `(parenthesis)`
after the name of the language. For example, `Go (gomodules)`
means "The Go language, using the 'gomodules' package manager".

The "analysis method" column is a list of all analysis methods used by any tactic in the strategy;
for more details click through to the reference for the strategy in question.

_Not sure what this means?_
_Check the [Primer: strategies in FOSSA CLI](#primer-strategies-in-fossa-cli) first!_

| Language/Package Manager                                               | Analysis Method(s) | Vulnerabilities | Full Graph Support | Dependency Scopes |
|------------------------------------------------------------------------|--------------------|-----------------|--------------------|-------------------|
| [C# (paket)](./languages/dotnet/paket.md)                              | Static             | :grey_question: | :white_check_mark: | :grey_question:   |
| [C# (project.assets.json)](./languages/dotnet/projectassetsjson.md)    | Static             | :grey_question: | :white_check_mark: | :grey_question:   |
| [C# (nuspec)](./languages/dotnet/nuspec.md)                            | Static             | :grey_question: | :x:                | :grey_question:   |
| [C# (PackageReference)](./languages/dotnet/packagereference.md)        | Static             | :grey_question: | :x:                | :grey_question:   |
| [C# (packages.config)](./languages/dotnet/packagesconfig.md)           | Static             | :grey_question: | :x:                | :grey_question:   |
| [C# (project.json)](./languages/dotnet/projectjson.md)                 | Static             | :grey_question: | :x:                | :grey_question:   |
| [Clojure (leiningen)](./languages/clojure/clojure.md)                  | :grey_question:    | :grey_question: | :grey_question:    | :grey_question:   |
| [Dart (pub)](./languages/dart/dart.md)                                 | :grey_question:    | :grey_question: | :grey_question:    | :grey_question:   |
| [Elixer (mix)](./languages/elixir/elixir.md)                           | :grey_question:    | :grey_question: | :grey_question:    | :grey_question:   |
| [Erlang (rebar3)](./languages/erlang/erlang.md)                        | :grey_question:    | :grey_question: | :grey_question:    | :grey_question:   |
| [Fortran](./languages/fortran/fortran.md)                              | :grey_question:    | :grey_question: | :grey_question:    | :grey_question:   |
| [Go (dep)](./languages/golang/godep.md)                                | :grey_question:    | :grey_question: | :grey_question:    | :grey_question:   |
| [Go (glide)](./languages/golang/glide.md)                              | :grey_question:    | :grey_question: | :grey_question:    | :grey_question:   |
| [Go (gomodules)](./languages/golang/gomodules.md)                      | :grey_question:    | :grey_question: | :grey_question:    | :grey_question:   |
| [Gradle](./languages/gradle/gradle.md)                                 | :grey_question:    | :grey_question: | :grey_question:    | :grey_question:   |
| [Haskell (cabal)](./languages/haskell/cabal.md)                        | :grey_question:    | :grey_question: | :grey_question:    | :grey_question:   |
| [Haskell (stack)](./languages/haskell/stack.md)                        | :grey_question:    | :grey_question: | :grey_question:    | :grey_question:   |
| [iOS (carthage)](./platforms/ios/carthage.md)                          | :grey_question:    | :grey_question: | :grey_question:    | :grey_question:   |
| [iOS (cocoapods)](./platforms/ios/cocoapods.md)                        | :grey_question:    | :grey_question: | :grey_question:    | :grey_question:   |
| [iOS (swift)](./platforms/ios/swift.md)                                | :grey_question:    | :grey_question: | :grey_question:    | :grey_question:   |
| [Maven](./languages/maven/maven.md)                                    | :grey_question:    | :grey_question: | :grey_question:    | :grey_question:   |
| [NodeJS (NPM/Yarn/pnpm)](./languages/nodejs/nodejs.md)                 | :grey_question:    | :grey_question: | :grey_question:    | :grey_question:   |
| [Perl](./languages/perl/perl.md)                                       | :grey_question:    | :grey_question: | :grey_question:    | :grey_question:   |
| [PHP (Composer)](./languages/php/composer.md)                          | :grey_question:    | :grey_question: | :grey_question:    | :grey_question:   |
| [Python (Conda)](./languages/python/conda.md)                          | :grey_question:    | :grey_question: | :grey_question:    | :grey_question:   |
| [Python (Pdm)](./languages/python/pdm.md)                              | :grey_question:    | :grey_question: | :grey_question:    | :grey_question:   |
| [Python (Pipenv)](./languages/python/pipenv.md)                        | :grey_question:    | :grey_question: | :grey_question:    | :grey_question:   |
| [Python (Poetry)](./languages/python/poetry.md)                        | :grey_question:    | :grey_question: | :grey_question:    | :grey_question:   |
| [Python (setup.py/requirements.txt)](./languages/python/setuptools.md) | :grey_question:    | :grey_question: | :grey_question:    | :grey_question:   |
| [R (renv)](./languages/r/renv.md)                                      | :grey_question:    | :grey_question: | :grey_question:    | :grey_question:   |
| [Ruby (bundler)](./languages/ruby/ruby.md)                             | :grey_question:    | :grey_question: | :grey_question:    | :grey_question:   |
| [Rust (cargo)](./languages/rust/rust.md)                               | :grey_question:    | :grey_question: | :grey_question:    | :grey_question:   |
| [Scala (sbt)](./languages/scala/sbt.md)                                | :grey_question:    | :grey_question: | :grey_question:    | :grey_question:   |

# Additional strategies

These are strategies that do not integrate with a package manager,
but instead work in some other method that is usually unique to the strategy.

For more information, click through to the documentation for the strategy.

| Strategy                                                                                                | Analysis Method | Vulnerabilities    | Full Graph Support | Dependency Scopes |
|---------------------------------------------------------------------------------------------------------|-----------------|--------------------|--------------------|-------------------|
| [C](./languages/c-cpp/c-cpp.md)                                                                         | Manual          | :white_check_mark: | :x:                | :x:               |
| [C++](./languages/c-cpp/c-cpp.md)                                                                       | Manual          | :white_check_mark: | :x:                | :x:               |
| [`fossa-deps` (referenced-dependencies)](../../features/manual-dependencies.md#referenced-dependencies) | Static          | :white_check_mark: | :x:                | :x:               |
| [`fossa-deps` (custom-dependencies)](../../features/manual-dependencies.md#custom-dependencies)         | Static          | :x:                | :x:                | :x:               |
| [`fossa-deps` (remote-dependencies)](../../features/manual-dependencies.md#remote-dependencies)         | Static          | :x:                | :x:                | :x:               |
| [`fossa-deps` (vendored-dependencies)](../../features/vendored-dependencies.md)                         | Static          | :x:                | :x:                | :x:               |

_Not sure what this "analysis method" means?_
_Check the [Primer: strategies in FOSSA CLI](#primer-strategies-in-fossa-cli)!_

# Other import methods

FOSSA also supports other ways of scanning projects that have some other entrypoint than `fossa analyze`.
If these sound useful to you, follow these links for more information.

> **Import methods and how they relate to analysis methods**
>
> The main area in which dynamic and static tactics and stratgies matter
> is in the context of using `Quick Import`, `Broker`, and `Container Scanning`.
> These import methods utilize only static strategies,
> since they do not run in the build environment of your organization.
> 
> As such, depending on the language and package manager,
> it may be extremely beneficial to use FOSSA CLI inside your CI instead:
> Any strategy without a static tactic is generally unsupported in these environments,
> and any strategy where the _primary tactic_ is dynamic is better supported in CI.
>
> _Not sure what this means?_
> _See the [Primer: strategies in FOSSA CLI](#primer-strategies-in-fossa-cli) for more information!_

| Tool                                                     | Summary                                                                                     | Strategies supported                                            |
|----------------------------------------------------------|---------------------------------------------------------------------------------------------|-----------------------------------------------------------------|
| [Container Scanning](../subcommands/container.md)        | Report system dependencies, and dependencies of detected projects, inside a container image | Any _static_ strategy                                           |
| [Quick Import](https://docs.fossa.com/docs/quick-import) | Scan your projects without configuring CI                                                   | Any _static_ strategy                                           |
| [Broker](https://github.com/fossas/broker)               | Scan your projects inside your firewall, without configuring CI or running FOSSA on premise | Any _static_ strategy                                           |
| [Yocto](https://github.com/fossas/meta-fossa)            | Report the licenses for your dependencies when building an OS with Yocto                    | `fossa-deps`: `vendored-dependencies` and `custom-dependencies` |
| [GitHub Action](https://github.com/fossas/fossa-action)  | Integrate with GitHub Actions instead of using FOSSA CLI directly                           | Any _static_ or _dynamic_ strategy                              |
