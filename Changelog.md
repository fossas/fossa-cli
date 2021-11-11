# Spectrometer Changelog

## v2.19.9

- Go: Fixes a regression, where deep dependencies were reported as direct dependencies. ([#443](https://github.com/fossas/spectrometer/pull/443/))

## v2.19.8

- Perl: Adds support for Perl with parsing of `META.json`, `META.yml`, `MYMETA.yml`, `MYMETA.json`. ([#428](https://github.com/fossas/spectrometer/pull/428))

## v2.19.7

- Resolves a regression when parsing npm `package-lock.json` files that do not contain a `version` field ([#445](https://github.com/fossas/spectrometer/pull/445))

## v2.19.6

- Special cases scans with a single VSI only filter to skip other analysis strategies ([#407](https://github.com/fossas/spectrometer/pull/407))
- Adds the ability to skip resolving dependencies from FOSSA projects discovered during VSI scans ([#435](https://github.com/fossas/spectrometer/pull/435))

## v2.19.5

- Fixes an issue observed during VSI analysis where fingerprinting files with lines longer than 64KiB would fail. ([#427](https://github.com/fossas/spectrometer/pull/427))

## v2.19.4

- Adds experimental capability for filtering gradle configuration for analysis. ([#425](https://github.com/fossas/spectrometer/pull/425))

Refer to: [Gradle documentation](docs/references/strategies/languages/gradle/gradle.md#experimental-only-selecting-set-of-configurations-for-analysis) for more details.

## v2.19.3

- Removes `fossa compatibility` command. ([#383](https://github.com/fossas/spectrometer/pull/383))

Use [`fossa-deps.{yml,json}`](docs/features/vendored-dependencies.md) file to facilitate archive uploading capability, previously provided by `fossa compatibility` command.

## v2.19.2

- Adds `--config` flag, which can set custom path for configuration file. If `--config` flag is not used, base directory will scanned for `.fossa.yml` file. ([#415](https://github.com/fossas/spectrometer/pull/415))

## v2.19.1

- Fixes an issue where nodeJS errors were reported when no NodeJS project were discovered. ([#424](https://github.com/fossas/spectrometer/pull/424))

## v2.19.0

- Adds support for `fossa analyze --include-unused-deps`, which prevents filtering out non-production dependencies. ([#412](https://github.com/fossas/spectrometer/pull/412))
- Yarn: Adds support for workspaces. ([#374](https://github.com/fossas/spectrometer/pull/374))
- Npm: Adds support for workspaces. ([#374](https://github.com/fossas/spectrometer/pull/374))
- Npm: Removes unreliable `npm ls`-based analysis tactic. ([#374](https://github.com/fossas/spectrometer/pull/374))
- `fossa-deps`: Adds support for `bower`-type in `referenced-dependencies`. ([#406](https://github.com/fossas/spectrometer/pull/406))
- Monorepo: Chunk AOSP files when uploading ([#421](https://github.com/fossas/spectrometer/pull/421)).
- Monorepo: Don't fail on files that are filtered during expansion ([#421](https://github.com/fossas/spectrometer/pull/421)).

## v2.18.1

- Monorepo: Send error state to UI if the CLI crashes, so scans won't appear to hang forever. ([#409](https://github.com/fossas/spectrometer/pull/409))
- Monorepo: Fix parsing nomos output bug where files contain newlines. ([#409](https://github.com/fossas/spectrometer/pull/409))

## v2.18.0

- Improves performance in scenarios where cgroups are used to limit the amount of CPU time available, such as K8S containers ([#403](https://github.com/fossas/spectrometer/pull/403))

## v2.17.3

- Monorepo: adds some optimizations to reduce the amount of file buffering in memory during a scan, resulting in less memory pressure and faster scans. ([#402](https://github.com/fossas/spectrometer/pull/402))
- Adds compatibility script for `fossa report attribution --json` ([#397](https://github.com/fossas/spectrometer/pull/397))

## v2.17.2

- Fortran: Supports fortran package manager. ([#377](https://github.com/fossas/spectrometer/pull/377))

## v2.17.1

- Adds support for reporting origin path for binaries discovered via `--experimental-enable-binary-discovery` ([#396](https://github.com/fossas/spectrometer/pull/396))

## v2.17.0

- When running `fossa analyze` with the `--debug` flag, we now create a `fossa.debug.json.gz` file containing detailed runtime traces for project discovery and dependency analysis

## v2.16.6

- Monorepo: Adds automatic retries to failed API calls. ([#392](https://github.com/fossas/spectrometer/pull/392))

## v2.16.5

- Adds JSON Output for `fossa test --json` when there are no issues. ([#387](https://github.com/fossas/spectrometer/pull/387))

## v2.16.4

- Monorepo: Fixes bug with symlink logic mismatch between walker and buildspec uploader. ([#388](https://github.com/fossas/spectrometer/pull/388))

## v2.16.3

- Monorepo: Fixes bug with non-glob exclusions. ([#386](https://github.com/fossas/spectrometer/pull/386))

## v2.16.2

- Monorepo: Fixes crash when there are no ninja/buildspec files to upload. ([#385](https://github.com/fossas/spectrometer/pull/385))
- Monorepo: Fixes issue with only-path/exclude-path globs.

## v2.16.1

- Gradle: Supports analysis of projects using gralde v3.3 or below. ([#370](https://github.com/fossas/spectrometer/pull/370))

## v2.16.0

- Swift: Supports dependencies analysis for dependencies managed by Swift Package Manager. ([#354](https://github.com/fossas/spectrometer/pull/354))

## v2.15.24

- Leiningen: Executes `lein --version` before performing any analysis, to ensure Leiningen has performed its install tasks (done on its first invocation). ([#379](https://github.com/fossas/spectrometer/pull/379))

## v2.15.23

- Maven: Fixes `mvn:dependency` tactic to exclude root project as direct dependency. ([#375](https://github.com/fossas/spectrometer/pull/375))

## v2.15.22

- Adds branch and revision information to the URL reported at the end of a `fossa analyze --experimental-enable-monorepo` scan. ([#378](https://github.com/fossas/spectrometer/pull/378))

## v2.15.21

- When using `--experimental-enable-binary-discovery`, prepopulates information discovered in JAR manfiests. ([#372](https://github.com/fossas/spectrometer/pull/372))

## v2.15.20

- Yarn: Fixes potential runtime errors, when yarn.lock contains deep dependency without specification at root level in yarn.lock. ([#369](https://github.com/fossas/spectrometer/pull/369))

## v2.15.19

- Fixes an issue with `fossa-deps.yml` `vendored-dependencies` entries where uploads would fail if the dependency was in a subdirectory. ([#373](https://github.com/fossas/spectrometer/pull/373))

## v2.15.18

- Monorepo: Speeds up commercial phrase detection by doing a first pass before trying to parse context. ([#371](https://github.com/fossas/spectrometer/issues/371))

## v2.15.17

- Gradle: Classifies dependency from `testCompileClasspath` and `testRuntimeClasspath` configurations as test dependencies. ([#366](https://github.com/fossas/spectrometer/pull/366))

## v2.15.16

- Yarn: Analyzes yarn.lock without runtime error, when yarn.lock includes symlinked package. ([#363](https://github.com/fossas/spectrometer/pull/363))

## v2.15.15

- Monorepo: Efficiently upload binary blobs for ninja & buildspec files ([#362](https://github.com/fossas/spectrometer/pull/362)).

## v2.15.14

- Yarn: Fixes missing dependency from the analyses, when dependency has zero deep dependencies, and is not a deep dependency of any other dependency. ([#359](https://github.com/fossas/spectrometer/pull/359))

## v2.15.13

Adds another closed beta feature around FOSSA C/C++ support.
For now this functionality is considered publicly undocumented, and is only used with support from FOSSA engineering.

- Adds support for reporting detected binaries as unlicensed dependencies ([#353](https://github.com/fossas/spectrometer/pull/353))

## v2.15.12

- Yarn: Analyzes yarn.lock without runtime error, when yarn.lock includes directory dependency. ([#361](https://github.com/fossas/spectrometer/pull/361))

## v2.15.11

- Gradle: Classifies dependency's environment correctly, when originating from common android development and test configurations. ([#338](https://github.com/fossas/spectrometer/pull/338))

## v2.15.10

- Monorepo: Ignore permission errors when searching for ninja or buildspec files. ([#351](https://github.com/fossas/spectrometer/pull/351))

## v2.15.9

- CocoaPods: Supports git sources in `Podfile.lock` analysis. ([#345](https://github.com/fossas/spectrometer/pull/345))

## v2.15.8

- `fossa analyze --experimental-enable-monorepo` now turns off proprietary language scanning by default, and has this feature controlled by a feature flag ([#343](https://github.com/fossas/spectrometer/pull/343))

## v2.15.7

- Resolves an issue where errors running `fossa report` and `fossa test` would be made more confusing when the project isn't a monorepo project ([#321](https://github.com/fossas/spectrometer/pull/321))
- Prevents uploading standard analysis results to monorepo projects, where they'd be silently ignored ([#341](https://github.com/fossas/spectrometer/pull/341))

## v2.15.6

- CocoaPods: Fixes `Podfile.lock` parsing. It safely parses when Pod and Dependencies entries are enclosed with quotations. ([#337](https://github.com/fossas/spectrometer/pull/337))

## v2.15.5

- Fixes an issue where `--json` would output the raw project ID, instead of a normalized ID ([#339](https://github.com/fossas/spectrometer/pull/339))

## v2.15.4

- Gradle: Search parent directories for gradlew and gradlew.bat ([#336](https://github.com/fossas/spectrometer/pull/336))

This release also adds a number of closed beta features around FOSSA C/C++ support.
For now this functionality is considered publicly undocumented, and is only used with support from FOSSA engineering.

As such this new functionality is hidden from the help and other documentation in this repo.
For questions using the new functionality in this release please contact us!

- Support linking user-defined dependency binaries. ([#323](https://github.com/fossas/spectrometer/pull/323))
- Support resolving linked user-defined binaries found in projects when VSI is enabled. ([#328](https://github.com/fossas/spectrometer/pull/328))
- Support linking user project binaries. ([#333](https://github.com/fossas/spectrometer/pull/333))
- Support resolving linked user project binaries found in projects when VSI is enabled. ([#333](https://github.com/fossas/spectrometer/pull/333))

## v2.15.3

- Resolve a scan performance regression for `fossa vps` invocations. ([#335](https://github.com/fossas/spectrometer/pull/335))
- Resolve a scan performance regression for `fossa analyze --experimental-enable-monorepo` invocations. ([#335](https://github.com/fossas/spectrometer/pull/335))

## v2.15.2

- Maven: Fixes an issue where dependencies parsed from `dependency:tree` would fail to resolve when uploaded. ([#332](https://github.com/fossas/spectrometer/pull/332))

## v2.15.1

- Maven: Fixes an issue where dependencies with a platform specifier were not correctly parsed. ([#329](https://github.com/fossas/spectrometer/pull/329))

## v2.15.0

- Dart: Adds support for pub package manager. ([#313](https://github.com/fossas/spectrometer/pull/313))
- Analyzed dependencies now report what file they were found in. ([#316](https://github.com/fossas/spectrometer/pull/316))

## v2.14.5

- Maven: Fixes an issue where projects with `settings.xml` files would not be analyzed correctly using the `dependency:tree` tactic. ([#327](https://github.com/fossas/spectrometer/pull/327))

## v2.14.4

- Gradle: Fixes an issue where all dependencies would appear as direct. ([#319](https://github.com/fossas/spectrometer/pull/319))

## v2.14.3

- Monorepo: archive expansion now respects `--exclude-path` and `--only-path`. ([#320](https://github.com/fossas/spectrometer/pull/320))

## v2.14.2

- Maven: `mvn dependency:tree` now correctly cleans up temporary files after an exception, and correctly uses `settings.xml` when available. ([#318](https://github.com/fossas/spectrometer/pull/318))

## v2.14.1

- Expanded proprietary language snippets in monorepo scans. ([#317](https://github.com/fossas/spectrometer/pull/317))

## v2.13.1

- Adds support for a new Maven tactic that produces the full dependency graph if `mvn dependency:tree` is available but the plugin is not. ([#310](https://github.com/fossas/spectrometer/pull/287))

## v2.13.0

- Elixir: Adds support for Elixir projects using `mix`. ([#287](https://github.com/fossas/spectrometer/pull/287))

## v2.12.3

- Gradle: Fixes an issue where unresolvable Gradle configurations would cause Gradle analysis to show no dependencies ([#292](https://github.com/fossas/spectrometer/pull/292)).

## v2.12.2

- Python: Fixes an issue where older Poetry lockfiles were not correctly identified. ([#309](https://github.com/fossas/spectrometer/pull/309))

## v2.12.1

- VPS: Adds `--exclude-path` and `--only-path` to monorepo functionality in `fossa analyze`. ([#291](https://github.com/fossas/spectrometer/pull/291))
- VPS: Support globs in `--{exclude,only}-path` flags. ([#291](https://github.com/fossas/spectrometer/pull/291))

## v2.12.0

- Python: Adds support for the Poetry package manager. ([#300](https://github.com/fossas/spectrometer/pull/300))

## v2.11.1

- Perl: Adds support for CPAN dependencies in `fossa-deps`. ([#296](https://github.com/fossas/spectrometer/pull/296))

## v2.11.0

- Adds support for selecting which folders analysis targets are discovered in. ([#273](https://github.com/fossas/spectrometer/pull/273))
- VPS: Adds support for `fossa test` and `fossa report` for monorepo projects. ([#290](https://github.com/fossas/spectrometer/pull/290))
- Maven: Adds support for `${property}` substitution for `<groupId>` and `<artifactId>` fields in dependencies. ([#282](https://github.com/fossas/spectrometer/pull/282))

## v2.10.3

- Adds support for specifying a release group on project creation. ([#283](https://github.com/fossas/spectrometer/pull/283))
- Adds support for non-HTTPS backends for archive uploads (e.g. for on-premises deployments). ([#276](https://github.com/fossas/spectrometer/pull/276))
- Adds `--experimental-enable-monorepo` and other associated flags to `fossa analyze`, which enables experimental monorepo support. ([#286](https://github.com/fossas/spectrometer/pull/286))
- Deprecates `fossa vps` subcommands. ([#286](https://github.com/fossas/spectrometer/pull/286))

## v2.10.2

- Fixes an issue where some `fossa` commands (including `fossa test`) would exit non-zero on success. ([#278](https://github.com/fossas/spectrometer/pull/278)).

## v2.10.1

- Fixes an issue where `fossa container analyze` exited zero on failure. ([#275](https://github.com/fossas/spectrometer/pull/275))

## v2.10.0

- Adds support for short flags. ([#264](https://github.com/fossas/spectrometer/pull/264))
- Adds a `remote-dependencies` section in the `fossa-deps` file to support archives at specific URLs. ([#260](https://github.com/fossas/spectrometer/pull/260))
- Renames some fields for `custom-dependencies` to avoid confusion. ([#260](https://github.com/fossas/spectrometer/pull/260))

## v2.9.2

- Adds JSON-formatted project information to the output of `fossa analyze` with `--json`. ([#255](https://github.com/fossas/spectrometer/pull/255))

## v2.9.1

- VPS: Bump wiggins - Updated `vps aosp-notice-file` subcommand to upload ninja files & trigger async task. ([#272](https://github.com/fossas/spectrometer/pull/272))

## v2.9.0

- Fixes an issue where stdout doesn't always flush to the console. ([#265](https://github.com/fossas/spectrometer/pull/265))
- Fixes an issue when referenced-dependencies are not being uploaded. ([#262](https://github.com/fossas/spectrometer/pull/262))
- Adds support for `fossa-deps.json`. ([#261](https://github.com/fossas/spectrometer/pull/261))
- Adds support for `vendored-dependencies` to be license scanned. ([#257](https://github.com/fossas/spectrometer/pull/257))

## v2.8.0

- Adds support for `--branch` flag on `fossa container analyze` command. ([#253](https://github.com/fossas/spectrometer/pull/253))
- Adds support and documentation for user-defined dependencies. ([#245](https://github.com/fossas/spectrometer/pull/245))
- Allows using `.yml` or `.yaml` extensions for `fossa-deps` file, but not both. ([#245](https://github.com/fossas/spectrometer/pull/245))
- `fossa analyze` now checks `fossa-deps` before running analysis (instead of checking in parallel with other analyses). ([#245](https://github.com/fossas/spectrometer/pull/245))

## v2.7.2

- VSI: Updates the VSI Plugin.
- VSI: Adds support for VSI powered dependency discovery as a strategy.

## v2.7.1

- Re-enables status messages for commands like `fossa test` in non-ANSI environments. ([#248](https://github.com/fossas/spectrometer/pull/248))
- Yarn: Adds support for Yarn v2 lockfiles. ([#244](https://github.com/fossas/spectrometer/pull/244))
- NuGet: Fixes the dependency version parser for `.csproj`, `.vbproj`, and similar .NET files. ([#247](https://github.com/fossas/spectrometer/pull/247))

## v2.7.0

- Conda: Adds support for the Conda package manager. ([#226](https://github.com/fossas/spectrometer/pull/226))

## v2.6.1

- VPS: Adds `--follow` to the `vps analyze` subcommand, which allows for following symbolic links during VPS scans. ([#243](https://github.com/fossas/spectrometer/pull/243))

## v2.6.0

- Display the progress of `fossa analyze` while running. ([#239](https://github.com/fossas/spectrometer/pull/239))

## v2.5.18

- NPM: Fixes issue where transitive dependencies could be missing in NPM projects. ([#240](https://github.com/fossas/spectrometer/pull/240))

## v2.5.17

- Containers: Fixes an issue where `--project` and `--revision` were not correctly handled in `fossa container analyze`. ([#238](https://github.com/fossas/spectrometer/pull/238))

## v2.5.16

- Adds support for `fossa-deps.yml`. ([#236](https://github.com/fossas/spectrometer/pull/236))

## v2.5.15

- Python: Fixes an issue where parsing unsupported fields in `requirements.txt` could prevent Python analyses from terminating. ([#235](https://github.com/fossas/spectrometer/pull/235))

## v2.5.14

- Go: Upload module identifiers instead of package identifiers to the backend. ([#234](https://github.com/fossas/spectrometer/pull/234))

## v2.5.13

- VPS: Update VPS plugin to `2021-04-27-312bbe8`. ([#233](https://github.com/fossas/spectrometer/pull/233))
  - Improve performance of scanning projects
  - Reduce memory pressure when scanning large projects

## v2.5.12

- VPS: Update VPS plugin to `2021-04-19-9162a26`. ([#231](https://github.com/fossas/spectrometer/pull/231))

## v2.5.11

- Allow flags to be set via configuration file. ([#220](https://github.com/fossas/spectrometer/pull/220))
- Containers: add support for layers. ([#228](https://github.com/fossas/spectrometer/pull/228))

## v2.5.10

- Only activate replay/record mode using `--replay`/`--record` (previously it was turned on in `--debug` mode). ([#212](https://github.com/fossas/spectrometer/pull/212))
- Containers: Fixed a bug where container scanning failed when ignored artifacts aren't in the right shape. ([#223](https://github.com/fossas/spectrometer/pull/223))

## v2.5.9

- VPS: Update the VPS scanning plugin:
  - Resolve issues reading IPR files with null byte content.
  - Workaround recursive variable declarations when parsing Android.mk files.

## v2.5.8

- VPS: Support makefiles in `fossa vps aosp-notice-file`. ([#216](https://github.com/fossas/spectrometer/pull/216))
- VPS: Require paths to ninja files as arguments in `fossa vps aosp-notice-file`. ([#217](https://github.com/fossas/spectrometer/pull/217))

## v2.5.7

- VPS: Print project URL after `fossa vps analyze`. ([#215](https://github.com/fossas/spectrometer/pull/215))

## v2.5.6

- Gradle: Fixes an issue that sometimes prevented Gradle project analyses from terminating. ([#211](https://github.com/fossas/spectrometer/pull/211))

## v2.5.5

- PHP: Fixes an issue where Composer lockfiles could cause a crash when parsing. ([#207](https://github.com/fossas/spectrometer/pull/207))

## v2.5.4

- Scala: Fixes an issue that sometimes prevented Scala analyses from terminating. ([#206](https://github.com/fossas/spectrometer/pull/187))

## v2.5.0

- Containers: Add container analysis toolchain. ([#173](https://github.com/fossas/spectrometer/pull/173))

## v2.4.11

- Fixes several issues that caused analysis failures during upload. ([#187](https://github.com/fossas/spectrometer/pull/187), [#188](https://github.com/fossas/spectrometer/pull/188))

## v2.4.9

- Python: Fixes an issue with `requirements.txt` parsing line extensions. ([#183](https://github.com/fossas/spectrometer/pull/183))
- Fixes an issue where we didn't read the cached revision when picking a revision for `fossa test` in projects without VCS. ([#182](https://github.com/fossas/spectrometer/pull/182))
- Fixes an issue where invalid project URLs would be printed for projects without VCS when `--branch` was not specified. ([#181](https://github.com/fossas/spectrometer/pull/181))

## v2.4.8

- Introduce a new hidden `fossa compatibility` command which runs fossa v1 `fossa analyze` and allows users to access the archive uploader. ([#179](https://github.com/fossas/spectrometer/pull/179))

## v2.4.7

- Fixes an issue where `fossa test` would always exit zero for push-only API keys. ([#170](https://github.com/fossas/spectrometer/pull/170))
- Fixes an issue where dependency graphs would be filtered out if they had no direct dependencies (e.g. in strategies like Yarn where direct dependencies are unknown). ([#172](https://github.com/fossas/spectrometer/pull/172))
- Go: Fixes an issue with `glide.lock` parser. ([#175](https://github.com/fossas/spectrometer/pull/175))
- Go: Adds multi-module project support to `go.mod` static analysis. ([#171](https://github.com/fossas/spectrometer/pull/171))
- NPM, Yarn: Fixes an issue where subdirectories were erroneously ignored. ([#174](https://github.com/fossas/spectrometer/pull/174))

## v2.4.6

- VPS: Update Wiggins CLI plugin to version `2020-12-11-5d581ea`

## v2.4.5

- VPS: Update `fossa vps analyze` to use a new VPS project scanning engine:
  - Improve scan performance
  - Support "License Only" scans, where the project is scanned for licenses but is not inspected for vendored dependencies.

## v2.4.4

- Maven: Add limited support for POM `${property}` interpolation. ([#158](https://github.com/fossas/spectrometer/pull/158))

## v2.4.3

- Adds `--version` flag. ([#157](https://github.com/fossas/spectrometer/pull/157))

## v2.4

- RPM: Adds support for unpacking of gzipped RPMs. ([#154](https://github.com/fossas/spectrometer/pull/154))
- VPS: Integrates `vpscli scan` as `fossa vps analyze`. ([#148](https://github.com/fossas/spectrometer/pull/148))
- VPS: Removes `vpscli` binary. ([#148](https://github.com/fossas/spectrometer/pull/148))
- VPS: Adds support for `--team` and other metadata flags to VPS analysis. ([#149](https://github.com/fossas/spectrometer/pull/149))
- VPS: Adds `fossa vps test` command, analogous to `fossa test` for VPS projects. ([#150](https://github.com/fossas/spectrometer/pull/150))
- VPS: Adds `fossa vps report` command, analogous to `fossa report` for VPS projects. ([#150](https://github.com/fossas/spectrometer/pull/150))

## v2.3.2

- Adds `fossa list-targets` to list "analysis targets" (projects and subprojects) available for analysis. ([#140](https://github.com/fossas/spectrometer/pull/140))
- Adds `--filter TARGET` option to `fossa analyze`. ([#140](https://github.com/fossas/spectrometer/pull/140))
- Adds support for "detached HEAD" state in `git` and `svn`. ([#141](https://github.com/fossas/spectrometer/pull/141))
- Python: Dependencies found via `*req*.txt` and `setup.py` are now merged. ([#140](https://github.com/fossas/spectrometer/pull/140))
- Maven: Natively support multi-POM Maven projects. ([#140](https://github.com/fossas/spectrometer/pull/140))
- Gradle: Fixes an issue where subprojects were not handled correctly. ([#140](https://github.com/fossas/spectrometer/pull/140))

## v2.3.1

- RPM: Dependencies from multiple `*.spec` files in the same directory are now merged. ([#138](https://github.com/fossas/spectrometer/pull/138))
- Erlang: Aliased packages in `rebar3` are now resolved to their true names. ([#139](https://github.com/fossas/spectrometer/pull/139))
- Gradle: Support all build configurations (instead of a hard-coded list of known configuration names). ([#134](https://github.com/fossas/spectrometer/pull/134))

## v2.3.0

- Erlang: Fixes an issue where the `rebar3` strategy would incorrectly identify dependencies as top-level projects. ([#119](https://github.com/fossas/spectrometer/pull/119))
- Python: Fixes various issues in the `setup.py` parser. ([#119](https://github.com/fossas/spectrometer/pull/119))
- Haskell: Adds support for Haskell projects using `cabal-install`. ([#122](https://github.com/fossas/spectrometer/pull/122))
- PHP: Adds support for PHP projects using `composer`. ([#121](https://github.com/fossas/spectrometer/pull/121))

## v2.2.4

- Scala: Adds support for Scala projects using `sbt`. ([#54](https://github.com/fossas/spectrometer/pull/54))

## v2.2.1

- Python: Fixes an issue where the `req.txt` strategy would run even when no relevant files were present. ([#109](https://github.com/fossas/spectrometer/pull/109))

## v2.2.0

- Improves contributor counting accuracy using repository metadata. ([#94](https://github.com/fossas/spectrometer/pull/94))
- Improves parallelism of strategy discovery. ([#93](https://github.com/fossas/spectrometer/pull/93))
- Fixes an issue where URLs printed by `fossa test` and other commands were incorrect for `git` projects with `https` remotes. ([#92](https://github.com/fossas/spectrometer/pull/92))
- Fixes an issue where `IOException`s (like "command not found") would cause strategies to crash. ([#106](https://github.com/fossas/spectrometer/pull/106))
- Fixes an issue where with effect typechecking. ([#100](https://github.com/fossas/spectrometer/pull/100))
- Python: Dependencies of multiple `*req*.txt` files in a single project are now merged. ([#102](https://github.com/fossas/spectrometer/pull/102))
- Go: Re-enables deep dependency reporting (which was previously disabled for development purposes). ([#98](https://github.com/fossas/spectrometer/pull/98))
- NuGet: Adds support for analyzing `paket.lock` files. ([#107](https://github.com/fossas/spectrometer/pull/107))
