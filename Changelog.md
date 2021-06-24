# Spectrometer Changelog

## Unreleased

- Fix an issue when referenced-dependencies are not being uploaded ([#262](https://github.com/fossas/spectrometer/pull/262))
- Adds support for `fossa-deps.json` ([#261](https://github.com/fossas/spectrometer/pull/261))
- Adds support for `vendored-dependencies` to be licensed scanned ([#257](https://github.com/fossas/spectrometer/pull/257))

## 2.8.0

- Adds support for `--branch` flag on `fossa container analyze` command ([#253](https://github.com/fossas/spectrometer/pull/253))
- Adds support and documentation for user-defined dependencies ([#245](https://github.com/fossas/spectrometer/pull/245))
- Allows using `.yml` or `.yaml` extensions for `fossa-deps` file, but not both ([#245](https://github.com/fossas/spectrometer/pull/245))
- `fossa-deps` file is checked before running discovery/analysis, and is no longer run in parallel with other analysis functions ([#245](https://github.com/fossas/spectrometer/pull/245))

## 2.7.2

- Updates the VSI Plugin.
- Adds support for VSI powered dependency discovery as a strategy.

## 2.7.1

- Adds support for Yarn v2 lockfiles ([#244](https://github.com/fossas/spectrometer/pull/244))
- Fixes the dependency version parser for `.csproj`, `.vbproj`, and similar .NET files ([#247](https://github.com/fossas/spectrometer/pull/247))
- Re-enables status messages for commands like `fossa test` in CI environments ([#248](https://github.com/fossas/spectrometer/pull/248))

## v2.7.0

- Adds support for the Conda package manager ([#226](https://github.com/fossas/spectrometer/pull/226))

## v2.6.1

- Adds --follow to the vps analyze subcommand, which allows for following symbolic links during VPS scans. ([#243](https://github.com/fossas/spectrometer/pull/243))

## v2.6.0

- Improves the output of `fossa analyze` by displaying the status of ongoing Project Discovery and Project Analysis tasks ([#239](https://github.com/fossas/spectrometer/pull/239))

## v2.5.18

- Fixes issue where transitive dependencies could be missing in npm projects ([#240](https://github.com/fossas/spectrometer/pull/240))

## v2.5.17

- Fix `fossa container analyze` `--project` and `--revision` flags ([#238](https://github.com/fossas/spectrometer/pull/238))

## v2.5.16

- Support for manually specified dependencies through `fossa-deps.yaml` ([#236](https://github.com/fossas/spectrometer/pull/236))

## v2.5.15

- Requirements.txt: fix hang when parsing unsupported fields ([#235](https://github.com/fossas/spectrometer/pull/235))

## v2.5.14

- Golang: map package imports to modules ([#234](https://github.com/fossas/spectrometer/pull/234))

## v2.5.13

Update VPS plugin to 2021-04-27-312bbe8 ([#233](https://github.com/fossas/spectrometer/pull/233))

- Improve performance of scanning projects
- Reduce memory pressure when scanning large projects

## v2.5.12

Update VPS plugin to 2021-04-19-9162a26 ([#231](https://github.com/fossas/spectrometer/pull/231))

## v2.5.11

- Update container scanning version ([#230](https://github.com/fossas/spectrometer/pull/230))
- Change container scanning layer scope ([#228](https://github.com/fossas/spectrometer/pull/228))
- Initial configuration file ([#220](https://github.com/fossas/spectrometer/pull/220))
- Glide.lock: Parse versions as Text ([#221](https://github.com/fossas/spectrometer/pull/221))
- Add container layers and artifact locations ([#225](https://github.com/fossas/spectrometer/pull/225))
- Tiny hlint/ormolu fixes ([#224](https://github.com/fossas/spectrometer/pull/224))

## v2.5.10

- Freeze dependencies to allow reproducible builds ([#222](https://github.com/fossas/spectrometer/pull/222))
- Add documentation for replay logging ([#212](https://github.com/fossas/spectrometer/pull/212))
- Only activate replay/record mode using --replay/--record (previously it was turned on in --debug mode) ([#212](https://github.com/fossas/spectrometer/pull/212))
- Fixed a bug where container scanning failed when ignored artifacts aren't in the right shape ([#223](https://github.com/fossas/spectrometer/pull/223))

## v2.5.9

- Update the VPS scanning plugin:
  - Resolve issues reading IPR files with null byte content.
  - Workaround recursive variable declarations when parsing Android.mk files.

## v2.5.8

- Support makefiles in `fossa vps aosp-notice-file` ([#216](https://github.com/fossas/spectrometer/pull/216))
- Require paths to ninja files as arguments in `fossa vps aosp-notice-file` ([#217](https://github.com/fossas/spectrometer/pull/217))

## v2.5.7

- Print project URL after `fossa vps analyze` ([#215](https://github.com/fossas/spectrometer/pull/215))

## v2.5.6

- Fixes an issue that could cause gradle project analysis to hang forever ([#211](https://github.com/fossas/spectrometer/pull/211))

## v2.5.5

- Fixes an issue where Composer lockfiles could cause a crash when parsing ([#207](https://github.com/fossas/spectrometer/pull/207))

## v2.5.4

- Fix an issue that could prevent scala analysis from terminating ([#206](https://github.com/fossas/spectrometer/pull/187))

## v2.5.0

- Add container analysis toolchain

## v2.4.11

- Fixes for a couple of issues that caused analysis failures during upload ([#187](https://github.com/fossas/spectrometer/pull/187)/[#188](https://github.com/fossas/spectrometer/pull/188))

## v2.4.9

- Fix a bug with `requirements.txt` parsing line extensions ([#183](https://github.com/fossas/spectrometer/pull/183))
- Fix a bug where we didn't read the cached fossa revision for projects without VCS ([#182](https://github.com/fossas/spectrometer/pull/182))
- Fix a bug with project URL output when no branch is supplied in instances where VCS does not exist ([#181](https://github.com/fossas/spectrometer/pull/181))

## v2.4.8

- Introduce a new hidden `fossa compatibility` command which runs fossa v1 `fossa analyze` and allows users to access the archive uploader([#179](https://github.com/fossas/spectrometer/pull/179))

## v2.4.7

- Fixes an issue where `fossa test` would always succeed for push-only API keys ([#170](https://github.com/fossas/spectrometer/pull/170))
- Fixes an issue with glide.lock parser ([#175](https://github.com/fossas/spectrometer/pull/175))
- Fixes an issue where subdirectories were erroneously ignored ([#174](https://github.com/fossas/spectrometer/pull/174))
- Fixes an issue where dependency graphs would be filtered out if they had no direct dependencies ([#172](https://github.com/fossas/spectrometer/pull/172))
- Adds multi-module project support to gomodules static analysis ([#171](https://github.com/fossas/spectrometer/pull/171))

## v2.4.6

- Update Wiggins CLI plugin to version `2020-12-11-5d581ea`

## v2.4.5

- Update `fossa vps analyze` to use a new VPS project scanning engine:
  - Improve scan performance
  - Support "License Only" scans, where the project is scanned for licenses but is not inspected for vendored dependencies.

## v2.4.4

- Improves maven pom `${property}` interpolation ([#158](https://github.com/fossas/spectrometer/pull/158))

## v2.4.3

- Adds `--version` flag ([#157](https://github.com/fossas/spectrometer/pull/157))

## v2.4

- Integrates `vpscli scan` as `fossa vps analyze` ([#148](https://github.com/fossas/spectrometer/pull/148))
- Removes `vpscli` binary ([#148](https://github.com/fossas/spectrometer/pull/148))
- Adds support for `--team` and other metadata flags to vps analysis ([#149](https://github.com/fossas/spectrometer/pull/149))
- Adds `fossa vps test` command, analogous to `fossa test` for vps projects ([#150](https://github.com/fossas/spectrometer/pull/150))
- Adds `fossa vps report` command, analogous to `fossa report` for vps projects ([#150](https://github.com/fossas/spectrometer/pull/150))
- Adds support for unpacking of gzipped RPMs ([#154](https://github.com/fossas/spectrometer/pull/154))

## v2.3.2

- Adds `fossa list-targets` to list "analysis-targets" (projects and subprojects) available for analysis ([#140](https://github.com/fossas/spectrometer/pull/140))
- Adds `--filter TARGET` option to `fossa analyze` ([#140](https://github.com/fossas/spectrometer/pull/140))
- Merges the dependencies of `*req*.txt` and `setup.py` files we find ([#140](https://github.com/fossas/spectrometer/pull/140))
- Improves maven project discovery ([#140](https://github.com/fossas/spectrometer/pull/140))
- Fixes gradle wrapper integration ([#140](https://github.com/fossas/spectrometer/pull/140))
- Adds support for "detached HEAD" state in git and svn ([#141](https://github.com/fossas/spectrometer/pull/141))

## v2.3.1

- RPM: Merge spec file results in the analyzer. ([#138](https://github.com/fossas/spectrometer/pull/138))
- Erlang: Resolve rebar3 aliased packages to their true names. ([#139](https://github.com/fossas/spectrometer/pull/139))
- Gradle: Accept and tag all build configuration names. ([#134](https://github.com/fossas/spectrometer/pull/134))

## v2.3.0

- Adds a user guide
- Fixes bug where the rebar3 strategy would incorrectly find dependencies as top-level projects ([#119](https://github.com/fossas/spectrometer/pull/119))
- Fixes various issues in the setup.py parser ([#119](https://github.com/fossas/spectrometer/pull/119))
- Adds an analyzer for haskell projects using cabal-install ([#122](https://github.com/fossas/spectrometer/pull/122))
- Adds an analyzer for PHP projects via composer ([#121](https://github.com/fossas/spectrometer/pull/121))

## v2.2.4

- Adds analyzer for scala via `sbt` ([#54](https://github.com/fossas/spectrometer/pull/54))

## v2.2.1

- Fixes bug where the req.txt strategy would run even when no relevant files were present ([#109](https://github.com/fossas/spectrometer/pull/109))

## v2.2.0

- Fixes `fossa test` and project links for git projects with `https` remotes ([#92](https://github.com/fossas/spectrometer/pull/92))

- Fixes strategy failures related to command-not-found errors ([#106](https://github.com/fossas/spectrometer/pull/106))

- Merges the dependencies of `*req*.txt` files we find ([#102](https://github.com/fossas/spectrometer/pull/102))

- Re-enables deep dependency gathering for golang projects ([#98](https://github.com/fossas/spectrometer/pull/98))

- Fixes directory skipping (e.g., `node_modules`) ([#100](https://github.com/fossas/spectrometer/pull/100))

- Adds CLI-side support for contributor counting ([#94](https://github.com/fossas/spectrometer/pull/94))

- Enables paket.lock strategy ([#107](https://github.com/fossas/spectrometer/pull/107))

- Improves parallelism of strategy discovery ([#93](https://github.com/fossas/spectrometer/pull/93))
