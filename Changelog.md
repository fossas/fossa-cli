# v2.5.0
- Add container analysis toolchain

# v2.4.11

- Fixes for a couple of issues that caused analysis failures during upload ([#187](https://github.com/fossas/spectrometer/pull/187)/[#188](https://github.com/fossas/spectrometer/pull/188))

# v2.4.9
- Fix a bug with `requirements.txt` parsing line extensions ([#183](https://github.com/fossas/spectrometer/pull/183))
- Fix a bug where we didn't read the cached fossa revision for projects without VCS ([#182](https://github.com/fossas/spectrometer/pull/182))
- Fix a bug with project URL output when no branch is supplied in instances where VCS does not exist ([#181](https://github.com/fossas/spectrometer/pull/181))

# v2.4.8
- Introduce a new hidden `fossa compatibility` command which runs fossa v1 `fossa analyze` and allows users to access the archive uploader([#179]https://github.com/fossas/spectrometer/pull/179)

# v2.4.7

- Fixes an issue where `fossa test` would always succeed for push-only API keys ([#170](https://github.com/fossas/spectrometer/pull/170))
- Fixes an issue with glide.lock parser ([#175](https://github.com/fossas/spectrometer/pull/175))
- Fixes an issue where subdirectories were erroneously ignored ([#174](https://github.com/fossas/spectrometer/pull/174))
- Fixes an issue where dependency graphs would be filtered out if they had no direct dependencies ([#172](https://github.com/fossas/spectrometer/pull/172))
- Adds multi-module project support to gomodules static analysis ([#171](https://github.com/fossas/spectrometer/pull/171))

# v2.4.6

- Update Wiggins CLI plugin to version `2020-12-11-5d581ea`

# v2.4.5

- Update `fossa vps analyze` to use a new VPS project scanning engine: 
  - Improve scan performance
  - Support "License Only" scans, where the project is scanned for licenses but is not inspected for vendored dependencies.

# v2.4.4

- Improves maven pom `${property}` interpolation ([#158](https://github.com/fossas/spectrometer/pull/158))

# v2.4.3

- Adds `--version` flag ([#157](https://github.com/fossas/spectrometer/pull/157))

# v2.4

- Integrates `vpscli scan` as `fossa vps analyze` ([#148](https://github.com/fossas/spectrometer/pull/148))
- Removes `vpscli` binary ([#148](https://github.com/fossas/spectrometer/pull/148))
- Adds support for `--team` and other metadata flags to vps analysis ([#149](https://github.com/fossas/spectrometer/pull/149))
- Adds `fossa vps test` command, analogous to `fossa test` for vps projects ([#150](https://github.com/fossas/spectrometer/pull/150))
- Adds `fossa vps report` command, analogous to `fossa report` for vps projects ([#150](https://github.com/fossas/spectrometer/pull/150))
- Adds support for unpacking of gzipped RPMs ([#154](https://github.com/fossas/spectrometer/pull/154))

# v2.3.2

- Adds `fossa list-targets` to list "analysis-targets" (projects and subprojects) available for analysis ([#140](https://github.com/fossas/spectrometer/pull/140))
- Adds `--filter TARGET` option to `fossa analyze` ([#140](https://github.com/fossas/spectrometer/pull/140))
- Merges the dependencies of `*req*.txt` and `setup.py` files we find ([#140](https://github.com/fossas/spectrometer/pull/140))
- Improves maven project discovery ([#140](https://github.com/fossas/spectrometer/pull/140))
- Fixes gradle wrapper integration ([#140](https://github.com/fossas/spectrometer/pull/140))
- Adds support for "detached HEAD" state in git and svn ([#141](https://github.com/fossas/spectrometer/pull/141))

# v2.3.1

- RPM: Merge spec file results in the analyzer. ([#138](https://github.com/fossas/spectrometer/pull/138))
- Erlang: Resolve rebar3 aliased packages to their true names. ([#139](https://github.com/fossas/spectrometer/pull/139))
- Gradle: Accept and tag all build configuration names. ([#134](https://github.com/fossas/spectrometer/pull/134))

# v2.3.0

- Adds a user guide
- Fixes bug where the rebar3 strategy would incorrectly find dependencies as top-level projects ([#119](https://github.com/fossas/spectrometer/pull/119))
- Fixes various issues in the setup.py parser ([#119](https://github.com/fossas/spectrometer/pull/119))
- Adds an analyzer for haskell projects using cabal-install ([#122](https://github.com/fossas/spectrometer/pull/122))
- Adds an analyzer for PHP projects via composer ([#121](https://github.com/fossas/spectrometer/pull/121))

# v2.2.4

- Adds analyzer for scala via `sbt` ([#54](https://github.com/fossas/spectrometer/pull/54))

# v2.2.1

- Fixes bug where the req.txt strategy would run even when no relevant files were present ([#109](https://github.com/fossas/spectrometer/pull/109))

# v2.2.0

- Fixes `fossa test` and project links for git projects with `https` remotes ([#92](https://github.com/fossas/spectrometer/pull/92))

- Fixes strategy failures related to command-not-found errors ([#106](https://github.com/fossas/spectrometer/pull/106))

- Merges the dependencies of `*req*.txt` files we find ([#102](https://github.com/fossas/spectrometer/pull/102))

- Re-enables deep dependency gathering for golang projects ([#98](https://github.com/fossas/spectrometer/pull/98))

- Fixes directory skipping (e.g., `node_modules`) ([#100](https://github.com/fossas/spectrometer/pull/100))

- Adds CLI-side support for contributor counting ([#94](https://github.com/fossas/spectrometer/pull/94))

- Enables paket.lock strategy ([#107](https://github.com/fossas/spectrometer/pull/107))

- Improves parallelism of strategy discovery ([#93](https://github.com/fossas/spectrometer/pull/93))
