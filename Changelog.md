# FOSSA CLI Changelog

## Unreleased

- container scanning: fixes a defect which led to incorrect `NotTarFormat` errors when parsing container layer. ([#1305](https://github.com/fossas/fossa-cli/pull/1305))

## v3.8.18

- golang: Updates go.mod parser to be compatible with golang v1.21. ([#1304](https://github.com/fossas/fossa-cli/pull/1304))
- `fossa list-targets`: list-target command supports `--format` option with: `ndjson`, `text`, and `legacy`. ([#1296](https://github.com/fossas/fossa-cli/pull/1296))

## v3.8.17

Integrates FOSSA snippet scanning into the main application.
For more details and a quick start guide, see [the subcommand reference](./docs/references/subcommands/snippets.md).

## v3.8.16

Delivers another update to the `millhone` early preview of FOSSA snippet scanning:

- Fixes surprising semantics in some subcommands, especially `commit`.
- Sorts and makes unique dependencies written to `fossa-deps` files.
- Overly noisy snippets are filtered entirely.
- Adds C++ snippet parsing.
- Reduces config and logging verbosity.

## v3.8.15

This version is a special release: it does not alter anything in FOSSA CLI, but instead adds `millhone`,
the new snippet scanning functionality for FOSSA, as a release asset.

Future releases will bundle this functionality into FOSSA CLI instead,
but we're making this CLI available standalone for now to enable immediate use!

Initial documentation for this functionality is here.
When we integrate this functionality into FOSSA CLI itself we'll have improved documentation as well.

Note: FOSSA is still ingesting sources into the snippet scanning database;
while this CLI is available earlier results will steadily improve as we crawl more sources.

## v3.8.14

- Custom License Searches and Keyword Searches allow you to search through your codebase, find matches to regular expressions and then either log the results to the scan summary (keyword search) or create a custom license match (custom license searches) ([#1274](https://github.com/fossas/fossa-cli/pull/1274))

## v3.8.13
- Maven: Prevent infinite recursion from Pom file property interpolation. ([#1271](https://github.com/fossas/fossa-cli/pull/1271))

## v3.8.12
- Conda: Support simple Pip packages in `environment.yml`. ([#1275](https://github.com/fossas/fossa-cli/pull/1275))

## v3.8.11
- Maven analysis: Prevent maven analysis from infinitely recursing when it encounters a recursive property ([#1268](https://github.com/fossas/fossa-cli/pull/1268))

## v3.8.10
- Reports: Can now export reports formatted as CycloneDX (json/xml), CSV, HTML, and JSON SPDX. ([#1266](https://github.com/fossas/fossa-cli/pull/1266))
- Containers: RPM packages installed in containers that use the NDB format for their RPM database are now parsed much faster. ([#1262](https://github.com/fossas/fossa-cli/pull/1262))

## v3.8.9
- CLI Binaries: Notarize Mac OS binaries. ([#1261](https://github.com/fossas/fossa-cli/pull/1261))

## v3.8.8
- CLI Binaries: Sign Mac OS builds using codesign. ([#1251](https://github.com/fossas/fossa-cli/pull/1251))
- CLI Binaries: Sign Linux builds using cosign. ([#1243](https://github.com/fossas/fossa-cli/pull/1243))

## v3.8.7
- Due to an issue with our release process [#1254](https://github.com/fossas/fossa-cli/pull/1254), this tag exists but was not released. The changes that would have been in 3.8.7 were released as v3.8.8.

## v3.8.6
- VSI: Fix a bug where root dependencies would cause analysis to fail. ([#1240](https://github.com/fossas/fossa-cli/pull/1240))
- Node (PNPM): Fixes a bug where analyses would fail when the `lockfileVersion` attribute was a string in `pnpm-lock.yaml`. ([1239](https://github.com/fossas/fossa-cli/pull/1239))
- License Scanning: Add a new "IBM type1 interpreter" license (no PR).

## v3.8.5
- Go: `--experimental-use-v3-go-resolver` is now the default. ([Documentation](./docs/references/strategies/languages/golang/v3-go-resolver-transition-qa.md). ([1224](https://github.com/fossas/fossa-cli/pull/1224))

## v3.8.4
- VSI: Report VSI rules and display them in FOSSA's UI. ([#1237](https://github.com/fossas/fossa-cli/pull/1237), [#1235](https://github.com/fossas/fossa-cli/pull/1235))

## v3.8.3
- Logging: Don't output the `[INFO]` prefix for regular CLI messages. ([#1226](https://github.com/fossas/fossa-cli/pull/1226))
- License Scanning: Fix a bug where we were identifying the "GPL with autoconf macro exception" license as "GPL with autoconf exception" in a few cases ([#1225](https://github.com/fossas/fossa-cli/pull/1225))
- Container Scanning: More resiliant os-release parser, accounting initial line comments in the file ([#1230](https://github.com/fossas/fossa-cli/pull/1230))
- Analysis: full paths to the files in archives are shown when running `fossa analyze --unpack-archives` ([#1231](https://github.com/fossas/fossa-cli/pull/1231))
- Telemetry: Collect GNU/Linux distribution information and `uname` output. ([#1222](https://github.com/fossas/fossa-cli/pull/1222))

## v3.8.2
- Poetry: Defaults `category` to `main` if not present in lockfile. ([#1211](https://github.com/fossas/fossa-cli/pull/1211))
- Maven: Revert ([#1218](https://github.com/fossas/fossa-cli/pull/1218)) from v3.8.2 due to performance impacts.

## v3.8.1
- Setup.py: Fixes an defect with `setup.py` parser, caused by failing to account for line comments or backslash. ([#1191](https://github.com/fossas/fossa-cli/pull/1191))
- Installation: `install-latest.sh` now directs `curl` and `wget` to pass `Cache-Control: no-cache` headers to the server. ([#1206](https://github.com/fossas/fossa-cli/pull/1206))
- `Go.mod`: Anaysis does not fail if `go.mod` includes `retract` block. ([#1213](https://github.com/fossas/fossa-cli/pull/1213))
- `.aar`: Supports `.aar` archive files with native license scanning, and with `--unpack-archives` option. ([#1217](https://github.com/fossas/fossa-cli/pull/1217))
- `remote-dependencies`: Analysis of `fossa-deps` fails, if remote-dependencies's character length is greater than maximum. It only applies during non-output mode. ([#1216](https://github.com/fossas/fossa-cli/pull/1216))
- Maven: Analyze a package separately from its parents if the module does not appear in its parent's `<modules>` tag when both the module and its parents are discovered as candidate targets. ([#1218](https://github.com/fossas/fossa-cli/pull/1218))
- Network requests: `fossa-cli` retries network requests which return response with status code of 502. ([#1220](https://github.com/fossas/fossa-cli/pull/1220))
- `PDM`: Adds support for PDM package manager. ([#1214](https://github.com/fossas/fossa-cli/pull/1214))

## v3.8.0
- License Scanning: You can license scan your first-party code with the `--experimental-force-first-party-scans` flag ([#1187](https://github.com/fossas/fossa-cli/pull/1187))
- Network requests: `fossa-cli` retries network requests, if it experiences timeout error. ([#1203](https://github.com/fossas/fossa-cli/pull/1203))
- Monorepo is no longer a supported feature of FOSSA. ([#1202](https://github.com/fossas/fossa-cli/pull/1202))
- `experimental-enable-binary-discovery`, `detect-vendored`: Redact file contents in debug bundles. ([#1201](https://github.com/fossas/fossa-cli/pull/1201))
- `setup.cfg`: Adds support for setup.cfg, in conjuction with `setup.py`. ([#1195](https://github.com/fossas/fossa-cli/pull/1195))
- Default Filters: Default filters are applied prior to analysis. Improves overall runtime performance. ([#1193](https://github.com/fossas/fossa-cli/pull/1194))
- `.fossa.yml` and CLI args: Allow setting a policy by id in addition to by name. ([#1203](https://github.com/fossas/fossa-cli/pull/1203))
- Doc only: Fixed an issue in the `fossa-deps` schema suggesting against the use of `name` for referenced RPM dependencies. If your editor utilizes SchemaStore, this file should now lint properly after this change propagates. ([#1199](https://github.com/fossas/fossa-cli/pull/1199)).

## v3.7.11
- `fossa-deps.yml`: Adds strict parsing to so that required field with only whitespace strings are prohibited early. Also throws an error, if incompatible character is used in vendor dependency's version field. ([#1192](https://github.com/fossas/fossa-cli/pull/1192))

## v3.7.10
- License Scanning: Fix a bug where the license scanner did not run on MacOS 13 on M1 Macs ([#1193](https://github.com/fossas/fossa-cli/pull/1193))
- Debug bundle: The raw dependency graph FOSSA CLI discovers is output in the FOSSA Debug Bundle. ([#1188](https://github.com/fossas/fossa-cli/pull/1188))

## v3.7.9
- License Scanning: Add support for "full file uploads" for CLI-side license scans. ([#1181](https://github.com/fossas/fossa-cli/pull/1181))

## v3.7.8
- Go: Do not fall back to module based analysis when using `--experimental-use-go-v3-resolver`. ([#1184](https://github.com/fossas/fossa-cli/pull/1184))

## v3.7.7
- Adds `--json` flag to `fossa container analyze` ([#1180](https://github.com/fossas/fossa-cli/pull/1180))
- License Scanning: Reduce false positives caused by indicator matches. This is done by only reporting indicator matches to SPDX keys and license names when we are scanning a manifest file ([#1182](https://github.com/fossas/fossa-cli/pull/1182))

## v3.7.6
- RPM: Support origin paths for RPM spec file analysis ([#1178](https://github.com/fossas/fossa-cli/pull/1178))
- Swift: Do not stop analysis if we encounter a badly formatted project.pbxproj file ([#1177](https://github.com/fossas/fossa-cli/pull/1177))

## v3.7.5
- Go: Introduce `--experimental-use-v3-go-resolver` to preview a new [tactic](./docs/references/strategies/languages/golang/gomodules.md#experimental-strategy-use-go-list-on-packages) for Go dependency scanning. ([#1168](https://github.com/fossas/fossa-cli/pull/1168),[#1173](https://github.com/fossas/fossa-cli/pull/1173))
- Themis: Update tag to support a new rule for the libdivide dependency. ([#1172](https://github.com/fossas/fossa-cli/pull/1172)

## v3.7.4
- Gradle: Fix possible ConcurrentModificationException that can occur when getting dependencies ([#1171](https://github.com/fossas/fossa-cli/pull/1171))

## v3.7.3
- Go: Collects environment variables in debug bundle. ([#1132](https://github.com/fossas/fossa-cli/pull/1132))
- Diagnostics: Improves user-facing error messages and debugging tips for external commands and some HTTP error conditions ([#1165](https://github.com/fossas/fossa-cli/pull/1165))
- License Scanning: Scan the full contents of "license.html" and "licence.html" for license content, not just the comments. ([#1169](https://github.com/fossas/fossa-cli/pull/1169))

## v3.7.2
- License Scanning: Add four new licenses: Pushwoosh, PalletsFlaskLogo, IntelDisclaimer and Instabug ([#1163](https://github.com/fossas/fossa-cli/pull/1163))

## v3.7.1
- Stack: Git based dependencies are detected and handled correctly. ([#1160](https://github.com/fossas/fossa-cli/pull/1160))

## v3.7.0
- Support Maven wrapper (`mvnw`) usage in Maven projects, and user-provided binary overrides for Maven projects ([#1149](https://github.com/fossas/fossa-cli/pull/1149))
  For more information, see the [Maven strategy documentation](./docs/references/strategies/languages/maven/maven.md).
- Installation Script: Verify that the sha256sum of the downloaded archive matches the recorded one. ([#1158](https://github.com/fossas/fossa-cli/pull/1158))

## v3.6.18
- License Scanning: Emit a warning if unarchiving fails rather than a fatal error. ([#1153](https://github.com/fossas/fossa-cli/pull/1153))

## v3.6.17

- Handle Leiningen deduped deps: expand groupID and artifactID in the leiningen tactic to satisfy the Maven fetcher ([#1152]](https://github.com/fossas/fossa-cli/pull/1152))

## v3.6.17

- `fossa test`: Display CVE, fixed version information, and issue dashboard links when possible. ([#1146](https://github.com/fossas/fossa-cli/pull/1146))

## v3.6.16

- Project labels: Support project labels from command line and configuration file ([1145](https://github.com/fossas/fossa-cli/pull/1145))

## v3.6.15

- Container scanning: support more tar formats. ([1142](https://github.com/fossas/fossa-cli/pull/1142))
- `--detect-dynamic`: Supports recursively inspecting binaries for dynamic dependencies. ([#1143](https://github.com/fossas/fossa-cli/pull/1143))

## v3.6.14

- `fossa test`: Improved reporting. ([#1135](https://github.com/fossas/fossa-cli/pull/1135))

## v3.6.13

- Vendored Dependencies: Add the unity companion license (https://unity.com/legal/licenses/unity-companion-license) and unity package distribution license (https://unity.com/legal/licenses/unity-package-distribution-license) to license scanning ([#1136](https://github.com/fossas/fossa-cli/pull/1136))

## v3.6.12

- Maven: If a package is both `"test"` and `"compile"`, it is no longer filtered ([#1138](https://github.com/fossas/fossa-cli/pull/1138)).

## v3.6.11

- Lib yarn protocol: When we encounter Yarn lib deps we should warn but not fail the scan ([#1134](https://github.com/fossas/fossa-cli/pull/1134))

## v3.6.10

- Vendored Dependencies: Allow path filtering when doing cli-side license scans ([#1128](https://github.com/fossas/fossa-cli/pull/1128))

## v3.6.9
- Yarn: Fix a bug where tarball URLs were recognized as git urls. ([#1126](https://github.com/fossas/fossa-cli/pull/1126))

## v3.6.8
- Go: Allow quotes module names in static analysis ([#1118](https://github.com/fossas/fossa-cli/pull/1118))
- `fossa test`: Includes revision summary and target information, when accessible ([#1119](https://github.com/fossas/fossa-cli/pull/1119))

## v3.6.7

- Rename `--experimental-license-scan` to `--license-scan` (https://github.com/fossas/fossa-cli/pull/1110)
- Emit a warning if the `--experimental-native-license-scan` flag is used

## v3.6.6

- Conda: Change dynamic strategy to simulate building an environment from `environment.yml` instead of reading from the currently active environment. ([#1099](https://github.com/fossas/fossa-cli/pull/1099))

## v3.6.5

- `fossa test`: deprecates `--json` flag in favor of `--format json` option. ([#1109](https://github.com/fossas/fossa-cli/pull/1109))
- `fossa container test`: deprecates `--json` flag in favor of `--format json` option. ([#1109](https://github.com/fossas/fossa-cli/pull/1109))
- UX: Added breadcrumb to main help output indicating that subcommands have additional options. ([#1106](https://github.com/fossas/fossa-cli/pull/1106))

## v3.6.4

- C/C++: Fixes `--detect-vendored` on Windows. ([#1096](https://github.com/fossas/fossa-cli/pull/1096))
- Uses an ISO timestamp for the revision if no better revision can be inferred. ([#1091](https://github.com/fossas/fossa-cli/pull/1091))

## v3.6.3

Gradle: Considers dependencies from `debugUnitTest*` configurations to be unused. ([#1097](https://github.com/fossas/fossa-cli/pull/1097))

## v3.6.2

- Don't promote transitive dependencies  [#1092](https://github.com/fossas/fossa-cli/pull/1092).
- Container Scanning: Fixes a bug where tar entry were not normalized within nested layer tar. [#1095](https://github.com/fossas/fossa-cli/pull/1095)

## v3.6.1

- Container Scanning: Fixes a bug where image source parser ignored '-' in host. Also fixes an issue regarding to redirect headers when communicating with registry. [#1089](https://github.com/fossas/fossa-cli/pull/1089)

## v3.6.0

- Promote C/C++ features to general availability ([#1087](https://github.com/fossas/fossa-cli/pull/1087)).
  - `--experimental-enable-vsi` is now `--detect-vendored`.
  - `--experimental-analyze-dynamic-deps` is now `--detect-dynamic`.

## v3.5.3

- Manual Dependencies: Linux Dependencies (`rpm-generic`, `apk`, `deb`) can be provided as reference dependency in fossa-deps file ([#1086](https://github.com/fossas/fossa-cli/pull/1086)).

## v3.5.2

- Container Scanning: Fixes an issue with base64 encoded raw authentications ([#1085](https://github.com/fossas/fossa-cli/pull/1085)).

## v3.5.1

- Contributor counting: update the contributor count range from 90 days to 365 days. ([#1083](https://github.com/fossas/fossa-cli/pull/1083))

## v3.5.0

- Container Scanning: Uses native container scanner, deprecates old container scanner ([#1078](https://github.com/fossas/fossa-cli/pull/1078)), ([#1079](https://github.com/fossas/fossa-cli/pull/1079)), ([#1080](https://github.com/fossas/fossa-cli/pull/1080)), ([1082](https://github.com/fossas/fossa-cli/pull/1082)).

_Notice:_

- Now, container scanning analyzes projects for applications (`npm`, `pip`, etc) dependencies.
- Now, container scanning can filter specific targets via target exclusions using [fossa configuration file](./docs/references/files/fossa-yml.md).
- Now, `fossa-cli`'s windows binary can perform container scanning.
- Now, container scanned projects will show origin path in FOSSA web UI.
- Now, container scanned projects can target specific architecture via digest.

You can use `--only-system-deps` flag to only scan for dependencies from `apk`, `dpkg`, `dpm`.
This will mimic behavior of older FOSSA CLI's container scanning (older than v3.5.0).

Learn more:
- [container scanner](./docs/references/subcommands/container/scanner.md)
- [fossa container analyze](./docs/references/subcommands/container.md)

If you experience any issue with this release, or have question, please contact [FOSSA Support](https://support.fossa.com).

## v3.4.11
- Npm (Lockfile v3) - Fixes a defect where, _sometimes_ wrong version of the dependency was reported if multiple version of the same dependency existed in the lock file. ([#1075](https://github.com/fossas/fossa-cli/pull/1075))
- Npm (Lockfile v2) - Fixes a defect where, _sometimes_ wrong version of the dependency was reported if multiple version of the same dependency existed in the lock file. ([#1075](https://github.com/fossas/fossa-cli/pull/1075))

## v3.4.10
- Scala: Supports analysis of multi-project sbt builds with `sbt-dependency-graph` plugin. ([#1074](https://github.com/fossas/fossa-cli/pull/1074)).

## v3.4.9
- Scan Summary: Identifies project skipped due to production path filtering, or exclusion filtering. ([#1071](https://github.com/fossas/fossa-cli/pull/1071))
- R: Adds support for `renv` package manager. ([#1062](https://github.com/fossas/fossa-cli/pull/1062))

## v3.4.8
- Report: Fixes a defect, where `report` command was failing due to invalid dependencies cache from endpoint ([#1068](https://github.com/fossas/fossa-cli/pull/1068)).

## v3.4.7
- Linux releases are now packaged as both tar.gz and zip to improve compatibility when installing ([#1066](https://github.com/fossas/fossa-cli/pull/1066))

## v3.4.6
- Container Scanning: Fixes a defect, where container registry `registry:3000/org/repo:tag` was incorrectly identifying `registry` as project name. ([#1050](https://github.com/fossas/fossa-cli/issues/1050))
- Container Scanning: Includes registry uri in project name (experimental scanner only). ([#1050](https://github.com/fossas/fossa-cli/issues/1050))

## v3.4.5
- FOSSA API: Adds resiliency against API errors occurring when retrieving endpoint versioning information. ([#1051](https://github.com/fossas/fossa-cli/pull/1051))

## v3.4.4
- Fix a bug in the v1 installers for Windows (install-v1.ps1 and install.ps1) ([#1052](https://github.com/fossas/fossa-cli/pull/1052))

## v3.4.3
- Container scanning: Supports hardlink file discovery for experimental scanner. ([#1047](https://github.com/fossas/fossa-cli/pull/1047))
- Container scanning: Supports busybox. ([#1047](https://github.com/fossas/fossa-cli/pull/1047))
- Container scanning: Increases timeout to 5 mins when extracting image from docker engine api for experimental scanner. ([#1047](https://github.com/fossas/fossa-cli/pull/1047))

## v3.4.2

- API: Error messages are more clear and provide user-actionable feedback. ([#1048](https://github.com/fossas/fossa-cli/pull/1048))
- Metrics: Reports the kind of CI environment in which FOSSA is running, if any. ([#1043](https://github.com/fossas/fossa-cli/pull/1043))

## v3.4.1

- Container scanning: RPM: Add support for the Sqlite backend. ([#1044](https://github.com/fossas/fossa-cli/pull/1044))
- Container scanning: RPM: Add support for the NDB backend. ([#1046](https://github.com/fossas/fossa-cli/pull/1046))

## v3.4.0

- Container scanning: New experimental scanner. ([#1001](https://github.com/fossas/fossa-cli/pull/1001), [#1002](https://github.com/fossas/fossa-cli/pull/1002), [#1003](https://github.com/fossas/fossa-cli/pull/1003), [#1004](https://github.com/fossas/fossa-cli/pull/1004), [#1005](https://github.com/fossas/fossa-cli/pull/1005), [#1006](https://github.com/fossas/fossa-cli/pull/1006), [#1010](https://github.com/fossas/fossa-cli/pull/1010), [#1011](https://github.com/fossas/fossa-cli/pull/1011), [#1012](https://github.com/fossas/fossa-cli/pull/1012), [#1014](https://github.com/fossas/fossa-cli/pull/1014), [#1016](https://github.com/fossas/fossa-cli/pull/1016), [#1017](https://github.com/fossas/fossa-cli/pull/1017), [#1021](https://github.com/fossas/fossa-cli/pull/1021), [#1025](https://github.com/fossas/fossa-cli/pull/1025), [#1026](https://github.com/fossas/fossa-cli/pull/1026), [#1029](https://github.com/fossas/fossa-cli/pull/1029), [#1031](https://github.com/fossas/fossa-cli/pull/1031), [#1032](https://github.com/fossas/fossa-cli/pull/1032), [#1034](https://github.com/fossas/fossa-cli/pull/1034))<br>
  For more information, see the [experimental container scanning documentation](./docs/references/subcommands/container/scanner.md).
- Filters: Add `dist-newstyle` to the list of automatically filtered directories. ([#1030](https://github.com/fossas/fossa-cli/pull/1035))
- `fossa-deps`: Fix a bug in `fossa-deps.schema.json`, it is now valid JSON. ([#1030](https://github.com/fossas/fossa-cli/pull/1030))

## v3.3.12

- CocoaPods: Fixes error when analyzing podspecs that print non-JSON text to stdout ([#1015](https://github.com/fossas/fossa-cli/pull/1015))
- VSI: Executes with at least two threads even on a single core system ([#1013](https://github.com/fossas/fossa-cli/pull/1013))
- VSI: Reports a better error when no dependencies are found ([#1023](https://github.com/fossas/fossa-cli/pull/1023)).

## v3.3.11

- `fossa test`: `fossa test --json` produces json output when there are 0 issues found. ([#999](https://github.com/fossas/fossa-cli/pull/999))

## v3.3.10

- Svn: Fixes project inference bug, where revision values included `\r`. ([#997](https://github.com/fossas/fossa-cli/pull/997))

## v3.3.9

- Maven: Always use Maven Install Plugin 3.0.0-M1 to install depgraph. This avoids a Maven bug with older versions failing to install the plugin correctly from a vendored JAR. ([#988](https://github.com/fossas/fossa-cli/pull/988/files))
- Cocoapods: Fixes podpsec bug in which nested subspecs (of external sources), were not appropriately handled. Improves logging. ([#994](https://github.com/fossas/fossa-cli/pull/994))

## v3.3.8

- Carthage: Fixes analysis of artifacts dervided from Github entry for GH enterprise urls. ([#989](https://github.com/fossas/fossa-cli/pull/989))

## v3.3.7

- Report: Changes copyrights field to copyrightsByLicense in the attribution report JSON output. [#966](https://github.com/fossas/fossa-cli/pull/966)
- Report: Always include the "downloadUrl" field in attribution reports, regardless of the setting in the FOSSA reports UI. ([#979](https://github.com/fossas/fossa-cli/pull/979))
- Debug: Includes version associated with endpoint in debug bundle, and scan summary. ([#984](https://github.com/fossas/fossa-cli/pull/984))
- Test: Adds `--diff` option for `fossa test` command. ([#986](https://github.com/fossas/fossa-cli/pull/986))

## v3.3.6
- License scanning: Make CLI-side license scanning the default method for `vendored-dependencies`.
- Maven: Report direct dependencies as direct rather than deep. ([#963](https://github.com/fossas/fossa-cli/pull/963))

## v3.3.5
- Pnpm: Adds support for dependency analysis using `pnpm-lock.yaml` file. ([#958](https://github.com/fossas/fossa-cli/pull/958))

## v3.3.4
- `fossa report attribution`: Removes copyright information from JSON output ([#945](https://github.com/fossas/fossa-cli/pull/945)) as it was never available from the server.
- VSI scans now automatically skip the `.git` directory inside the scan root ([#969](https://github.com/fossas/fossa-cli/pull/969)).

## v3.3.3
- Cocoapods: Cocoapods analyzer does not handle subspecs in vendored podspecs. ([#964](https://github.com/fossas/fossa-cli/pull/964/files))

## v3.3.2
- License scanning: Skip rescanning revisions that are already known to FOSSA. This can be overridden by using the `--force-vendored-dependency-rescans` flag.
- Swift: Added support for `Package.resolved` v2 files ([#957](https://github.com/fossas/fossa-cli/pull/957)).
- Perl: Updated version number parser to be more lenient on non-textual version numbers ([#960](https://github.com/fossas/fossa-cli/pull/960))

## v3.3.1
- Vendor Dependencies: Considers `licence` and `license` equivalent when performing native license scan ([#939](https://github.com/fossas/fossa-cli/pull/939)).
- Vendor Dependencies: Native license scanning works in alpine linux without additional dependencies ([#949](https://github.com/fossas/fossa-cli/pull/949)).
- `fossa report attribution`: Adds copyright information to JSON output ([#945](https://github.com/fossas/fossa-cli/pull/945)).
- Scala: non-multi sbt projects include deep dependencies ([#942](https://github.com/fossas/fossa-cli/pull/942)).

## v3.3.0
- Telemetry: CLI collects telemetry by default. ([#936](https://github.com/fossas/fossa-cli/pull/936))

Read more about telemetry: https://github.com/fossas/fossa-cli/blob/master/docs/telemetry.md. To opt-out of telemetry, provide `FOSSA_TELEMETRY_SCOPE` environment variable with value of: `off` in your shell prior to running fossa.

## v3.2.17
- Archive upload: Fix a bug when trying to tar to a filename that already exists. ([#927](https://github.com/fossas/fossa-cli/pull/927))
- Npm: Supports lockfile v3. ([#932](https://github.com/fossas/fossa-cli/pull/932))

## v3.2.16
- Go: When statically analyzing a project, apply reported replacements. ([#926](https://github.com/fossas/fossa-cli/pull/926))

## v3.2.15

- Maven: Update `depGraph` plugin to `4.0.1` and add a fallback ot the legacy `3.3.0` plugin ([#895](https://github.com/fossas/fossa-cli/pull/895))

## v3.2.14

- Gradle: Considers `testFixturesApi` and `testFixturesImplementation` to be test configuration, and it's dependencies are excluded in analyzed dependency graph. ([#920](https://github.com/fossas/fossa-cli/pull/920))

## v3.2.13

- Filters: Fixes the disabled path filtering in discovery exclusion. ([#908](https://github.com/fossas/fossa-cli/pull/908))

## v3.2.12

- `fossa report attribution`: Adds `text` as an option to `--format`. ([#921](https://github.com/fossas/fossa-cli/pull/921))
- Go: The standard library is no longer reported as a dependency. ([#918](https://github.com/fossas/fossa-cli/pull/918))

## v3.2.11

- nodejs: Refine how workspace packages are recognized/skipped. ([#916](https://github.com/fossas/fossa-cli/pull/916))
- Cocoapods: Resolves vendored local podspecs into their source Git repositories when possible. ([#875](https://github.com/fossas/fossa-cli/pull/875))

## v3.2.10

- Haskell: Generates build plan properly for multi-home Cabal projects (h/t [@jmickelin](https://github.com/jmickelin)) ([#910](https://github.com/fossas/fossa-cli/pull/910))

## v3.2.9

- Container Scanning: supports rpm databases using `ndb` or `sqlite` backend. ([#894](https://github.com/fossas/fossa-cli/pull/894))

## v3.2.8

- Filtering: Don't use included paths for discovery exclusion. ([#907](https://github.com/fossas/fossa-cli/pull/907))
- Filtering: add `--debug-no-discovery-exclusion` for client-side filter debugging. (#[901](https://github.com/fossas/fossa-cli/pull/901))

## v3.2.7

- Debug: Redact all known API keys from the debug bundle (#[897](https://github.com/fossas/fossa-cli/pull/897))
- Nodejs: Discover peer deps and transitive deps for name-spaced packages in package-lock.json. ([#882](https://github.com/fossas/fossa-cli/pull/882))

## v3.2.6

- Filters: Apply filters during the discvoery phase, reducing end-to-end runtime. ([#877](https://github.com/fossas/fossa-cli/pull/877))

## v3.2.5

- Debug: Reduce the size of debug bundles. ([#890](https://github.com/fossas/fossa-cli/pull/890))

## v3.2.4

- Nodejs: Fixed a bug where dev deps that only appear in requires were considered production dependencies. ([#884](https://github.com/fossas/fossa-cli/pull/884))

## v3.2.3

- Nodejs: Fixed a bug where some dev dependencies weren't removed during shrinking. ([#859](https://github.com/fossas/fossa-cli/pull/859))

## v3.2.2

- Nodejs: Fix a bug where cycles involved peer dependencies would cause an infinite loop. ([#870](https://github.com/fossas/fossa-cli/pull/870))
- Experimental: Allow local license scanning of vendored dependencies (specified in `fossa-deps.yml` file) when using `--experimental-native-license-scan`.
  - [#868](https://github.com/fossas/fossa-cli/pull/868)
  - [#858](https://github.com/fossas/fossa-cli/pull/858)
  - [#838](https://github.com/fossas/fossa-cli/pull/838)
  - [#814](https://github.com/fossas/fossa-cli/pull/814)
  - [#873](https://github.com/fossas/fossa-cli/pull/873)

## v3.2.1

- Experimental: native license scanning is now disabled by default. ([#865](https://github.com/fossas/fossa-cli/pull/865))

## v3.2.0
- Telemetry: Introduces fossa cli telemetry for fatal errors and warnings. By default, telemetry is disabled. ([#831](https://github.com/fossas/fossa-cli/pull/831))
Please read for details on telemetry [here](./docs/telemetry.md)

- Configuration: Fixes a bug where `.fossa.yml` was picked up only in the working directory, not in the analysis directory. ([#854](https://github.com/fossas/fossa-cli/pull/854))
- Configuration: Reports an error when provided API key is an empty string ([#856](https://github.com/fossas/fossa-cli/pull/856))

## v3.1.8

- Windows: Fixes a --version command for windows release binary.

## v3.1.7

- Configuration: Users can now use `.fossa.yaml` as a configuration file name. Previously, only `.fossa.yml` was supported. ([#851](https://github.com/fossas/fossa-cli/pull/851))
- fossa-deps: Fixes an archive uploading bug for vendor dependency by queuing archive builds individually. ([#826](https://github.com/fossas/fossa-cli/pull/826))
- nodejs: Capture peer dependencies transitively for npm `package-lock.json` files. ([#849](https://github.com/fossas/fossa-cli/pull/849))

## v3.1.6

- Respects Go module replacement directives in the Go Mod Graph strategy. ([#841](https://github.com/fossas/fossa-cli/pull/841))

## v3.1.5

- Adds `--format` to `fossa report attribution` and deprecates `--json`. ([#844](https://github.com/fossas/fossa-cli/pull/844))

## v3.1.4

- Handles symlink loops in directory structure. ([#827](https://github.com/fossas/fossa-cli/pull/827))
- No longer crashes when `fossa-deps.yml` exists but has an empty `archived-dependencies` property. ([#832](https://github.com/fossas/fossa-cli/pull/832))

## v3.1.3

- Adds support for identifying dynamically linked dependencies in an output binary. ([#818](https://github.com/fossas/fossa-cli/pull/818), [#788](https://github.com/fossas/fossa-cli/pull/788), [#780](https://github.com/fossas/fossa-cli/pull/780), [#788](https://github.com/fossas/fossa-cli/pull/778), [#771](https://github.com/fossas/fossa-cli/pull/771), [#770](https://github.com/fossas/fossa-cli/pull/770))

## v3.1.2

- Fixes a bug which ignored the `server` field in the config file. ([#821](https://github.com/fossas/fossa-cli/pull/821))

## v3.1.1

- UX: Parser error messages include call to action. ([#801](https://github.com/fossas/fossa-cli/pull/801))
- UX: Improves error message when executable is not found. ([#813](https://github.com/fossas/fossa-cli/pull/813))
- UX: Fixes minor scan summary ordering bug. ([#813](https://github.com/fossas/fossa-cli/pull/813))
- UX: Writes errors and warnings encountered in analyze to temp file. ([#813](https://github.com/fossas/fossa-cli/pull/813))
- Ruby: Improves error and warning messages. ([#800](https://github.com/fossas/fossa-cli/pull/800))
- Python: `setup.py` error messages are _less_ noisy. ([#801](https://github.com/fossas/fossa-cli/pull/801))
- Dart: Improves error and warning messages. ([#800](https://github.com/fossas/fossa-cli/pull/806))
- Pipenv: Improves error and warning messages. ([#803](https://github.com/fossas/fossa-cli/pull/803))
- Poetry: Improves error and warning messages. ([#803](https://github.com/fossas/fossa-cli/pull/803))
- Maven: Improves error and warning messages. ([#808](https://github.com/fossas/fossa-cli/pull/808))
- Nodejs: Improves error and warning messages. ([#805](https://github.com/fossas/fossa-cli/pull/805))
- Swift: Improves error and warning messages. ([#802](https://github.com/fossas/fossa-cli/pull/802))
- Cocoapods: Improves error and warning messages. ([#807](https://github.com/fossas/fossa-cli/pull/807))
- Golang: Improves error and warning messages. ([#809](https://github.com/fossas/fossa-cli/pull/809))
- Gradle: Improves error and warning messages. ([#804](https://github.com/fossas/fossa-cli/pull/804))
- Scala: Improves error and warning messages. ([#813](https://github.com/fossas/fossa-cli/pull/813))
- Clojure: Improves error and warning messages. ([#813](https://github.com/fossas/fossa-cli/pull/813))
- Nim: Improves error and warning messages. ([#813](https://github.com/fossas/fossa-cli/pull/813))
- Rust: Improves error and warning messages. ([#813](https://github.com/fossas/fossa-cli/pull/813))
- UX: Improves errors for dynamic deps, and binary deps analysis. ([#819](https://github.com/fossas/fossa-cli/pull/819))
- UX: Improves analysis scan summary rendering. ([#819](https://github.com/fossas/fossa-cli/pull/819))


## v3.1.0

- FOSSA API: Uses `SSL_CERT_FILE`, and `SSL_CERT_DIR` environment variable for certificates when provided. ([#760](https://github.com/fossas/fossa-cli/pull/760))
- UX: Uses error messages received from FOSSA api, when reporting API related errors. ([#792](https://github.com/fossas/fossa-cli/pull/792))
- UX: Adds scan summary tabulating errors, warnings, project directory, and skipped projects. ([#790](https://github.com/fossas/fossa-cli/pull/790))

## v3.0.18

- Fully percent-encode sub-paths in generated URLs. ([#789](https://github.com/fossas/fossa-cli/pull/789))
- Improve error tracking and outputs. ([#774](https://github.com/fossas/fossa-cli/pull/774))
- Cabal: Fixed a filter error that treated cabal projects as stack projects. ([#787](https://github.com/fossas/fossa-cli/pull/787))

## v3.0.17

- Npm: Fixes an issue where a package-lock.json dep with a boolean 'resolved' key wouldn't parse. ([#775](https://github.com/fossas/fossa-cli/pull/775))
- Npm: Fixes an issue where analyzing `package-lock.json` would miss duplicate packages with different versions. ([#779](https://github.com/fossas/fossa-cli/pull/779))
- Gradle: Projects with only a top-level `settings.gradle` file will now be detected. ([#785](https://github.com/fossas/fossa-cli/pull/785))

## v3.0.16

- Monorepo: Upload file data and licenses together during monorepo scans, speed up issue scans. ([#772](https://github.com/fossas/fossa-cli/pull/772))
- Improves the overall performance and progress reporting of VSI scans. ([#765](https://github.com/fossas/fossa-cli/pull/765))
- Rebar: Fix `rebar.config` parser failing on unneccessary escapes. ([#764](https://github.com/fossas/fossa-cli/pull/764))

## v3.0.15

- Improve archive upload logging. ([#761](https://github.com/fossas/fossa-cli/pull/761))

## v3.0.14

- Maven: Updates implementation to delineate classifier, and consequently maven dependencies with classifier can be scanned without failure in FOSSA. ([#755](https://github.com/fossas/fossa-cli/pull/755/))

## v3.0.13

- `package-lock.json` parser ignores name field. ([#757](https://github.com/fossas/fossa-cli/pull/757))

## v3.0.12

- log4j: Adds `fossa log4j` command to identify log4j dependencies. ([#744](https://github.com/fossas/fossa-cli/pull/744))

## v3.0.11

- Yarn: Fixes an issue, where entry missing `resolved` attribute in `yarn.lock` would throw exception. ([#741](https://github.com/fossas/fossa-cli/pull/741))

## v3.0.10

- Gradle: Uses ResolutionAPI for gradle analysis. ([#740](https://github.com/fossas/fossa-cli/pull/740/))
- Cleans up duplicated internal hashing primitives ([#737](https://github.com/fossas/fossa-cli/pull/737))
- Adds a prerequisite required for future VSI improvements ([#736](https://github.com/fossas/fossa-cli/pull/736))

## v3.0.9

- Makes experimental flags discoverable and documents them. ([#723](https://github.com/fossas/fossa-cli/pull/723))
- Supports extracting `.tar.xz` files ([#734](https://github.com/fossas/fossa-cli/pull/734))
- Supports extracting `.tar.bz2` files ([#734](https://github.com/fossas/fossa-cli/pull/734))
- Adds explicit `xz` support for `rpm` files ([#735](https://github.com/fossas/fossa-cli/pull/735))
- Adds `zstd` support for `rpm` files ([#735](https://github.com/fossas/fossa-cli/pull/735))
- Adds a prerequisite required for future VSI improvements ([#730](https://github.com/fossas/fossa-cli/pull/730))

## v3.0.8

- Nuget: Fixes analysis performance when working with `project.assets.json` ([#733](https://github.com/fossas/fossa-cli/pull/733))

## v3.0.7

- Go: `go mod graph` is used as default tactic for gomod strategy. ([#707](https://github.com/fossas/fossa-cli/pull/707))

## v3.0.6

- Yarn: Fixes a bug with yarn v1 lock file analysis, where direct dependencies were not reported sometimes. ([#716](https://github.com/fossas/fossa-cli/pull/716))

## v3.0.5

- Nim: Adds support for dependency analysis using `nimble.lock` file. ([#711](https://github.com/fossas/fossa-cli/pull/711))

## v3.0.4

- Npm: Fixes a bug where dev dependencies were not included in result when using `--include-unused-deps` ([#710](https://github.com/fossas/fossa-cli/pull/710))

## v3.0.3

- Increases default timeout to 3600 seconds (1 hour) for commands listed below ([#712](https://github.com/fossas/fossa-cli/pull/712))
  - `fossa test`
  - `fossa container test`
  - `fossa vps test`
  - `fossa report`
  - `fossa vps report`

## v3.0.2

- Nuget (projectassetsjson): Ignores project type dependencies in reporting ([#704](https://github.com/fossas/fossa-cli/pull/704))
- Nuget (projectassetsjson): Fixes a bug, where indirect dependencies where appearing as direct dependencies([#704](https://github.com/fossas/fossa-cli/pull/704))

## v3.0.1

- Deduplicates `vendored-dependencies` entries when possible, and provides a better error message when not. ([#689](https://github.com/fossas/fossa-cli/pull/689))
- Adds logging to `vendored-dependencies` processing. ([#703](https://github.com/fossas/fossa-cli/pull/703))

# Version 3 Changelog

- Migrates source code from [spectrometer](https://github.com/fossas/spectrometer) into fossa-cli (this repository).

# Version 2 Changelog

Releases for CLI 2.x can be found at: https://github.com/fossas/spectrometer/releases

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

# Version 1 Changelog

## v1.1.10

- 7013d3b fix: Remove evicted SBT dependencies (#667)
- 8aa77d8 Update genny calls to not use gopath (#668)
- 4e6cced fix: Unit test failures should cause CI failure (#666)

## v1.1.9

- a1ec875 Fix node_modules strategy (#665)

## v1.1.8

- 6ad8e86 fix ant subdirectoy analysis (#664)
- 4fe7d83 add faq (#661)

## v1.1.7

- 246294c fix downloaded parse error (#660)
- 2cd3dcd fix wrong config file field (#623)
- 01fe79a doc: Homebrew is no longer a supported installation method (#659)

## v1.1.6

- 9f7d083 Send projectURL on upload-project (#656)

## v1.1.5

- dd56406 Use gomodules instead of dep (#653)
- 9c1523e Ant: use pom.xml's <parent> version if one isn't declared at the top level (#652)

## v1.1.4

- fabc9ef Remove e2e test from blocking a release (#649)
- 44d13b2 Use 'go list' to determine transitive dependencies for gomodules projects (#648)
- 84818e9 Add support for titles with upload project (#646)
- 444330f SAML build link (#647)

## v1.1.3

- fc60c66 Update documentation for newer sbt versions (#638)
- 3255628 Add ARM64 in goreleaser (#626)
- 871e94f improve license scan fix (#643)

## v1.1.2

- b1e910a Fix Goreleaser after deprecation (#642)
- 89b8691 fossa upload-project command (#639)
- 38fdbac Update README.md (#636)

## v1.1.2-alpha.1

- 57fe304 feat: Use name field to name modules (#635)

## v1.1.1

- 94d95b5 Send CLI version in upload (#633)
- e41733a Update docs and help output for flags that only effective on project creation. (#632)
- a4bddd0 Handle multi module maven builds without distinct pom files (#631)
- 8330391 improve docs on `--suppress-issues` flag (#624)

## v1.1.0

- 1706109 chore: Update Docker development images (#601)
- 8aa42f0 remove mention of overwrite (#621)
- d7467dc Timeout flag correction (#619)
- 2cd9167 Add a pull request template (#618)
- ac0dc90 Replace "Python" with "Ruby" in Ruby documentation (#544)
- 11358c6 Fix typo on "options" param (#608)
- 1ae3c54 Update maven.md (#612)
- 028812f Replace .NET with nodejs on nodejs documentation (#610)
- 90d625c Git contrib count (#611)
- fff1e23 remove spectrometer install (#606)

## v1.0.30

- 09c02d6 Add site-packages to the list of ignored directories (#605)

## v1.0.29

- cc3b1ec fix: Do not fail when analyzing go.mod modules when lockfile is not in same directory as module (#602)

## v1.0.28

- 091f2a9 Allow double-quoted strings in setup.py (#600)

## v1.0.27

- f511238 Add -mod=readonly flag to gomodules resolver (#599)

## v1.0.26

- 55cc629 Use -mod=readonly flag for go list (#595)
- 7103b56 Go repository bazel files (#594)
- d4b00cd Use the same BINDIR for hscli (#592)

## v1.0.25

- 08dbd38 prevent comparison tooling with custom endpoint (#591)

## v1.0.24

- b530020 feat (npm dev) include npm development dependencies (#589)
- dff5651 Let hscli installation fail silently (#590)
- 22ca461 feat (yarn list) support for scanning yarn list output (#588)

## v1.0.23

- 7d22c91 CLI v2 Comparison (#568)
- 32b9351 Resolve documentation nits (#585)

## v1.0.22

- 6dee5c6 upload the policy paramater if a user adds it (#577)

## v1.0.21

- 7458683 Use unix file separators in archive uploads (#584)
- 6187e44 remove isbuilt errors and warnings for commands that we don't need (#576)

## v1.0.20

- 39656fd changes to scan ant better (#575)

## v1.0.19

- 3a98c56 Allow Leiningen to output on stderr without failing (#574)
- cf5391b feat (better bazel) support for bazel deps command (#570)
- 4efffa5 handle maven downloaded line (#573)
- f0abc89 flag change (#572)
- 4ccb6fd add title attribution row (#571)
- b431b2e docs update (#567)

## v1.0.18

- 4a98113 archive -> archives (#566)
- 244e757 return error so file info cannot be nil (#565)

## v1.0.17

- 85a7e9c fix (hanging commands) refactor sbt and timeout hanging commands (#563)
- b243965 revise ant options (#561)
- a0358b0 feat (pipenv discovery) (#560)

## v1.0.16

- 30316bc fix(json report) print json reports when provided json flag (#558)
- 3a27b27 remove gradle sub projects from dependency graph (#556)

## v1.0.15

- 98c3b7f Request download url when rendering reports (#557)

## v1.0.14

- 5296cf3 fix (gradle error) error on exit code and stderr (#555)
- 7c7aa6f [FC-1212] create integration with new report endpoint (#554)

## v1.0.13

- 701adbd remove .fossa.yml (#553)
- 8299613 feat (bazel files) parse bazel files for static support (#552)

## v1.0.12

- d6cab2c Add DownloadURL to Revision type. (#551)
- 35ce938 [FC-1059] Added consideration of lockfiles to nodejs IsBuilt() (#543)
- b2697e6 Updated README for generating license notices (#546)

## v1.0.11

- 3e7c7b3 [FC-977] Added strategy for parsing package-lock.json files (#541)

## v1.0.10

- 2961722 fix (log file sync) defer file syncing until after the command finishes (#539)
- d1fa5ed Fixed gradle project discovery (#538)

## v1.0.9

- 3ead389 fix (rpm install) return stdout instead of zero value (#537)
- d74872c Implement new python analyzer (#534)

## v1.0.8

- 36d3766 unmarshal additional information (#535)
- 877e552 feat (ruby v2 analysis) ruby analysis conforms to v2 format (#530)

## v1.0.7

- 4940add feat (rpm scanning) support for system level and individual rpms. (#520)

## v1.0.6

- 78d3b72 fix (type issue) handle empty pkg types (#532)
- eaf8b55 fix (update fail) don't fail when there are no updates available (#526)
- 9b5c9ff errors (extend errors) use the errors package in more places (#503)
- c160edb refactor (ruby) (#528)

## v1.0.5

- eaa6c94 turn off cgo (#529)
- ab7c478 new analysis strategies / fallbacks (#511)
- 69dc144 errors (wrong arguments) better errors when users manually specify modules. (#525)

## v1.0.4

- 6cad43a Trim $GOPATH from source path stacktraces (#524)
- 707ca11 add hash and version field to dep reports (#521)
- 2b63679 lint (golang ci) add custom linting configuration (#508)
- 6f324ad add the --server-scan flag to treat raw modules separately (#518)

## v1.0.3

- 638f9f7 fix (ruby errors) change is built check and fix errors (#519)

## v1.0.2

- 1c58d98 warn error and handle nil typed errors (#512)
- 4bc1dbc improve error messages when running commands (#510)
- 94be39b testing (fossa test) use a test server to test fossa test (#305)

## v1.0.1

- 39676c8 release (go version) (#507)
- d478879 release after approval (#506)
- 25ed63d feat (gomodules vendor) support users who vendor custom deps and use gomodules (#505)
- e72db93 feat (golang fallbacks) break go strategies down and fallback easier (#504)
- 4c5a991 fix (missing remote error) set project name to directory if git cannot be read. (#502)
- 72e21d6 feat (clojure support) clojure support through leiningen (#501)
- 7e64aa9 Fix parsing of gradle dependency tree (#500)

## v1.0.0

- 235c83318 better buck error (#499)
- a8412e0e2 Add setup.py scanning (#493)
- 1bdd0432d Log message if on Windows or not using ANSI (#438)
- 582091364 errors (better errors) extend the errors package further (#492)
- 953ec7464 Init: allow use of --project (and other API-related) flags (#498)
- 5c9f72c9e filter warnings prefix (#497)
- 4f90d785b feat (dep static analysis) read manifest and lockfiles created by dep (#491)
- 5a81a616a fix output requiring api key (#495)
- e6aefa91c golang: fix support for go modules in subdirectories (#439)
- 8af01eb7d Publish homebrew formula (#494)
- 1187e9d0a docs(readme): add download count (#490)
- d68373963 errors (no API key) Common error when users forget to add an API key.  (#489)
- 78a841865 feat (gomodules scanning) scan go.mod and go.sum for dependencies. (#488)

## Version 0 Changelog

We generally follow semantic versioning, but semantic versioning does not
specify pre-1.0.0 behavior. Here is how `fossa` <1.0.0 releases work:

- Any update that creates a breaking change (i.e. a change that causes a
  previously working configuration to fail) will bump the minor version.
- All other updates will bump the patch version.
- Preview, beta, and other special releases will have a pre-release identifer in
  the semantic version, and will be marked as pre-release on GitHub Releases.

## v0.7.34

- 0a838a506 Fix WalkUp to be OS-agnostic (#487)
- a5fcdca1b fix (requirements parser) whitespace in dependencies removed (#486)
- c2cbf8eac feat (errors) better errors package (#462)
- 8656b4e12 Use runtimeClasspath configuration for resolution (#484)
- b2d510c05 feat (clean debug logs) add --verbose flag to allow cleaner logs (#485)
- 643451839 docs (docker faq) update faq for custom docker images (#481)
- fd8fa7c17 Discover gradle projects using non-groovy dialects (#482) (Closes #395)
- 66e205192 Replace "Python" with "Ruby" in Ruby documentation (#483)
- 569f1e867 feat (rust support) Support rust through Cargo.lock parsing. (#474)
- c8d6e7dd5 feat (dependencyManagement field) add dependencyManagement parsing. (#477)
- 28096cc8b Haskell project support via cabal-install and stack (#444)
- 202eda88c fix (support method) change support to email (#476)

## v0.7.33

- 0567ca5 fix (zero deps error) log when a project has no dependencies (#473)
- 5d0e2b9 fix (nested poms) fix a bug where nested pom files could not be found  (#475)

## v0.7.32

- df7ee6967 Update how-it-works.md (#472)
- 4b85dce8b feat (license scan tar) use the rawLicenseScan parameter to run a full license scan (#469)

## v0.7.31

- b7cb71ad2 feat (fallbacks) don't look for setup.py and log if cocoapods is missing instead of failing (#470)
- fcc63f259 fix (sbt parsing) make a small change to ensure sbt graphs are created accurately. (#471)
- 9f346efc6 feat (buckw) discover buck command and prefer buckw (#467)
- 4d380793d fix (module filter) consider windows file paths when filtering modules (#466)
- 3bd9ffaec Update CONTRIBUTING.md (#465)
- 999641227 docs (golang) strategy and faq updates (#464)

## v0.7.30

- 5ec7a9e07 Add fallback to no VCS when VCS not supported, and support Mercurial (#463)
- f54ae192e copy installation (#461)

## v0.7.29

- 33808e000 remove upper bound on test (#460)
- ec5990cf7 Update how Maven modules are described in .fossa.yml (#459)
- 3c4e41f7d feat (command timeout) add timeout for gradle analyzer (#458)

## v0.7.28

- 94e829228 Fix gradle project name parsing for projects without group ID (#457)

## v0.7.27

- 5c59eafa2 improve dockerfiles (#456)
- e6da7d3ba feat (format fossa test) improve fossa test output for readability (#455)
- d184d6a5d Harden parsing of Gradle's output (#454)
- 9ea851336 Check each Maven POM manifest only once when discovering modules (#452)
- 89eb004f5 Improve discovery of Maven modules and dependencies (#449)
- bfdfaa1c4 feat (dotnet support) complete dotnet support with fallbacks. (#450)
- ad23bb55d Support projects without VCS and support Subversion (#448)
- 5a24f6891 fix empty Composer dependencies list parsing (#392)
- 92d9c9f98 Fix some typos in docs (#447)
- 4cfe1b459 remove comment and kill unused images (#446)
- e7319ddec Bump fossa/fossa-cli:base image's golang version to 1.12

## v0.7.26

- aad54f605 fix (api error) return api error messages with bad requests (#442)
- 47a005dd1 feat (report license text) add full license and attribution text for fossa report (#441)
- 368a1382b docs (FAQ page) add a faq page and other updates (#440)

## v0.7.25

- b0571b804 feat (gradle project dir) enable the gradle analyzer to work with a monorepo like structure (#434)
- 89aafbd77 test (carthage) add test structure for empty cartfile (#437)
- ebfa09147 fix (empty cartfile) Check for nil graph from empty Cartfile.resolved (#435)
- 7fd62b6b4 fix (report url) switch dependency url back to the full url (#436)
- f2d05aa12 fix (ruby tests) fix flaky ruby integration tests. (#426)
- 156ae380c fix (carthage error) improve carthage error message (#432)
- be9042349 feat (go dependency versions) Favor explicit versions instead of dependency hashes (#427)

## v0.7.24

- 7a5ba032b feat (buck all targets) add option to specify a target such as //third-party/... (#428)
- e9fd4642e fix (ant analyzer) clean up and prevent failure from missing binaries (#429)
- bb551cd04 refactor (gradle analysis) add a test suite and clean up code. (#425)
- 8e02a4a80 fix (.io to .com) update endpoints (#423)

## v0.7.23

- e26421e28 fix (gradle parser) bug related to windows line endings (#421)
- 75994dadf fix (windows script) add a correct download script for windows users to the cli manual (#422)
- cbd0f751a testing and comment logic (#420)
- 2a2a23f14 fix (report dependencies) Change report dependencies to track fossa.com results (#419)
- fa135e191 feat (test pass) add the --supress-issues flag to fossa test (#418)
- f6660fb91 fix (raw modules) prevent modules from appearing as projects (#416)
- 2068b2d8f fix (manual links) broken links on the cli manual (#415)
- 541beceee docs (manual rewrite) manual and user guide overhaul (#410)

## v0.7.22

- 5e22532 Update README.md (#412)
- c13d22c fix (gradle configurations) add default configurations and change how targets are handled (#411)

## v0.7.21

- 11d74e8 Fix (readtree generic) Fix bug that prevented dependencies from being listed in the transitive graph. (#407)

## v0.7.20

- 2f552ca feat (paket analyzer) introduce support for the paket package manager (#404)

## v0.7.19

- 757a3df feat (okbuck classpath) add a flag to buck analysis for specific types of dependencies. (#400)
- 5b86e5b fix (ruby no specs) Do not panic when Gemfile.lock has a malformed empty specs section (#402)
- 7ad3119 fix (go analyzer) do not fail when there are no go dependencies. (#401)

## v0.7.18

- 1ed03f7 feat(okbuck support) Provide support for analyzing software managed by okbuck. (#398)
- 34babdf fix (Gradle analyzer) Fix gradle discovery for root projects and add a flag to scan all submodules (#399)
- cef13fe fix(cmd): Correctly parse module options when passed from command line (#393)
- 28a6f0e feat (debian analysis) Build functionality to analyze debian packages (#388)
- 3e54a0b fix (fossa report licenses) change the way that we find dependency license information. (#390)
- 65c2534 fix (fossa test) Poll the fossa issues endpoint until a scan is complete. (#397)
- 4ccf0d5 feat(buck) add cli support for the Buck package manager (#380)

## v0.7.17

- be61f55 Gradle multi-configuration parsing (#387)
- b9cf6ae fix (ant discovery) ensure valid directories are discovered. (#384)
- f6731eb feat(build tags) analyze go build tags (#372)
- 098b089 fix (readtree) correct the way we parse dependency graphs (#385)
- 861f567 Fix csproj regex (#386)
- 1d39bb8 fix(ant) Fix error message (#363)
- d1c781d fix: Correctly set directory modules are passed in from the command line (#383)
- 9509164 Add machine-readable output format to license report (#379)
## v0.7.16-rc.1+buckpreview

- d8e6d3a add buck project discovery
- eb4bc12 basic functionality
- 7005a1e first fully working changes
- 9088c10 WIP push
- cb54eea WIP upload
- 301b654 WIP basic functionality

## v0.7.15

- 434a2ae Pass -no-colors about as two separate arguments to sbt, not one (#376)
- 740da0d Link to CONTRIBUTING.md was broken (#377)
- b9a1f3b update go-git to v4.7.1 (#374)

## v0.7.14

- 4821bdc feat(gomodules) add support for gomodules (#368)
- 2dd95e9 fix(python) Add options support to requirements.txt parsing (#370)
- 2347102 fix(python) requirements parses extras properly (#365)
- c6aceb3 fix(maven) fix line parsing in Windows OS (#362)
- 71b489c fix(upload) remove duplication of flags to fix upload (#366)
- 4f6199d fix(make) fix conflict with auto generated file (#364)
- 9e6a4d8 fix(npm) npm analyze incorrectly finds module from .fossa.yml (#355)

## v0.7.13

- 623f658 fix(module options) allow command line options for multi module builds (#359)
- f188555 feat(pipenv) add support for pipenv projects (#315)
- cb206fd fix(glide) ensure that glide checks for aliased packages (#352)
- bb05c2b fix (buildtools) logic for bower and pip (#350)

## v0.7.12

- 4bd3b42 Merge pull request #339 from fossas/fix/yml-relative-paths-golang
- 48e7afd chore(go): Import grouping
- 9632cbe Merge pull request #338 from fossas/feat/warn-old-config
- a3792d0 test(nodejs): Add tests for checking import graphs (#337)
- 9439aa0 feat(ruby analyzer integration test) add ruby analyzer integration test (#320)
- 94c5f92 refactor(integration test) do not use TestMain in integration tests (#336)
- f4fc244 nit: Remove debugging code
- eeb82cd Merge pull request #276 from fossas/fix/phpDeps
- b8baa4f feat(config): Warn on old configuration files
- 36cc01d fix(go): Use relative paths when discovering modules
- 2495072 Filter out deps with name 'php'
- 201b13f test(yarn fixtures) add rev not resolve to suffix fixture (#326)
- 4f8a134 Merge branch 'master' into test/nodeFixtures3
- 61ce589 build: Rebuild on source change, use MacOS-compatible find (#332)
- ccb3bec Merge branch 'master' of github.com:fossas/fossa-cli into test/nodeFixtures3
- 915c70f  update fixtures and tests according to fixture dir
- 77b64e5 define fixture
- 50277fa add second case for trans prod dep collisions
- cd8ef3c add trans dev dep case 1
- 10fff2b fix some naming to be more clear
- 4fc7473 rename directories
- 0d36e90 setup fixture

## v0.7.11

- 5f558c5 fix added for dep graph creation (#331)
- 2de096e test(yarn fixtures) define fixtures and tests for additional parsing edge cases (#325)
- 1d32b91 test(python analyzer) add native python analyzer integration tests (#307)
- e432486 feat(circle.yml) aggregate coverage for multiple reports within a single PR (#306)

## v0.7.10

- 58b0fb7 test(yarn fixtures) add name only collision with direct prod dep case (#324)
- 4c51b77 test(yarn fixtures) initial edge case fixture structure example for yarn tooling (#323)
- 3c243bc Make sure that release tags start with 'v' (#322)
- 5f47dc1 feat(yarn.lock parsing) do not include dev deps in lockfile parsing (#312)
- 146f015 feat(nodejs dev deps) filter nodejs dev deps when using npm ls (#314)
- 72f7ec7 fix(vndr user repos) add support for user specified package locations (#316) (#318)
- ca28520 feat(buildtools Dockerfile) add rails tooling on buildtools image (#319)
- 0ae2c5e feat(yarn/npm) do not eagerly fail if either yarn or npm is not available (#313)
- dbfbb85 refactor(integration tests) abstract/simplify project initialization post cloning (#310)

## v0.7.9

- adec6f3 build: Fix Makefile dependencies in release process (#309)
- 03b24fb Improve .NET analyzer performance by reducing logging (#296)
- e9ac753 test: Don't run integration tests while unit testing (#308)
- fcb3783 fix(Makefile) update dev-osx to not error out for missing run command (#304)
- 4e1af41 test(nodejs integration) add full nodejs integration test (#297)
- 54bed30 refactor(codegen): Clean up old code, make codegen consistent (#300)
- a938e90 Add dependency check to build (#302)
- fc78d44 fix(ci): Fix reversed coverage logic for CI unit tests (#301)
- 72d7ca4 refactor(install): Use godownloader instead of custom install script (#298)
- ce2e3bf coveralls removed for forked repos and alternate junit test path added (#299)
- 48afaf4 feat(yarn lockfile fallback) add yarn lockfile reading fallback (#293)
- c94e175 added flags for specifying the team in fossa, and a link (#287)
- 718bf28 test(yarn lockfile) improve fixture case coverage (#290)
- c90d051  feat(AnalyzerTest) canonical reference for slimming docker test image and native analyzer testing (#271)
- 67c2ff1 release(v0.7.8-1): Release version v0.7.8-1

## v0.7.8-1

- 67c2ff1 release(v0.7.8-1): Release version v0.7.8-1
- f7bc7ea Fix/test upload (#289)
- 70aa12a Overhaul FOSSA CLI CI (#286)
- 6e77c5b feat(yarn) add yarn lockfile parsing (#283)
- 6d8b99d fix(.circleci/config.yml) use test base docker image and check out branch cli code into the image (#277)

## v0.7.8

- 65a187a release(v0.7.8): Release version v0.7.8
- b03de4d Fix/test custom (#284)
- 4c9206d Issue #247, fix how custom fetchers are handled (#281)
- 9e52d6e feat(npm fallback) use node_modules to build dep graph if npm fails
- 6999ee6 Fix/go dep ignore (#272)
- 80e2819 fix(exec) append args provided in WithEnv instead of Env for cmds (#273)
- 5e040f8 default error value changed when an API key is not provided (#275)
- 12cd4c8 feat(npm buildtool) update FromManifest to return a package and rename it accordingly
- 9b6bc04 fully define TestFromNodeModules

## v0.7.7

- e874ec2 release(v0.7.7): Release version v0.7.7
- a66383e Work around NPM reporting for transitive dependencies with deduplication (#258)
- 27fcf55 Move fixtures to correct folder
- 551f5a4 refactor(npm): Move fixtures to correct folder
- dfcf109 Add NPM tests and mock NPM execution
- 42d448f Merge pull request #260 from fossas/ci/coveralls
- c75503c Merge branch 'master' into ci/coveralls
- 58e029f  Use CLI-specific API endpoints so that `fossa test` works with push-only API keys (#253)
- 7654166 coveralls support added
- ca66f70 feat(ruby analyzer) add fallback mechanism for ruby analyzers when bundler fails
- a1bcdc9 remove shouldFallback
- 000d93d remove trash
- 4742c3f flatten structure
- d2c7dda prefer fallbacks within switch statement
- 8bedfaf update to no longer need gemfile-lock-path
- 560691d add fallback option on each bundler command failure
- c864b74 remove dockerfile change in favor of separate PR
- f247dd9 remove shouldFallback from final fallback case
- 2ca741f fix fallback ordering
- 89df70d clean up tests; remove unused code
- 6a8dd6f correct ordering
- c46eba5 remove helper function
- 571be27 remove debugger line
- 996525a add remote debugging tools and settings
- b447304 update to not include lockfile path
- 41dba65 rename dev-mac -> dev-osx
- f8c5d93 reorder to check errs earlier, ensure that end result is actually populated
- 7a38c7f update buildTarget to use CWD
- e34bcad break out switch into functions
- b0a8ab3 Merge branch 'master' of github.com:fossas/fossa-cli into feat/rubyFallback
- eb8b518 use fallback strategy
- 8377bc7 add osx dev
- d9afc8f Correctly upload bad input with empty lines (#249)

## v0.7.6

- a35fd0b release(v0.7.6): Release version v0.7.6
- edecf87 fix(upload): add API flags (#248)
- 5b0c18b fix(install): Fix installer checksumming
- e290faf Added Jira project key and Project URL flags (#237)
- efa8f60 Improve default logging format (#244)
- f41a1c8 use shasum when available to verify download (#239)
- 92341dc lint(golangci): Fix GolangCI lints (#242)
- e7f9c39 Refactor logging: add structured fields and multiplexed backends (#241)
- 1206d63 allow local install without sudo using LOCAL=true (#238)
- b361644 test(flags): ignore order during combination test
- 7d8509c Support exclamation marks in Gemfile.lock (#230)

## v0.7.5

- 8e28232 release(v0.7.5): Release version v0.7.5
- abbe5d3 Tarball upload bugfixes (#228)
- 674e99b fix(gradle): Strip additional Gradle annotations (#229)

## v0.7.4

- ed1784b release(v0.7.4): Release version v0.7.4
- 6378305 Third-party tarballs (#227)
- 6719541 release(v0.7.3-2): Release version v0.7.3-2

## v0.7.3-2

- 6719541 release(v0.7.3-2): Release version v0.7.3-2
- ee7b30d chore: Add more .gitignore targets
- 14daf05 fix(readtree): Fix 1-index handling
- 2129901 release(v0.7.3-1): Release version v0.7.3-1

## v0.7.3-1

- 2129901 release(v0.7.3-1): Release version v0.7.3-1
- 33e478a fix(nodejs): Allow NPM errors by default
- 4e13873 Add init and analyze flags to default command (#225)

## v0.7.3

- fc83ebc release(v0.7.3): Release version v0.7.3
- 4157cc5 Do not cause config to fail if no supported VCS is detected (#224)
- b8a1457 Carthage support (#218)
- 983716a Added check for locator in upload response (#223)

## v0.7.2

- a259210 release(v0.7.2): Release version v0.7.2
- 017f69d fix(analyzers): Don't short-circuit module discovery on error
- 83fae0f Remove polymorphic monads (#219)

## v0.7.1

- 89661a4 release(v0.7.1): Release version v0.7.1
- eac7f22 fix(cmd): Correctly initialise main default command

## v0.7.0

- 917cd16 release(v0.7.0): Release version v0.7.0
- 4c96066 build(release): Do release preparation outside of Docker container for commit sign-off
- 8768224 build(release): Improve release abort
- 9a99d35 build(release): Improve release process (#217)
- 683d7c7 fix(bower): Fix undefined variable breakage due to bad refactor rebase
- e334f18 test(bower): Add .bowerrc directory handling tests
- 6961225 Merge pull request #214 from fossas/fix/bower-defaults
- 602a951 Refactor analyser discovery (#211)
- 8650211 fix(analzers): fix syntax err
- 6730b18 fix(analyzers): support relative paths in `.bowerrc`
- 1aafc1b fix(buildtools): add bower config constructor to apply defaults
- 9c0cbd2 fix(cmd): Main command should expose debug flag
- d41a952 chore: Consolidate GH templates
- ec6c681 Update issue templates (#208)
- 057e4f6 Enable unit tests in CI (#207)
- 72dc9ab feat(installer): Add install-latest in addition to stable

## v0.7.0-beta.9

- 357c041 chore: 0.7.0-beta.9 release
- 7abe7f4 fix(mvn): Fix Maven initialisation -- read POMs instead of parsing output (#206)
- dbabe3e feat(vcs): Correctly infer VCS settings when within a repo but not at the root (#205)
- 488b88c chore: update dependencies
- 4671510 refactor(init): Make explicit config file existence check for init
- 2f09aae feat(cmd): support --update flag in `fossa init`
- c5f82fe hotfix(install): Hotfix installer URL
- 6bea504 feat(ruby): Configurable `Gemfile.lock` path (#200)
- aa528bd fix(pkg): Fix Composer type parsing

## v0.7.0-beta.8

- 1626218 chore: release 0.7.0-beta.8
- 0ea3cce refactor(build): add releaser checks and fix installer generation
- 9823f3c feat(api): Enable proxy support via environment variables (#199)
- 2017bf9 fix(install): avoid hitting rate limit on install script (#197)

## v0.7.0-beta.7

- 3cd1ac9 fix(api): Correctly set SBT locators

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.10.3 linux/amd64

## v0.7.0-beta.6

- 85d8f02 build(release): Remove development release options
- ef9e578 build(release): Correctly set GOVERSION on release
- b496f40 refactor(sbt): Use graph XML parsing instead of output parsing (#198)
- 1ac53ea fix(log): Do not use ANSI control characters on Windows (#194)

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.10.3 linux/amd64

## v0.7.0-beta.5

- 4aa0803 feat(cmd): Add default command (#192)
- 90905f6 fix(test): Correctly generate URLs for custom fetchers (#191)
- b769ceb refactor(upload): Avoid redundant declarations
- 94b9162 fix(nodejs): Fix IsBuilt detection and Init target
- 971d844 chore: Add TODO structs, doc formatting nits

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.10.3 linux/amd64

## v0.7.0-beta.4

- 6619d34 fix(sbt): Fix SBT project detection

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.10.3 linux/amd64

## v0.7.0-beta.3

- 739329b fix(test): Fail on panic and returned errors
- 3a59e35 fix(sbt): Add ignored lines for SBT output parsing

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.10.3 linux/amd64

## v0.7.0-beta.1

- 2e46b41 refactor(cmd): Refactor output commands
- 7e8b560 fix(test): Fix test bugs
- bb8f1a5 feat(test): Implement fossa test
- b0a8b72 refactor(api): Refactor api package
- fcec296 Implement the report command. (#171)
- d938632 chore: update deps
- ef7b165 feat(ant): Ant analyser ported
- 2b371d0 feat(cocoapods): Cocoapods analyser
- 17202fe WIP: cocoapods
- 902e56f fix(reports): Properly escape report URLs
- 6b5f59d fix(ruby): Parse direct imports from lockfiles
- d2e851f feat(scala): Implement scala analyser
- 4d35c6c fix(test): Fix Python test project name
- 94783fc fix(python): Fix pipdeptree parsing, add regression test

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.10.3 linux/amd64

## v0.6.7

- 2588e95 Merge pull request #174 from fossas/feat/respect-node-unresolved-flag
- b4140c3 fix(builders): pass -B flag to maven analysis
- 867c912 feat(builders): add unresolved flag to nodejs builder
- 517d2c0 doc(builders): fix nuget doc file
- 41123fc doc(builders): update gradle config docs

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.10 darwin/amd64
## v0.7.0-alpha.12

- 6796b63 feat(mvn): Add custom commands
- 506ce8b fix(config): Don't write branch name to config file
- e95e475 WIP: scala work

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.10.3 linux/amd64

## v0.7.0-alpha.11

- 2e60b98 fix(python): Always set m.Deps
- dbb633e fix(api): Add default project title

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.10.3 linux/amd64

## v0.7.0-alpha.10

- c1b7a43 fix(api): Add title flag to API flags
- 14fd035 ci(build): Use base image to avoid bad checkout

## v0.7.0-alpha.9

- 18a28a4 feat(nuget): Implement nuget analyzer
- 724d8fb WIP: NuGet
- b28604d feat(api): Send custom revision IDs
- 28b9461 feat(maven): Implement Maven analyser
- 156ccb4 refactor(graph): Use code generation instead of reflection
- 52d2482 WIP

## v0.5.2

- 09c0f55 backport(api): Backport branch flag to 0.5.x

## v0.7.0-alpha.4

- e4bf442 feat(go): Manifest strategies
- c6c959f feat(go): glide, gpm support
- 01edf8d refactor(go): Remove dead code, add make test command
- ecc397b ci: CircleCI configuration chores
- dd0772b ci: Don't use env vars in non-shell commands
- f72b2bc build(docker-test): Don't build all of Kubernetes (this kills the crab)
- 7d65cf2 refactor(docker): Use Quay for building Docker images
- 55ddd49 feat(go): vndr/gpm, multi-project vendoring support, integration tests on Docker
- 0250f61 feat(go): nested vendoring support, automated integration tests
- 23526c0 chore: Clean up merge cruft
- 2f83e6f Merge branch 'master' into wip/v0.7.0
- 89a3103 fix(api): Fix uploading UX nits and URL formatting issues
- fccaa00 refactor(go): Support Godep, add integration tests
- 79a162a refactor(api): Remove extraneous build data
- d90cc8a feat(api): Add normalized imports
- 1a88076 WIP: Go dep analysis
- dbd027b Merge branch 'wip/v2' of github.com:fossas/fossa-cli into wip/v2
- fae5186 WIP: Go option-based analyzers
- f943789 WIP: Go analyzer strategies
- c211600 WIP: analysis command refactor complete
- 85e221c WIP
- b4c5bda WIP
- a356dbf WIP
- 527ecce WIP
- 2171372 WIP
- cc58b15 WIP: Go option-based analyzers
- 8899464 WIP: Go analyzer strategies
- 64d77b4 WIP: analysis command refactor complete
- 20365ba WIP
- f4d22b6 WIP
- 1c6c5e8 WIP
- e8bfc5c WIP
- 52f7fd3 WIP

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.10.3 linux/amd64

## v0.7.0-alpha.8

- da53a35 feat(php): Implement PHP analyser
- 4149700 feat(bower): Implement bower analysers
- dc57fe3 WIP: switching to config file v2

## v0.7.0-alpha.7

- 39b3d19 feat(gradle): Implement Gradle analyser
- 8ba5602 WIP: gradle
- 7cdef88 feat(config): Warn when users pass options but use config file
- 35638c7 fix(go): Don't include root in transitive dependency graph

## v0.7.0-alpha.6

- b9cddb0 feat(ruby): Add Ruby analysis
- b06eb62 test(fixtures): Remove obsolete fixtures
- 1c09b4e test(go): add Jaeger to testing suite
- 0de97ca feat(python): Python analyser
- 82397b2 feat(nodejs): Add NodeJS analyzer
- a0c22ff build(docker): Refactor test-base Dockerfile to avoid long build times
- 5cccf4e chore: Add ISSUE_TEMPLATE
- 7ab9965 Merge branch 'master' into wip/v0.7.0
- 515102d build: Rename docker-devel target to docker
- 9447e2d Merge pull request #160 from fossas/fix/fix-hash-calculation
- 1239291 fix(builders): fix hash calculation

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.10.3 linux/amd64

## v0.7.0-alpha.5

- 1c09b4e test(go): add Jaeger to testing suite
- 0de97ca feat(python): Python analyser
- 82397b2 feat(nodejs): Add NodeJS analyzer
- a0c22ff build(docker): Refactor test-base Dockerfile to avoid long build times
- 5cccf4e chore: Add ISSUE_TEMPLATE
- 7ab9965 Merge branch 'master' into wip/v0.7.0
- 515102d build: Rename docker-devel target to docker
- 9447e2d Merge pull request #160 from fossas/fix/fix-hash-calculation
- 1239291 fix(builders): fix hash calculation

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.10.3 linux/amd64

## v0.7.0-alpha.5

- 1c09b4e test(go): add Jaeger to testing suite
- 0de97ca feat(python): Python analyser
- 82397b2 feat(nodejs): Add NodeJS analyzer
- a0c22ff build(docker): Refactor test-base Dockerfile to avoid long build times
- 5cccf4e chore: Add ISSUE_TEMPLATE
- 7ab9965 Merge branch 'master' into wip/v0.7.0
- 515102d build: Rename docker-devel target to docker
- 9447e2d Merge pull request #160 from fossas/fix/fix-hash-calculation
- 1239291 fix(builders): fix hash calculation

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.10.3 linux/amd64

## v0.7.0-alpha.3

- c6c959f feat(go): glide, gpm support
- 01edf8d refactor(go): Remove dead code, add make test command
- ecc397b ci: CircleCI configuration chores
- dd0772b ci: Don't use env vars in non-shell commands
- f72b2bc build(docker-test): Don't build all of Kubernetes (this kills the crab)
- 7d65cf2 refactor(docker): Use Quay for building Docker images
- 55ddd49 feat(go): vndr/gpm, multi-project vendoring support, integration tests on Docker

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.10.3 linux/amd64

## v0.7.0-alpha.2

- 0250f61 feat(go): nested vendoring support, automated integration tests
- 23526c0 chore: Clean up merge cruft
- 2f83e6f Merge branch 'master' into wip/v0.7.0
- 607de55 fix(gradle): Improve gradle error logging
- 62ae510 fix(gradle): Fix gradle version detection
- cfe95a5 Merge pull request #151 from joshuapetryk/josh/powershell
- 6213639 Add Powershell streams
- ea187bb Fix syntax error with temp dir path join
- 9cc0989 fix(builders): fix go-bindata error
- 2c39f70 doc(license): add license header and link to pipdeptree

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.10.3 linux/amd64

## v0.6.6

- 607de55 fix(gradle): Improve gradle error logging
- 62ae510 fix(gradle): Fix gradle version detection
- cfe95a5 Merge pull request #151 from joshuapetryk/josh/powershell
- 6213639 Add Powershell streams
- ea187bb Fix syntax error with temp dir path join

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.10.3 linux/amd64

## v0.7.0-alpha.1

- 89a3103 fix(api): Fix uploading UX nits and URL formatting issues
- fccaa00 refactor(go): Support Godep, add integration tests
- 79a162a refactor(api): Remove extraneous build data
- d90cc8a feat(api): Add normalized imports
- 1a88076 WIP: Go dep analysis
- dbd027b Merge branch 'wip/v2' of github.com:fossas/fossa-cli into wip/v2
- fae5186 WIP: Go option-based analyzers
- f943789 WIP: Go analyzer strategies
- c211600 WIP: analysis command refactor complete
- 85e221c WIP
- b4c5bda WIP
- a356dbf WIP
- 527ecce WIP
- 2171372 WIP
- cc58b15 WIP: Go option-based analyzers
- 8899464 WIP: Go analyzer strategies
- 64d77b4 WIP: analysis command refactor complete
- 20365ba WIP
- f4d22b6 WIP
- 1c6c5e8 WIP
- e8bfc5c WIP
- 52f7fd3 WIP

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.10.3 linux/amd64

## v0.6.5

- 9cc0989 fix(builders): fix go-bindata error
- 2c39f70 doc(license): add license header and link to pipdeptree

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.10 darwin/amd64
## v0.6.4

- e17ebd5 fix(nuget): Fix NuGet discovery path
- 1423561 fix(install): fix powershell install script

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.10.3 linux/amd64

## v0.6.3

- c86fa51 Merge pull request #143 from fossas/feat/compute-dependency-hashes
- 9049c67 fix(common): bump timeout to 60s
- e4a5aec Merge branch 'master' into feat/compute-dependency-hashes
- 9b8818f feat(install): modify windows install script
- a535311 test(go): Add previously ignored govendor manifest fixture
- 9b73bc7 Add Powershell script for Windows based CI pipeline
- e704dc6 chore(lint): correct lint ignore into golangci-lint format
- dc558a5 chore(lint): silence gas linter
- e323dd0 feat(builders): return hex string for hashing utils
- bd0af6a feat(builders): add dependency hashing for ant
- b908880 feat(module): define hashes type
- 01a97ce chore: Cleanup merge cruft
- 9204650 fix(bower): Fix bower IsBuilt check
- aae8bf6 refactor(builders): Separate builders into distinct packages
- 04248c7 doc(readme): add slack badge and link
- 92553b2 Detect and install go-bindata if missing
- 9c77639 fix(upload): Fix upload report link and API endpoint
- eb2d07d fix(npm): Allow empty node_modules folders when no dependencies

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.10 darwin/amd64

## v0.6.2

- 3453eb5 feat(upload): Configurable upload branch

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.10.2 linux/amd64

## v0.6.1

- 269a380 feat(builders): improve ant name parsing
- ec20967 fix(builders): #139 fix out of range error with ant
- 4898773 newline at end of file
- e197444 add override for zip format on windows to goreleaser
- f661ebf doc(support): document cocoapods support

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.10 darwin/amd64

## v0.6.0-beta.1

- 41d8e4d fix(upload): Get custom project titles from 'project' configuration
- e37d325 fix(node): Use NPM_BINARY when set
- d50d94a chore: Update dependencies
- 4913fc0 refactor(cmd): Don't initialise API unless needed
- a8988f7 refactor(cmd): Remove IO services
- 604b036 fix(cmd): Fix merge conflicts
- 58e174a refactor(cmd): Move commands into one subtree
- f68b9ab refactor(cmd): Refactor upload, update, version commands
- 2d4a88c chore(builders): ignore generated files
- a66e3d4 Merge pull request #136 from fossas/fix/sbt-deps
- 1bd9039 changed fetcher type from sbt to mvn for the SBT Builder
- 980691d feat(log): Add structured field logging
- 6f8408a fix(go): Remove debugging around internal imports
- 7418dfa fix(go): Fix recursion in internal imports
- e702181 fix(go): Debug recursion in internal imports

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.10.2 linux/amd64

## v0.6.0-alpha.4.gopaths

- fca1223 WIP
- 4b4d5a7 refactor(log): Improve logging diagnostics, fix Go project names
- cc9586b fix(go): Fix go import resolution from within vendored packages
- 9e856c8 chore: Add new dependencies to lockfiles
- 47b26f8 test: move fixtures to testdata/
- 3fe3ad3 feat(builders): refactor ant builder to avoid nesting
- f82135c fix(analyze): fix syntax error
- d9fc322 Merge pull request #134 from fossas/feat/ant-support
- 15743b6 Merge branch 'master' into feat/ant-support
- d6276f0 chore(builders): complete ant comment
- 1ed9769 test(builders): add ant fixture
- 022ff8f doc(readme): update api doc link
- 93ac0ea refactor(log): Migrate to idiomatic logging package
- a348971 refactor(log): Add idiomatic logging package
- 119c635 Update FOSSA status badge
- 27ea4ff feat(builders): add some basic jar name parsing for ant
- c6eb417 feat(analyze): refactor analysis data model
- b0cf179 doc(builders): add ant docs
- b888620 feat(builders): add ant support
- 31affba add more aliases (#133)

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.10.2 linux/amd64

## v0.6.0-alpha.3

- 5f0299b fix(upload): Escape upload report URL

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.10.1 linux/amd64

## v0.6.0-alpha.2

- 4a1bdd2 fix(upload): Fix managedBuild flag for custom fetcher upload
- 1e27f85 feat(builders): #44 add Cocoapods integration with path data (#130)

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.10.1 linux/amd64

## v0.6.0-alpha.1

- 75e6747 fix: Ignore root dep locators when computing import paths
- 6b1e7cb ci: Add go-bindata to Dockerfile
- 6acc2f7 fix(npm): Don't include extraneous root
- 448c1fe feature(api): Serialize locators in FOSSA format
- cc1cc9a feat(ruby): Ruby path data parser
- 9e00849 Merge branch 'master' of github.com:fossas/fossa-cli
- 66bb021 feat(sbt): SBT path data parsing
- 0a58a70 feat(sbt): SBT path data parsing
- 75f22ce feat(pip): Pip path data parsing
- f14664e Merge branch 'next'
- 316fe02 feat(nuget): NuGet path data (very slow)
- ddd17ef [WIP] Path parsing for NuGet
- 574e421 fix(npm): Allow NPM to have errors because npm i is inconsistent
- 3a7c81b feat(nodejs): Add path data parsing
- 8ffd098 fix(go): Correctly handle internal and root packages
- c3f0847 feat(maven): Maven relationship data
- e1bb72e feat(gradle): Add gradle path data and fix bullshit memory bug
- 413f55a feat(go): Fast golang path data
- c21dc4c feat(go): Golang path data (very slow)
- 06d9cd8 feat(composer): Add composer path data
- c31a975 feat(bower): Add origin path detection
- 571cf4e refactor(cmd): Use IO services for effects [WIP]
- 013e269 feat(di): Implement common side-effecting services

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.10.1 linux/amd64

## v0.5.1

- 64ddd93 Merge pull request #127 from fossas/fix/support-bower-custom-folder
- e142a95 fix(builders): #125 add bower component dir resolution
- da16a44 doc(readme): update badge to use provided build
- 986f053 chore: fix typo comments, remove dead code

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.10 darwin/amd64

## v0.5.0

- 2954eee Merge pull request #121 from fossas/feat/nuget-support
- 8d58e9c test(builders): add nuget fixtures
- 6884192 doc(readme): update readme
- 99d3f8c Merge branch 'master' into feat/nuget-support
- 1dbda7d feat(build): turn built module error into a warning
- bc5811c doc(builders): add nuget docs
- 377a05a feat(builders): add nuget lockfile parsing
- 843299a feat(builders): add nuget support
- c168cce chore(deps): Update dependencies
- 5e146c5 feat(builders): Add Pip support

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.10 darwin/amd64

## v0.4.6-1

- a708d86 fix(go): Work around golang/go#16333

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.10 linux/amd64

## v0.4.6

- 85c1788 Merge pull request #116 from fossas/feat/support-gradle-root-deps
- 99a9552 fix(builders): fix PR comments
- 7ef81e0 feat(cmd): add spinner to init cmd
- 0583626 doc(builders): add another gradle common task
- 748f307 doc(builders): improve gradle builder docs
- bffa8df Merge branch 'feat/support-gradle-root-deps' of https://github.com/fossas/fossa-cli into feat/support-gradle-root-deps
- db5b36b fix(builders): fix gradle syntax err
- 60818b4 Merge branch 'master' into feat/support-gradle-root-deps
- 1030bd6 fix(builders): set TERM=dumb when running gradle dependencies task
- 15f5af5 doc(builders): add better gradle docs
- 5b73fa4 fix(builders): allow for empty configuration in gradle
- 97c7315 feat(builders): #114 support root dependencies task

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.10 linux/amd64

## v0.4.5-1

- 8b28d1f fix(go): Don't require Go project folder for build, and do actual Go build
- 26c0d12 chore: update CHANGELOG

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.10 linux/amd64

## v0.4.5

- 7ee5a3c fix(installer): Fallback to su if sudo is unavailable
- 70fc3a5 fix(builders): Don't fail on non-fatal missing binaries
- 91944c9 chore: Add TODOs, ignore third_party in autoconfig
- ceac46e Various improvements to install.sh (#109)
- 99cf015 test(fixtures): Use shell script instead of submodules for fixtures to avoid slow go get
- 3de42a8 test(java): Pin java submodule fixture commits
- 6c4db9b test(go): Ignore golang test fixture vendored dependencies
- b88e58e fix(update): Fix incorrect latest version check
- 019b3d0 fix(go): allowUnresolved should also suppress lockfile errors for builds
- 91183e7 doc(contributing): add issue assignment
- 73b55c9 Merge pull request #107 from fossas/add-code-of-conduct-1
- a6a1f97 doc(code): add code of conduct
- 52af690 doc(readme): add meetup link
- abc1399 feat(upload): switch url to dep report
- 7a7961d chore(license): switch to MPL-2.0
- c19b51b Refactor module functionality (#100)
- 6600859 build(Makefile): Build to GOPATH instead of local directory
- 4fde932 Improve Makefile, add multiple targets (#98)
- 44fb451  Introduce vendor directory into the repo (#99)
- 16cf268 Release v0.4.4

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.10 linux/amd64

## v0.4.4

- 46d1dbd feat(go): Implement Go module discovery (#97)
- b476653 fix(go): Do Go import tracing with go list instead of depth (#96)
- 451ab20 README: Fix rendering of a link to `https://fossa.io` (#88)
- 2893145 chore(cli): update help text
- c285037 Merge branch 'master' of https://github.com/fossas/fossa-cli
- d604f5b release(0.4.3): update readme and installer
- 8235155 revert(install): remove sha validation

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.10 linux/amd64

## v0.4.3

- 57b397c doc(notice): clean up notice
- 9d05a2f chore(installer): add original license notice
- 4c69500 doc(readme): add `fossa test` output
- 3826022 doc(readme): add goreportcard badge
- 1cd47e4 feat(report): add default report cmd
- 414ca08 doc(readme): fix notice links
- 8b1c3ba doc(notice): move notice to raw file
- d090cfd doc(report): add license notice docs
- 21f29ad docs(readme): add report feature
- d8e60d2 doc(readme): fix link to contribution guidelines
- c6640d0 doc(readme): add output examples
- b57d43b doc(readme): add report PV
- 87a3429 feat(cli): improve error messages from servers
- b8a2912 doc(readme): improve readme copy
- 6a72302 fix(golang): do not error on lack of vendor folder
- 869df5a doc(readme): additional cleanup
- 8957638 doc(readme): update background section
- 48107e1 doc(readme): resize header
- a69c9c5 doc(readme): refactor home doc and readme
- 7f10415 doc(readme): update readme and user guide
- 9e3bf98 Merge pull request #66 from fossas/feat/report-command
- 835c014 fix(upload): Add more debugging stuff to upload and use standard API function
- 83b0d07 fix(report): Add fetcher flag
- c6e9d2e feat(report): Implement reports using revisions API instead of dependencies
- e47ea99 fix(report): Use SPDX ID for licenses
- b6dbdfc feat(report): Add basic NOTICE generation
- 5635878 doc(cli): update project help text to enforce URL
- dc738e4 feat(config): refactor git normalizing into separate function
- 9bd1acc feat(upload): add title support for managed projects
- 08e3f61 test(fixtures): Use shallow submodules for fixtures to improve clone time
- 703578e chore(fixtures): Keep fixtures up to date
- dc0ac39 Merge pull request #84 from fossas/feat/add-build-interactive
- 6411b37 feat(build): add interactive feedback to `fossa build`
- 01e3820 Merge pull request #80 from fossas/fix/mvn-colors
- bfe8a33 Merge pull request #81 from fossas/fix/non-custom-fetchers
- 7f4d52f Merge pull request #82 from fossas/fix/fix-builders-config
- db9710b Merge pull request #83 from fossas/feat/install-script
- a5202e0 chore(builders): fix typo in comment
- c77092b fix(mvn): Run Maven in batch mode to turn off ANSI color chars
- 0b0c833 fix(builders): fix ruby build check
- 4a6e0dd style(installer): Use consistent whitespace (2 spaces)
- 6801f94 feat(builders): improve autoconfiguration for gradle
- b954112 fix(builders): fix relative path for maven
- 0c3de4a docs(installer): Update README with installer
- 150c2bc feat(installer): Add bash install script
- f0ac553 fix(ci): Fix .fossa.yaml to not use implicit managed builds

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.10 darwin/amd64

## v0.4.2

- 5fe21df feat(builders): add ability to add configuration

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.10 darwin/amd64

## v0.4.1

- d0720f8 Merge pull request #40 from fossas/alex/managed-builds
- c3aa016 doc(readme): #32 add one-liner install
- 3105635 Merge branch 'master' into alex/managed-builds
- ddcc341 Merge pull request #74 from fossas/feat/gradle-support
- d073805 Merge pull request #75 from fossas/fix/fix-builder-paths
- 5fdf4a5 doc(builders): add initial gradle docs
- c3ccc74 switch back to using fetcher
- 80555e4 chore(builders): fix comments and nits
- 5a63b9f test(submodules): Update submodule commits
- 93dee58 test(gradle): Add Gradle fixtures and Docker test tools
- f23ee34 fix(init): fix maven and ruby default module naming
- 272363c feat(init): add more ignores
- dbd8516 fix(builders): make relative paths for node and ruby builders
- f2e5560 feat(builders): add gradle task and dependency parsing
- 4d60fd3 feat(builders): add initial gradle builder
- 4d9806c change flag to ExistingProject in config. defaults to false
- 5c98745 add or clause with revision
- 12c077b added fix to function
- a1fb05f changes after rebase
- d7fbaf2 added custom-project flag
- 5c5bdcd updated comment
- c20e574 PR changes
- 0e52c61 fixed comment
- 1b87475 removed locator from config. We now have project and revision

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.10 darwin/amd64

## v0.4.0

- a2b474c Merge pull request #73 from fossas/feat/upload-locators-flag
- b2c680a feat(upload): Add Locators flag and data format

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.10 linux/amd64

## v0.3.1

- ec4e164 Merge pull request #36 from fossas/feat/selfupdate
- da90056 fix(http): Improve timeout handling
- d577588 fix(http): Correctly handle EOF timeouts
- 6300fa4 fix(http): Always close HTTP request connections
- 546d381 ci: Improve CI caching
- 29d496b ci: Improve logging and diagnostics on upload failure
- 7393553 style: fix PR nits
- 765cbcd fix(update): fix update default logic
- 7ce1571 feat(update): represent states better in update cmd
- 95e446e chore(update): fix nits
- 9e570f9 feat(update): add debug logging for update
- 5d76012 feat(update): add semver parsing
- 2e9af5d feat(update): #23 add fossa update command
- 89a8aa0 doc(go): document gdm support
- 49da5b4 doc(go): add gdm support
- 3f8f208 Merge pull request #37 from fossas/feat/go-gdm-integration
- 134b777 Add gdm to Dockerfile tools
- 056fca5 feat(go): Add gdm support
- e496598 docs: Add upload format user guide reference to walkthrough
- 5daa5be docs: Upload format documentation
- 0af727e Improve `user-guide.md` formatting
- 667cfb9 Merge pull request #34 from fossas/feat/docs
- a7e4b6d docs: README overhaul + user guide update
- 86089cb feat(upload): Add custom upload command
- 3115938 Merge pull request #28 from fossas/ci/run-provided-build
- 9d06606 ci(license-test): Run license check on CI
- ddc1bf7 Run FOSSA build after tests complete
- 0f0fe37 Merge pull request #25 from fossas/feat/add-test-command
- fcaca81 feat(test): Add JSON output to test cmd
- d65a651 fix(misc): Fix spinner issues, whitespace issues, golang isInternal, debug formatting, test unmarshalling
- 891526a refactor(test): Refactor test command and fix timeout
- 743c35f refactor(errors): Use errors.New instead of fmt.Errorf when there is no format string
- 283440e fix(test): fix timeout checks
- 71e6169 fix(config): fix locator normalization #21
- 2fef009 docs(test): properly document test cmd
- 37f8a21 fix(common): handle request errors properly
- 4ac31ad chore(errors): prefer fmt.Errorf to errors.New
- 89dcaea feat(test): add test command
- 661a7a5 Merge pull request #22 from fossas/refactor/common-build-utils
- 5c946b6 docs(readme): update readme
- 2f890b5 docs(readme): update readme
- f21c9ab refactor(sbt): Refactor SBT builder
- c76b0d4 refactor(ruby): Refactor Ruby builder
- 8feac38 refactor(nodejs): Refactor Nodejs builder
- 1f499e5 refactor(mvn): Refactor Maven builder
- c73cf5d refactor(go): Refactor Go builder
- 6284822 refactor(build): Refactor Composer builder
- e95f717 refactor(build): Refactor common utils + Bower builder
- 5e07519 Merge pull request #18 from fossas/feat/rpm-support
- fedeca4 Merge pull request #17 from fossas/fix/circleci-tests
- df44909 doc(build): add vendoredarchives docs
- f027f34 feat(build): add archive format support
- 455abd0 ci(circle-ci): Fix CircleCI config for new tests
- b7dff83 test(sbt): Add first test
- 920ac9b test(docker): Create docker image with build tools
- 7897993 doc(readme): update readme

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.10 linux/amd64

## v0.3.0

- e84d0ea build(merge): Remove bad file merge
- 336406d Merge pull request #15 from fossas/feat/overhaul
- 3281995 feat(sbt): Improve SBT instrumentation
- 1929bef docs: Massive documentation overhaul
- becd5e3 Add SBT parsing + test fixtures
- baa673e feat(ruby): Add Ruby parsing + test fixtures
- b63d740 feat(mvn): add Maven support + test fixture
- 15e6175 refactor(logging): Use %#v for debug logging
- 6c4de98 feat(go): correctly resolve packages + add test fixtures
- d40578a feat(go): Add much better go support
- 60a1e38 docs: Add basic documentation
- 0634835 feat(composer): Add composer parsing + test fixtures
- 4fbc44f feat(bower): Add bower parsing + test fixtures
- 222bf74 feat(cmd): Add uploading to default command
- d909f16 refactor: Refactor CLI, with NodeJS support

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.9.4 linux/amd64

## v0.2.6

- f53f6e1 Preliminary SBT support
- f6e14ea fix(go): Allow unresolved golang dependencies
- 9ad32d4 chore(readme): Update README with gigantic warning
- eba8735 fix(env): fix env var for fossa api key
- 4df5715 feat(docs): add maven docs and alpha notice
- e3ccd88 chore(doc): add status badges to README
- 0a2a634 Merge pull request #7 from fossas/ci/circleci-tests
- 21d5d2c ci(tests): Add CircleCI tests
- 17d5e5f chore(doc): add DCO
- 7d66202 Clean up unused Makefile lines

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.9.2 darwin/amd64

## v0.2.5-1

- 605a9c0 build(versions): Fix version linking

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.9.3 linux/amd64

## v0.2.5

- 20b2d6b chore(deps): Update deps, prune unused constraints
- b16e851 fix(commonjs): Substitute doublestar for zglob to fix data race
- c7d449d build(version): Add revision to --version output
- fdf200a fix(js): fix concurrency race condition
- 4a234b3 feat(config): Allow server endpoint to be set by environment variable
- 38d8615 chore(dep): Commit lockfile changes
- b5b71eb fix(maven): fix maven verify logic
- 79b5b64 fix(cmd): move validation to upload cmd

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.9.3 linux/amd64

## v0.2.4

- b0d5c7a Release v0.2.4
- 0e20f0b chore(flags): clean up flag parsing
- 41c2d3e fix(config): refactor to fix locator flag setting
- 668a4f9 Release v0.2.3
- 4c0286c fix(cmd): make build cmd runnable again
- a848a58 chore(errors): reformat some error copy

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.9.2 darwin/amd64

## v0.2.3

- 41c2d3e fix(config): refactor to fix locator flag setting
- 668a4f9 Release v0.2.3
- 4c0286c fix(cmd): make build cmd runnable again
- a848a58 chore(errors): reformat some error copy

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.9.2 darwin/amd64

## v0.2.2

- 867cc0b Release v0.2.2
- 732038c feat(errors): better error handling and feedback
- b0ec539 feat(config): add ability to read from custom config file
- 2574402 fix(commonjs): fix node_modules traversal in subdir

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.9.2 darwin/amd64

## v0.2.1

- 3f7ccf0 Release v0.2.1
- 5a6f382 feat(config): add default run mode to output json and exit w/o upload

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.9.2 darwin/amd64

## v0.2.0

- 7243d0f Release v0.2.0
- eb054a3 feat(composer): support composer builds in subdirs
- 3c2bccc feat(gems): support bundler builds in subdirs
- 58d98df fix(maven): fix maven output command
- 811ecb0 feat(maven): use module manifest in builds
- 6c5ab1c feat(bower): support bower builds in subfolders
- 2c4b1a6 feat(build): support multi-module builds

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.9.2 darwin/amd64

## v0.1.0

- f36ce39 fix(release): fix .goreleaser entry point
- 124bb47 chore(release): change package entry point
- 55acfd3 feat(upload): send report link
- f678cf0 fix(build): fix locator and build output
- 59eec8a fix(cmd): Guard against under-specified user input
- 5161162 feat(cmd): Refactor CLI and config structure
- 97626c5 feat(config): Read API key from environment variable
- 384b13d Merge pull request #6 from fossas/feat/3-upload-builds-results
- 0537aaf Merge branch 'feat/3-upload-builds-results' of github.com:fossas/fossa-cli into feat/3-upload-builds-results
- 4aec471 feat(upload): #3 add build upload cmd
- 271ba79 Merge pull request #5 from fossas/feat/4-fossa-yaml
- f70ca36 fix(config): Remove debugging statement
- 64c67ca feat(config): Add config options for locator
- 1e98346 feat(upload): #3 add build upload cmd
- d4383d2 fix(main): Remove debugging comment
- 0dd5ee9 feat(config): Set existing build options from configuration file
- 6010976 feat(config): Read config file values
- df2d7d8 Merge pull request #2 from fossas/feat/1-go-get
- b476866 feat(go): Run go get on incomplete builds
- 9f47778 fix(gem): install only production deps by default
- aa4ba7d fix(json): fix json keys in dependency

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.9.2 darwin/amd64

## v0.0.0

- 699d58d feat(build): ignore RawDependencies in serialization
- 834466a feat(build): refactor dependency and logging
- 82f4830 chore(build): ignore dist folder
- 74edd98 Merge branch 'master' of https://github.com/fossas/fossa-cli
- 5e71265 feat(build): add release spec
- cf3de9b Merge branch 'master' of github.com:fossas/fossa-cli
- 0b7331d feat(go): Fall back to import path tracing when no lockfiles
- 7305c46 feat(log): add logging config
- b3d5b72 fix(gem): fix bundle command
- f87cc95 feat(composer): composer support
- f30e125 fix(build): fix build and maven command
- 9221cea feat(build): update logging and docs
- 36f5668 Merge branch 'master' of https://github.com/fossas/fossa-cli
- e2f557a feat(mvn): add maven support
- 5773c86 feat(go): Add glide, godep, govendor, vndr support
- 8ebfd7a feat(go): Add dep support
- f555b48 style(golint): Fix lint and vet warnings
- 7fa1098 doc(readme): update licensing guidance
- 1afe4a0 doc(readme): update readme
- 103d685 doc(license): add readme and license
- 1800cc3 Add Gopkg manifest
- 0d43673 feat(bower): add bower suppot
- 364cebf feat(cli): Refuse to build unless --install flag is explicitly passed
- 5f117dc fix(npm): Fix npm build logic
- 05ae3f5 Initial Commit

---
Automated with [GoReleaser](https://github.com/goreleaser)
Built with go version go1.9.2 darwin/amd64
