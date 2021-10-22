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
- 2a2a23f14 fix (report dependencies) Change report dependencies to track Fossa.com results (#419)
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
- 451ab20 README: Fix rendering of a link to https://fossa.io (#88)
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
