# User Guide

## Configuration


The intention of the fossa-cli is to create an accurate configuration by running `fossa init` followed by `fossa analyze`. 

The CLI can be configured by either generating and modifying a configuration file, `fossa.yml`, from `fossa init` or by providing flags to each command.

1. Configuration file `.fossa.yaml` generated from `fossa init`.
1. Flags passed to each command

### `.fossa.yaml`
The CLI searches for a `.fossa.yaml` in the working directory. Information about each of the fields and customization can be found in [.fossa.yml](/docs/config-file) documentation.

#### v1 (current)

```yaml
version: 1

cli:
  # # Defaults to https://app.fossa.io
  # server: https://fossa.on-prem
  api_key: some-key-here
  # # If `project` or `locator` are unset, infer locator from VCS.
  # project: git+github.com/fossas/fossa-cli
  # locator: git+github.com/fossas/fossa-cli$revision

analyze:
  modules:
    - name: fossa-cli
      type: gopackage
      target: ./cmd/fossa
      path: ./cmd/fossa
      options:
        allowUnresolved: true
```

### Modules and the module spec

The CLI must be configured to build specific modules as entry points. 

Providing entry points helps reduce false positives. For example, a Go project may have a `docs/` folder that provides documentation using a Ruby static site generator. If the static site generator is not distributed to users, it's generally not necessary to analyze it for license obligations.

When used with the CLI flag, the module spec is defined as:

```
module1,module2,module3
```

where a module is defined as:

```
module_type:module_path
```
`module_path` can be specified as either a relative path from the working directory of the `fossa` invocation, or as an absolute path.

`module_type` is one of the following:

- `nodejs`: Node.js support (including NPM and Yarn)
- `bower`: JavaScript support using Bower
- `composer`: Composer support
- `go`: Go support for `dep`, `glide`, `godep`, `govendor`, and `vndr`
- `maven`: Java support using Maven
- `gradle`: Java support using Gradle
- `bundler`: Ruby support using Bundler
- `sbt`: Scala support using SBT
- `vendoredarchives`: Support for Node modules inside RPMs. This is highly experimental.

The characters `:` and `,` are regarded as special characters. They may be escaped by a preceding backslash (`\`). <!-- TODO: this escaping is not actually implemented. -->

#### Examples

- `nodejs:.,rubygem:./docs`
- `go:./cmd/fossa`

## Project Analysis
### `fossa analyze`

## Uploading Custom Builds

If your build is too complex or your build system is heavily customized, `fossa` may be unable to correctly analyze your build. In that case, you can still upload build information using `fossa upload` to fossa.io for build analysis and issue triage.

### Data Format

Custom builds are uploaded in the following format:

```typescript
// You can upload builds for many modules at a time.
type UploadData = Module[];

type Module = {
  Name: string, // The name of the module/entry-point. This is currently ignored.
  Type: string, // The type of the entry point.
  Manifest: string, // The path of the project manifest. This is currently ignored.
  Build: {
    Artifact: string, // The build artifact name. This is currently ignored.
    Context: any, // Metadata for the build. This is currently ignored.
    Succeeded: boolean, // Generally, this is `true`.
    Error?: string, // An optional error string for failed builds.
    Dependencies: Dependency[]
  }
};

type Dependency = {
  locator: string, // See below.
  data?: any // Optional metadata for the dependency. This is currently ignored.
}
```

### Locator spec

Projects and packages in FOSSA are identified by their _locator_, which is a combination of ecosystem, package name, and package revision.

An _ecosystem_ is one of:

<!-- - `bower`
- `cargo`
- `cart`
- `comp`
- `cpan`
- `gem`
- `git`
- `go`
- `mvn`
- `npm`
- `nuget`
- `pod`
- `pip` -->

- `bower`: Bower dependencies
- `comp`: Composer dependencies
- `gem`: RubyGems dependencies
- `git`: `git` submodules
- `go`: `go get` dependencies
- `mvn`: Maven dependencies
- `npm`: NPM dependencies

The _package name_ is the name of the package within the ecosystem (e.g. `express`).

The _revision_ is the revision identifier of the package within the ecosystem (e.g. `3.0.0`).

These are combined as:

```
ecosystem+package$revision
```

#### Example

```
npm+express$3.0.0
go+github.com/golang/dep$06d527172446499363c465968a132d7aa528e550
mvn+org.apache.hadoop:hadoop-core$2.6.0-mr1-cdh5.5.0
```

## CLI Reference

All flags should be passed to the invoked subcommand. Global flags are currently NOT supported.

### `fossa`
Create a configuration file and run an analysis, uploading the results to FOSSA. Can optionally build if required.

#### Example
```bash
# Runs an analysis and uploads the results.
FOSSA_API_KEY=YOUR_API_KEY fossa -m go:./cmd/fossa
```

#### Flags
##### `-c, --config file_name`
Path to a configuration file. Defaults to `.fossa.yaml`.

##### `-p, --project project_name`
Sets the configuration value for the FOSSA project.

##### `-r, --revision revision_hash`
Sets the configuration value for the FOSSA project's revision.

##### `-e, --endpoint url`
Sets the endpoint of the FOSSA server. Useful for on-premises deployments.

##### `-m, --modules module_spec`
Passed to `fossa build` and `fossa analyze`.

<!-- ##### `-i, --ignore ignore_spec`
Passed to `fossa analyze`. -->

##### `-o, --output`
Passed to `fossa analyze`.

##### `-b, --build`
Runs a build if required.

##### `-f, --force`
Passed to `fossa build`.

##### `--debug`
Print debugging information to `stderr`.

##### `-v, --version`
Print the version, then exit.

##### `-h, --help`
Print a help message, then exit.

### `fossa init`

Makes a best-effort attempt at inferring the correct configuration from current system state. If successful, it will write the configuration to a new or existing config file (defaults to `.fossa.yml`).

If there are no modules defined in the configuration, it will scan the working directory for code modules to analyze.  You can pass the `--overwrite` to overwrite any existing modules in configuration.

By default, this command will filter out any modules that have `docs`, `test` or `example` in the path.  You can disable this by passing the `--include-all` flag.

#### Flags
##### `-O, --overwrite`
Force scanning for new modules and overwrite any existing config.

##### `--include-all`
Include any suspicious modules that would have been filtered out (`docs`, `test`, `example`).

##### `--debug`
Print debugging information to `stderr`.

##### `-h, --help`
Print a help message, then exit.


### `fossa build`

Makes a best-effort attempt at building the project using default build conventions.

#### Example
```bash
# No API key is required for builds
fossa build -m go:./cmd/fossa
```

#### Flags
##### `-c, --config file_name`
Path to a configuration file. Defaults to `.fossa.yaml`.

##### `-m, --modules module_spec`
Sets the modules to use as entry points for building the project.

##### `-f, --force`
Clear cached build artifacts (such as a `node_modules` folder), and run the build from scratch.

**WARNING:** This command will delete cached build artifacts! This is generally not what you want, unless you intend to use FOSSA to build your production binary. If you build your production binary ahead of time, you should NOT delete your build artifacts. Deleting build artifacts and re-running the build may cause some build systems to non-deterministically resolve a different set of dependencies, which will make your FOSSA analysis less accurate.

##### `--debug`
Print debugging information to `stderr`.

##### `-h, --help`
Print a help message, then exit.

### `fossa analyze`

Analyzes the project for a list of its dependencies, optionally uploading the results to FOSSA.

#### Example
```bash
# Show analysis results
FOSSA_API_KEY=YOUR_API_KEY fossa analyze --output
```

#### Flags
##### `-c, --config file_name`
Path to a configuration file. Defaults to `.fossa.yaml`.

##### `-p, --project project_name`
Sets the configuration value for the FOSSA project.

##### `-r, --revision revision_hash`
Sets the configuration value for the FOSSA project's revision.

##### `-e, --endpoint url`
Sets the endpoint of the FOSSA server. Useful for on-premises deployments.

##### `-m, --modules module_spec`
Sets the modules to use as entry points when analyzing dependencies.

<!-- ##### `-i, --ignore ignore_spec`
Sets the modules and paths to ignore when analyzing dependencies. -->

##### `-o, --output`
Prints analysis results to `stdout` instead of uploading results to a FOSSA server. When this flag is set, `fossa analyze` provides interactive output on `stderr`.

##### `--allow-unresolved`
Do not fail on unresolved dependencies.

For some languages, running `fossa analyze` will result in the following error even if you've built your code:

```bash
CRITICAL Module {MODULE_NAME} does not appear to be built. Try first running your build or `fossa build`, and then running `fossa`.
```

This happens when `fossa` fails to verify whether your environment has completed a build due to some kind of error. This could be due to a highly custom build process, non-conventional environment state or a misconfiguration of your build.

Passing `--allow-unresolved` will soften the verification standards that `fossa` runs for each language integration.

##### `--debug`
Print debugging information to `stderr`.

##### `-h, --help`
Print a help message, then exit.

### `fossa upload`

Uploads user-provided build data to FOSSA. This allows users to manually provide a dependency list if their build system is too complex for FOSSA's default analyzer.

#### Example
```bash
# You can manually override the project and revision name.
# This is useful when `fossa` can't manually infer them from `git`.
FOSSA_API_KEY=YOUR_API_KEY_HERE fossa upload --project=PROJECT_NAME --revision=SOME_HASH --data=$(fossa analyze --output)
```

#### Flags
##### `-c, --config file_name`
Path to a configuration file. Defaults to `.fossa.yaml`.

##### `-p, --project project_name`
Sets the configuration value for the FOSSA project.

##### `-r, --revision revision_hash`
Sets the configuration value for the FOSSA project's revision.

##### `-e, --endpoint url`
Sets the endpoint of the FOSSA server. Useful for on-premises deployments.

##### `--data`
The user-provided build data. See `fossa analyze --output` for an example of the correct build data format.

##### `--debug`
Print debugging information to `stderr`.

##### `-h, --help`
Print a help message, then exit.

### `fossa report`

Print the project's license report.

#### Flags
<!-- ##### `-j, --json`
Print the report data in JSON format.
-->

##### `--debug`
Print debugging information to `stderr`.

##### `-h, --help`
Print a help message, then exit.

### `fossa test`

#### Example
```bash
# Test your revision for issues and exit with a non-zero code if issues are found.
FOSSA_API_KEY=YOUR_API_KEY_HERE fossa test --timeout 600
```

Checks whether the project has licensing issues, as configured by its policy within FOSSA. If there are issues, it prints them on `stdout` and exits with code 1. If there are not issues, it exits with code 0.

#### Flags
<!-- ##### `-j, --json`
Print issues in JSON format. -->

##### `-c, --config file_name`
Path to a configuration file. Defaults to `.fossa.yaml`.

##### `-p, --project project_name`
Sets the configuration value for the FOSSA project.

##### `-r, --revision revision_hash`
Sets the configuration value for the FOSSA project's revision.

##### `-e, --endpoint url`
Sets the endpoint of the FOSSA server. Useful for on-premises deployments.

##### `-t, --timeout`
The amount of time to wait for an issue report (in seconds). Defaults to 10 minutes.