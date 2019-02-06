# User Guide

Fossa is most commonly used to analyze a project and extract its full dependency graph,which can then be uploaded to fossa.com using an API key. This page explains how to configure this workflow as well as the other features of the fossa-cli. If you are looking for a guided walk-through refer to our [demo guide](how-it-works.md).

## 1. Configuring a Project

Configuration can be achieved through a configuration file, `fossa.yml`, or directly with arguments and flags to the `fossa` command. The fossa-cli was built to create accurate configuration files by running [`fossa init`](#fossa-init) and only require manual configuration for complex builds or to tweak personal preferences.

### Configuration file
A configuration file is created by running [`fossa init`](#fossa-init) at the root of the project you wish to analyze. The cli will move down the file tree and search for all relevant modules.
> Note: Fossa will exclude modules it has determined to be test, development, or dependency modules by default. Add `--include-all` to retain all modules.


```yaml
version: 1

cli:
  server: https://app.fossa.io
  fetcher: custom
  project: git+github.com/fossas/fossa-cli
analyze:
  modules:
    - name: fossa-cli
      type: go
      target: github.com/fossas/fossa-cli/cmd/fossa
      path: cmd/fossa
```

Information about each of these fields and customization can be found in [.fossa.yml](/docs/config-file) documentation.

### Argument configuration

Single modules can be provided by including an argument such as `fossa analyze <module_type>:<target>` which first resolves the type of analysis and then supplies the build target for the analysis. More examples located [here](#Examples).

> Note: Argument and flag configurations take precedence over a configuration file.

#### Examples

- `nodejs:.,rubygem:./docs`
- `go:./cmd/fossa`

## 2. Analyzing a Project

1. Run `fossa analyze -o` to verify that analysis succeeds. This command will output analysis to stdout instead of uploading them.
2. Obtain a `FOSSA_API_KEY`. Refer to the [Fossa manual].(https://docs.fossa.com/docs/api-reference#section-api-tokens) for instructions.
3. Run `export FOSSA_API_KEY=<your-api-key>` to set the environment variable.
4. Run [`fossa analyze`](#fossa-analyze). This will analyze your project and upload the results to the specified server, app.fossa.com by default. 

> Note: You can add the FOSSA_API_KEY inline with the command `FOSSA_API_KEY=<your-key> fossa analyze`

Analysis can vary greatly depending on which environment you are in. Refer to the individual [supported environments](../README.md/#supported-environments) pages for more information and available options.

## Upload Custom Builds
### `fossa upload`
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

All flags should be passed to the invoked sub-command. Global flags are currently NOT supported.
| Command                         | Description                                       |
| ------------------------------- | ------------------------------------------------- |
| [fossa](#fossa)                 | Initialization and analysis.                      |
| [fossa init](#fossa-init)       | Generate configuration file.                      |
| [fossa analyze](#fossa-analyze) | Analyze the current configuration.                |
| [fossa test](#fossa-test)       | Fail a CI job on the latest fossa scan.           |
| [fossa upload](#fossa-upload)   | Upload a custom build.                            |
| [fossa report](#fossa-report)   | Retrieve information about the latest fossa scan. |
| [fossa update](#fossa-update)   | Update the current cli version.                   |

### `fossa`
Combination of [`fossa init`](#fossa-init) and [`fossa analyze`](#fossa-analyze) for simplicity.

#### Example all-in-one command
```bash
# Creates a config file, runs an analysis, and uploads the results.
FOSSA_API_KEY=YOUR_API_KEY fossa
```
  
| Flag                         | Short | Description                                                     |
| ---------------------------- | ----- | --------------------------------------------------------------- |
| [--config](#fossa)           | `-c`  | Path to a config file                                           |
| [--project](#fossa-init)     | `-p`  | Configuration value for [project](/docs/config-file.md/#Fields) |
| [--revision](#fossa-analyze) | `-r`  |
| [--endpoint](#fossa-test)    | `-e`  |
| [--output](#fossa-upload)    | `-o`  |
| [--debug](#fossa-report)     |       |
| [--version](#fossa-update)   | `-v`  |
| [--help](#fossa-update)      | `-h`  |
#### Flags
##### `-c, --config file_name`
Path to a configuration file. Defaults to `.fossa.yml`.

##### `-p, --project project_name`
Sets the configuration value for the FOSSA project.

##### `-r, --revision revision_hash`
Sets the configuration value for the FOSSA project's revision.

##### `-e, --endpoint url`
Sets the endpoint of the FOSSA server. Useful for on-premises deployments.

##### `-o, --output`
Passed to `fossa analyze`.

##### `--debug`
Print debugging information to `stderr`.

##### `-v, --version`
Print the version, then exit.

##### `-h, --help`
Print a help message, then exit.

### `fossa init`

Makes a best-effort attempt at inferring the correct configuration from current system state. If successful, it will write the configuration to a new or existing config file (defaults to `.fossa.yml`).

> Note: if a configuration file exists, `fossa init` will **not** overwrite it.

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
Prints analysis results to `stdout` instead of uploading results to a FOSSA server.

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