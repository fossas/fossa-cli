# User Guide

Fossa is most commonly used to analyze a project and extract its full dependency graph, which can then be uploaded to fossa.com using an API key. This page explains how to configure this workflow as well as the other features of the FOSSA CLI. If you are looking for a guided walk-through with information along the way refer to [How it Works](how-it-works.md#how-it-works)

## 1. Installation

The following commands will execute scripts to fetch and install the latest [GitHub Releases](https://github.com/fossas/fossa-cli/releases) on the corresponding operating system.

### MacOS (Darwin) or Linux amd64:
```bash
curl -H 'Cache-Control: no-cache' https://raw.githubusercontent.com/fossas/fossa-cli/master/install.sh | bash
```

### Windows with Powershell:
```powershell
Set-ExecutionPolicy Bypass -Scope Process -Force; iex  ((New-Object System.Net.WebClient).DownloadString('https://raw.githubusercontent.com/fossas/fossa-cli/master/install.ps1'))
```

Add `C:\ProgramData\fossa-cli` to your path by modifying your `profile.ps1` file or temporarily with the following command:
```powershell
$env:Path += ";C:\ProgramData\fossa-cli"
```

## 2. Configuring a Project

Configuration can be achieved through a configuration file, `fossa.yml`(config-file.md#fossayml), or directly with arguments and flags to the [`fossa`](#fossa) command. The FOSSA CLI was built to create accurate configuration files by running [`fossa init`](#fossa-init) and only require manual configuration for complex builds or to tweak personal preferences.

### Configuration file
A configuration file is created by running [`fossa init`](#fossa-init) at the root of the project you wish to analyze. The cli will move down the file tree and search for all relevant modules.
> Note: Fossa will exclude modules it has determined to be test, development, or dependency modules by default. Add `--include-all` to retain all modules.


```yaml
version: 1

cli:
  server: https://app.fossa.com
  fetcher: custom
  project: git+github.com/fossas/fossa-cli
analyze:
  modules:
    - name: fossa-cli
      type: go
      target: github.com/fossas/fossa-cli/cmd/fossa
      path: cmd/fossa
```

Information about each of these fields and customization can be found in [.fossa.yml](config-file.md#fossayml) documentation.

### Argument configuration

Modules can be provided by including an argument such as `fossa analyze <module_type>:<target>` which determines the type of analysis and then supplies the build target. More examples are located below.

> Note: Argument and flag configurations take precedence over a configuration file.

#### Examples

- `nodejs:.,rubygem:./docs`
- `go:./cmd/fossa`

## 3. Analyzing a Project

1. Verify that analysis succeeds by running [`fossa analyze -o`](#fossa-analyze) without error. This command will output analysis to stdout instead of uploading.
2. Obtain a `FOSSA_API_KEY`. Refer to the [FOSSA.com manual](https://docs.fossa.com/docs/api-reference#section-api-tokens) for instructions.
3. Run `export FOSSA_API_KEY=<your-api-key>` to set the environment variable.
4. Run [`fossa analyze`](#fossa-analyze). This will analyze your project and upload the results to the specified server, app.fossa.com by default. 

> Note: You can add the FOSSA_API_KEY inline with the command `FOSSA_API_KEY=<your-key> fossa analyze`

Analysis can vary greatly depending on which environment you are in. Refer to the individual [supported environments](../README.md/#supported-environments) pages for more information and available options.

## Upload Custom Builds
If your build is too complex or your build system is heavily customized, `fossa` may be unable to correctly analyze your build. In that case, you can still upload build information using [`fossa upload`](#fossa-upload) to fossa.com for build analysis and issue triage.

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
- `hackage`: Haskell dependencies
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

## 4. CLI Reference

All flags should be passed to the invoked sub-command. Global flags are currently NOT supported.

| Command                           | Description                                       |
| --------------------------------- | ------------------------------------------------- |
| [`fossa`](#fossa)                 | Initialization and analysis.                      |
| [`fossa init`](#fossa-init)       | Generate configuration file.                      |
| [`fossa analyze`](#fossa-analyze) | Analyze the current configuration.                |
| [`fossa test`](#fossa-test)       | Fail a CI job on the latest fossa scan.           |
| [`fossa upload`](#fossa-upload)   | Upload a custom build.                            |
| [`fossa report`](#fossa-report)   | Retrieve information about the latest fossa scan. |
| `fossa update`                    | Update the current cli version.                   |

### `fossa`
Combination of [`fossa init`](#fossa-init) and [`fossa analyze`](#fossa-analyze) for simplicity.

#### Example all-in-one command
```bash
# Creates a config file, runs an analysis, and uploads the results.
FOSSA_API_KEY=YOUR_API_KEY fossa
```
  
| Flag         | Short | Description                                                                  |
| ------------ | ----- | ---------------------------------------------------------------------------- |
| `--config`   | `-c`  | Path to a [configuration file](/docs/config-file.md) including filename.     |
| `--project`  | `-p`  | Configuration value for [project](/docs/config-file.md/#project-optional).   |
| `--revision` | `-r`  | Configuration value for [revision](/docs/config-file.md/#revision-optional). |
| `--endpoint` | `-e`  | Configuration value for [endpoint](/docs/config-file.md/#endpoint-optional). |
| `--output`   | `-o`  | Output `fossa analyze` results to stdout.                                    |
| `--debug`    |       | Print debugging information to stderr.                                       |
| `--version ` | `-v`  | Print the currently installed FOSSA CLI version.                             |
| `--help`     | `-h`  | Print a help message.                                                        |

### `fossa init`

Makes a best-effort attempt at inferring the correct configuration from current system state. If successful, it will write the configuration to a new or existing config file (defaults to `.fossa.yml`).

> Note: if a configuration file exists, `fossa init` will **not** overwrite it.

If there are no modules defined in the configuration, it will scan the working directory for code modules to analyze.  You can pass the `--overwrite` to overwrite any existing modules in configuration.

By default, `fossa init` filters modules that are suspected to be development, test, or dependency. The filter removes all modules that have any of the following in their filepath: `docs, test, examples, third-party, vendor, tmp,node_modules, .srclib-cache, spec, Godeps, .git, bower_components,  Carthage, and Checkouts`. Filtering can be disabled by passing the `--include-all` flag.

#### Example
```bash
# Creates the .fossa.yml file in the current directory.
fossa init --overwrite
```

| Flag            | Short | Description                                   |
| --------------- | ----- | --------------------------------------------- |
| `--overwrite`   | `-O`  | Overwrite the file `.fossa.yml` if it exists. |
| `--include-all` |       | Include all modules.                          |
| `--debug`       |       | Print debugging information to stderr.        |
| `--help`        | `-h`  | Print a help message.                         |

### `fossa analyze`

Analyzes the project for a list of its dependencies, optionally uploading the results to FOSSA. If analysis fails, first look at the documentation in the [supported environments](../README.md/#supported-environments) pages for information specific to your environment, and flags that can be set to condition fossa to your setup. 

> Note: Analysis requires an API Key unless `--output` is set.
#### Example
```bash
# Run analysis using .fossa.yml modules and upload to the server endpoint.
FOSSA_API_KEY=YOUR_API_KEY fossa analyze
```

| Flag         | Short | Description                                                                  |
| ------------ | ----- | ---------------------------------------------------------------------------- |
| `--config`   | `-c`  | Path to a [configuration file](/docs/config-file.md) including filename.     |
| `--project`  | `-p`  | Configuration value for [project](/docs/config-file.md/#project-optional).   |
| `--revision` | `-r`  | Configuration value for [revision](/docs/config-file.md/#revision-optional). |
| `--endpoint` | `-e`  | Configuration value for [endpoint](/docs/config-file.md/#endpoint-optional). |
| `--output`   | `-o`  | Output `fossa analyze` results to stdout.                                    |
| `--debug`    |       | Print debugging information to stderr.                                       |
| `--help`     | `-h`  | Print a help message.                                                        |

### `fossa test`
Checks whether the project has licensing issues, as configured by its policy within FOSSA. If there are issues, it prints them on `stdout` and exits with code 1. If there are not issues, it exits with code 0. Fossa test can be used to fail a CI pipeline job.

> Note: Report always requires an API Key to be set. 

#### Example
```bash
# Test your revision for issues and exit with a non-zero code if issues are found.
FOSSA_API_KEY=YOUR_API_KEY_HERE fossa test --timeout 600
```

| flag                | short | description                                                                       |
| ------------------- | ----- | --------------------------------------------------------------------------------- |
| `--config`          | `-c`  | Path to a [configuration file](/docs/config-file.md) including filename.          |
| `--project`         | `-p`  | Configuration value for [project](/docs/config-file.md/#project-optional).        |
| `--revision`        | `-r`  | Configuration value for [revision](/docs/config-file.md/#revision-optional).      |
| `--endpoint`        | `-e`  | Configuration value for [endpoint](/docs/config-file.md/#endpoint-optional).      |
| `--timeout`         | `-t`  | The amount of seconds to wait for an issue scan to complete. Default: 10 minutes. |
| `--debug`           |       | Print debugging information to stderr.                                            |
| `--help`            | `-h`  | Print a help message.                                                             |
| `--suppress-issues` |       | Don't exit on stderr if issues are found.                                         |

### `fossa upload`

Uploads user-provided build data to FOSSA. This allows users to manually provide a dependency list if their build system is too complex for FOSSA's default analyzer.

> Note: Upload always requires an API Key to be set.

#### Example
```bash
# Manually overriding the project and revision name is useful when `fossa` cannot infer them from `git`.
FOSSA_API_KEY=YOUR_API_KEY_HERE fossa upload --project=PROJECT_NAME --revision=SOME_HASH --data=output.txt
```

| flag         | short | description                                                                                   |
| ------------ | ----- | --------------------------------------------------------------------------------------------- |
| `--config`   | `-c`  | Path to a [configuration file](/docs/config-file.md) including filename.                      |
| `--project`  | `-p`  | Configuration value for [project](/docs/config-file.md/#project-optional).                    |
| `--revision` | `-r`  | Configuration value for [revision](/docs/config-file.md/#revision-optional).                  |
| `--endpoint` | `-e`  | Configuration value for [endpoint](/docs/config-file.md/#endpoint-optional).                  |
| `--data`     |       | User provided build data. see [custom builds](#uploading-custom-builds) for more information. |
| `--debug`    |       | Print debugging information to stderr.                                                        |
| `--help`     | `-h`  | Print a help message.                                                                         |

### `fossa report`

Report accesses the scanned report on FOSSA.com using the existing configuration file and outputs information directly to the command line. Report offers two different commands:

#### `fossa report licenses` 
Outputs detailed information about the licenses and corresponding dependencies used by the project. An example of this can be found in the [Notice file](./Notice) for the FOSSA CLI.

#### `fossa report dependencies`
Outputs detailed information about the dependencies that are being used by the current project.

> Note: `fossa report` requires an API Key to be set. 

#### Example
```bash
FOSSA_API_KEY=YOUR_API_KEY_HERE fossa report licenses --json
```

| flag      | short | description                                  |
| --------- | ----- | -------------------------------------------- |
| `--json`  |       | Print the report information in JSON format. |
| `--debug` |       | Print debugging information to stderr.       |
| `--help`  | `-h`  | Print a help message.                        |

