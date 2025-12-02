## `fossa analyze`

By default, the analyze command:

- looks for projects in the current directory (and recursively in subdirectories)
- extracts dependency graphs from those projects
- infers a project name, branch, and revision for the project (from git or svn)
- uploads the dependency graphs to FOSSA

For supported command-line flags, use `fossa analyze --help`

### Specifying FOSSA project details

In addition to the [usual FOSSA project flags](#common-fossa-project-flags) supported by all commands, the analyze command supports the following FOSSA-project-related flags:

| Name                                   | Short | Description                                                                         |
|----------------------------------------|-------|-------------------------------------------------------------------------------------|
| `--title 'some title'`                 | `-t`  | Set the title of the FOSSA project                                                  |
| `--branch 'some branch'`               | `-b`  | Override the detected FOSSA project branch                                          |
| `--project-url 'https://example.com'`  | `-P`  | Add a URL to the FOSSA project                                                      |
| `--jira-project-key 'some-key'`        | `-j`  | Add a Jira project key to the FOSSA project                                         |
| `--link 'https://example.com'`         | `-L`  | Attach a link to the current FOSSA build                                            |
| `--team 'some team'`                   | `-T`  | Specify a team within your FOSSA organization                                       |
| `--policy 'some policy'`               |       | Assign a specific FOSSA policy to this project. Mutually excludes `--policy-id`.    |
| `--policy-id 'some policy id'`         |       | Assign a specific FOSSA policy to this project by id. Mutually excludes `--policy`. |
| `--project-label`                      |       | assign up to 5 labels to the project                                                |
| `--release-group-name 'MY_RG'`         |       | add the project to this release group (also requires `--release-group-release`)     |
| `--release-group-release 'MY_RELEASE'` |       | add the project to this release version within the release group                    |

### Filtering Paths and Targets

The paths and targets filtering options allow you to specify the exact targets which be should be scanned.

| Name                             | Description                                                                                                              |
|----------------------------------|--------------------------------------------------------------------------------------------------------------------------|
| `--only-target`                  | Only scan these targets. See [targets.only](../files/fossa-yml.md#targets.only) in the fossa.yml spec.                   |
| `--exclude-target`               | Exclude these targets from scanning. See [targets.exclude](../files/fossa-yml.md#targets.exclude) in the fossa.yml spec. |
| `--only-path`                    | Only scan these paths. See [paths.only](../files/fossa-yml.md#paths.only) in the fossa.yml spec.                         |
| `--exclude-path`                 | Exclude these paths from scannig. See [paths.exclude](../files/fossa-yml.md#paths.exclude) in the fossa.yml spec.        |
| `--include-unused-deps`          | Include all deps found, instead of filtering non-production deps.  Ignored by VSI.                                       |
| `--debug-no-discovery-exclusion` | Ignore these filters during discovery phase.  This flag is for debugging only and may be removed without warning.        |
| `--without-default-filters`      | Ignore default path filters. See [default path filters](./analyze.md#what-are-the-default-filters)                       |


### Printing FOSSA results

The `--output` flag can be used to print projects and dependency graph information to stdout, rather than uploading to FOSSA

```sh
fossa analyze --output
```

To print projects and dependency graph information to stdout *in addition* to uploading to FOSSA as normal, use the `--tee-output` flag.

```sh
fossa analyze --tee-output
```

### Printing project metadata

The `--json` flag can be used to print project metadata after running `fossa analyze` successfully. This metadata can be used to reference your project when integrating with the FOSSA API.

```sh
fossa analyze --json
```

```json
{"branch":"master", "id":"custom+<org-id>/new-project$123", "project":"<org-id>/new-project", "projectId":"custom+<org-id>/new-project", "revision":"123", "url":"https://app.fossa.com/projects/custom%2b+<org-id>$2fnew-project/refs/branch/master/123"}
```

### Running a specific fossa-deps file

The `--fossa-deps-file` flag can be used to specify the `fossa-deps` file that you want to use. The name of the file is arbitrary.

See the [fossa-deps documentation](../files/fossa-deps.md) for configuration.

```sh
fossa analyze --fossa-deps-file /path/to/file
```

### Vendored Dependencies

The Vendored Dependencies feature allows you to scan for licenses directly in your code. For more information, please see the [Vendored Dependencies documentation](../../features/vendored-dependencies.md).

| Name                                      | Description                                                                                                                                                                           |
|-------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `--force-vendored-dependency-scan-method` | Force the vendored dependency scan method. The options are 'CLILicenseScan' or 'ArchiveUpload'. 'CLILicenseScan' is usually the default unless your organization has overridden this. |
| `--force-vendored-dependency-rescans`     | Force vendored dependencies to be rescanned even if the revision has been previously analyzed by FOSSA. This currently only works for CLI-side license scans.                         |

### Custom License Searches

The "Custom License Searches" feature allows you to define text strings or regular expressions that are used to search through your codebase. If a match to the text string or regular expression is found, then a custom license is reported for that match.

An administrator of your organization can set up custom license searches that are always run when you analyze your codebase with `fossa analyze`. Using the `--ignore-org-wide-custom-license-scan-configs` flag will ignore those organization-wide custom license searches.

See the [Custom License and Keyword Searches documentation](../../features/custom-license-and-keyword-searches.md) for more details.

### Running in a specific directory

```sh
fossa analyze path/to/directory
```

### Scanning archive contents

With the `--unpack-archives` flag present, we unpack discovered archives to a temporary directory for dependency analysis. We recursively unpack archives-in-archives.

```sh
fossa analyze --unpack-archives
```

We support the following archive formats:

- `.zip`
- `.tar`
- `.tar.gz`
- `.taz`
- `.tgz`
- `.tar.xz`
- `.txz`
- `.tar.bz2`
- `.tbz`
- `.tbz2`
- `.tz2`
- `.jar`
- `.aar`
- `.rpm`, with...
  - `gzip` compression
  - `lzma` compression
  - `xz` compression
  - `zstd` compression

### Enabling or disabling additional strategies

In addition to the [standard flags](#specifying-fossa-project-details), the analyze command supports the following additional strategy flags:

| Name                                                                              | Description                                                                                                                                                                                      |
|-----------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| [`--detect-vendored`](./analyze/detect-vendored.md)                               | Enable the vendored source identification engine. For more information, see the [C and C++ overview](../strategies/languages/c-cpp/c-cpp.md).                                                    |
| [`--detect-dynamic './some-binary`](./analyze/detect-dynamic.md)                  | Analyze the binary at the provided path for dynamically linked dependencies. For more information, see the [C and C++ overview](../strategies/languages/c-cpp/c-cpp.md).                         |
| [`--static-only-analysis`](../strategies/README.md#static-and-dynamic-strategies) | Do not use third-party tools when analyzing projects.                                                                                                                                            |
| `--strict`                                                                        | Enforces strict analysis to ensure the most accurate results by rejecting fallbacks. When run with `--static-only-analysis`, the most optimal static strategy will be applied without fallbacks. |


### Snippet Scanning

Snippet scanning identifies potential open source code snippets within your first-party source code by comparing file fingerprints against FOSSA's knowledge base. This feature helps detect code that may have been copied from open source projects.

#### Enabling Snippet Scanning

| Name                | Description                                                                                                                                                                           |
|---------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `--snippet-scan`  | Enable snippet scanning during analysis. This feature fingerprints your source files and checks them against FOSSA's snippet database.        |

Snippet Scanning must also be enabled for your organization, and is only available for enterprise customers. If you would like to enable it for your organization, please [contact us](https://support.fossa.com).

#### More detail

For more detail about how Snippet Scanning works, how to use file filtering during Snippet Scanning, what information is sent to FOSSA's servers and a description of the Snippet Scan Summary, see [the Snippet Scanning feature documentation](../../features/snippet-scanning.md).

### Experimental Options

_Important: For support and other general information, refer to the [experimental options overview](../experimental/README.md) before using experimental options._

In addition to the [standard flags](#specifying-fossa-project-details), the analyze command supports the following experimental flags:

| Name                                                                                     | Description                                                                                                                                                                                    |
|------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| [`--experimental-enable-binary-discovery`](../experimental/binary-discovery/README.md)   | Enable reporting binary files as unlicensed dependencies. For more information, see the [binary discovery overview](../experimental/binary-discovery/README.md).                               |
| [`--experimental-link-project-binary './some-dir'`](../experimental/msb/README.md)       | Link the provided binary files to the project being analyzed. For more information, see the [multi stage builds overview](../experimental/msb/README.md).                                      |
| [`--experimental-skip-vsi-graph 'custom+1/some$locator'`](../experimental/msb/README.md) | Skip resolving the dependencies of the given project that was previously linked via `--experimental-link-project-binary`.                                                                      |
| `--experimental-force-first-party-scans`                                                 | Force [first party scans](../../features/first-party-license-scans.md) to run                                                                                                                  |
| `--experimental-block-first-party-scans`                                                 | Force [first party scans](../../features/first-party-license-scans.md) to not run. This can be used to forcibly turn off first-party scans if your organization defaults to first-party scans. |
| `--experimental-analyze-path-dependencies`                                               | License scan path dependencies, and include them in the final analysis. For more information, see the [path dependency overview](../experimental/path-dependency.md).                          |


### F.A.Q.

#### How do I add a project to a release group when it is analyzed?

To add the project you're analyzing to a [release group](https://docs.fossa.com/docs/release-groups), use
`fossa analyze --release-group-name 'MY_RG' --release-group-release 'MY_RELEASE_VERSION'`

Note that the `MY_RG` release group must already exist, as well as `MY_RELEASE_VERSION` within it. You may use the `release-group` [subcommand](https://github.com/fossas/fossa-cli/blob/master/docs/references/subcommands/release-group.md) in advance to create these or do so [within the FOSSA UI](https://docs.fossa.com/docs/release-groups).

#### Why is the `fossa-cli` skipping my project?

`fossa-cli` may sometimes report a project of interest was skipped from the analysis. For example,

```text
[ INFO] Scan Summary
[ INFO] ------------
[ INFO] 3 projects scanned;  2 skipped,  1 succeeded,  0 failed,  1 analysis warning
[ INFO]
[ INFO] * setuptools project in "sandbox/": succeeded with 1 warning
[ INFO] * setuptools project in "sandbox/example/": skipped (default path filters)
[ INFO] * setuptools project in "sandbox/external/": skipped (exclusion filters)
```

`fossa-cli` skips analysis, if and only if

- (a) Target is excluded via [fossa configuration file](https://github.com/fossas/fossa-cli/blob/master/docs/references/files/fossa-yml.md#analysis-target-configuration) (this filtering is referred to as "exclusion filters").
- (b) Target is excluded via [default path filters](./analyze.md#what-are-the-default-filters) (this filtering was previously referred to as "production path filtering").

#### What are the default filters?

Default filters are filters which `fossa-cli` applies by default. These filters,
provide sensible non-production target exclusion. As `fossa-cli` relies on manifest and lock files provided in the project's directory,
default filters, intentionally skip `node_modules/` and such directories. If `fossa-cli` discovers and
analyzes project found in `node_modules/`: `fossa-cli` will not be able to infer
the dependency's scope (development or production) and may double count dependencies.

Specifically, `fossa-cli` by default skips any targets found within the following directories:

- `dist-newstyle`
- `doc/`
- `docs/`
- `test/`
- `tests/`
- `example/`
- `examples/`
- `vendor/`
- `node_modules/`
- `.srclib-cache/`
- `spec/`
- `Godeps/`
- `.git/`
- `bower_components/`
- `third_party/`
- `third-party/`
- `Carthage/`
- `Checkouts/`

To disable default filters, provide `--without-default-filters` flag when performing `fossa analyze` command. Currently,
it is not possible to disable only a subset of default filters. If you would like to only apply a subset of default filters, you can
use `--without-default-filters` in conjunction with [exclusion filters](./../files/fossa-yml.md#analysis-target-configuration). Refer to
[exclusion filters walkthough](../../walkthroughs/analysis-target-configuration.md) for example on how to apply path and target exclusion filters.

#### Can `fossa-cli` detect licensed/copyright content downloaded at runtime by dependencies?

Unfortunately, as of yet, `fossa-cli` cannot discover or analyze any licensed or copyrighted
content retrieved at runtime by dependencies when it is not referenced in manifest or lock files.

For example, in `python` with [datasets](https://pypi.org/project/datasets/) package,

```python
from datasets import load_dataset
dataset = load_dataset("bigscience/P3", revision="f2cade2") # retrieved at runtime
```

In this case, `fossa-cli` would not be able to identify `bigscience/P3` and its associated compliance
obligation since they were not part of the manifest or lockfile - e.g. `requirements.txt`, `setup.py`, `poetry.lock`.

In this scenario, if the inclusion of `bigscience/P3` is desired in FOSSA's reporting, we recommend
[fossa-deps.yml](./../files/fossa-deps.md) file to explicitly include `bigscience/P3` in FOSSA's reporting. Likewise, you can
also download `huggingface.co/datasets/bigscience/P3` to disk and use [vendor dependency scanning](./../../features/vendored-dependencies.md).

```yaml
# Example fossa-deps.yml

custom-dependencies:
- name: huggingface.co/datasets/bigscience/P3
  version: "f2cade2"
  license: "Apache-2.0"
```

If you need more assistance, please contact [FOSSA support](https://support.fossa.com).

#### How do I ensure `fossa analyze` does not exit fatally when no targets are discovered?

In some scenarios, you may want to configure the `fossa analyze` and `fossa test` CI workflow on an empty repository or directory with 0 targets. Unfortunately, `fossa-cli` does not have a configuration yet, which will allow for successful analysis (exit code of 0) when 0 targets are discovered.

For a workaround, create an empty `reqs.txt` file before running `fossa analyze,` which will force `fossa-cli` to presume an empty pip project (with 0 dependencies).

```bash
touch reqs.txt && fossa analyze && rm reqs.txt && fossa test
```

## Common FOSSA Project Flags

All `fossa` commands support the following FOSSA-project-related flags:

| Name                               | Short | Description                                                                                                                              |
|------------------------------------|-------|------------------------------------------------------------------------------------------------------------------------------------------|
| `--project 'some project'`         | `-p`  | Override the detected project name                                                                                                       |
| `--revision 'some revision'`       | `-r`  | -Override the detected project revision                                                                                                  |
| `--fossa-api-key 'my-api-key'`     |       | An alternative to using the `FOSSA_API_KEY` environment variable to specify a FOSSA API key                                              |
| `--endpoint 'https://example.com'` | `-e`  | Override the FOSSA API server base URL                                                                                                   |
| `--config /path/to/file`           | `-c`  | Path to a [configuration file](../files/fossa-yml.md) including filename. By default we look for `.fossa.yml` in base working directory. |
