## `fossa analyze`

By default, the analyze command:

- looks for projects in the current directory (and recursively in subdirectories)
- extracts dependency graphs from those projects
- infers a project name, branch, and revision for the project (from git or svn)
- uploads the dependency graphs to FOSSA

For supported command-line flags, use `fossa analyze --help`

### Specifying FOSSA project details

In addition to the [usual FOSSA project flags](#common-fossa-project-flags) supported by all commands, the analyze command supports the following FOSSA-project-related flags:

| Name                                  | Short | Description                                                                         |
|---------------------------------------|-------|-------------------------------------------------------------------------------------|
| `--title 'some title'`                | `-t`  | Set the title of the FOSSA project                                                  |
| `--branch 'some branch'`              | `-b`  | Override the detected FOSSA project branch                                          |
| `--project-url 'https://example.com'` | `-P`  | Add a URL to the FOSSA project                                                      |
| `--jira-project-key 'some-key'`       | `-j`  | Add a Jira project key to the FOSSA project                                         |
| `--link 'https://example.com'`        | `-L`  | Attach a link to the current FOSSA build                                            |
| `--team 'some team'`                  | `-T`  | Specify a team within your FOSSA organization                                       |
| `--policy 'some policy'`              |       | Assign a specific FOSSA policy to this project. Mutually excludes `--policy-id`.    |
| `--policy-id 'some policy id'`        |       | Assign a specific FOSSA policy to this project by id. Mutually excludes `--policy`. |
| `--project-label`                     |       | assign up to 5 labels to the project                                                |
| `--release-group-name`                |       | the name of the release group to add this project to                                |

### Filtering Paths and Targets

The paths and targets filtering options allow you to specify the exact targets which be should be scanned.

| Name                             | Short | Description                                                                                                              |
|----------------------------------|-------|--------------------------------------------------------------------------------------------------------------------------|
| `--only-target`                  |       | Only scan these targets. See [targets.only](../files/fossa-yml.md#targets.only) in the fossa.yml spec.                   |
| `--exclude-target`               |       | Exclude these targets from scanning. See [targets.exclude](../files/fossa-yml.md#targets.exclude) in the fossa.yml spec. |
| `--only-path`                    |       | Only scan these paths. See [paths.only](../files/fossa-yml.md#paths.only) in the fossa.yml spec.                         |
| `--exclude-path`                 |       | Exclude these paths from scannig. See [paths.exclude](../files/fossa-yml.md#paths.exclude) in the fossa.yml spec.        |
| `--include-unused-deps`          |       | Include all deps found, instead of filtering non-production deps.  Ignored by VSI.                                       |
| `--debug-no-discovery-exclusion` |       | Ignore these filters during discovery phase.  This flag is for debugging only and may be removed without warning.        |

### Printing results without uploading to FOSSA

The `--output` flag can be used to print projects and dependency graph information to stdout, rather than uploading to FOSSA

```sh
fossa analyze --output
```

### Printing project metadata

The `--json` flag can be used to print project metadata after running `fossa analyze` successfully. This metadata can be used to reference your project when integrating with the FOSSA API.

```sh
fossa analyze --json
```
```json
{"project":{"name":"custom@new-project","branch":"master","revision":"123","url":"https://app.fossa.com/projects/custom+<org-id>/new-project/refs/branch/master/123","id":"custom+<org-id>/new-project$123"}}
```

### Vendored Dependencies

The Vendored Dependencies feature allows you to scan for licenses directly in your code. For more information, please see the [Vendored Dependencies documentation](../../features/vendored-dependencies.md).

| Name                                      | Short | Description                                                                                                                                                                           |
|-------------------------------------------|-------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `--force-vendored-dependency-scan-method` |       | Force the vendored dependency scan method. The options are 'CLILicenseScan' or 'ArchiveUpload'. 'CLILicenseScan' is usually the default unless your organization has overridden this. |
| `--force-vendored-dependency-rescans`     |       | Force vendored dependencies to be rescanned even if the revision has been previously analyzed by FOSSA. This currently only works for CLI-side license scans.                         |

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
- `.tar.xz`
- `.tar.bz2`
- `.jar`
- `.aar`
- `.rpm`, with...
  - `gzip` compression
  - `lzma` compression
  - `xz` compression
  - `zstd` compression

### Enabling additional strategies

In addition to the [standard flags](#specifying-fossa-project-details), the analyze command supports the following additional strategy flags:

| Name                                                             | Description                                                                                                                                                              |
|------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| [`--detect-vendored`](./analyze/detect-vendored.md)              | Enable the vendored source identification engine. For more information, see the [C and C++ overview](../strategies/languages/c-cpp/c-cpp.md).                            |
| [`--detect-dynamic './some-binary`](./analyze/detect-dynamic.md) | Analyze the binary at the provided path for dynamically linked dependencies. For more information, see the [C and C++ overview](../strategies/languages/c-cpp/c-cpp.md). |


### Experimental Options

_Important: For support and other general information, refer to the [experimental options overview](../experimental/README.md) before using experimental options._

In addition to the [standard flags](#specifying-fossa-project-details), the analyze command supports the following experimental flags:

| Name                                                                                     | Description                                                                                                                                                      |
|------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| [`--experimental-enable-binary-discovery`](../experimental/binary-discovery/README.md)   | Enable reporting binary files as unlicensed dependencies. For more information, see the [binary discovery overview](../experimental/binary-discovery/README.md). |
| [`--experimental-link-project-binary './some-dir'`](../experimental/msb/README.md)       | Link the provided binary files to the project being analyzed. For more information, see the [multi stage builds overview](../experimental/msb/README.md).        |
| [`--experimental-skip-vsi-graph 'custom+1/some$locator'`](../experimental/msb/README.md) | Skip resolving the dependencies of the given project that was previously linked via `--experimental-link-project-binary`.                                        |
| `--experimental-force-first-party-scans` | | Force [first party scans](../../features/first-party-license-scans.md) to run|
| `--experimental-block-first-party-scans` | | Force [first party scans](../../features/first-party-license-scans.md) to not run. This can be used to forcibly turn off first-party scans if your organization defaults to first-party scans.|

### F.A.Q.

1. Why is the `fossa-cli` skipping my project?

`fossa-cli` may sometimes report a project of interest was skipped from the analysis. For example,

```text
[ INFO] Scan Summary
[ INFO] ------------
[ INFO] 3 projects scanned;  2 skipped,  1 succeeded,  0 failed,  1 analysis warning
[ INFO]
[ INFO] * setuptools project in "sandbox/": succeeded with 1 warning
[ INFO] * setuptools project in "sandbox/example/": skipped (production path filtering)
[ INFO] * setuptools project in "sandbox/external/": skipped (exclusion filters)
```

`fossa-cli` skips analysis, if and only if

- (a) Target is excluded via [fossa configuration file](https://github.com/fossas/fossa-cli/blob/master/docs/references/files/fossa-yml.md#analysis-target-configuration) (this filtering is referred to as "exclusion filters").
- (b) Target is skipped because the `fossa-cli` does not consider the target to be a production target (this filtering is referred to as "production path filtering").

`fossa-cli` skips any target per (b), if the target is found within the following directories:

- `dist-newstyle`
- `doc/`
- `docs/`
- `test/`
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

As `fossa-cli` relies on manifest and lock files provided in the project's directory, we
intentionally skip `node_modules/` and such directories. If `fossa-cli` discovers and
analyzes project found in `node_modules/`: `fossa-cli` will not be able to infer
the dependency's scope (development or production) and may double count dependencies.

2. Can `fossa-cli` detect licensed/copyright content downloaded at runtime by dependencies?

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

3. How do I ensure `fossa analyze` does not exit fatally when no targets are discovered?

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
