## `fossa analyze`

By default, the analyze command:

- looks for projects in the current directory (and recursively in subdirectories)
- extracts dependency graphs from those projects
- infers a project name, branch, and revision for the project (from git or svn)
- uploads the dependency graphs to FOSSA

For supported command-line flags, use `fossa analyze --help`

### Specifying FOSSA project details

In addition to the [usual FOSSA project flags](#common-fossa-project-flags) supported by all commands, the analyze command supports the following FOSSA-project-related flags:

| Name                                  | Short | Description                                                                                                                                                         |
| ------------------------------------- | ----- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `--title 'some title'`                | `-t`  | Set the title of the FOSSA project                                                                                                                                  |
| `--branch 'some branch'`              | `-b`  | Override the detected FOSSA project branch                                                                                                                          |
| `--project-url 'https://example.com'` | `-P`  | Add a URL to the FOSSA project                                                                                                                                      |
| `--jira-project-key 'some-key'`       | `-j`  | Add a Jira project key to the FOSSA project                                                                                                                         |
| `--link 'https://example.com'`        | `-L`  | Attach a link to the current FOSSA build                                                                                                                            |
| `--team 'some team'`                  | `-T`  | Specify a team within your FOSSA organization                                                                                                                       |
| `--policy 'some policy'`              |       | Assign a specific FOSSA policy to this project                                                                                                                      |
| `--config /path/to/file`              | `-c`  | Path to a [configuration file](../files/fossa-yml.md) including filename. By default we look for `.fossa.yml` in target directory of analyze command. |

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
- `.jar`
- `.rpm`
