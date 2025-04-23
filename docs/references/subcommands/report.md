## `fossa report`

The report command downloads a report of the most-recent scan of your FOSSA project. This command is usually run immedately after `fossa analyze` or `fossa test`

The report type **must** be specified to successfully run, for example:

```sh
fossa report attribution
```

`fossa report` supports the [Common FOSSA Project Flags](#common-fossa-project-flags) supported by all commands.

### Report types

- `fossa report attribution` - A report that contains information about your dependencies and their authors. For more info about attributions, check the [FOSSA docs page illustrating the topic](https://docs.fossa.com/docs/generating-reports).

### Specifying a report timeout

By default, `fossa report` waits a maximum of 3600 seconds (1 hour) for report contents. To override the default timeout, use, e.g.:

```sh
fossa report attribution --timeout 60
```

Where `60` is the maximum number of seconds to wait for the report to be downloaded.

### Valid Report Targets

#### Project Directory

You can specify a project directly like you would with `fossa analyze` to generate a report.
For example:

```
fossa report attribution --format json ~/my-project
```

With no final path, FOSSA will try to fetch a report for the current directory's project.

#### SBOM Files

After using [`fossa sbom analyze`](./sbom.md), you can specify an SBOM that you would like to generate a report for using its file:

```
fossa report attribution --format json ~/my-project-sbom.txt
```

#### Project Arguments

All `fossa` commands support the following FOSSA-project-related flags:

| Name                               | Short | Description                                                                                                                                            |
| ---------------------------------- | ----- | ------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `--project 'some project'`         | `-p`  | Override the detected project name                                                                                                                     |
| `--revision 'some revision'`       | `-r`  | -Override the detected project revision                                                                                                                |
| `--fossa-api-key 'my-api-key'`     |       | An alternative to using the `FOSSA_API_KEY` environment variable to specify a FOSSA API key                                                            |
| `--endpoint 'https://example.com'` | `-e`  | Override the FOSSA API server base URL                                                                                                                 |
| `--config /path/to/file`           | `-c`  | Path to a [configuration file](../files/fossa-yml.md) including filename. By default we look for `.fossa.yml` in base working directory. |

In this case, FOSSA will attempt to fetch any report it can find matching the `project` and `revision` criteria.

### Specifying a report format

`fossa report` supports customizing the format used to render a report via the `--format` flag.
Available options are:
- `csv`
- `cyclonedx-json`
- `cyclonedx-xml`
- `html`
- `json`
- `markdown`
- `spdx`
- `spdx-json`
- `text`

For example, to render the report in JSON format, use `fossa report attribution --format json`.

In earlier versions of the FOSSA CLI, the `--json` flag was used to denote rendering the report in JSON format.
For backwards compatibility, this flag is still supported and takes precedence over the `--format` flag if present.
However, it is deprecated, and should be replaced with `--format json` to prevent future changes from breaking this behavior.

### FOSSAv1 report compatibility

FOSSA supported a compatibility script that converts FOSSAv2 attribution report output to the same format as that in FOSSAv1.

This script was supported until the end of April 2022 and was included in the release assets, under the name `compat-attribution`.
After April 2022, it's no longer supported, but can still be found on older releases, for example https://github.com/fossas/fossa-cli/releases/tag/v3.2.12.

**We strongly recommend not trying to use this script, and instead migrating to the new report format.**

To use this compatibility script:

1. Download and extract `compat-attribution` for your platform to the same place as the `fossa` binary.
2. Run `fossa report attribution --format json`, piping its output to `compat-attribution`.
   For example, `fossa report attribution --format json | compat-attribution`
3. Parse the resulting output as you would have from FOSSAv1.
