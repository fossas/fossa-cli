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

### Print report as JSON

By default, `fossa report` displays issues in a human-readable format. To instead print issues as JSON, use:

```sh
fossa report attribution --json
```

*NOTE: Currently, text reports are not supported, and the report will be*
*printed as JSON.  It is recommended to use the `--json` flag anyway, since*
*the behavior of the command without the flag will change in the future.*
### FOSSAv1 report compatibility

FOSSA supports a compatibility script that converts FOSSAv2 attribution report output to the same format as that in FOSSAv1.

This script will be supported until the end of April 2022 and is included in the release assets, under the name `compat-attribution`.

To use this compatibility script:

1. Download and extract `compat-attribution` for your platform to the same place as the `fossa` binary.
2. Run `fossa report attribution --json`, piping its output to `compat-attribution`.
   For example, `fossa report attribution --json | compat-attribution`
3. Parse the resulting output as you would have from FOSSAv1.

## Common FOSSA Project Flags

All `fossa` commands support the following FOSSA-project-related flags:

| Name                               | Short | Description                                                                                                                                            |
| ---------------------------------- | ----- | ------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `--project 'some project'`         | `-p`  | Override the detected project name                                                                                                                     |
| `--revision 'some revision'`       | `-r`  | -Override the detected project revision                                                                                                                |
| `--fossa-api-key 'my-api-key'`     |       | An alternative to using the `FOSSA_API_KEY` environment variable to specify a FOSSA API key                                                            |
| `--endpoint 'https://example.com'` | `-e`  | Override the FOSSA API server base URL                                                                                                                 |
| `--config /path/to/file`           | `-c`  | Path to a [configuration file](../files/fossa-yml.md) including filename. By default we look for `.fossa.yml` in base working directory. |
