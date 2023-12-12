## Quick Start

FOSSA CLI (occasionally referred to as Spectrometer) is a tool that requires minimal configuration: usually, only a FOSSA API key is required.

If you do not have an API key, please check the [FOSSA documentation](https://docs.fossa.com/docs/api-reference) for instructions on generating an API key.

### Configure your API key

```sh
export FOSSA_API_KEY=abcdef123456
```

### Run Analysis

This runs dependency analysis in the current directory, uploading results to FOSSA

```sh
fossa analyze
```

### Check for FOSSA scan results

```sh
fossa test
```

This may take some time to return while FOSSA downloads and scans each dependency for display in our web UI.

For additional commands and command flags, use `--help`:

```sh
fossa --help
fossa analyze --help
# etc
```
