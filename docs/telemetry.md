### FOSSA CLI Telemetry

FOSSA CLI collects anonymous feature usage information, observed errors and warnings, 
and performance diagnostic information to help improve the experience for everyone. 
	
Getting a better understanding of usage patterns, the impact of exceptions, 
and crash reports, helps our team to better prioritize work and 
ultimately deliver a better user experience. 

### What data is collected in CLI telemetry?

CLI telemetry collects the following data: 

- Fatal and non-fatal errors and warnings
- Configurations, and command-line arguments used
- Anonymous system information (operating system, and architecture)
- Feature usage data

CLI telemetry does not collect personally identifiable information or metadata about source code.

### How is CLI telemetry data collected and processed?

CLI telemetry data is collected at the provided endpoint in configuration (with --endpoint). 
CLI telemetry data is validated and processed for our logging system.

### How can I inspect CLI telemetry data?

You can view collected CLI telemetry data by running cli with environment variable `FOSSA_TELEMETRY_DEBUG=1`, with scope of `FOSSA_TELEMETRY_SCOPE=full`. 
This will generate `fossa.telemetry.json` file in your current working directory, and will not send any telemetry data to server.

```bash
FOSSA_TELEMETRY_DEBUG=1 FOSSA_TELEMETRY_SCOPE=full fossa analyze sandbox
```

### How can I disable CLI telemetry?

You can disable telemetry with any of the following options, 

(1) Setting environment variable

```
$ FOSSA_TELEMETRY_SCOPE=off fossa analyze
```

(2) Using command line flag

```
$ fossa analyze --with-telemetry-scope=off
```

(3) Using the configuration file: 

```
# .fossa.yml
version: 3

telemetry:
    scope: off
```

Also, we do not collect telemetry when `--output` is used with analyze command.

### I’m an on-prem customer, what telemetry data is collected by FOSSA?

CLI sends telemetry data to the endpoint specified in the FOSSA CLI command. 
We never receive telemetry data collected from your endpoint. 

You can inspect collected telemetry data on your endpoint from your on-prem FOSSA database instance.