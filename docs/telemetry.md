### Fossa CLI Telemetry

Fossa CLI collects anonymous feature usage information, observed errors and warnings, 
and performance diagnostic information, to help improve fossa-cli for everyone. 

Our team must have an accurate understanding of the usage pattern, occurrences, 
and impact of exceptions, and crash reports, so we can proactively fix defects, 
prioritize work, better design functionality, and ultimately deliver reliable
and better user outcomes. 

### What data is collected in CLI telemetry?

CLI telemetry collects the following data: 

- Fatal and non-fatal errors and warnings
- Configurations, and command-line arguments used
- Anonymous system information (operating system, and architecture)
- Feature usage data

CLI telemetry does not collect personally identifiable information, 
nor collects content or metadata of your source code.

You can view the CLI telemetry data collected by running, 
fossa cli with `FOSSA_TELEMETRY_DEBUG=1` environment variable. 
It will produce `fossa.telemetry.json` in your current working directory. 

### How is CLI telemetry data collected and processed?

CLI telemetry data is collected at the provided endpoint in configuration (with --endpoint). 
CLI telemetry data is validated, and processed for our logging system.

### How can I inspect CLI telemetry data?

You can view collected CLI telemetry data by running cli with environment variable `FOSSA_TELEMETRY_DEBUG=1`. 
This will generate `fossa.telemetry.json` file in your current working directory.

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

### I’m an on-prem customer, what telemetry data is collected by FOSSA?

CLI sends telemetry data to the endpoint specified in the fossa cli command. 
We never receive telemetry data collected from your endpoint. 

You can inspect collected telemetry data on your endpoint from your on-prem’s FOSSA’s database instance.
