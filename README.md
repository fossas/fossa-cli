# Spectrometer

Spectrometer is a minimal-configuration dependency analysis tool. It supports a wide array of languages and buildtools.

Spectrometer extracts dependency graphs from your projects and reports them to [FOSSA](https://fossa.com) for license scanning, vulnerability scanning, and more.

## Table of Contents

1. [Installation](#installation)
2. [Using Spectrometer](#using-spectrometer)
3. [Reporting Issues](#reporting-issues)

## Installation

### macOS or 64-bit Linux:
```bash
curl -H 'Cache-Control: no-cache' https://raw.githubusercontent.com/fossas/spectrometer/master/install.sh | bash
```

### Windows with Powershell:
```powershell
Set-ExecutionPolicy Bypass -Scope Process -Force; iex  ((New-Object System.Net.WebClient).DownloadString('https://raw.githubusercontent.com/fossas/spectrometer/master/install.ps1'))
```

**NOTE**: You may need to add the executable to your PATH. The installer reports the installation path of the executable.

## Using Spectrometer

See the [User Guide](docs/userguide.md) for detailed instructions.

Usually, this is sufficient:

``` sh
# configure api key
export FOSSA_API_KEY=your-api-key-goes-here

# run dependency analysis in the current
# directory, uploading results to FOSSA
fossa analyze

# check for FOSSA license- and vulnerability-scan results
fossa test
```

## Reporting Issues

If you are experiencing an issue related to the results on the FOSSA website/dashboard, please contact [support@fossa.com](mailto:support@fossa.com)

Issues specific to Spectrometer should be filed through the [Github issues page](https://github.com/fossas/spectrometer/issues/new). 

Please include the following in your bug report:

- Steps to reproduce your issue
- Relevant project manifest files (e.g., `pom.xml` or `package.json`)
