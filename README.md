# Spectrometer

Spectrometer is a polyglot dependency analysis tool, designed to identify and
report dependency trees for many languages and package managers.  See the
[docs](docs/strategies.md) for a non-exhaustive list of supported
languages/managers.

```sh
export FOSSA_API_KEY=your-key-goes-here
fossa analyze
```

If you do not have an API key, please check the [FOSSA Documentation](https://docs.fossa.com/docs/api-reference)
for instructions on creating API tokens.

## Table of Contents

1. [Installation](#installation)
2. [Basic Usage](#basic-usage)
3. [Supported Languages](#supported-languages)
4. [Contributing](#contributing)

## Installation

### MacOS (Darwin) or Linux amd64:
```bash
curl -H 'Cache-Control: no-cache' https://raw.githubusercontent.com/fossas/spectrometer/master/install.sh | bash
```

### Windows with Powershell:
```powershell
Set-ExecutionPolicy Bypass -Scope Process -Force; iex  ((New-Object System.Net.WebClient).DownloadString('https://raw.githubusercontent.com/fossas/spectrometer/master/install.ps1'))
```

**NOTE**: In order to use the executable, you may need to add it to your PATH.
The installer will report the installation path of the executable to make this
easier.

## Basic Usage

The tool requires little-to-no configuration to run on its own.

### Analysis
```sh
export FOSSA_API_KEY=your-key-goes-here
fossa analyze
# OR
fossa analyze --fossa-api-key your-key-goes-here
```

**NOTE:** *If leaked, your FOSSA API key can grant an attacker access to your FOSSA projects, and should be kept secret.  For this reason, we recommend the API key with the environment variable, especially in a shared environment like a CI/CD server.*

By default, the program will analyze the current working directory.  If you need
to run in a different directory, you can simply pass that directory as an argument.

```sh
fossa analyze /path/to/project
```

The `--output` flag tells the analyzer not to connect to a FOSSA server,
instead printing out the analysis result to the console.

```sh
fossa analyze --output
# OR
fossa analyze -o
```

### Testing

The fossa cli provides the ability to check if your project has passed its license scan after running analysis. This can be useful if you wish to block a CI pipeline on successful scan results.

Running the following command from the same directory as analysis and with the same API key will fail if the license scan is failing.
```sh
fossa test
```

## Supported Languages

A non-exhaustive list of supported languages and managers can be found
[here](docs/strategies.md).  This list is a work-in-progress, as some existing 
strategies are not yet documented, but are implemented.

## Contributing

### Building

Use [ghcup](https://gitlab.haskell.org/haskell/ghcup) to install the `cabal` cli tool and the ghc version we're using:

```sh
$ ghcup install-cabal
$ ghcup install 8.8
$ ghcup set 8.8
```

In the base directory, run `cabal build`

### Running

```sh
$ cabal run fossa -- analyze -o path/to/basedir/
```

This will produce analysis results on stdout

### Testing

Configure the project to enable tests
```sh
cabal configure --enable-tests
```

Run the tests:
```
cabal test
```

### Issue Reporting

Issues with the FOSSA CLI itself should be filed through the [Github issue's page](https://github.com/fossas/spectrometer/issues/new). However, if the issue you are seeing is related to the results you see on the FOSSA dashboard, you should contact support@fossa.com.

When filing an issue please record any information required to reproduce the issue you are experiencing so that the development team can help you as quickly as possible. Pictures and detailed information about your project are greatly appreciated!
