<!-- markdown-link-check-disable-next-line -->
![FOSSA](https://raw.githubusercontent.com/fossas/fossa-cli/master/docs/assets/header.png)

# FOSSA CLI

[![Build](https://github.com/fossas/fossa-cli/actions/workflows/build-all.yml/badge.svg)](https://github.com/fossas/fossa-cli/actions/workflows/build.yml)
[![Dependency scan](https://github.com/fossas/fossa-cli/actions/workflows/dependency-scan.yml/badge.svg)](https://github.com/fossas/fossa-cli/actions/workflows/dependency-scan.yml) <!-- markdown-link-check-disable-next-line -->
[![FOSSA Status](https://app.fossa.com/api/projects/custom%2B1%2Fgit%40github.com%3Afossas%2Ffossa-cli.svg?type=shield)](https://app.fossa.com/projects/custom%2B1%2Fgit%40github.com%3Afossas%2Ffossa-cli?ref=badge_shield) <!-- markdown-link-check-disable-next-line -->
[![FOSSA Downloads](https://img.shields.io/github/downloads/fossas/fossa-cli/total.svg)](https://github.com/fossas/fossa-cli/releases)


`fossa-cli` is a zero-configuration polyglot dependency analysis tool. You can point fossa CLI at any codebase or build, and it will automatically detect dependencies being used by your project.

`fossa-cli` currently supports automatic dependency analysis for [many different build tools and languages](docs/references/strategies/README.md#supported-languages). It also has limited support for vendored dependency detection, container scanning, and system dependency detection. These features are still a work in progress. Our goal is to make the FOSSA CLI a universal tool for dependency analysis.

`fossa-cli` integrates with [FOSSA](https://fossa.com) for dependency analysis, license scanning, vulnerability scanning, attribution report generation, and more.

## Table of Contents

1. [Installation](#installation)
2. [Getting Started](#getting-started)
3. [User Manual](#user-manual)
4. [Reporting Issues](#reporting-issues)
5. [Contributing](#contributing)

## Installation

### Using the install script

FOSSA CLI provides an install script that downloads the latest release from GitHub Releases for your computer's architecture. You can see the source code and flags at [`install-latest.sh`](./install-latest.sh) for Mac and Linux or [`install-latest.ps1`](./install-latest.ps1) for Windows.

**NOTE:** You may need to add the downloaded executable to your `$PATH`. The installer script will output the installed path of the executable. You can also use `-b` to pick the installation directory when using `install-latest.sh` (see [the `install-latest.sh` source code](./install-latest.sh) for details).

#### macOS or 64-bit Linux

```bash
curl -H 'Cache-Control: no-cache' https://raw.githubusercontent.com/fossas/fossa-cli/master/install-latest.sh | bash
```

#### Windows with Powershell

```powershell
Set-ExecutionPolicy Bypass -Scope Process -Force; iex  ((New-Object System.Net.WebClient).DownloadString('https://raw.githubusercontent.com/fossas/fossa-cli/master/install-latest.ps1'))
```
<!-- markdown-link-check-disable-next-line -->
Alternatively, install using [Scoop](https://scoop.sh/):

```
scoop install fossa
```

Please refer to detailed walkthrough [Installing FOSSA CLI](./docs/walkthroughs/installing-fossa-cli.md), for installing [FOSSA CLI 1.x](./docs/walkthroughs/installing-fossa-cli.md#installing-cli-1x-using-installation-script) and using [GitHub Releases](https://github.com/fossas/fossa-cli/releases) to install [FOSSA CLI manually](./docs/walkthroughs/installing-fossa-cli.md#installing-manually-with-github-releases).

## Getting Started

### Integrating your project with FOSSA

#### TL;DR, Linux, Mac, \*nix-like

```sh
# Download FOSSA.
curl -H 'Cache-Control: no-cache' https://raw.githubusercontent.com/fossas/fossa-cli/master/install-latest.sh | bash

# Set your API key. Get this from the FOSSA web application.
export FOSSA_API_KEY=XXXX

# Run an analysis in your project's directory.
cd $MY_PROJECT_DIR
fossa analyze
```

#### TL;DR, Windows

```powershell
# Download FOSSA.
Set-ExecutionPolicy Bypass -Scope Process -Force; iex  ((New-Object System.Net.WebClient).DownloadString('https://raw.githubusercontent.com/fossas/fossa-cli/master/install-latest.ps1'))

# Set your API key. Get this from the FOSSA web application.
$env:FOSSA_API_KEY=XXXX

# Run an analysis in your project's directory.
cd $MY_PROJECT_DIR
fossa analyze
```

#### Installing FOSSA CLI

Follow [the installation instructions](#installation) above to install the FOSSA CLI. Once installed, you should have a new binary named `fossa` available on your `$PATH`.

#### Generating an API key

To get started with integrating your project into FOSSA, you'll need to [generate an API key](https://docs.fossa.com/docs/api-reference). You'll get this API key from the FOSSA web application ([app.fossa.com](https://app.fossa.com)).

Once you have your API key:

```sh
export FOSSA_API_KEY=XXXX # Use your API key here.
```

#### Running an analysis

Now we can run an analysis. To run an analysis, all you need to do is navigate to your project's directory and run `fossa analyze`.

**NOTE:** While `fossa` will try its best to report available results for any kind of project, you'll get the best results by running in a directory with a working project build. A working build lets us integrate directly with your build tool to identify dependencies, instead of trying to infer dependencies from your source code.

```sh
$ cd $MY_PROJECT_DIR # Use your actual project location here.

$ fossa analyze
[ INFO] Using project name: `https://github.com/fossas/fossa-cli`
[ INFO] Using revision: `09ca72e398bb32747b27c0f43731678fa42c3c26`
[ INFO] Using branch: `No branch (detached HEAD)`
[ INFO] ============================================================

      View FOSSA Report:
      https://app.fossa.com/projects/custom+1%2fgithub.com%2ffossas%2ffossa-cli/refs/branch/master/09ca72e398bb32747b27c0f43731678fa42c3c26

  ============================================================
```

#### Viewing your results

Once an analysis has been uploaded, you can view your results in the FOSSA web application. You can see your analysis by using the link provided as output by `fossa analyze`, or by navigating to your project and revision in the FOSSA web application.

#### What next?

Now that your analysis is complete, there are a couple things you might want to do after an initial integration:

- **Double-check your results.** Some analysis methods may produce partial or unexpected results depending on what information was available when you ran the analysis. If something seems wrong, [our debugging guide](./docs/walkthroughs/debugging-your-integration.md) can help you diagnose and debug your integration.

- **Scan for issues and generate a compliance report.** Once your analysis is ready, we'll automatically queue an issue scan and report the results in the web application. Once an issue scan is complete, you can also [generate a report](https://docs.fossa.com/docs/running-a-scan) from the web application.

- **Set up FOSSA in your CI.** You can also use your issue scan results as inputs to CI scripts. For GitHub repositories, you can use FOSSA's [native GitHub integration](https://docs.fossa.com/docs/automatic-updates#pull-request--commit-statuses-github-only) to report a status check on your PRs. For other CI integrations, you can also [use `fossa test`](docs/references/subcommands/test.md) to get programmatic issue status in CI.

## User Manual

For most users, the FOSSA CLI will work out-of-the-box without any configuration. Just get an API key, run `fossa analyze`, and view your results in the FOSSA web application.

Users who need advanced customization or features should see the [User Manual](./docs/README.md). Some common topics of interest include:

- [Debugging your integration](./docs/walkthroughs/debugging-your-integration.md)
- [Specifying vendored dependencies](docs/features/vendored-dependencies.md)
- [Adding manual dependencies](docs/features/manual-dependencies.md)

## Reporting Issues

If you've found a bug or need support, the best way to get support is via the [FOSSA support portal](https://support.fossa.com).

Make sure to include reproduction steps and any relevant project files (e.g. `pom.xml`s, `package.json`s, etc.). Including the output from `fossa analyze --debug` in the email as well as any relevant fossa files (`fossa-deps.json`, `.fossa.yml`) will help expedite a solution.

We'll try to respond to issues opened in this repository on a best-effort basis, but we mostly provide support via the [FOSSA support portal](https://support.fossa.com).

## Contributing

If you're interested in contributing, check out our [contributor documentation](./docs/contributing/README.md). PRs are welcome!

## Licensing Data

Some of the data used for CLI-side license scanning is provided by nexB under a [Creative Commons Attribution 4.0 International](https://creativecommons.org/licenses/by/4.0/legalcode) license. This data can be found here: https://github.com/nexB/scancode-toolkit/tree/develop/src/licensedcode/data.

The licensing data is Copyright (c) nexB Inc. and others. All rights reserved.
    ScanCode is a trademark of nexB Inc.
    SPDX-License-Identifier: CC-BY-4.0
    See https://creativecommons.org/licenses/by/4.0/legalcode for the license text.
    See https://github.com/nexB/scancode-toolkit for support or download.
    See https://aboutcode.org for more information about nexB OSS projects.
