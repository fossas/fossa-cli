![FOSSA](https://raw.githubusercontent.com/fossas/fossa-cli/master/docs/assets/header.png)

<p align="center">
  <b>fossa-cli</b> - Fast, portable, and reliable dependency analysis for any codebase.
</p>

<p align="center">
  <a href="https://app.fossa.io/projects/git%2Bgithub.com%2Ffossas%2Ffossa-cli?ref=badge_shield" alt="FOSSA Status">
    <img src="https://app.fossa.io/api/projects/git%2Bgithub.com%2Ffossas%2Ffossa-cli.svg?type=shield"/>
  </a>
  <a href="https://github.com/fossas/fossa-cli/releases" alt="Github Downloads">
    <img src="https://img.shields.io/github/downloads/fossas/fossa-cli/total.svg"/>
  </a>
  <a href="https://circleci.com/gh/fossas/fossa-cli" alt="CircleCI Tests">
    <img src="https://circleci.com/gh/fossas/fossa-cli.svg?style=shield&circle-token=f55f707e21ac39a80127d3372a1a1452ec94f4f7"/>
  </a>
  <a href="https://goreportcard.com/report/github.com/fossas/fossa-cli">
    <img src="https://goreportcard.com/badge/github.com/fossas/fossa-cli">
  </a>
  <a href="https://golangci.com/r/github.com/fossas/fossa-cli">
    <img src="https://golangci.com/badges/github.com/fossas/fossa-cli.svg">
  </a>
  <a href="https://codecov.io/gh/fossas/fossa-cli">
    <img src="https://codecov.io/gh/fossas/fossa-cli/branch/master/graph/badge.svg" />
  </a>
</p>

## Background

`fossa` analyzes complex codebases to generate dependency reports and license notices. It can generate fast and highly-accurate results, by leveraging existing build environments. Refer to the [FOSSA CLI User Manual](docs/README.md#fossa-cli-documentation) for in depth information about using this tool.

**Features:**

- Supports over 20+ languages & environments (JavaScript, Java, Ruby, Python, Golang, PHP, .NET, etc...)
- Auto-configures for monoliths; instantly handles multiple builds in large codebases.
- Fast & portable; a cross-platform binary you can drop into CI or dev machines.
- Generates offline documentation for license notices & third-party attributions.
- Tests dependencies against license violations, audits and vulnerabilities (coming soon!) by integrating with https://fossa.com.

## Installation

The following commands will execute scripts to fetch and install the latest [GitHub Releases](https://github.com/fossas/fossa-cli/releases) on the corresponding operating system.

### Install with Homebrew (MacOS or Linux):
```bash
brew install fossas/tap/fossa
```

### MacOS (Darwin) or Linux amd64/arm64:
```bash
curl -H 'Cache-Control: no-cache' https://raw.githubusercontent.com/fossas/fossa-cli/master/install.sh | bash
```

This command will install the FOSSA CLI into `usr/local/bin`. If you do not have permissions to access this folder you may specify the directory you would like by modifying to command to:
```bash
curl -H 'Cache-Control: no-cache' https://raw.githubusercontent.com/fossas/fossa-cli/master/install.sh | bash -s -- -b <custom directory>
```

### Windows with Powershell:
```powershell
Set-ExecutionPolicy Bypass -Scope Process -Force; iex  ((New-Object System.Net.WebClient).DownloadString('https://raw.githubusercontent.com/fossas/fossa-cli/master/install.ps1'))
```

Add `C:\ProgramData\fossa-cli` to your path by modifying your `profile.ps1` file or temporarily with the following command:
```powershell
$env:Path += ";C:\ProgramData\fossa-cli"
``` 

Make to sure to have your [FOSSA API Key](https://docs.fossa.com/docs/api-reference) set as an environment variable

```powershell
$env:FOSSA_API_KEY = "YOUR_API_KEY_HERE"
``` 

## Quick Start

Run `fossa` and provide a [FOSSA API Key](https://docs.fossa.com/docs/api-reference) to create a local [configuration file](docs/config-file.md#fossayml) and analyze the project. The project information will be uploaded and a link to a rich, hosted report on [fossa.com](https://fossa.com) will be output:

```bash
FOSSA_API_KEY="YOUR_API_KEY_HERE" fossa

# Output:
# ==========================================================
#
#    View FOSSA Report: https://app.fossa.com/{YOUR_LINK}
#
# ==========================================================
```
> Note: Running [`fossa`](docs/user-guide.md/#fossa) is equivalent to running [`fossa init`](docs/user-guide.md#fossa-init) followed by [`fossa analyze`](docs/user-guide.md#fossa-analyze).
## Documentation

If you run into a problem using the FOSSA CLI, most issues can be resolved by looking at our documentation in the [FOSSA CLI User Manual](docs/README.md#fossa-cli-documentation). This will shed light to how we analyze specific ecosystems and how to accurately configure your project.

If you have questions please refer to the [FAQ](docs/faq.md#frequently-asked-questions). If your question is related to a bug or feature please open an issue on GitHub. You can also reach out to fossa directly at support@fossa.com.

If you are interested in learning more about FOSSA you can visit our homepage at fossa.com and look at our [online documentation](https://docs.fossa.com/docs).

## Supported Environments
| Environment                                     | Package Managers                                                                                                                  |
| ----------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------- |
| Android                                         | [Gradle](docs/integrations/gradle.md#gradle)                                                                                      |
| [Clojure](docs/integrations/clojure.md#clojure) | Leiningen                                                                                                                         |
| [Debian](docs/integrations/debian.md#debian)    | Dpkg                                                                                                                              |
| [Golang](docs/integrations/golang.md#go)        | Dep, Gomodules, Vndr, GDM, Glide, Godep, Govendor                                                                                 |
| Groovy                                          | [Gradle](docs/integrations/gradle.md#gradle)                                                                                      |
| [Haskell](docs/integrations/haskell.md#haskell) | Cabal and Stack                                                                                                                   |
| Java                                            | [Gradle](docs/integrations/gradle.md#gradle), [Maven](docs/integrations/maven.md#maven), [Ant](docs/integrations/ant.md#ant--ivy) |
| Javascript                                      | [nodejs & npm](docs/integrations/nodejs.md#nodejs)                                                                                |
| Kotlin                                          | [Gradle](docs/integrations/gradle.md#gradle)                                                                                      |
| Monorepo tooling                                | [okbuck](docs/integrations/okbuck.md#okbuck), [Buck](docs/integrations/buck.md#buck)                                              |
| [.NET](docs/integrations/dotnet.md#net)         | NuGet, Paket                                                                                                                      |
| Objective-C                                     | [Cocoapods](docs/integrations/cocoapods.md#cocoapods), [Carthage](docs/integrations/carthage.md#carthage)                         |
| PHP                                             | [Composer](docs/integrations/composer.md#composer)                                                                                |
| [Python](docs/integrations/python.md#python)    | Pip, Pipenv, requirements.txt                                                                                                     |
| [Ruby](docs/integrations/ruby.md#ruby)          | Bundler                                                                                                                           |
| [Rust](docs/integrations/rust.md#rust)          | Cargo                                                                                                                             |
| Scala                                           | [SBT](docs/integrations/sbt.md#sbt)                                                                                               |
| Swift                                           | [Cocoapods](docs/integrations/cocoapods.md#cocoapods), [Carthage](docs/integrations/carthage.md#carthage)                         |
| C, C++                                          | [Archive Uploader](docs/integrations/archive.md#archive)                                                                          |

If your development environment is not supported, check out the [archive](docs/integrations/archive.md#archive) uploader which allows direct license scanning of source code files.

[Click here to learn more](docs/user-guide.md#user-guide) about the technical details behind this project.

## Configuration

```bash
fossa init # writes to `.fossa.yml`
```

Running `fossa init` will create a hidden configuration file called [`.fossa.yml`](docs/config-file.md#fossayml) which looks like this:

```yaml
version: 1
cli:
  server: https://app.fossa.com
  fetcher: custom
  project: github.com/fossas/fossa-cli
analyze:
  modules:
    - name: fossa-cli
      type: go
      target: github.com/fossas/fossa-cli/cmd/fossa
      path: ./cmd/fossa
```

Check out the [User Guide](docs/user-guide.md#user-guide) to learn about editing this file.

After [configuration](docs/user-guide.md#1-configuring-a-project), you can now [analyze](docs/user-guide.md#2-analyzing-a-project) the project and upload new results:

```bash
# Run FOSSA analysis and preview the results to be uploaded.
fossa analyze -o

# Run FOSSA and upload results
# Going forward, you only need to run this one-liner
FOSSA_API_KEY=YOUR_API_KEY_HERE fossa analyze
```

## Integrating with CI

### Testing for License Violations
If you've integrated with https://fossa.com, you can use [`fossa test`](docs/user-guide.md#fossa-test) to fail builds against your [FOSSA scan status](https://app.fossa.io/projects/git%2Bgithub.com%2Ffossas%2Ffossa-cli/refs/branch/master/5e225327846320e9dfb8bf12673afa2eb4144fb4/preview).

```bash
# Exit with a failing status and dump an issue report to stderr
# if your project fails its license scan
FOSSA_API_KEY=YOUR_API_KEY_HERE fossa test

# Output:
# --------------------------
# - exit status (1)
#
# * FOSSA discovered 7 license issue(s) in your dependencies:
#
# UNLICENSED_DEPENDENCY (3)
# * pod+FBSnapshotTestCase$1.8.1
# * pod+FBSnapshotTestCase$2.1.4
# * pod+Then$2.1.0
#
# POLICY_FLAG (4)
# * mvn+com.fasterxml.jackson.core:jackson-core$2.2.3
# * npm+xmldom$0.1.27
# * pod+UICKeyChainStore$1.0.5
# * gem+json$1.7.7
#
# ✖ FOSSA license scan failed: 7 issue(s) found.
```

### Generating License Notices

You are able to generate a license notice with each CI build using the [`fossa report`](docs/user-guide.md#fossa-report) command:
  
```bash
# write a license notice to NOTICE.txt
fossa report licenses > NOTICE.txt
```

[See this repo's NOTICE file](NOTICE) for an example.

License data is provided by [https://fossa.com](https://fossa.com)'s 500GB open source registry.

## Development

View our [Contribution Guidelines](.github/CONTRIBUTING.md) to get started.

## License

`fossa` is Open Source and licensed under the [MPL-2.0](https://tldrlegal.com/license/mozilla-public-license-2.0-(mpl-2)).

You are free to use `fossa` for commercial or personal purposes. Enjoy!

[![FOSSA Status](https://app.fossa.io/api/projects/git%2Bgithub.com%2Ffossas%2Ffossa-cli.svg?type=large)](https://app.fossa.io/projects/git%2Bgithub.com%2Ffossas%2Ffossa-cli?ref=badge_large)
