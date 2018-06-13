![FOSSA](https://raw.githubusercontent.com/fossas/fossa-cli/master/docs/assets/header.png)

<p align="center">
  <b>fossa-cli</b> - Fast, portable and reliable dependency analysis for any codebase.
</p>

<p align="center">
  <a href="https://app.fossa.io/projects/git%2Bgithub.com%2Ffossas%2Ffossa-cli?ref=badge_shield" alt="FOSSA Status">
    <img src="https://app.fossa.io/api/projects/git%2Bgithub.com%2Ffossas%2Ffossa-cli.svg?type=shield"/>
  </a>
  <a href="https://circleci.com/gh/fossas/fossa-cli" alt="CircleCI Tests">
    <img src="https://circleci.com/gh/fossas/fossa-cli.svg?style=shield&circle-token=f55f707e21ac39a80127d3372a1a1452ec94f4f7"/>
  </a>
  <a href="https://goreportcard.com/report/github.com/fossas/fossa-cli">
    <img src="https://goreportcard.com/badge/github.com/fossas/fossa-cli">
  </a>
  <a href="http://slack.fossa.io/">
    <img src="https://slack.fossa.io/badge.svg">
  </a>
</p>

## Background
`fossa` analyzes complex codebases to generate dependency reports and license notices.  By leveraging existing build environments, it can generate fast and highly-accurate results.

**Features:**

- Supports [over 20+ languages & environments](docs/how-it-works.md) (JavaScript, Java, Ruby, Python, Golang, PHP, .NET, etc...)
- Auto-configures for monoliths; instantly handles multiple builds in large codebases
- Fast & portable; a cross-platform binary you can drop into CI or dev machines
- Generates offline documentation for license notices & third-party attributions
- Tests dependencies against license violations, audits and vulnerabilities (coming soon!) by integrating with https://fossa.io

[Click here to learn more](docs/how-it-works.md) about the reasons and technical details behind this project.

## Installation

Install on **MacOS (Darwin) or Linux amd6** using `curl`:
```bash
curl -H 'Cache-Control: no-cache' https://raw.githubusercontent.com/fossas/fossa-cli/master/install.sh | bash
```

Install on **Windows** using `cmd.exe`:
```powershell
@"%SystemRoot%\System32\WindowsPowerShell\v1.0\powershell.exe" -NoProfile -InputFormat None -ExecutionPolicy Bypass -Command "iex ((New-Object System.Net.WebClient).DownloadString('https://raw.githubusercontent.com/fossas/fossa-cli/master/install.ps1'))"
```

These commands will execute scripts to fetch and install the latest [Github Release](releases/).

## Quick Start

Run `fossa analyze -o` in your repo directory to output a dependency report in JSON:
```json
[
  {
    "Name": "fossa-cli",
    "Type": "golang",
    "Manifest": "github.com/fossas/fossa-cli/cmd/fossa",
    "Build": {
      "Dependencies": [
        {
          "locator": "go+github.com/rhysd/go-github-selfupdate$d5c53b8d0552a7bf6b36457cd458d27c80e0210b",
          "data": {
            "name": "github.com/rhysd/go-github-selfupdate",
            "version": "d5c53b8d0552a7bf6b36457cd458d27c80e0210b"
          }
        },
        ...
      ],
      ...
    }
  },
  ...
]
```

Run `fossa` and provide a [FOSSA API Key](https://docs.fossa.io/docs/api-reference) to get a rich, hosted report:

```bash
export FOSSA_API_KEY="YOUR_API_KEY_HERE"

# Now, you can just run `fossa`!
fossa

# Output:
# ==========================================================
#
#    View FOSSA Report: https://app.fossa.io/{YOUR_LINK}
#
# ==========================================================
```

## Configuration

Initialize configuation and scan for supported modules:

```bash
fossa init # writes to `.fossa.yml`
```

This will initialize a `.fossa.yml` file that looks like this:

```yaml
version: 1

cli:
  server: https://app.fossa.io
  project: github.com/fossas/fossa-cli

analyze:
  modules:
    - name: fossa-cli
      path: ./cmd/fossa
      type: go

# ...
```

Check out our [User Guide](docs/user-guide.md) to learn about editing this file.

After configuration, you can now preview and upload new results:

```bash
# Run FOSSA analysis and preview the results we're going to upload
fossa analyze -o

# Run FOSSA and upload results
# Going forward, you only need to run this one-liner
FOSSA_API_KEY=YOUR_API_KEY_HERE fossa
```

## Integrating with CI

### Testing for License Violations
If you've integrated with https://fossa.io, you can use `fossa test` to fail builds against your FOSSA scan status.

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

To generate a license notice with each CI build, you can use the `fossa report` command:

```bash
# write a license notice to NOTICE.txt
fossa report --type licenses > NOTICE.txt
```

[See this repo's NOTICE file](NOTICE) for an example.

License data is provided by [https://fossa.io](https://fossa.io)'s 500GB open source registry.

## Reference
Check out the [User Guide](docs/user-guide.md) for more details.

## Development

View our [Contribution Guidelines](CONTRIBUTING.md) to get started.

Join our public [Slack Channel](https://slack.fossa.io).

If you're in San Francisco, come to our monthly [Open Source Happy Hour](http://meetup.fossa.io) to meet us F2F!

## License

`fossa` is Open Source and licensed under the [MPL-2.0](https://tldrlegal.com/license/mozilla-public-license-2.0-(mpl-2)).

You are free to use `fossa` for commercial or personal purposes.  Enjoy!

[![FOSSA Status](https://app.fossa.io/api/projects/git%2Bgithub.com%2Ffossas%2Ffossa-cli.svg?type=large)](https://app.fossa.io/projects/git%2Bgithub.com%2Ffossas%2Ffossa-cli?ref=badge_large)
