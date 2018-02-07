<p align="center">
	<img src="https://fossa.io/images/logo.svg"/><br/><br/>
	<a href="https://app.fossa.io/projects/git%2Bgithub.com%2Ffossas%2Ffossa-cli?ref=badge_shield" alt="FOSSA Status"><img src="https://app.fossa.io/api/projects/git%2Bgithub.com%2Ffossas%2Ffossa-cli.svg?type=shield"/></a>
	<a href="https://circleci.com/gh/fossas/fossa-cli" alt="CircleCI Tests"><img src="https://circleci.com/gh/fossas/fossa-cli.svg?style=svg&circle-token=f55f707e21ac39a80127d3372a1a1452ec94f4f7"/></a>
</p>

# FOSSA CLI

`fossa-cli` is a tool that interrogates your environment and build to discover the dependencies getting included in your software.  It can be used alone or within build systems to dig out dependency data from complex monolithic builds.

This tool integrates directly with the [FOSSA](https://fossa.io) web service to provide open source reports, search intelligence, vulnerability/license scanning and triage workflows to help your organization to manage its open source use.

The CLI provides an alternative path to integrating your code repositories and builds in a way that doesn't require code access or FOSSA environment setup, or enrich dependency reports with dynamically confirmed dependency artifacts.

**FOSSA CLI is currently in alpha and will have a changing API; join our contributor group to get development updates**

## Installation

`fossa-cli` is compatible with Windows, Darwin (MacOS) and *nix-based operating systems.

### Install with Curl (Linux / MacOS)

```bash
  curl -L
```

### Install with npm/yarn (All Platforms)

If you have npm/yarn on your machine, you can get `fossa-cli` with:

```bash
  npm install -g fossa
```

OR

```bash
  yarn add --global fossa
```

### Install with Homebrew (MacOS)

```bash
  brew install fossa
```

## Usage

```bash
  fossa
```

## Development



## License

`fossa-cli` is Open Source and licensed under the [AGPLv3](https://tldrlegal.com/license/gnu-affero-general-public-license-v3-(agpl-3.0)).

You are free to use `fossa-cli` and its binaries under all common use conditions (i.e. integrating personal projects, securely scanning corporate code, adding steps to build plugins) without contributing or releasing any integrated / proprietary code... Enjoy!

You are free to contribute or develop applications on top of `fossa-cli`, but any modifications / derivative works must be released back to the community under this license.

Please contact [support@fossa.io](mailto:support@fossa.io) for additional licensing guidance.

```
    Copyright (C) 2018 FOSSA, Inc.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
```

[![FOSSA Status](https://app.fossa.io/api/projects/git%2Bgithub.com%2Ffossas%2Ffossa-cli.svg?type=large)](https://app.fossa.io/projects/git%2Bgithub.com%2Ffossas%2Ffossa-cli?ref=badge_large)
