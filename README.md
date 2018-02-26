<p style="text-align: center">
	<img src="https://fossa.io/images/logo.svg"/><br/><br/>
	<a href="https://app.fossa.io/projects/git%2Bgithub.com%2Ffossas%2Ffossa-cli?ref=badge_shield" alt="FOSSA Status">
    <img src="https://app.fossa.io/api/projects/git%2Bgithub.com%2Ffossas%2Ffossa-cli.svg?type=shield"/>
  </a>
	<a href="https://circleci.com/gh/fossas/fossa-cli" alt="CircleCI Tests">
    <img src="https://circleci.com/gh/fossas/fossa-cli.svg?style=svg&circle-token=f55f707e21ac39a80127d3372a1a1452ec94f4f7"/>
  </a>
</p>

# FOSSA CLI

**WARNING:** This tool is in alpha and is being actively developed. There may be breaking changes.

`fossa` is a tool that interrogates your environment and build to discover the dependencies getting included in your code. With one command, you can generate dependency reports and license notices across many build systems.

**Features:**

- Supports over 20+ build systems (npm, golang, maven, etc...)
- Supports monoliths; auto-detects and configures for multiple builds and modules in one codebase
- Fast and portable; a single cross-platform binary you can drop into CI builds and test your dependencies live
- Integrates with https://fossa.io for metadata and license auditing across open source packages

## Installation

Grab the latest release from the [Github Releases](releases/) page!

We support Windows, MacOS (Darwin), and Linux amd64 machines.

### QuickStart

#### One-Liners

```bash
# Out of the box, run this to initialize config,
# analyze and output results
fossa -o
```

```bash
# To upload results to FOSSA (https://fossa.io), 
# provide an API key (https://fossa.io/docs/projects/api-endpoints/)
# and remove the `-o` flag
FOSSA_API_KEY={ YOUR_API_KEY } fossa
```

#### Start with Configuration

```bash
# Initialize FOSSA configuration and scan for supported modules
fossa init

# Edit your .fossa.yml configuration to make sure we're analyzing the right modules
vi .fossa.yml

# Run FOSSA analysis and preview the results we're going to upload
fossa -o

# Run FOSSA and upload results
# Going forward, you only need to run this one-liner
FOSSA_API_KEY=YOUR_API_KEY_HERE fossa
```

#### Alternative Configurations

```bash
# You can also bootstrap config from the command line
# This is how we run `fossa` on `fossa`
FOSSA_API_KEY=YOUR_API_KEY_HERE fossa --modules go:./cmd/fossa

# Want to provide your own dependency list? We support that too.
# Check out the [user guide](docs/user-guide.md) for the upload format.
FOSSA_API_KEY=YOUR_API_KEY_HERE fossa upload --project PROJECT_NAME --revision REVISION_HASH --data YOUR_DATA_HERE
```

#### Integrating with FOSSA

```bash
# Block the build if your project fails its license scan
FOSSA_API_KEY=YOUR_API_KEY_HERE fossa test
```

## How it works
`fossa` works by analyzing your project for dependencies after your build system has built your project. This provides much more precise dependency information than just reading package manifest files:

- Some build tools are non-deterministic, so two builds with the same configuration may result in different dependencies being used.
- Many ecosystems use semantic versioning to specify dependency ranges, so running the same build at different points in time may result in different dependencies if a new version was published.
- Some build tools will execute external commands or arbitrary code which is impossible to statically analyze.

Instead of trying to guess at your build system's behavior, `fossa` runs locally using your build tools to determine a list of exact dependencies used by your binary.

`fossa` supports a wide variety of languages, package managers, and build tools out of the box:

 - JavaScript: `bower`, `npm`, `yarn`
 - Java: `mvn`
 - Ruby: `bundler`
 - Scala: `sbt`
 - PHP: `composer`
 - Go: `dep`, `glide`, `godep`, `govendor`, `vndr`, `gdm`
 - Archives: `*.rpm`

## Walkthrough

In this walkthrough, we'll demonstrate running `fossa` on `fossa`. By the end, you should be set up to just invoke the default command on every run:

```bash
# This will just Do The Right Thing
FOSSA_API_KEY=YOUR_API_KEY_HERE fossa
```

### Step 1. Building

Run a production build of your project. For most conventional builds, FOSSA can handle this for you:

```bash
# Check out the [user guide](docs/user-guide.md) for details on the module spec.
fossa build --modules go:./cmd/fossa
```

This will attempt a best-effort build by invoking your local build tools.

**NOTE:** Since many build systems are non-deterministic, we don't necessarily recommend building using `fossa`. Instead, you should build using your usual production method and then use `fossa` to analyze your build.

### Step 2. Analyzing
Once your project is built, `fossa` can analyze it for dependencies:

```bash
# For most supported languages, this should work out of the box
FOSSA_API_KEY=YOUR_API_KEY_HERE fossa analyze --modules go:./cmd/fossa

# I can also output the results
FOSSA_API_KEY=YOUR_API_KEY_HERE fossa analyze --output --modules go:./cmd/fossa
```

By default, this uploads your build results to fossa.io where you can use them to check for licensing and other issues. You can optionally disable this feature:

```bash
# Just output the analysis results
fossa analyze --output --modules go:./cmd/fossa
```

If FOSSA can't analyze your build correctly, you can also manually provide a set of results to upload:

```bash
# Check out the [user guide](docs/user-guide.md) for the upload format.
FOSSA_API_KEY=YOUR_API_KEY_HERE fossa upload --data=YOUR_DATA_HERE
```

### Step 3. Testing your builds

You can use `fossa` with projects on fossa.io to test your build for licensing and compliance issues. You can specify the project's licensing policy at app.fossa.io, and the CLI will automatically pull it from there.

(You can read more about [Provided Builds](https://fossa.io/docs/getting-started/provided-builds) here).

With policies, you can test your build for issues after you upload an analysis:

```bash
# This fails with exit code 1 if your project has issues
FOSSA_API_KEY=YOUR_API_KEY_HERE fossa test
```

You can add this as part of your CI's build and testing process to prevent builds with licensing issues from being pushed to production.

### Step 4. Committing your configurations

These configurations can be saved to a `.fossa.yaml` configuration file that `fossa` will read on every invocation. Use these to share your configuration among your teammates. Here's `fossa`'s configuration:

```yaml
version: 1

# For more details, check out the [user guide](docs/user-guide.md)
analyze:
  modules:
    - name: fossa-cli
      path: ./cmd/fossa
      type: go
```

With a configuration in place, you can just run `fossa` to analyze your project's build:

```bash
# Build, analyze, and upload -- pluggable into your development workflow
FOSSA_API_KEY=YOUR_API_KEY_HERE fossa

# The one-liner for testing after upload is pretty simple too
FOSSA_API_KEY=YOUR_API_KEY_HERE fossa && fossa test
```

## Reference
Check out the [User Guide](docs/user-guide.md) for more details.

## Development
### Adding language integrations

See [Adding New Languages](docs/integrations/adding-new-languages.md).

### Running tests

Since `fossa` relies on having the correct build tools in your local environment, running `fossa` tests requires being able to successfully build all projects in `test/fixtures/`. To provide these tools and prevent you from clobbering your local machine, we have run tests in a Docker container defined at `test/Dockerfile`.

## License

`fossa` is Open Source and licensed under the [AGPLv3](https://tldrlegal.com/license/gnu-affero-general-public-license-v3-(agpl-3.0)).

You are free to use `fossa` and its binaries under all common use conditions (i.e. integrating personal projects, securely scanning proprietary code, adding steps to build plugins) without contributing or releasing any integrated / proprietary code. Enjoy!

You are free to contribute or develop applications on top of `fossa`, but any modifications / derivative works must be released back to the community under this license.

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
