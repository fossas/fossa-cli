# How it works
`fossa` analyzes your project for dependencies after a build system has finished building a project. This provides more precise dependency information than just reading package manifest files which cause the following issues:

- Build tools can be non-deterministic, meaning two builds with the same configuration may result in different dependencies.
- Many ecosystems use semantic versioning to specify dependency ranges in manifests which can lead to a build with newer dependencies than the production system. 

Instead of trying to guess at your build system's behavior, `fossa` runs locally using your build tools and analyzing lockfiles to determine a list of exact dependencies used by your binary.

## Walkthrough

In this walkthrough, the `fossa-cli` will analyze the [FOSSA CLI](https://github.com/fossas/fossa-cli) to illustrate how the tool can be used. By the end, you should be able to set up fossa for your own project and retrieve results from fossa.com.

### Step 1. Building

#### Prerequisites
- Go1.10 or higher installed.
- A working [GOPATH directory](https://github.com/golang/go/wiki/GOPATH), commonly located at `/Users/<user_name>/go`.

#### Steps
1. Clone the cli repository into the src directory of your GOPATH.
   - `cd $GOPATH/src`
   - `git clone https://github.com/fossas/fossa-cli.git`
2. Build the fossa project by running `make` in the root directory to install all required dependencies. 

A working build is not required for all module types but it is best practice to ensure that the project is built before running an analysis. Building a project ensures that the dependencies and lockfiles are accurate as well as guaranteeing a working project. Fossa will first validate that the project can be built when debugging any cli issues.

### Step 2. Configuration

After a build has completed, the FOSSA project can be configured. FOSSA recommends configuring a project using [.fossa.yml](config-file.md#fossayml) created by [`fossa init`](user-guide.md/#fossa-init). Navigate to the root of the project directory and run the following to create a configuration file:

```bash
fossa init
```

### Step 3. Analysis
Once your project is built, [`fossa analyze`](user-guide.md/#fossa-analyze) can be used to scan for dependencies and upload a dependency graph.

Verify that analysis completes before uploading by testing with the `--output` flag to show the data which will be uploaded to the server endpoint.
```bash
# Using .fossa.yml configuration analyze and output the results.
FOSSA_API_KEY=YOUR_API_KEY_HERE fossa analyze --output
```
After analysis succeeds, run again without the output command to upload results.
```bash
FOSSA_API_KEY=YOUR_API_KEY_HERE fossa analyze
```
[`fossa analyze`](user-guide.md/#fossa-analyze) will upload your build results to [FOSSA.com]([FOSSA.com](https://fossa.com)) by defult where the project is scanned for licensing and security issues.

### Step 4. Testing

Uploading a build to [FOSSA.com](https://fossa.com) allows the user to run [license and compliancy scans](https://docs.fossa.com/docs/running-a-scan) on the project. Scans are run against user defined [policies](https://docs.fossa.com/docs/policies) which single out non compliant licenses and create [issues](https://docs.fossa.com/docs/triaging-issues) for the user to address. Information about this integration can be found in the [FOSSA.com manual](https://docs.fossa.com/docs/getting-started).

Issues on a project represent policy violations. FOSSA CLI can run a check using `fossa test` in the user's CI pipeline to verify that all new PR's pass their policy on [FOSSA.com](https://fossa.com). This guarantees that all new code in the repository is compliant with existing mandates. This command can be run locally or in a CI immediately after analysis completes. 

> Note: `fossa test` will wait up to 10 minutes for the project's latest build to finish scanning before timing out.

```bash
# This fails with exit code 1 if your project has unresolved issues.
# Run with the same configuration file as analysis.
FOSSA_API_KEY=YOUR_API_KEY_HERE fossa test
```

### Step 5. Finalizing

Save your [configuration file](config-file.md) to the repository so that it can be shared amongst teammates and stored for CI runs. 

Fossa provides many ways to share the results of a scan. If you maintain an open source repository this is a good way to let your users know you care about staying open source. Publicizing your results is an easy way to show what dependencies you use and the licenses they bring with it. Use the badge below to share your results!

[![FOSSA Status](https://app.fossa.io/api/projects/git%2Bgithub.com%2Ffossas%2Ffossa-cli.svg?type=large)](https://app.fossa.io/projects/git%2Bgithub.com%2Ffossas%2Ffossa-cli?ref=badge_large)