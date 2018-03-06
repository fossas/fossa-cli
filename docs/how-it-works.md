## How it works
`fossa` works by analyzing your project for dependencies after your build system has built your project. This provides much more precise dependency information than just reading package manifest files:

- Some build tools are non-deterministic, so two builds with the same configuration may result in different dependencies being used.
- Many ecosystems use semantic versioning to specify dependency ranges, so running the same build at different points in time may result in different dependencies if a new version was published.
- Some build tools will execute external commands or arbitrary code which is impossible to statically analyze.

Instead of trying to guess at your build system's behavior, `fossa` runs locally using your build tools to determine a list of exact dependencies used by your binary.

`fossa` supports a wide variety of languages, package managers, and build tools out of the box:

 - JavaScript: `bower`, `npm`, `yarn`
 - Java/Scala: `mvn`, `gradle`, `sbt`
 - Ruby: `bundler`
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