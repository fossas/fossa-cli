## Vendored Source Identification (VSI)

Some projects, especially C or C++ projects, vendor open source libraries in directories adjacent to the first party code being developed in-house.
FOSSA's Vendored Source Identification is intended to support identifying and categorizing such vendored libraries.

### How does it work?

FOSSA fingerprints all files in your project and uploads those fingerprints to our analysis service.
The analysis service then uses a proprietary algorithm to compare those fingerprints with the fingerprints in our database of open source projects.

VSI can be enabled with the `--experimental-enable-vsi` flag when running `fossa analyze`. For example:

```
fossa analyze --experimental-enable-vsi
```

### Prerequisites

VSI support requires a feature flag enabled in your FOSSA organization. If you're interested in using this feature please contact us!

### Default Filters

By default, VSI ignores the following directory:

- `{scandir}/.git`

### FAQ

#### Why not always enable VSI?

VSI is more computationally intensive, and therefore takes longer to run.
For this reason, we recommend only enabling it when you are reasonably confident that you will obtain useful information by enabling it.

To explain, enabling VSI causes the CLI to:

1. Fingerprint all files in the project
2. Send those fingerprints to the FOSSA analysis service for vendored source identification
3. Wait for remote analysis to complete

This process has undergone a lot of optimization to improve its performance (and this is an area we’re continually investing in), but it will never be as fast as scanning local files for dependency information.

#### Failed to resolve dependencies for FOSSA projects

> _Note: This example mentions functionality provided by the "multi-stage builds" family of features; it's documented here because you're likely to see this error during an `--experimental-enable-vsi` run._
> _For more information on multi-stage builds see the [multi stage builds overview](../msb/README.md)._

Let's say we have two C projects: `LibProject` and `CliProject`.
`LibProject` is a library that is used inside `CliProject`, and we want to report that relationship in FOSSA, such that dependencies of `LibProject` appear as transitive dependencies of `CliProject`.

Let's go further and say our CI pipeline for `LibProject` looks like this, where we compile its output binary (`libproject.o`) into the directory `out/`:

```bash
make build -o out
```

We can then integrate FOSSA here to essentially tie the fingerprint of that `libproject.o` binary to `LibProject` in FOSSA:

```bash
make build -o out
fossa analyze --experimental-link-project-binary out --experimental-enable-vsi
```

This causes FOSSA to store a record saying "whenever I see a file with the same cryptographic fingerprint as `libproject.o` in a future scan, I know that means `LibProject`".

Later, when we scan `CliProject`, so long as it contains a copy of `libproject.o` inside, we'll then perform the following steps:

1. The CLI will determine that `LibProject` is a dependency of `CliProject`, because it sees that copy of `libproject.o` inside the `CliProject` directory tree.
2. The CLI will then ask the FOSSA server for the list of `LibProject`'s dependencies.
3. The CLI will then report to the FOSSA server that `LibProject` is a dependency of `CliProject`, and include `LibProject`'s dependencies as transitive dependencies.

The trouble is, what happens if the user scanning `CliProject` doesn't have access to view the dependencies of `LibProject`?

In that case, we fail with an error like this one:

```bash
Failed to resolve dependencies for the following FOSSA projects:
  custom+1/libproject$1635972409

You may not have access to the projects, or they may not exist (see the warnings below for details).
If desired you can use --experimental-skip-vsi-graph to skip resolving the dependencies of these projects.
```

This is the CLI telling us "I know `LibProject` is a dependency of `CliProject`, but I can't view `LibProject` in the FOSSA server, so I don't know its dependencies and therefore I can't report its dependencies as transitive dependencies of `CliProject`".

The best way to handle this is to ensure that anyone using `LibProject` as a dependency has access to view it in FOSSA, so that they can get an accurate dependency graph.
However, as a workaround, the user may also use `--experimental-skip-vsi-graph custom+1/libproject$1635972409`, which tells the CLI "that's OK, I know you can't see it, and I'm OK with not getting the transitive dependencies from that project".
