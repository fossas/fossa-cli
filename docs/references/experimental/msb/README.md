## Multi Stage Builds

The _Multi-stage builds_ feature allows users give FOSSA metadata attached to a binary ahead of time (for example, a team working on an internal library can tell FOSSA about that library as part of that CI process).
FOSSA then can detect binaries that are registered as dependencies in downstream projects that include that binary in their source tree when [detecting vendored dependencies](../../subcommands/analyze/detect-vendored.md).

### Use Cases

#### Vendor a known binary

The _Multi-stage builds_ feature supports the ability to link metadata to binaries (example: `name`, `version`, `license`) for which you do not have the source available.
The binary will be identified within the org by its unique fingerprint as it is shared internally between the projects it will appear as a regular direct dependency with all of the specified metadata that was previously specified.
Such binary can be a library purchased from a vendor, or a prebuilt open source project.

#### Vendor an internal binary

If the team building the internal binary links the binary to its project, multi-stage build support can identify such binaries vendored into the directory structure of another project.
Linking will preserve all of the metadata known about the artifact during the linking.

The upstream library project will be shown as a direct dependency of the project that is vendoring the binary artifact, and upstream project’s dependencies are shown as transitive dependencies of the vendoring project.

#### Build a binary used downstream

The _Multi-stage builds_ feature supports the ability to link your output binaries when analyzing your project to support the internal binary use case.
This allows you to configure the CI pipeline for your project to always keep the linked binary up to date.

### Link a binary’s fingerprint to a user project

When the source of a library is available, FOSSA can scan that library for its dependencies and associate one or more binaries with the project.
Then, when scanning a downstream project that uses one of those binaries, all the dependency information from the library will appear as transitive dependency information for the downstream project.

To link one or more binaries to a project, use `--experimental-link-project-binary`.

Example:

```bash
fossa analyze --experimental-enable-vsi \
  --experimental-link-project-binary <BIN_DIR>
```

### Link a binary’s fingerprint to a user-defined dependency

When the source of a binary library is not available, FOSSA can still surface a binary as a user-defined dependency when it is present in a downstream project, so long as the dependency information has been linked to the binary ahead of time.

To link one or more binaries to a set of user-defined dependency metadata, use the subcommand `experimental-link-user-defined-dependency-binary`.

Example:

```bash
fossa experimental-link-user-defined-dependency-binary [DIR] \
  --name <NAME> \
  --version <VERSION> \
  --license <LICENSE>
```

Optionally, a project homepage and/or a description may be provided.

Example:

```bash
fossa experimental-link-user-defined-dependency-binary [DIR] \
  --name <NAME> \
  --version <VERSION> \
  --license <LICENSE> \
  --description <DESCRIPTION> \
  --homepage <HOMEPAGE>
```

**Note**: Similar to the `analyze` subcommand, this supports the `--endpoint` and `--fossa-api-key` commands to customize how the client connects to FOSSA.

### Troubleshooting

#### Failing to resolve dependencies for FOSSA projects

> _Note: This error may present during a `--detect-vendored` strategy._
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
fossa analyze --experimental-link-project-binary out --detect-vendored
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
