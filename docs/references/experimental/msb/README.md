## Multi Stage Builds

The _Multi-stage builds_ feature allows users give FOSSA metadata attached to a binary ahead of time (for example, a team working on an internal library can tell FOSSA about that library as part of that CI process).
FOSSA then can detect binaries that are registered as dependencies in downstream projects that include that binary in their source tree when running with [VSI enabled](../vsi/README.md).

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
