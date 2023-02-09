## Dynamic Linked Dependency Detection

Some projects, especially C or C++ projects, result in binaries that dynamically link with system installed libaries.

### How does it work?

Dynamic Linked Dependency Detection works by using `ldd` to inspect target binaries for a list of linked libraries.
It then interacts with the local package manager (`rpm` for RedHat-based systems, `dpkg` for Debian-based systems) to determine the library which owns the binary.

If a library is found, it is reported to FOSSA, which uses our database of Linux packages to lookup licensing and vulnerability information for the package.
If a library is not found, the dependency is reported in FOSSA as an "unmanaged dynamically linked dependency". From there users may use the FOSSA UI to add a license or ignore the dependency.

### Prerequisites

Dynamic Linked Dependency Detection is supported on the following platforms:

* RedHat-based Linux systems (examples: RedHat, CentOS, Fedora)
* Debian-based Linux systems (examples: Debian, Ubuntu)

### Security

Dynamic Linked Dependency Detection is currently powered by `ldd`.

It is possible for a program to be built with its own loader which ignores the `LD_TRACE_LOADED_OBJECTS` variable used by `ldd`.
This could result in the binary being executed instead of being inspected.

For this reason, we strongly recommend against running Dynamic Linked Dependency Detection on an untrusted binary within a trusted environment.
If you are using Dynamic Linked Dependency Detection on your own binary, we recommend against running the feature on a binary with its own execution loader.

## How to use

`--detect-dynamic` supports scanning binaries directly, as well as scanning a directory recursively.

### Scanning a binary directly

When running `fossa analyze`, use the `--detect-dynamic <BINARY>` flag.
Point the `<BINARY>` at the binary that is built from the project.

As an example, if the project is located at `~/projects/my-project`,
and results in a binary at `~/projects/my-project/out/project`,
the scan invocation would be as follows:

```shell
fossa analyze ~/projects/my-project --detect-dynamic ~/projects/my-project/out/my-project-bin
```

This results in the FOSSA reporting both the dependencies discovered inside the project directory and the dependencies discovered by analyzing the binary.

### Scanning all binaries in a directory

When running `fossa analyze`, use the `--detect-dynamic <DIR>` flag.
Point the `<DIR>` at the directory containing binaries built from the project.

As an example, if the project is located at `~/projects/my-project`,
and results in binaries output in `~/projects/my-project/out/`,
the scan invocation would be as follows:

```shell
fossa analyze ~/projects/my-project --detect-dynamic ~/projects/my-project/out
```

This results in the FOSSA reporting both the dependencies discovered inside the project directory
and the dependencies discovered by analyzing any binaries in `~/projects/my-project/out` recursively.

#### Filters

When running against a directory, `--detect-dynamic` respects path filters provided to FOSSA CLI (via `.fossa.yml` or command line flags).

This uses the same mechanism as the CLI uses for determining projects to include in the report.

- To learn how path filters work, see [path filtering](../../../contributing/filtering.md#discovery-exclusion-by-path).
- To learn how to set path filters, see [configuring path filters](../../../references/files/fossa-yml.md#paths).

As an example, if the project is located at `~/projects/my-project`,
and results in binaries output in subdirectories of `~/projects/my-project/out/`,
but it should skip `~/projects/my-project/out/docs/`,
the scan invocation would be as follows:

```shell
fossa analyze ~/projects/my-project \
  --detect-dynamic ~/projects/my-project/out \
  --exclude-path ~/projects/my-project/out/docs/
```

This results in the FOSSA reporting both the dependencies discovered inside the project directory
and the dependencies discovered by analyzing any binaries in `~/projects/my-project/out` recursively,
skipping the `~/projects/my-project/out/docs` directory.

### Quick Example

As an example of this functionality, the analysis can run against a common system binary and an empty project directory.
For example, this invocation results in reporting only the dynamic dependencies for the `/bin/ls` system binary:

```shell
mkdir empty
fossa analyze empty --detect-dynamic /bin/ls
```

**Important:** It's possible to use this method to analyze arbitrary binaries. Keep the guidance in the [Security](#security) section in mind when using this feature.
