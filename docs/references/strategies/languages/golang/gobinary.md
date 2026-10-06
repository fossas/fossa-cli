# Go Binaries

Go binaries built with module support include a list of the modules linked into the binary. This is the same data printed by:

```bash
go version -m <binary>
```

FOSSA can read this data directly, so Go dependencies can be found even when there is no `go.mod`, `go.sum`, or Go source next to the binary.

This includes Go binaries shipped in artifacts such as:

- gomobile SDKs containing `jni/<abi>/lib<name>.so` inside an AAR
- Go binaries vendored into repositories that are otherwise not Go projects
- Go binaries packaged inside JARs or other archives

Since the module list is written into the binary at build time, it represents the modules used to build that binary.

## Enabling

Go binary analysis is opt-in:

```bash
fossa analyze --enable-go-binary-analysis
```

If the binary is inside an archive, also pass `--unpack-archives`:

```bash
fossa analyze --enable-go-binary-analysis --unpack-archives
```

The two flags are independent.

## Project Discovery

We walk the scan directory and check ELF, Mach-O, and PE files of at least 4 KiB for Go buildinfo. A file is only reported if buildinfo is present and contains at least one module with a version we can use.

When `--unpack-archives` is enabled, extracted files are checked as well. This includes binaries inside AARs, JARs, and other supported archives.

Go binaries in the same directory are grouped into one project. Their module lists are combined, and each binary is included as an origin path.

Default path filters still apply. For example, binaries under `vendor/` are skipped unless `--without-default-filters` is passed.

## Analysis

We read the module list directly from the binary. The Go toolchain is not invoked, and the binary is not executed.

Each module is reported as a direct `go+` dependency.

Versions are normalized in the same way as `go.mod` dependencies:

- pseudo-versions are reduced to their commit hash
- semantic versions keep their `v` prefix

The main module is skipped when its version is `(devel)`, which is what the linker normally records for a locally built binary.

If the main module has a real version, such as a binary built with:

```bash
go install <module>@<version>
```

it is reported as well.

## Limitations

- Go binaries built with Go versions before 1.18 use an older pointer-based buildinfo encoding and are skipped.
- Binaries built without module information, including GOPATH-mode binaries and CGO-only objects, do not contain a module list.
- Buildinfo contains modules but not the dependency edges between them, so the resulting graph is flat.
- Stripping a binary does not remove buildinfo, but rewriting or packing the binary, for example with UPX, can.

## FAQ

### How do I only analyze Go binaries?

Pass `--only-target gobinary` with the enabling flag:

```bash
fossa analyze --enable-go-binary-analysis --only-target gobinary
```

`--only-target gobinary` does not enable Go binary analysis on its own.

### How do I inspect the same data manually?

```bash
go version -m path/to/binary
```
