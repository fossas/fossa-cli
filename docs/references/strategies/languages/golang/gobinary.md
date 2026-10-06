# Go Binaries

Go binaries built with module support contain build information describing the modules linked into the binary. This is the same information shown by:

```bash
go version -m <binary>
```

FOSSA CLI can read this metadata directly, allowing it to identify Go dependencies even when the binary is distributed without its `go.mod`, `go.sum`, or source code.

Examples include:

- a gomobile SDK containing `jni/<abi>/lib<name>.so` inside an AAR
- a Go binary vendored into a non-Go repository
- a Go binary packaged inside a JAR or other archive

Because this metadata is written by the Go linker at build time, it reflects the modules present in the compiled artifact rather than a separately maintained dependency list.

## Enabling

Go binary analysis is opt-in:

```bash
fossa analyze --enable-go-binary-analysis
```

To scan binaries contained in archives, also enable archive unpacking:

```bash
fossa analyze --enable-go-binary-analysis --unpack-archives
```

The flags are independent. `--unpack-archives` does not enable Go binary analysis, and enabling Go binary analysis does not unpack archives.

## Project Discovery

FOSSA walks the scan directory and checks eligible binaries for Go build information. A file is reported only when build information is present and contains at least one usable module version.

When `--unpack-archives` is enabled, extracted archive contents are scanned as well. This allows Go binaries inside AARs, JARs, and other supported archives to be discovered.

Only ELF, Mach-O, and PE files of at least 4 KiB are examined.

Go binaries in the same directory are grouped into one project. Each binary is retained as an origin path, and their module lists are combined.

Normal path filtering still applies. For example, binaries under `vendor/` are skipped unless `--without-default-filters` is used.

## Analysis

FOSSA reads the module list directly from the binary. It does not invoke the Go toolchain or execute the binary.

Each module is reported as a direct `go+` dependency.

Versions use the same normalization as `go.mod` analysis:

- pseudo-versions are reduced to their commit hash
- semantic versions retain their `v` prefix

The main module is omitted when its version is `(devel)`, as is typical for locally built binaries. If the binary contains a real main-module version, such as one produced by `go install <module>@<version>`, it is reported.

## Limitations

- Go binaries built before Go 1.18 use an older pointer-based buildinfo encoding and are not supported.
- Binaries built without module information, such as GOPATH-mode binaries or CGO-only objects, do not contain a module list.
- Build information records modules but not dependency edges, so the resulting graph is flat.
- Stripping a binary does not normally remove build information, but binary rewriting or packing tools such as UPX may make it unavailable.

## FAQ

### How do I analyze only Go binaries?

Use `--only-target gobinary` together with the enabling flag:

```bash
fossa analyze --enable-go-binary-analysis --only-target gobinary
```

`--only-target gobinary` does not enable Go binary analysis by itself.

### How do I inspect the embedded module information manually?

Use:

```bash
go version -m path/to/binary
```
