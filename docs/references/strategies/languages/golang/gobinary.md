# Go Binaries

Go binaries built with module support include build information listing the modules linked into them. This is the same data shown by:

```bash
go version -m <binary>
```

FOSSA can read that metadata directly, so it can report Go dependencies even when the binary is shipped without a `go.mod`, `go.sum`, or Go source.

This is useful for things like:

- gomobile SDKs that ship `jni/<abi>/lib<name>.so` inside an AAR
- Go binaries vendored into otherwise non-Go repositories
- Go binaries packaged inside JARs or other archives

Because the module list comes from the binary itself, it usually reflects what was actually built more accurately than a separately maintained third-party notice file.

## Enabling

Go binary analysis is opt-in:

```bash
fossa analyze --enable-go-binary-analysis
```

If the binary is inside an archive, also pass `--unpack-archives`:

```bash
fossa analyze --enable-go-binary-analysis --unpack-archives
```

`--unpack-archives` and `--enable-go-binary-analysis` are independent flags.

## Project Discovery

FOSSA walks the scan directory and checks eligible files for Go build information. A file is only reported if buildinfo is present and contains at least one module with a usable version.

With `--unpack-archives`, FOSSA also checks extracted archive contents, including binaries inside AARs and JARs.

Only ELF, Mach-O, and PE files of at least 4 KiB are checked.

Go binaries in the same directory are grouped into one project. Their module lists are combined, and each binary is recorded as an origin path.

Normal path filters still apply. For example, binaries under `vendor/` are skipped unless you pass `--without-default-filters`.

## Analysis

FOSSA reads the module list directly from the binary. It does not run the binary or invoke the Go toolchain.

Each module is reported as a direct `go+` dependency.

Versions are normalized the same way as `go.mod` dependencies:

- pseudo-versions are reduced to their commit hash
- semantic versions keep their `v` prefix

The main module is skipped when its version is `(devel)`, which is typical for locally built binaries.

If the binary includes a real main-module version, such as one built with:

```bash
go install <module>@<version>
```

that module is reported as well.

## Limitations

- Go binaries built with Go versions before 1.18 use an older pointer-based buildinfo format and are skipped.
- Binaries built without module information, including GOPATH-mode binaries and CGO-only objects, do not contain a module list.
- Buildinfo contains the module set, but not dependency edges, so the resulting graph is flat.
- Stripping a binary does not normally remove buildinfo, but rewriting or packing the binary, for example with UPX, can make it unavailable.

## FAQ

### How do I only analyze Go binaries?

Use `--only-target gobinary` with the enabling flag:

```bash
fossa analyze --enable-go-binary-analysis --only-target gobinary
```

`--only-target gobinary` does not enable Go binary analysis on its own.

### How do I inspect the same data manually?

```bash
go version -m path/to/binary
```
