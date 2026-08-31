# Go Binaries (buildinfo)

Go binaries built with module support (Go >= 1.18) embed the list of every
module linked into them. This is the same data `go version -m <binary>` prints.

FOSSA CLI reads that list, so Go code shipped as a compiled binary is reported
even when no `go.mod`, `go.sum`, or Go source is present next to it.

This matters for artifacts such as:

- a gomobile SDK shipping `jni/<abi>/lib<name>.so` inside an AAR
- a Go binary vendored into a repository that is otherwise not a Go project
- a Go binary packaged inside a JAR or other archive

The embedded module list is generally more accurate than a hand-maintained
third-party notice file, because the linker writes it from what was actually
built into the binary.

## Enabling

This strategy is opt-in. `fossa analyze` otherwise reports only what package
managers declare, and reading binaries would add dependencies to existing
projects without the user asking for them.

```bash
fossa analyze --enable-go-binary-analysis
```

To reach a binary nested inside an archive, pass `--unpack-archives` as well.
The two flags are independent - neither implies the other:

```bash
fossa analyze --enable-go-binary-analysis --unpack-archives
```

## Project Discovery

Walk the scan directory and sniff each file for an embedded buildinfo section.
A file is reported only if buildinfo is found and it yields at least one
usable module version.

Binaries nested inside archives are found when `--unpack-archives` is passed:
discovery runs again over the extracted contents, so a binary inside an AAR or
JAR is reached the same way a manifest inside one would be.

Only ELF, Mach-O, and PE files at least 4 KiB in size are examined, so the
walk is cheap on repositories that contain unrelated binary files.

All Go binaries found in one directory are reported as a single project, because
a source unit is named after its directory. Each contributing binary appears as
an origin path, and their module lists are combined.

Default path filters still apply: a binary under `vendor/` is skipped unless you
pass `--include-path vendor`.

## Analysis

The module list is read directly out of the binary; no Go toolchain is invoked
and nothing is executed. Every module found is reported as a direct `go`
dependency.

Versions are normalized the same way `go.mod` analysis normalizes them:
pseudo-versions are reduced to their commit hash, and semantic versions keep
their `v` prefix.

The main module is skipped when it is unversioned (the linker records `(devel)`
for a locally built binary), and reported when it carries a real version, which
happens for binaries built via `go install <module>@<version>`.

## Limitations

- Binaries built by Go < 1.18 use an older pointer-based buildinfo encoding and
  are skipped.
- Binaries built without module support (`GOPATH` mode, or `CGO`-only objects)
  carry no module list.
- Buildinfo records modules, not the dependency edges between them, so the
  resulting graph is flat. The set of modules is complete.
- Stripping a binary does not remove buildinfo, but rewriting or packing it
  (for example with UPX) can.

## FAQ

### How do I only perform analysis for Go binaries?

Pass `--only-target gobinary` alongside the enabling flag:

```bash
fossa analyze --enable-go-binary-analysis --only-target gobinary
```

`--only-target gobinary` on its own reports nothing, because the strategy is
still disabled.

### How do I inspect the same data by hand?

```bash
go version -m path/to/binary
```
