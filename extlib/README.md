
# about

`extlib` is the home for libraries or library-like binaries that are used in FOSSA CLI but written in a different language (namely, Rust).

# build artifacts

Build libraries with `cargo build` (optionally, use `--release` for release-optimized builds).
From there, the output is found in `target/{release|debug}/{crate_name}`.

Depending on the target:
- If it is a binary crate, it should have no extension or a `.exe` for Windows.
- If it is a library, it should have the appropriate extension for the link type and platform.
