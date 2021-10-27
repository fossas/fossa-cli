# Quick reference: cargo

## Requirements

**Ideal/Minimum**

- `cargo` buildtool installed on your machine
- `Cargo.toml` file present in your project

## Project discovery

Directories containing `Cargo.toml` files are considered cargo projects.

Subdirectories of cargo projects are not re-scanned for nested cargo projects, because the dependency tree of the parent project already includes the dependency tree of those subprojects.  To scan only the subprojects, run the `fossa` tool against the subproject directories
