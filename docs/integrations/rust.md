# Rust

## Support

Rust support relies on the presence of the `Cargo.lock` and `Cargo.toml` files.

## Configuration

### Automatic

Run `fossa init` to detect all directories that contain `Cargo.lock`.

### Manual

Add a module with `type: cargo`, and `target` and `dir` set to the directory where `Cargo.lock` is.

```yaml
analyze:
  modules:
    - name: cargo-project
      type: cargo
      target: rust/root
      dir:  rust/root
```

## Analysis

Rust analysis is a simple three step process:

1. Parse `Cargo.lock` and retrieve information about the full dependency tree and each dependency's resolved version.
2. Read the [root crate](https://doc.rust-lang.org/cargo/reference/manifest.html#the-workspace-section)'s `Cargo.toml` and detect if any "member" crates exist.
3. Parse all `Cargo.toml` files to retrieve information about which dependencies are declared.