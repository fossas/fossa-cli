# Carthage

## Support

Carthage support relies on the existence of:
1. The `carthage` binary or `$CARTHAGE_BINARY` to be set.
2. `Cartfile` or `Cartfile.private` for automatic configuration.
3. `Cartfile.resolved` for analysis.

## Configuaration

### Automatic

Run `fossa init` to detect all carthage projects by walking the file tree and searching for either a `Cartfile` or a `Cartfile.private`.

### Manual

Add a module with `type: cart`, and `target` and `dir` set to the directory where `Cartfile.resolved` exists.

```yaml
analyze:
  modules:
    - name: carthage-project
      type: cart
      target: python/project
      dir:  python/project
```

## Analysis

Cartage projects are analyzed by parsing a `Cartfile.resolved` file and massaging the data into a dependency graph.