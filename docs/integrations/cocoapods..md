# Cocoapods

## Support

Cocoapods support relies on the presence of 
1. A `Podfile`.
2. `pod` binary or `$COCOAPODS_BINARY` set.

## Configuration

### Automatic

Run `fossa init` to walk the filepath and find all directories that contain a `Podfile`.

### Manual

Add a module with `type: pod` and `target` and `dir` set to the location of the `Podfile.lock`.

```yaml
analyze:
  modules:
    - name: cocoa-repo
      type: pod
      target: .
      dir:  .
```

## Analysis

The `Podfile.lock` is parsed in order to create a dependency graph for the project. The hardest part of this process is massaging the data extracted from `Podfile.lock` into an appropriate dependency graph.

## Known Limitations

Parsing the `Podfile.lock` alone prevents us from being able to analyze targets specified inside of the `Podfile` which limits the ability to distinguish between separate Cocoapod's projects when there are several.