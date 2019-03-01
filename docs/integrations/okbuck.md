# [OkBuck](#https://github.com/uber/okbuck#okbuck)

## Support

OkBuck support relies on the presence of a buck wrapper, `./buckw`, to be present in an okbuck repository.

## Configuration

### Automatic

Run `fossa init` to detect if a `buckw` binary exists in the present directory.

### Manual

Add a module with `type: okbuck`, `target` set to a valid buck target, and `dir` set to the root of the project.

```yaml
analyze:
  modules:
    - name: okbuck
      type: okbuck
      target: //...
      dir:   .
      options:
            classpath: //app:bin_prodRelease
```

## Options

| Option      |  Type  | Name                              | Common Use Case                                      |
| ----------- | :----: | --------------------------------- | ---------------------------------------------------- |
| `classpath` | string | [Class Path](#classpath:-<string>) | Retrieve the dependencies from a specific classpath. |

#### `classpath: <striong>`

Specifying a classpath ensures that only dependencies related to the specified classpath are returned. In order to do this, FOSSA must modify the normal analysis to:
1. Run `./buckw audit classpath <target> ` to find jar's used by the classpath.
2. Map the jar's to the dependency information from `./buckw targets //...`.
3. Return the corresponding Maven coordinates.

## Analysis

Analysis of an okbuck project leverages the existence of maven coordinates in the build information. By default, FOSSA will run `buck targets //... --json` and parse the Maven coordinates from each target into a dependency.