# Ant / Ivy

## Support

Apache Ant / Ivy support in FOSSA CLI depends on the following tools existing in your environment:

- Java (defaults to `java`, configure with `$JAVA_BINARY`)
- Any (defaults to `ant`, configure with `$ANT_BINARY`)

## Configuration

### Automatic

Running `fossa init` will search for ant modules by traversing the file tree and checking for two conditions:
1. A valid `build.xml` file.
2. The existence of a `lib/` directory where jars are stored.

If these conditions are satisfied, a new module will be added to the configuration file.

### Manual
In order to manually add an Ant module follow the blueprint below:

```yaml
analyze:
  modules:
    - name: {module}
      type: ant
      path: {path-to-build.xml}
      target: {path-to-build.xml}
```

If you have a complex Ant build that stores libraries in multiple locations, you can either specify multiple modules with custom `libdir` locations or upload raw dependency signatures directly to FOSSA. This can be done using the `libdir` option.

## Options
| Option   |  Type  | Name                        | Common Use Case                            |
| -------- | :----: | --------------------------- | ------------------------------------------ |
| `libdir` | string | [LibDir](#libdir:-<string>) | Specify the path to the ant lib directory. |


#### `libdir: <string>` 
`libdir` refers to the directory where all the JAR libraries used in a production build are stored.  These directories can be difficult to automatically infer without running a build, you will often have to specify them yourself in configuration.

Default: `lib`

## Design

### Analysis
Analysis starts from the specified `libdir` and collects all `.jar` files. Dependency locators containing the project name and revision are created for each jar using the following process:
1. Attempt to find a pom file within the jar to obtain dependency information.
2. Search for a Manifest file within `META-INF` to obtain dependency information.
3. Fallback to using the name of the `jar` to obtain dependency information.

Once dependency information is obtained, the list will be compiled into a complete dependency graph corresponding to the ant module. As fossa moves from step 1 to step 3, jars become increasingly difficult to resolve into known dependencies.