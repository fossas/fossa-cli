# Ant / Ivy

## Installation

Apache Ant / Ivy support in FOSSA CLI depends on the following tools existing in your environment:

- Java (defaults to `java`, configure with `$JAVA_BINARY`)
- Any (defaults to `ant`, configure with `$ANT_BINARY`)

## Usage

### Configuration

First, run `fossa init` to automatically generate a `.fossa.yml` file a pre-configured `ant` module:

```yaml
analyze:
  modules:
    - name: {module}
      path: {path-to-build.xml}
      type: ant
```

Next, you will have to configure your library directory using the `options.libdir` property.

`libdir` refers to the directory where you store all the JAR libraries used in a production build.  Since these directories are difficult to automatically infer without running a build, you will often have to specify them yourself in configuration:

```yaml
  - name: ant-example
    path: build.xml
    type: ant
    options:
      libdir: ./project/jars
```

If unspecified, the `libdir` will default to `lib/`.

If you have a complex Ant build that stores libraries in multiple locations, you can either specify multiple modules or upload raw dependency signatures directly to FOSSA.