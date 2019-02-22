# Bower

## Installation

Bower support in FOSSA CLI depends on the following tools existing in your environment:

- Node.js (defaults to `node`, configure with `$NODE_BINARY`)
- Bower (defaults to `bower`, configure with `$BOWER_BINARY`)

## Configuration

Automatic: Run `fossa init` to detect all directories with a `bower.json` file at their roots and create a module for each.

Manual: Add a module with `type` set to `bower`, `dir` set to the directory where `bower.json` was found, and `target` .

```yaml
analyze:
  modules:
    - name: your-bower-project
      type: bower
      path: bower.json
      target: configuration/bower.json
```

## Analysis

fossa runs `bower list --json` and parses the output to create a dependency tree.

## Known limitations

- We assume that your Bower packages are installed at `bower_components`. Ideally, we would read this location from your `bower.json`, but this has not yet been implemented.
- We assume that you have installed exactly one version of each of your transitive Bower dependencies. If you have manually edited your local Bower build process, this may not be true. That said, having multiple versions of a Bower dependency is generally indicative of a broken build.
