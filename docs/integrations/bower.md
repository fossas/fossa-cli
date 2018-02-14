# Bower

## Installation

Bower support in FOSSA CLI depends on the following tools existing in your environment:

- Node.js (defaults to `node`, configure with `$NODE_BINARY`)
- Bower (defaults to `bower`, configure with `$BOWER_BINARY`)

## Usage

Add a `bower` module with the path to the `bower.json` in your project.

```yaml
analyze:
  modules:
    - name: your-bower-project
      path: bower.json
      type: bower
```

## Design
### Building

Builds are run using `bower install --production`. If `--force` is set, the build command also runs `rm -rf bower_components` before running the build.

### Analysis

Analysis checks for Bower lockfiles at `glob(**/bower_components/*/.bower.json)`. It reads the `version` key (which is a required key) in these lockfiles to determine the resolved version of your Bower dependencies.

#### Known limitations

- We assume that your Bower packages are installed at `bower_components`. Ideally, we would read this location from your `bower.json`, but this has not yet been implemented.
- We assume that you have installed exactly one version of each of your transitive Bower dependencies. If you have manually edited your local Bower build process, this may not be true. That said, having multiple versions of a Bower dependency is generally indicative of a broken build.
