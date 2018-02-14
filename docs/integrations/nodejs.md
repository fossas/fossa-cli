# Node.js

## Installation

Node.js support in FOSSA CLI depends on the following tools existing in your environment:

- Node.js (defaults to `node`, configure with `$NODE_BINARY`)
- At least one of:
  1. NPM (defaults to `npm`, configure with `$NPM_BINARY`)
  2. Yarn (defaults to `yarn`, configure with `$YARN_BINARY`)

## Usage

Add a `nodejs` module with the path to the `package.json` in your project.

```yaml
analyze:
  modules:
    - name: your-nodejs-project
      path: package.json
      type: nodejs
```

## Design
### Building

If a `yarn.lock` is found, then builds are run using `yarn install --frozen-lockfile`. Otherwise, builds are run using `npm install --production`.

If `--force` is set, the build command also runs `rm -rf node_modules` before running the build.

### Analysis

Analysis checks for Node.js package manifests at `glob(**/node_modules/*/package.json)`. It reads the `version` key (which is a required key) in these manifests to determine the resolved version of your Node dependencies.

#### Known limitations

- We assume that your Node packages are installed at `node_modules`. Ideally, we would read this location from your `$NODE_PATH`, but this has not yet been implemented.