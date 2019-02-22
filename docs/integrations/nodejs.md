# Node.js

## Support

Node.js support in FOSSA CLI depends on the following tools existing in your environment:

- Node.js (defaults to `node`, configure with `$FOSSA_NODE_CMD`)
- NPM (defaults to `npm`, configure with `$NPM_BINARY`)

## Configuration
   
Automatic: Run `fossa init` to detect all `package.json` files in the file tree not located inside of a `node_modules` folder.

Manual: Add a `nodejs` module with path and target set to the directory where the `package.json` file is located in your project.

```yaml
analyze:
  modules:
    - name: your-nodejs-project
      type: nodejs
      path: .
      target: .
```

## Analysis

Analysis for nodejs projects is executed a number of ways starting with the most accurate method and falling back to the least likely method to succeed as ordered:
1. Parse output from `npm ls --json --production` - Runs if `npm` exists on the system and provides an accurate list of all dependencies needed to build the production project.
2. Parse `package.json` - Runs if `package.json` can be successfully parsed into a dependency graph.
3. Parse `yarn.lock` - Final strategy which detects dependencies based on the yarn lockfile.

## Known limitations

- We assume that your Node packages are installed at `node_modules`. Currently we do not offer a way to read this directory to determine what packages are installed.