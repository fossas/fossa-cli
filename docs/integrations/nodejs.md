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

## Options

| Option     |  Type  | Name                         | Common Use Case                     |
| ---------- | :----: | ---------------------------- | ----------------------------------- |
| `strategy` | string | [Strategy](#strategy-string) | Specify a nodejs analysis strategy. |

#### `strategy: <string>`

Manually specify the nodejs analysis strategy to be used. Supported options are as follows and the individual behavior is listed in the Analysis section further down:
- `npm-list`
- `npm-lockfile`
- `package.json`
- `yarn.lock`
- `yarn-list`

## Analysis

Analysis for nodejs projects is executed a number of ways starting with the most accurate method and falling back to the least likely method to succeed as ordered:
1. Parse output from `npm ls --json --production` - Runs if `npm` exists on the system and provides an accurate list of all dependencies needed to build the production project.
1. Parse `package.json` - Runs if `package.json` can be successfully parsed into a dependency graph.
1. Run `yarn list --json` - This command verifies through `yarn` what the actual dependencies which are installed on the system are. This strategy runs with `NODE_ENV=production` by default to find production dependencies.
1. Parse `yarn.lock` - Detects dependencies based on the yarn lockfile.
1. Parse `npm-shrinkwrap.json` - Detects dependencies based on the lockfile.
1. Parse `package-lock.json` - Detects dependencies based on the lockfile.

## Known limitations

- We assume that your Node packages are installed at `node_modules`. Currently we do not offer a way to read this directory to determine what packages are installed.
