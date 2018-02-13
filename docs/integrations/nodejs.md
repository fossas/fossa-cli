# Node.js Support

Node.js support in FOSSA CLI depends on the following tools existing in your environment:

- Node.js
- Either Yarn or NPM

## Configuration

Add a `nodejs` module with the path to the `package.json` in your project.

```yaml
analyze:
  modules:
    - name: your-nodejs-project
      path: package.json
      type: nodejs
```

If you have an existing passing production build, you can run `fossa` from within the build environment and it should succeed.

Otherwise, you can run `fossa build` to execute the default build command. If a Yarn lockfile is detected, FOSSA will run `yarn install --production --frozen-lockfile`. Otherwise, it will run `npm install --production`.

## Troubleshooting

FOSSA CLI parses the package manifests in `node_modules` to generate dependency IDs.  If FOSSA fails, your build might be failing.