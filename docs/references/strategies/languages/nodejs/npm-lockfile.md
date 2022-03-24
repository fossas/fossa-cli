# Npm Lockfile

The `package-lock.json` file is generated when npm modifies `node_modules` or `package.json` and describes the exact dependency tree generated. One example of this is when `npm install` is run.

> Note: In old versions of npm, `package-lock.json` was only modified when dependencies were installed.

## Project Discovery

Search for files named `package.json` and check for a corresponding `package-lock.json` in the same directory, ignoring directories named `node_modules`.

> Note: When using NPM workspaces, only the root of the project will have a `package-lock.json`. The other `package.json` files in the project directory will be combined to determine which dependencies are direct and which ones are development.

## Analysis

Opening a `package-lock.json` file reveals the project's dependency tree. This dependency tree contains information about a dependency's version, its transitive dependencies, the URL where the dependency is located at, and whether or not the dependency is used as a development dependency or not. The transitive dependency information is listed in an unintuitive way. Under each dependency there may be two fields, `requires` and `dependencies` as in the following example:

```json
    "babel-code-frame": {
      "version": "6.26.0",
      "resolved": "https://registry.npmjs.org/babel-code-frame/-/babel-code-frame-6.26.0.tgz",
      "integrity": "sha1-Y/1D99weO7fONZR9uP42mj9Yx0s=",
      "requires": {
        "chalk": "1.1.3",
        "esutils": "2.0.3",
        "js-tokens": "3.0.2"
      },
      "dependencies": {
        "ansi-styles": {
          "version": "2.2.1",
          "resolved": "https://registry.npmjs.org/ansi-styles/-/ansi-styles-2.2.1.tgz",
          "integrity": "sha1-tDLdM1i2NM914eRmQ2gkBTPB3b4="
        },
        "js-tokens": {
          "version": "3.0.2",
          "resolved": "https://registry.npmjs.org/js-tokens/-/js-tokens-3.0.2.tgz",
          "integrity": "sha1-mGbfOVECEw449/mWvOtlRDIJwls="
        }
      }
    }
```

The `requires` field signifies all of the dependencies that are needed by the dependency in order to properly function.

The `dependencies` field signifies all of the dependencies included in `babel-code-frame`'s `node_modules` folder within the top level `node_modules` folder. Notice that these dependencies are not always included in the `requires` section.

> Note: `npm-shrinkwrap.json` is an identically formatted file that can be used for [publishing packages](https://docs.npmjs.com/cli/shrinkwrap).

### Peer Dependencies

Top-level peer dependencies from `package.json` are treated as if they were direct dependencies of the project. Peer dependencies are for transitive deps are also treated as transitive dependencies.

In newer version of NPM the `package-lock.json` file may also include a `packagages` key which holds similar information to the those under the `dependencies` key. `packages` includes information about
peer dependencies for each dependency or transitive dependency the project uses. For example, an entry might look like this:

```json
    "node_modules/chai-dom": {
        "version": "1.11.0",
        "resolved": "https://registry.npmjs.org/chai-dom/-/chai-dom-1.11.0.tgz",
        "integrity": "sha512-ZzGlEfk1UhHH5+N0t9bDqstOxPEXmn3EyXvtsok5rfXVDOFDJbHVy12rED6ZwkJAUDs2w7/Da4Hlq2LB63kltg==",
        "peer": true,
        "engines": {
            "node": ">= 0.12.0"
        },
        "peerDependencies": {
            "chai": ">= 3",
            "mocha": ">= 2"
        }
    }
```

When `fossa-cli` does analysis of `chai-dom`, it will include `chai` and `mocha` as dependencies`chai-dom`. The transitive deps for `chai` and `mocha` will also be captured.
