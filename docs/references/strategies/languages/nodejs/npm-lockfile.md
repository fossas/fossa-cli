# Npm Lockfile

The `package-lock.json` file is generated when npm modifies `node_modules` or
`package.json` and describes the exact dependency tree generated. One example of
this is when `npm install` is run.

> Note: In old versions of npm, `package-lock.json` was only modified when
> dependencies were installed.

## Project Discovery

Search for files named `package.json` and check for a corresponding
`package-lock.json` in the same directory, ignoring directories named
`node_modules`.

> Note: When using NPM workspaces, only the root of the project will have a
> `package-lock.json`. The other `package.json` files in the project directory
> will be combined to determine which dependencies are direct and which ones are
> development.

## Analysis (for lockFile version 3)

We consider `package-lock.json` to be version 3 compatible, if and only if, 

- `lockFileVersion` field exists, and is of value 3
- `packages` field exists in the `package-lock.json`.

In `package-lock.json`, `packages` object's key refer to filepath for associated `package.json`.

Generally, we have three types of data shape within `packages` map.

1) Root module (denoted by having `""` key)

```json
  "": {
      "name": "packageLockV3",
      "version": "1.0.0",
      "license": "ISC",
      "dependencies": {
          "foo": "^1.0.0",
          "boo": "^1.0.0"
      }
  }
```

2) Workspace module

```json
  "packages/a": {
      "name": "packageLockV3PkgA",
      "version": "2.0.0",
      "license": "ISC",
      "dependencies": {
          "foo": "^2.0.0"
      }
  }
```

Workspace module, in essence is type of root module whose parent is top level root module (as shown in (1)).

3) Package Module (these are the dependencies which fossa will report)

```json
  "node_modules/foo": {
      "version": "1.2.0",
      "resolved": "https://registry.npmjs.org/foo/-/foo-1.2.0.tgz"
  },
  "node_modules/bar": {
      "version": "1.3.0",
      "resolved": "https://registry.npmjs.org/bar/-/bar-1.3.0.tgz",
      "dependencies": {
          "baz": "^1.0.0"
      }
  },
  "node_modules/baz": {
      "version": "1.9.0",
      "resolved": "https://registry.npmjs.org/baz/-/baz-1.9.0.tgz"
  },
  "packages/a/node_modules/foo": {
      "version": "2.0.0",
      "resolved": "https://registry.npmjs.org/foo/-/foo-2.0.0.tgz"
  }
```

We will attempt infer the resolved version by looking at package's key. 

For instance, to infer resolved version of `foo@^2.0.0` for `packages/a`,
we will attempt to see if the dependency was resolved at vendored path, 
or at top level path. 

If we find, `packages/a/node_modules/foo` key in our packages object, we use it's
resolved version. If this key does not exist we fallback to `node_modules/foo`.

With this approach, for aforementioned example, we will generate following dependency tree:

```text
-- foo@2.0.0      (via workspace package a)
-- foo@1.2.0      (via root package.json)
-- bar@1.3.0      (via root package.json)
    \- baz@1.9.0  (transitive dep via bar@1.3.0)
```

We use `dev` field to infer if the dependency is development dependency or not.

We analyze `optional` dependencies. If you would like to ignore them, you can do
so from [FOSSA UI](https://docs.fossa.com/docs/generating-reports#modifying-report-information).

## Analysis (for lockFile version 1)

Opening a `package-lock.json` file reveals the project's dependency tree. This
dependency tree contains information about a dependency's version, its
transitive dependencies, the URL where the dependency is located at, and whether
or not the dependency is used as a development dependency or not. The transitive
dependency information is listed in an unintuitive way. Under each dependency
there may be two fields, `requires` and `dependencies` as in the following
example:

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

The `requires` field signifies all of the dependencies that are needed by the
dependency in order to properly function.

The `dependencies` field signifies all of the dependencies included in
`babel-code-frame`'s `node_modules` folder within the top level `node_modules`
folder. Notice that these dependencies are not always included in the `requires`
section.

> Note: `npm-shrinkwrap.json` is an identically formatted file that can be used
> for [publishing packages](https://docs.npmjs.com/cli/shrinkwrap).

### Peer Dependencies

Top-level peer dependencies from `package.json` are treated as if they were
direct dependencies of the project. Peer dependencies of transitive deps are
also treated as transitive dependencies. If a transitive peer dependency also
happens to be a direct dependency then it is reported as direct rather than
transitive, however. The reason FOSSA does this is because as of `npm` 7+ peer
dependencies and transitive peer dependencies are installed by default.

`fossa-cli` doesn't report peer dependencies in a special way because FOSSA does
not model peer dependencies natively. Because direct peer dependencies are
defined in the same place as regular direct dependencies (`package.json`) and
because NPM installs them by default for FOSSA's purposes they are considered
direct. If FOSSA didn't report these dependencies users may potentially miss
license information about dependencies that would have implications for the
distribution of their projects.

Transitive peer dependencies are found in the `packages` key of
`package-lock.json` files produced by recent versions of `npm`.  The `packages`
key holds similar information to the those under the `dependencies` key, but
`packages` includes information about peer dependencies for each dependency or
transitive dependency the project uses. For example, an entry might look like
this:

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

In this example, when `fossa-cli` does analysis of `chai-dom`, it will include
`chai` and `mocha` as dependencies`chai-dom`. The transitive deps for `chai` and
`mocha` will also be captured.
