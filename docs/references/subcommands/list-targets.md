## `fossa list targets`

The list targets command lists all valid analysis targets in a directory. This output can be useful to understand what is going to be analyzed when `fossa analyze` is run. `list-targets` can be also be used with [analysis target configuration](../files/fossa-yml.md#analysis-target-configuration) to limit what is ultimately analyzed.


### Command output

Example output
```bash
$ fossa list-targets
[ INFO] Found target: cabal@./
[ INFO] Found target: cocoapods@project/mobile/
[ INFO] Found target: setuptools@scripts/python/
[ INFO] Found target: yarn@./
```

This output tells us that when `fossa analyze` is run, we will be analyzing `cabal`, `cocoapods`, `pipenv`, and `yarn` projects. This can be useful to determine if there are targets you expect or don't expect to see.

#### Command output formats

The list-targets command supports the following formats (via `fossa list-targets --format=<OPTION>`):

- `legacy` (this is the default format, as shown above in the command output section)
- `text` (renders valid analysis targets per line, in the format of [`fossa analyze --only-target`](./analyze.md#filtering-paths-and-targets) and [`fossa analyze --exclude-target`](./analyze.md#filtering-paths-and-targets))
- `ndjson` (renders valid analysis targets in JSON format separated by line breaks)

### Utilizing [analysis target configuration](../files/fossa-yml.md#analysis-target-configuration)

Analysis target configuration through the fossa configuration file allows users to manually determine what they would like to analyze. Looking at the example above, if we know that the dependencies for the `setuptools@scripts/python/` target are not part of the production release, we can exclude it from the analysis. Example exclusion format:

```yaml
exclude:
  - type: setuptools
    path: scripts/python
```

Adding this section to your configuration file at the root of your project will ensure that when `fossa analyze` is run, the `setuptools` target is skipped.

### F.A.Q.

#### How can I ensure that my project is discoverable before running `fossa analyze` or `fossa test`?

In some scenarios, you may want to set up automation for projects that have been discovered. For example, you might want the process to fail if the project of interest isn't discovered by `fossa` before running `fossa analyze`.

First, execute `fossa list-targets --format=text` to see what `fossa` currently discovers:

```text
yarn@javascript/with-v2-yarn-workspaces/
yarn@javascript/with-v1-yarn/
npm@javascript/with-package-json/
npm@javascript/with-npm-package-lock/
npm@javascript/with-npm-lockfile-v3/
pnpm@javascript/pnpm/
```

From here, we assert that `fossa` finds the npm project at the directory `javascript/with-npm-package-lock/` before running `fossa analyze` using the following in CI:

```bash
if fossa list-targets --format=text | grep -q 'npm@javascript/with-npm-package-lock/' ; then
  fossa analyze && fossa test
else
  exit 1
fi
```
