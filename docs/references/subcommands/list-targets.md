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

### Utilizing [analysis target configuration](../files/fossa-yml.md#analysis-target-configuration)

Analysis target configuration through the fossa configuration file allows users to manually determine what they would like to analyze. Looking at the example above, if we know that the dependencies for the `setuptools@scripts/python/` target are not part of the production release, we can exclude it from the analysis. Example exclusion format:

```yaml
exclude:
  - type: setuptools
    path: scripts/python
```

Adding this section to your configuration file at the root of your project will ensure that when `fossa analyze` is run, the `setuptools` target is skipped.
