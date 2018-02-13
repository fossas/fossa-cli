# Composer Support

Composer support in FOSSA CLI depends on the following tools existing in your environment:

- PHP
- Composer

## Configuration

Add a `composer` module with the path to the `composer.json` in your project.

```yaml
analyze:
  modules:
    - name: your-composer-project
      path: composer.json
      type: composer
```

If you have an existing passing production build, you can run `fossa` from within the build environment and it should succeed.

Otherwise, you can run `fossa build` to execute the default build command `composer install --no-dev --prefer-dist`.

## Troubleshooting

FOSSA CLI runs and parses the output of `composer show -f json` to generate dependency IDs.  If FOSSA fails, your build or `composer show -f json` might be failing.

Run `composer show -f json` and check the output to diagnose what went wrong.