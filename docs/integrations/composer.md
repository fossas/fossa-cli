# Composer

## Installation

Composer support in FOSSA CLI depends on the following tools existing in your environment:

- PHP (defaults to `php`, configure with `$PHP_BINARY`)
- Composer (defaults to `composer`, configure with `$COMPOSER_BINARY`)

## Usage

Add a `composer` module with the path to the `composer.json` in your project.

```yaml
analyze:
  modules:
    - name: your-composer-project
      path: composer.json
      type: composer
```

## Design
### Building

Builds are run using `composer install --prefer-dist --no-dev`. If `--force` is set, the build command also runs `rm -rf vendor` before running the build.

### Analysis

Analysis parses the output of `composer show -f json --no-ansi`.
