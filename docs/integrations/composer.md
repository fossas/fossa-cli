# Composer

## Support

Composer support in FOSSA CLI depends on the following tools existing in your environment:

- PHP (defaults to `php`, configure with `$PHP_BINARY`)
- Composer (defaults to `composer`, configure with `$COMPOSER_BINARY`)

## Configuration

Automatic: Run `fossa init` to detect all instances of a `composer.json` file signaling the existence of a composer project located in your file tree.

Manual: Add a `composer` module with `dir` set to the directory where a `composer.json` file is located and `target` to the full path of the file.

```yaml
analyze:
  modules:
    - name: your-composer-project
      type: composer
      dir: composer.json
      target: directory/composer.json
```

## Design

### Analysis

Analysis parses the output of `composer show --tree` and turns it into a dependency graph.