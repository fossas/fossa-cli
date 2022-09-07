# Composer Analysis

When developing in PHP, [composer](https://getcomposer.org/) is commonly used to manage dependencies.

| Strategy      | Direct Deps        | Deep Deps          | Edges              | Classifies Dev Dependencies | Container Scanning (experimental) |
| ------------- | ------------------ | ------------------ | ------------------ | --------------------------- | --------------------------------- |
| composer.lock | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark:          | :white_check_mark:                |

## Project Discovery

Find a file named `composer.lock`.

## Analysis

1. Parse `composer.lock` to identify direct and deep dependencies.

## Example 

1. Execute `composer init` to create a new project or create `composer.json` manually:

Example composer.json:
```json
{
    "name": "fossa/php-project",
    "description": "example php project",
    "require": {
        "michelf/php-markdown": "^1.9"
    },
    "require-dev": {
        "webmozart/assert": "^1.10"
    },
    "authors": [
        {
            "name": "Megh",
            "email": "megh@fossa.com"
        }
    ]
}
```

3. Execute `composer update` to install and pin dependencies - this will create (or modify) the `composer.lock` file.
4. Execute `fossa analyze -o` on the project to print analyzed dependency graphing (this will not upload any analysis to any endpoint)

## FAQ

### How do I *only perform analysis* for the composer?

You can explicitly specify an analysis target in `.fossa.yml` file. The example below will exclude all analysis targets except for the composer. 

```yaml
# .fossa.yml 

version: 3
targets:
  only:
    - type: composer
```