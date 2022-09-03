# Perl Analysis

| Strategy           | Direct Deps        | Deep Deps          | Edges | Classifies Dev Dependencies |
| ------------------ | ------------------ | ------------------ | ----- | --------------------------- |
| `*META.{yml, json} | :white_check_mark: | :white_check_mark: | :x:   | :white_check_mark:          |

## Project Discovery

Find a file named `MYMETA.json`, `MYMETA.yml`, `META.json`, or `META.yml`.

## Analysis

1. Parse `MYMETA.{yml, json}` or `META.{yml, json}` to identify dependencies.

## Limitation

- Dependency required for `runtime` only will be reported.
- Reported analysis will not have any edges.

## Example 

1. Build your perl target. When you do this, you should have `MYMETA.yml` and `MYMETA.json`.
2. Execute `fossa analyze -o` on the project to print analyzed dependency graphing (this will not upload any analysis to any endpoint)

## FAQ

### How do I *only perform analysis* for Perl?

Explicitly specify an analysis target in `.fossa.yml` file. The example below excludes all other analysis targets:

```yaml
# .fossa.yml 

version: 3
targets:
  only:
    - type: perl
```
