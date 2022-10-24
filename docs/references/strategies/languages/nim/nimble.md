# Nimble Analysis

When developing in [nim](https://nim-lang.org/), nimble is used to manage dependencies.

| Strategy                      | Direct Deps        | Transitive Deps    | Edges              | Classifies Dev Dependencies | Container Scanning |
| ----------------------------- | ------------------ | ------------------ | ------------------ | --------------------------- | ------------------ |
| nimble.lock and `nimble dump` | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x:                         | :x:                |
| nimble.lock                   | :warning:          | :white_check_mark: | :white_check_mark: | :x:                         | :white_check_mark: |

## Project Discovery

Find a file named `nimble.lock`.

## Analysis

1. Parse `nimble.lock` to identify dependencies and edges among them.
2. Perform `nimble dump --json` to identify direct dependencies.

Limitation:
- Dependencies downloaded from mercurial are not reported.
- When `nimble dump --json` fails, any dependencies without incoming edge is considered to be a direct dependency

## Example 

1. Execute `nimble init` to create a new project or create `nim.nimble` manually:

Example `nim.nimble`:
```nim
# Package

version       = "0.1.0"
author        = "User"
description   = "A new awesome nimble package"
license       = "MIT"
srcDir        = "src"
installExt    = @["nim"]
bin           = @["nim"]


# Dependencies

requires "nim >= 1.6.0"
requires "jester >= 0.4.1 & < 0.5.0"
```

3. Execute `nimble lock` to install and pin dependencies - this will create (or modify) the `nimble.lock` file.
4. Execute `fossa analyze -o` on the project to print analyzed dependency graphing (this will not upload any analysis to any endpoint)

>> If you are using an on older version of Nim, you may need to do following:
>>  Perform: `choosenim 1.6.0`
>>  Perform: `nimble install https://github.com/nim-lang/nimble@\#head` (get latest nimble or any version after lock file support!)

## FAQ

### How do I *only perform analysis* for the nimble?

You can explicitly specify an analysis target in `.fossa.yml` file. The example below will exclude all analysis targets except for the composer. 

```yaml
# .fossa.yml 

version: 3
targets:
  only:
    - type: nimble
```