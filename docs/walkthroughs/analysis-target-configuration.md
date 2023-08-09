# Analyze only a set of targets

FOSSA CLI can be configured to discover and analyze, based on the target type (e.g. gradle, rebar3, etc.) and by its path. This can be useful when multiple targets exist in the directory, but we are only interested in a select few. 

To do so, we will use the following:

- [fossa list-targets](./../references/subcommands/list-targets.md) command
- [.fossa.yml](./../references/files/fossa-yml.md) configuration file

## Example

For an example scenario, presume our source code is structured in the following manner (simplified for brevity):

```bash
.
├── src
│   ├── back-end
│   │   └── pyproject.toml
│   └── front-end
│       ├── v1
│       │   └── package.json
│       └── v2
│           └── package.json
├── test-suite
│   ├── browser
│   │   ├── package.json
│   │   └── yarn.lock
│   └── integration
│       └── build.gradle
└── utils
    ├── helpers
    │   └── requirements.txt
    ├── migration-tests
    │   └── build.gradle
    ├── requirements.txt
    └── scripts
        └── requirements.txt
```

And we are only interested in analyzing:

- Any targets under `src/back-end/` and `src/front-end/v2/` directory
- Any targets under `utils/` directory, 
  - But excluding only setuptools targets found in `utils/scripts` directory. 
  - But excluding any targets under `utils/migration-tests` directory

To identify, target and its path discovered by fossa CLI, we can use: `fossa list-targets` command. 

When command is executed, it would produce a list of target and their path:

```bash
[ INFO] Found project: yarn@test-suite/browser/
[ INFO] Found target: yarn@test-suite/browser/
[ INFO] Found project: setuptools@utils/helpers/
[ INFO] Found target: setuptools@utils/helpers/
[ INFO] Found project: setuptools@utils/scripts/
[ INFO] Found target: setuptools@utils/scripts/
[ INFO] Found project: setuptools@utils/
[ INFO] Found target: setuptools@utils/
[ INFO] Found project: poetry@src/back-end/
[ INFO] Found target: poetry@src/back-end/
[ INFO] Found project: npm@src/front-end/v1/
[ INFO] Found target: npm@src/front-end/v1/
[ INFO] Found project: npm@src/front-end/v2/
[ INFO] Found target: npm@src/front-end/v2/
[ INFO] Found project: gradle@utils/migration-tests/
[ INFO] Found target: gradle@utils/migration-tests/:
[ INFO] Found project: gradle@test-suite/integration/
[ INFO] Found target: gradle@test-suite/integration/:
```

So,

1. Let's select any targets under `src/back-end` and `src/front-end/v2/` directory using [`paths.only`](./../references/files/fossa-yml.md#`paths.only:`) directive:

```yaml
version: 3

paths:
    only:
        - src/back-end/
        - src/front-end/v2/
```

2. We want to scan for targets in the `utils` directory. Let's add that to the paths to scan for targets.

```yaml
version: 3

paths:
    only:
        - src/back-end/
        - src/front-end/v2/
        - utils/
```

3. We want to exclude any targets in `utils/migration-tests/` directory, to do so, use [`paths.exclude`](./../references/files/fossa-yml.md#`paths.exclude:`) directive. This will ensure cli does not scan `utils/migrations/` directory for analysis.

```yaml
version: 3

paths:
    exclude:
        - utils/migration-tests/
    only:
        - src/back-end/
        - src/front-end/v2/
        - utils/

targets:
  exclude:
    - type: setuptools
      path: utils/scripts/
```

4. We want to exclude only `setuptools` targets in `utils/scripts-tests/` directory. Since there may be other type of targets in `utils/scripts-tests/` directory, use [`targets.exclude`](./../references/files/fossa-yml.md#`targets.exclude`) directive to explicitly ignore analysis of `setuptools` type in `utils/scripts/` directory.

```yaml
version: 3

paths:
    exclude:
        - utils/migration-tests/
    only:
        - src/back-end/
        - src/front-end/v2/
        - utils/

targets:
  exclude:
    - type: setuptools
      path: utils/scripts/
```

Likewise, we can also use [`targets.only`](./../references/files/fossa-yml.md#`targets.only`) directive to explicitly indicate which targets we are interested. This will achieve the same the behavior.

```yaml
version: 3

targets:
  only:
    - type: npm
      path: src/front-end/v2/
    - type: poetry
      path: src/back-end/
    - type: setuptools
      path: utils/
    - type: setuptools
      path: utils/helpers/
    - type: setuptools
      path: utils/helpers/
```

## Target Filtering for Submodules

For some package managers, you may have submodules or sub-projects within a single project
that you are analyzing and you may want to analyze only specifics sub project in some cases.

Here is an example with gradle:

1) Running `fossa list-targets`
```bash
[ INFO] Found project: gradle@./
[ INFO] Found target: gradle@./::app
[ INFO] Found target: gradle@./::list
[ INFO] Found target: gradle@./::utilities
```

Note that, targets are denoted in following format `type@path:target`. For 
example `gradle@./::utilities`:

Note: gradle attaches leading colons to submodules, so the utilities submodule here is referenced by ":utilities"

```
gradle  @           ./      :         :utilities
------ ---          ---    ---        -----------
Type   Path         Path   Target      Target
       separator           separator
```

2) Now to analyze only `utilities`, use `.fossa.yml` file.

```yaml
version: 3
targets:
  only:
    - type: gradle
      path: ./
      target: ':utilities'
```

3) Running `fossa analyze --output -c .fossa.yml`, will only analyze `utilities` submodule.
