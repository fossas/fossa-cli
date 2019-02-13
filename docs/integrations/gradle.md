# Gradle

## Installation

Gradle support in FOSSA CLI depends on the following tools existing in your environment:

- Gradle (defaults to `gradle` or local `gradlew`, configure with `$GRADLE_BINARY`)

## Configuration

### Automatic

`fossa init` will attempt a "best-effort" strategy to look through all available Gradle tasks/configurations and elect the most likely ones used for a production build.

 1. Look for a root Gradle build (`build.gradle`).
 2. Run `gradle tasks --all -q -a` to find all available sub-modules that support a `:dependencies` command.
 3. If no valid tasks are found, list the root project, `:`, as a Build Target.
 3. Filter out any suspicious-looking tasks (i.e. labeled `test` or `testCompile`).
 4. Write tasks to configuration (`fossa.yml`).

If a gradle wrapper was provided in the same directory (`./gradlew`), `fossa` will use that over the local `gradle` binary.

### Manual

`fossa init` will automatically generate this configuration for you, but if you'd like to manually configure, add a `gradle` module to your `fossa.yml`:

```yaml
analyze:
  modules:
    - name: project-name
      path: path-to-build.gradle
      target: subproject
      type: gradle
```
## Options

| Option          |  Type  | Name                                      | Common Use Case                                                 |
| --------------- | :----: | ----------------------------------------- | --------------------------------------------------------------- |
| `cmd`           | string | [Cmd](#cmd:-string>)                      | Specify the gradle command to use.                              |
| `task`          | string | [Task](#task:-<string>)                   | Specify the gradle task to run.                                 |
| `online`        |  bool  | [Online](#online:-<bool>)                 | Specify a go package manager.                                   |
| `allsubmodules` |  bool  | [All Submodules](#all-submodules:-<bool>) | Add `--offline` to the `gradle <project>:dependencies` command. |
| `configuration` | string | [Configuration](#configuration:-<string>) | List of configurations to analyze.                              |


#### `cmd: <string>` 

Specify the command for fossa to use when it runs gradle commands. By default, the cli will select the first command of `cmd` option, `$FOSSA_GRADLE_COMMAND`, `./gradlew`, and `gradle` to execute `<cmd> -v` successfully.

#### `task: <string>`

Specify the exact arguments to be run by the gradle command before analyzing output for dependencies. By default this is `<project>:dependencies --quiet --offline` but this can be changed to anything using this option.

#### `online: <bool>`

When set to true, this option will remove the `--offline` flag from the command `gradle <project>:dependencies --quiet --offline` used to find the dependencies of the specified project.

#### `allsubmodules: <bool>`

This options tells Fossa to scan for all sub projects of the specified module. The primary use case of this option is when running fossa using only arguments such as `fossa analyze gradle:.`. This ensures that all sub-projects of the root project are also scanned.

#### `configuration: <string>`

This option takes a comma delimited list of configurations to include in the dependency scan. Fossa includes a few configurations by default but allows the user to specify any specific configurations they are interested in.

Example:
```yaml
    configuration: implementation,implementationTest,customConfiguration
```

## Design

### Building
### Analysis
### Known Limitations
### FAQ
For Gradle modules, `fossa` requires a few pieces of information to run dependency analysis:

- `task` - the name of the task/subtask used to generate your build (most often this is `app` or empty string `''` for root)
- `configuration` - the configuration your task runs during a production build (if undefined, defaults to `compile`)
- `path` - path to your `*.gradle` build file relative to your repo root directory (usually this is just `build.gradle`)

If you don't specify a task, `fossa` will run the default `dependencies` located in your root `*.gradle` file:

```yaml
analyze:
  modules:
    - name: :compile # runs the root `dependencies` task with `compile` configuration
      path: build.gradle
      type: gradle
```

You can customize your configuration by modifying the `name` entry using the template `{task}:{configuration}`.

```yaml
analyze:
  modules:
    - name: app:runtime # runs `app:dependencies` task with `runtime` configuration
      path: build.gradle
      type: gradle
```

NOTE: If you don't specify a configuration at all, `fossa` will default to `compile`.  For example:

```yaml
analyze:
  modules:
    - name: app # runs `app:dependencies` task with `compile` configuration
      path: build.gradle
      type: gradle
    - name: '' # empty - runs root `dependencies` task with `compile` configuration
      path: build.gradle
      type: gradle
```

NOTE: While `app` and `compile` are two very common tasks/configurations, your Gradle build may be different.  For instance, many Gradle builds may use the configuration `implementation` (for Android), `release` or `default` instead of `compile`.  See the `Troubleshooting` section below for steps in figuring out the right configuration for you.

## Troubleshooting
Since `fossa` operates directly on Gradle, it requires your build environment to be satisfied and Gradle tasks to reliably succeed before `fossa` can run.

Most issues are often fixed by simply verifying that your Gradle build succeeds.

### Errors during `fossa init`
FOSSA's autoconfiguration depends on `gradle tasks --all`.  If `fossa init` is failing to discover the right tasks/configuration, make sure:

1. `gradle tasks --all` succeeds in your environment
2. Your `path` entry in `fossa.yml` is points to the right `*.gradle` file

### Issues with dependency data

If you're having trouble getting correct dependency data, try verifying the following:

1. Your configuration and task name is valid (i.e. `app:compile` vs `customTask:customConfiguration`) in `fossa.yml`
2. You get the desired output from your configured dependencies task `gradle {subtask}:dependencies --configuration={configuration}` (i.e. `gradle app:dependencies --configuration=compile`)

If running the gradle command in your terminal does not succeed that means your gradle build is not properly configured.

If it does succeed but produces unexpected or empty output, it means you have likely selected the wrong task or configuration for `fossa` to analyze.  Running without the `--configuration` will give you the full output and help you find the right configuration to select.