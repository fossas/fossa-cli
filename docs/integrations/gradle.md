# Gradle

## Installation

Gradle support in FOSSA CLI depends on the following tools existing in your environment:

- Gradle (defaults to the first successful command to run from the following list: `$FOSSA_GRADLE_CMD`, `gradlew`, `gradlew.bat`, `gradle`)

## Configuration

### Automatic

`fossa init` will attempt a "best-effort" strategy to look through all available Gradle tasks/configurations and elect the most likely ones used for a production build.

 1. Look for a root Gradle build (`build.gradle`).
 2. Run `gradle tasks --all` to find all available sub-modules that support a `:dependencies` command.
 3. If no valid tasks are found, list the root project, `:`, as a Build Target.
 4. Filter out any suspicious-looking tasks (i.e. labeled `test` or `testCompile`).
 5. Write tasks to configuration (`fossa.yml`).

If a gradle wrapper was provided in the same directory (`./gradlew`, `./gradlew.bat`), `fossa` will use that over the local `gradle` binary.

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

  | Option               |  Type  | Name                                           | Common Use Case                                                             |
  | -------------------- | :----: | ---------------------------------------------- | --------------------------------------------------------------------------- |
  | `cmd`                | string | [Cmd](#cmd-string)                             | Specify the gradle command to use.                                          |
  | `task`               | string | [Task](#task-string)                           | Specify the gradle task to run.                                             |
  | `timeout`            |  int   | [Timeout](#timeout-int)                        | Specify the length of time in seconds a `gradle` command is allowed to run. |
  | `retries`            |  int   | [Retries](#retries-int)                        | Specify the number of times to retry a `gradle` command when it fails.      |
  | `online`             |  bool  | [Online](#online-bool)                         | Remove `--offline` from the `gradle <project>:dependencies` command.        |
  | `all-submodules`     |  bool  | [All Submodules](#all-submodules-bool)         | Running `fossa analyze gradle:.` and you want to analyze all sub-projects.  |
  | `configuration`      | string | [Configuration](#configuration-string)         | Comma separated list of configurations to analyze.                          |
  | `all-configurations` |  bool  | [All Configurations](#all-configurations-bool) | Analyze all configurations for the gradle project.                          |



#### `cmd: <string>` 

Specify the command for fossa to use when it runs gradle commands. By default, the cli will select the first command of `cmd` option, `$FOSSA_GRADLE_COMMAND`, `./gradlew`, and `gradle` to execute `<cmd> -v` successfully.

#### `task: <string>`

Specify the exact arguments to be run by the gradle command before analyzing output for dependencies. By default this is `<project>:dependencies --quiet --offline` but this can be changed to anything using this option.

#### `timeout: <int>` 

Specify the amount of time in seconds that a gradle command is allowed to run before timing out. When fossa shells out to run `gradle` or `gradlew` the command can hang and consume extra resources. This option allows a user to kill the command and continue analyzing a project.

#### `retries: <int>`

Specify the amount of times to retry running the gradle command after it fails to complete. This command works best in conjunction with the `timeout` option in order to allow a command to be killed and retried.

#### `online: <bool>`

When set to true, this option will remove the `--offline` flag from the command `gradle <project>:dependencies --quiet --offline` used to find the dependencies of the specified project.

#### `all-submodules: <bool>`

This options tells Fossa to scan for all sub projects of the specified module. The primary use case of this option is when running fossa using only arguments such as `fossa analyze gradle:.`. This ensures that all sub-projects of the root project are also scanned. If `fossa init` has been run, this command is largely unnecessary as all of the sub-projects will already be listed as build targets within the configuration file.

#### `configuration: <string>`

This option takes a comma delimited list of configurations to include in the dependency scan. Fossa includes a few configurations by default but allows the user to specify any specific configurations they are interested in.

The default list of configurations is: `compile, api, implementation, compileDependenciesMetadata, apiDependenciesMetadata, implementationDependenciesMetadata`

Example:
```yaml
    configuration: implementation,implementationTest,customConfiguration
```

#### `all-configurations: <bool>`

When set to true, fossa will ignore the default list of configurations and include the dependencies from every configuration found. This is useful when analyzing development, test, and debug dependencies is desired. 

## Analysis

Analysis for gradle projects happens in 3 steps:

1. `<cmd> <project>:dependencies` is run.
2. Output is separated by configuration and dependency graphs are created.
3. Desired configurations are selected and their merged dependency graph is finalized.

The most complicated part of this process is separating dependency information by configuration and returning the desired information. Projects can have an overwhelming amount of configurations, many of which are undesirable for license scanning purposes such as test and development configurations. Fossa selects a default list of dependencies to avoid this noise, listed in the [configuration](#configuration:-<string>) option above. If you desire to scan [all configurations](#all-configurations:-<bool>) or a [known set](#configuration:-<string>), both of these options are available.

## FAQ

### Q: Why does fossa fail immediately?
One reason `fossa` can fail immediately is if gradle is not properly setup. Fossa requires your build environment to be satisfied and Gradle tasks to reliably succeed before `fossa` can run.

Most issues are often fixed by simply verifying that your Gradle build succeeds.

### Q: Why are my dependencies incorrect?
If you're having trouble getting correct dependency data, try verifying the following:

1. Your configuration and task name is valid (i.e. `app:compile` vs `customTask:customConfiguration`) in `fossa.yml`
2. You get the desired output from your configured dependencies task `gradle {subtask}:dependencies --configuration={configuration}` (i.e. `gradle app:dependencies --configuration=compile`)

If running the gradle command in your terminal does not succeed that means your gradle build is not properly configured.

If it does succeed but produces unexpected or empty output, it means you have likely selected the wrong task or configuration for `fossa` to analyze.  Running without the `--configuration` will give you the full output and help you find the right configuration to select.