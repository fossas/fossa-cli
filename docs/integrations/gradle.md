# Gradle

## Installation

Gradle support in FOSSA CLI depends on the following tools existing in your environment:

- Java (defaults to `java`, configure with `$JAVA_BINARY`)
- Gradle (defaults to `gradle` or local `gradlew`, configure with `$GRADLE_BINARY`)

## Usage

### Automatic Configuration

`fossa init` will attempt a "best-effort" strategy to look through all available Gradle tasks/configurations and elect the most likely ones used for a production build.

 1. Look for a root Gradle build (`build.gradle`)
 2. Run and parse the output of `gradle tasks --all -q -a --offline` to find all available sub-tasks that support a `:dependencies` command
 3. If task list succeeds but no sub-tasks are available, fallback to the root `dependencies` task in `build.gradle`
 4. Filter out any suspicious-looking tasks (i.e. labeled `test` or `testCompile`)
 5. Write tasks to configuration (`fossa.yml`)

If a gradle wrapper was provided in the same directory (`./gradlew`), `fossa` will prefer to use that over the local `gradle` binary.

### Manual Configuration

`fossa init` will automatically generate this configuration for you, but if you'd like to manually configure, add a `gradle` module to your `fossa.yml`:

```yaml
analyze:
  modules:
    - name: {task}:{configuration}
      path: {path-to-build.gradle}
      type: gradle
```

For Gradle modules, `fossa` requires a few peices of information to run dependency analysis:

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

### Reporting issues

If you're still having problems, open up an issue on this repo with the full logs/output of `fossa -o --debug` and your `fossa.yml` file.

Optionally, provide your `build.gradle` for more context.

## Roadmap

If you'd like to help us improve our Gradle integration, please feel free to file and assign yourself to an issue on this repo.

 1. Support multiple independent "root" Gradle builds that may be segregated in a codebase
 2. Implement a cheap build validation method to tell the user whether a Gradle build is satisfied without invoking a Gradle runtime
 3. Support multiple configurations for a single module definition i.e. `app:compile:runtime`
