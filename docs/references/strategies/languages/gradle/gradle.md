# Gradle

[Gradle](https://gradle.org/) is a polyglot tool mostly used by JVM projects. It's popular with Java and Android projects.

Gradle users generally specify their builds using a `build.gradle`/`settings.gradle` file (written in Groovy) or a `build.gradle.kts`/`settings.gradle.kts` file (written in Kotlin). These builds are specified as programs that must be dynamically evaluated. Examples of many different kinds of `gradle` projects are distributed on Gradle's website or for older versions of Gradle in the "complete" release archive in the `examples/` directory.

|           |                                                                           |
| --------- | ------------------------------------------------------------------------- |
| :warning: | This strategy requires dynamic analysis, which requires a CI integration. |

<!-- omit in toc -->
## Table of contents

- [Gradle](#gradle)
  - [Concepts](#concepts)
    - [Subprojects and configurations](#subprojects-and-configurations)
      - [Subprojects](#subprojects)
      - [Configurations](#configurations)
      - [Relationship to analysis targets](#relationship-to-analysis-targets)
    - [Gradle wrappers](#gradle-wrappers)
  - [Running Gradle](#running-gradle)
  - [Discovery](#discovery)
  - [Tactics](#tactics)
    - [Tactic selection](#tactic-selection)
    - [Gradle build plugin](#gradle-build-plugin)
    - [Parsing `gradle :dependencies`](#parsing-gradle-dependencies)
  - [Debugging an integration](#debugging-an-integration)
    - [Determining whether Gradle targets are detected](#determining-whether-gradle-targets-are-detected)
    - [Manually checking Gradle dependency results](#manually-checking-gradle-dependency-results)
    - [Debugging the "Gradle build plugin" tactic](#debugging-the-gradle-build-plugin-tactic)
  - [Manually specifying Gradle dependencies](#manually-specifying-gradle-dependencies)
  - [Configurations For Development and Testing](#configurations-for-development-and-testing)
  - [Android Gradle Configurations For Development and Testing](#android-gradle-configurations-for-development-and-testing)
  - [Only analyzing specific sub project](#only-analyzing-specific-sub-project)
  - [(Experimental) Only Selecting Set of Configurations For Analysis](#experimental-only-selecting-set-of-configurations-for-analysis)

## Concepts

### Subprojects and configurations

Most sizable Gradle builds organize their dependencies with two concepts: subprojects and configurations.

#### Subprojects

_Subprojects_ are used when you have multiple "projects" in a single Gradle build (e.g. having multiple projects in a single repository managed by with single `settings.gradle` and zero or more `build.gradle` files). Gradle calls these "multi-projects". Gradle multi-projects have one root project, and one or more subprojects.

A single subproject roughly corresponds to a single set of outputs. Hence, we treat subprojects as separate analysis targets.

For details, see [Creating a multi-project build](https://docs.gradle.org/current/userguide/multi_project_builds.html#authoring-multi-project-builds).

#### Configurations

Within a single subproject, Gradle builds can declare dependencies for different "scopes" (i.e. different contexts, such as compilation, test execution, or runtime execution).

Gradle calls these scopes _configurations_. Examples of configurations include `implementation`, `testRuntime`, or `compileClasspath`.

Different subprojects can have different configurations. Configurations are specific to a subproject, although there are many common configurations (e.g. `compileClasspath`) that most subprojects have.

For more details, see [What are dependency configurations](https://docs.gradle.org/current/userguide/declaring_dependencies.html#sec:what-are-dependency-configurations).

#### Relationship to analysis targets

Each pair of `(subproject, configuration)` corresponds to one dependency graph.

Ideally, each `(subproject, configuration)` pair would be a separate analysis target. For technical reasons, the current implementation treats separate subprojects as analysis targets, and treats each subproject as the union of all of its configurations.

In practice, this should not affect returned dependency results.

### Gradle wrappers

Instead of invoking `gradle` directly, most Gradle projects use a "Gradle wrapper", which is a shell script vendored in the project that selects and downloads the correct version of Gradle. See [The Gradle Wrapper](https://docs.gradle.org/current/userguide/gradle_wrapper.html) for details.

Because of this, the Gradle analyzer has logic for selecting which `gradle` executable to actually use. See [Running Gradle](#running-gradle) for details.

## Running Gradle

This strategy requires dynamic analysis in its discovery phase (not just in the analysis phase). This is because we need to execute Gradle in order to list subprojects and evaluate `build.gradle` or `settings.gradle` files.

When executing Gradle for an analysis target at directory `ANALYSIS_TARGET_DIR`, the CLI will prefer (in order):

1. `gradlew`. First looking in `ANALYSIS_TARGET_DIR` and then recursively searching parent directories until `gradlew` is found.
1. `gradlew.bat`. First looking in `ANALYSIS_TARGET_DIR` and then recursively searching parent directories until `gradlew.bat` is found.
1. `gradle` (from `$PATH`)

For more details, see [Gradle wrappers](#gradle-wrappers).

In this documentation below, for brevity, we'll always refer to the selected tool as `gradle`.

## Discovery

This strategy discovers analysis targets by looking for files in the folder being analyzed whose names start with `build.gradle` or `settings.gradle`. This matches any of `build.gradle`, or `settings.gradle` and Gradle build scripts in other Gradle-supported languages (`build.gradle.*`, `settings.gradle.*`).

It then executes `gradle projects` in the directory where the build file is found to get a list of subprojects for this Gradle build. These subprojects are used to create the analysis targets for this Gradle build.

If there are no subprojects, an analysis target is created that analyzes the root project. Otherwise, a set of analysis targets is created: one for each Gradle subproject.

## Tactics

### Tactic selection

This strategy selects tactics by trying them in preference order and uses the results of the first tactic to succeed.

The order of tactics for this strategy is:

1. Gradle build plugin
2. Parsing `gradle :dependencies` (not yet implemented)

### Gradle build plugin

|                    |                                                           |
| ------------------ | --------------------------------------------------------- |
| :heavy_check_mark: | This tactic reports dependencies for all subprojects.     |
| :heavy_check_mark: | This tactic provides a graph for subproject dependencies. |
| :warning:          | This tactic requires dynamic analysis.                    |

This tactic runs a Gradle [init script](https://docs.gradle.org/current/userguide/init_scripts.html) to output the dependencies in each Gradle subproject. Mechanically, this tactic:

1. Unpacks our init script to a temporary directory.
2. Invokes the init script with `gradle jsonDeps -Ipath/to/init.gradle`.
3. Parses the JSON output of the init script.

This init script is implemented [here](https://github.com/fossas/fossa-cli/blob/master/scripts/jsondeps.gradle) and bundled into the CLI during compilation.

The script works by iterating through configurations, getting resolution result for the configuration, and then serializing those dependencies into JSON. Please note that we currently only support analysis for builds using gradle v3.3 or greater.

> Gradle analysis is not supported in Container Scanning (experimental).

### Parsing `gradle :dependencies`

|           |                                        |
| --------- | -------------------------------------- |
| :x:       | This tactic is not yet implemented.    |
| :warning: | This tactic requires dynamic analysis. |

This not-yet-implemented tactic will execute `gradle $SUBPROJECT:dependencies` for each analysis target, and parse the tool's output.

## Debugging an integration

### Determining whether Gradle targets are detected

To determine whether the CLI is properly detecting your Gradle project, run `fossa list-targets`. The output of this command is a list of analysis targets, in the format `type@path`.

<!-- TODO: is there a guide for `fossa list-targets` I can reference here? -->

For each of your Gradle subprojects, you should see a `gradle@PATH_TO_ROOT_PROJECT:SUBPROJECT` target in the list of analysis targets.

If you _don't_ see this, one of two things is likely happening:

1. Your Gradle project does not have a `build.gradle` file. This is an unsupported configuration.
2. `gradle projects` is failing to execute. Make sure that a Gradle wrapper is accessible (see [Running Gradle](#running-gradle)), and make sure `gradle projects` runs successfully.

### Manually checking Gradle dependency results

To manually verify the correctness of the CLI's results, run `gradle :dependencies` in your root project, and `gradle $SUBPROJECT:dependencies` for each subproject.

The CLI should produce a graph of dependencies that's a union of the dependencies of each subproject.

If your CLI run uploads versions that differ from the output of `gradle $SUBPROJECT:dependencies`, check to make sure that the subproject dependency's version is the actual version resolved across the entire Gradle build. Different Gradle subprojects may select different dependency versions when resolved independently, but will select a single resolved version when the build is resolved as a whole.

If you'd like to make a bug report about incorrect dependencies, make sure to include the list of incorrect dependencies, as well as the commands you ran to obtain that list.

### Debugging the "Gradle build plugin" tactic

The Gradle build plugin is a Gradle [init script](https://docs.gradle.org/current/userguide/init_scripts.html) implemented [here](../../../../../scripts/jsondeps.gradle).

If this tactic doesn't appear to be working (e.g. is giving you incorrect dependencies or is missing dependencies), you can run the init script directly using:

```
gradle -I$PATH_TO_SCRIPT jsonDeps
```

For example, with the script extracted to `/tmp/jsondeps.gradle`, you should run (from within the Gradle build script's working directory):

```
gradle -I/tmp/jsondeps.gradle jsonDeps
```

Providing this output with a bug report will help us debug issues with the analysis.

## Manually specifying Gradle dependencies

If the CLI doesn't natively integrate with your build tool (e.g. if you have a homegrown tool), and your build tool uses Gradle dependencies, you can still manually add Gradle dependencies to an uploaded build. This feature is generally known as [manual dependencies](../../../../features/manual-dependencies.md)

Gradle in particular actually uploads _Maven_ dependencies, since most Gradle builds use Gradle's Maven interoperability to get dependencies from Maven repositories.

An example configuration file looks like:

```yaml
# fossa-deps.yml
referenced-dependencies:
- type: maven
  name: javax.xml.bind:jaxb-api
  version: 1.0.0
```

Notice that the `name` field follows Maven conventions: `groupId:artifactId`.

For more details, see the [manual dependencies](../../../../features/manual-dependencies.md) documentation.

## Configurations For Development and Testing

We classify any dependencies originating from the following configurations as a development dependency:

```
- compileOnly
```

We classify any dependencies originating from the following configurations as a test dependency:

```
- testImplementation
- testCompileOnly
- testRuntimeOnly
- testCompileClasspath
- testRuntimeClasspath
```

Note, by default we exclude test and development dependencies from the analysis.

## Android Gradle Configurations For Development and Testing

We classify following configurations, and any dependencies originating from it as a test environment dependency:
```
Any dependencies with following prefixes:
- androidTest
- debugAndroidTest
- releaseUnitTest

And any configuration named:
- androidJacocoAnt
- testApiDependenciesMetadata
- testCompileOnlyDependenciesMetadata
- debugUnitTestApiDependenciesMetadata
- debugUnitTestCompileOnlyDependenciesMetadata
- debugUnitTestImplementationDependenciesMetadata
- debugUnitTestIntransitiveDependenciesMetadata
- debugUnitTestRuntimeOnlyDependenciesMetadata
- testDebugApiDependenciesMetadata
- testDebugCompileOnlyDependenciesMetadata
- testDebugImplementationDependenciesMetadata
- testDebugIntransitiveDependenciesMetadata
- testDebugRuntimeOnlyDependenciesMetadata
- testImplementationDependenciesMetadata
- testIntransitiveDependenciesMetadata
- testReleaseApiDependenciesMetadata
- testReleaseCompileOnlyDependenciesMetadata
- testReleaseImplementationDependenciesMetadata
- testReleaseIntransitiveDependenciesMetadata
- testReleaseRuntimeOnlyDependenciesMetadata
- testRuntimeOnlyDependenciesMetadata
- debugUnitTestAnnotationProcessorClasspath

```

We classify following configurations, and dependencies originating from it as a development (or debug) environment dependency:
```
- lintChecks
- lintClassPath
- lintPublish
- debugApiDependenciesMetadata
- debugCompileClasspath
- debugCompileOnly
- debugCompileOnlyDependenciesMetadata
- debugImplementationDependenciesMetadata
- debugIntransitiveDependenciesMetadata
- debugReverseMetadataValues
- debugRuntimeClasspath
- debugRuntimeOnlyDependenciesMetadata
- compileOnlyDependenciesMetadata
- releaseCompileOnly
- releaseCompileOnlyDependenciesMetadata
- debugWearBundling
- debugAnnotationProcessorClasspath
- releaseAnnotationProcessorClasspath
- androidJdkImage
- kotlinCompilerClasspath
- kotlinCompilerPluginClasspathDebug
- kotlinCompilerPluginClasspathDebugAndroidTest
- kotlinCompilerPluginClasspathDebugUnitTest
- kotlinCompilerPluginClasspathReleaseUnitTest
- kotlinCompilerPluginClasspathRelease
- kotlinKlibCommonizerClasspath
- kotlinNativeCompilerPluginClasspath
```

## Only analyzing specific sub project

If you have gradle project which has one or more subprojects, you may
only want to analyze a specific set of subprojects in some cases.


In `fossa-cli`, this can be achieved by using [exclusion filtering](./../../../files/fossa-yml.md).

1) Run `fossa list-targets`, to identify project directory and identifier of subprojects.
```bash
[ INFO] Found project: gradle@./
[ INFO] Found target: gradle@./::app
[ INFO] Found target: gradle@./::list
[ INFO] Found target: gradle@./::utilities
```

Note that, targets are denoted in following format `type@path:target`. For 
example `gradle@./::utilities`:

```
gradle  @           ./      :         :utilities
------ ---          ---    ---        -----------
Type   Path         Path   Target      Target
       separator           separator
```

2) Now to analyze only `utilities`, use a `.fossa.yml` file in the project root.

```yaml
# filename: .fossa.yml
#
# analyze only gradle@./::utilities
version: 3
targets:
  only:
    - type: gradle
      path: ./
      target: ':utilities'
```

Likewise, if you want to exclude specific set of subprojects, you can do following:

```yaml
# filename: .fossa.yml
#
# do not analyze gradle@./::app, and gradle@./::utilities
version: 3
targets:
  only:
    - type: gradle
  exclude:
    - type: gradle
      path: ./
      target: ':app'
    - type: gradle
      path: ./
      target: ':utilities'
```


1) Running `fossa analyze --output -c .fossa.yml`, will only analyze `utilities` submodule.

## (Experimental) Only Selecting Set of Configurations For Analysis

You can use [configuration file](../../../files/fossa-yml.md) to provide set of configurations to filter the analysis for. Any configurations not listed will be excluded from analysis. This feature is experimental and may be changed or removed at any time, without warning.

```yaml
version: 3

experimental:
  gradle:
    configurations-only:
      - example-1-config-to-include
      - example-2-config-to-include
```
