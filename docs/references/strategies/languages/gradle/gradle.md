# Gradle

[Gradle](https://gradle.org/) is a polyglot tool mostly used by JVM projects. It's popular with Java and Android projects.

Gradle users generally specify their builds using a `build.gradle`/`settings.gradle` file (written in Groovy) or a `build.gradle.kts`/`settings.gradle.kts` file (written in Kotlin). These builds are specified as programs that must be dynamically evaluated. Examples of many different kinds of `gradle` projects are distributed on Gradle's website or for older versions of Gradle in the "complete" release archive in the `examples/` directory.

> [!WARNING]
> This strategy requires dynamic analysis, which requires a CI integration.

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

This strategy selects tactics by trying them in preference order and uses the results of the first tactic to succeed.
Click the tactic in the table below for more information and debugging steps specific to that tactic.

| Tactic              | Direct Deps        | Transitive Deps    | Edges              | Container Scanning |
|---------------------|--------------------|--------------------|--------------------|--------------------|
| [Plugin](plugin.md) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x:                |

## Debugging

> [!TIP]
> These debugging steps are for Gradle generally, independent of tactic;
> make sure to check the [tactic-specific documentation](#tactics) for the specific tactic your project uses.

### Determine whether Gradle targets are detected

To determine whether the CLI is properly detecting your Gradle project, run `fossa list-targets`.
The output of this command is a list of analysis targets, in the format `type@path`.

> [!NOTE]
> For more information on `fossa list-targets`, [reference the guide here](../../../subcommands/list-targets.md).

For each of your Gradle subprojects, you should see a `gradle@PATH_TO_ROOT_PROJECT:SUBPROJECT` target in the list of analysis targets.

If you _don't_ see this, one of two things is likely happening:

1. Your Gradle project does not have a `build.gradle` file.
   This is an unsupported configuration.
2. `gradle projects` is failing to execute.
   Make sure that a Gradle wrapper is accessible (see [Running Gradle](#running-gradle)), and make sure `gradle projects` runs successfully.

### Manually checking Gradle dependency results

To manually verify the correctness of the CLI's results, run `gradle :dependencies` in your root project, and `gradle $SUBPROJECT:dependencies` for each subproject.

The CLI should produce a graph of dependencies that's a union of the dependencies of each subproject.

If your CLI run uploads versions that differ from the output of `gradle $SUBPROJECT:dependencies`, check to make sure that the subproject dependency's version is the actual version resolved across the entire Gradle build. Different Gradle subprojects may select different dependency versions when resolved independently, but will select a single resolved version when the build is resolved as a whole.

If you'd like to make a bug report about incorrect dependencies, make sure to include the list of incorrect dependencies, as well as the commands you ran to obtain that list.

## Walkthroughs

### Only analyzing specific sub project

If you have gradle project which has one or more subprojects, you may only want to analyze a specific set of subprojects in some cases.
In `fossa-cli`, this can be achieved by using [exclusion filtering](./../../../files/fossa-yml.md) according to the below steps.

First, run `fossa list-targets` to identify the project directory and identifiers of subprojects:
```bash
[ INFO] Found project: gradle@./             # <- The root project. './' means "the project is at the root of the analysis directory".
[ INFO] Found target: gradle@./::app         # <- Submodule of the root project.
[ INFO] Found target: gradle@./::list
[ INFO] Found target: gradle@./::utilities
```

> [!NOTE]
> Targets are denoted in following format: `type@path:target`.
> For example `gradle@./::utilities` means "a gradle project at the scan root, with the target ':utilities'.
> As another example, `gradle@./subdir/` means "a gradle project inside 'subdir', without a target (meaning it is a root project).
>
> For more information on `fossa list-targets`, refer to [the reference here](../../../subcommands/list-targets.md).

> [!IMPORTANT]
> Gradle attaches leading colons to submodules, and as referenced in the note above FOSSA also uses a colon to separate "path" and "target".
> This means that the target identifier `gradle@./::utilities` breaks down to the below sections:
> ```
> gradle  @           ./      :         :utilities
> ------ ---          ---    ---        -----------
> Type   Path         Path   Target      Target
>        separator           separator
> ```
>
> Since Gradle prepends a colon, this means that the `:utilities` target corresponds to the "utilities" submodule of the main project.

Now to analyze only `utilities`, use a `.fossa.yml` file in the project root.

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

### Manually specifying Gradle dependencies

> [!NOTE]
> The FOSSA Gradle integration internally refers to Gradle dependencies as _Maven_ dependencies;
> this is because the Gradle build tool is used to integrate dependencies from Maven code hosts (such as Maven Central).
>
> Most of the time this isn't very relevant for end users, but it is definitely relevant when manually specifying dependencies.

If FOSSA CLI doesn't natively integrate with your build tool, and your build tool uses Gradle dependencies, you can still manually add Gradle dependencies to an uploaded build. This feature is generally known as [manual dependencies](../../../../features/manual-dependencies.md)

An example configuration file looks like:

```yaml
# fossa-deps.yml
referenced-dependencies:
- type: maven                    # <- Note that this is not "gradle", as referenced in the note above.
  name: javax.xml.bind:jaxb-api  # <- Note that this uses the Maven convention of 'groupId:artifactId'.
  version: 1.0.0
```

For more details, see the [manual dependencies](../../../../features/manual-dependencies.md) documentation.


### (Experimental) Only Selecting Set of Configurations For Analysis

You can use [configuration file](../../../files/fossa-yml.md) to provide set of configurations to filter the analysis for.
Any configurations not listed will be excluded from analysis.

> [!IMPORTANT]
> This feature is experimental and may be changed or removed at any time, without warning.
> For more information on experimental features see the [experimental features reference](../../../experimental/README.md).

```yaml
version: 3

experimental:
  gradle:
    configurations-only:
      - example-1-config-to-include
      - example-2-config-to-include
```


## Reference

### Development and Test Configurations

> [!NOTE]
> By default FOSSA CLI excludes test and development dependencies from analysis.

FOSSA classifies any dependencies originating from the following configurations as development dependencies:
```
- compileOnly
```

FOSSA classifies any dependencies originating from the following configurations as test dependencies:
```
- testImplementation
- testCompileOnly
- testRuntimeOnly
- testCompileClasspath
- testRuntimeClasspath
```

### Development and Test Configurations in Android projects

> [!NOTE]
> By default FOSSA CLI excludes test and development dependencies from analysis.

FOSSA classifies the following configurations as development dependencies:
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

FOSSA classifies the following configurations as test dependencies:
```
- androidJacocoAnt
- testApiDependenciesMetadata
- testCompileOnlyDependenciesMetadata
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
```

Additionally, FOSSA classifies any dependencies with any of the following values prefixing the dependency name as test dependencies:
```
- androidTest
- debugAndroidTest
- releaseUnitTest
- debugUnitTest
```
