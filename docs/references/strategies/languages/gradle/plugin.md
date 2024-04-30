
# Gradle build plugin

> [!WARNING]
> This tactic does not support static analysis, which means it requires a CI integration executing FOSSA CLI.

> [!NOTE]
> Gradle analysis is not supported in Container Scanning.

This tactic runs a [Gradle init script](https://docs.gradle.org/current/userguide/init_scripts.html) to output the dependencies in each Gradle subproject. Mechanically, this tactic:

1. Unpacks an [init script] to a temporary directory. Elsewhere in this document, we refer to this as "the plugin".
2. Invokes the plugin with `gradle jsonDeps -Ipath/to/init.gradle`.
3. Parses the JSON output of the plugin.

The plugin works by iterating through configurations, getting resolution result for the configuration, and then serializing those dependencies into JSON.

> [!WARNING]
> The plugin requires Gradle v3.3 or greater.

## Debugging

### Manually view plugin output

If the plugin doesn't appear to be working correctly, you can perform the following steps to run it directly:

1. [Download it from this repository.][init script]
2. Run the command `gradle -I$PATH_TO_SCRIPT jsonDeps`, where `$PATH_TO_SCRIPT` is the location to which the plugin was downloaded.

For example, with the plugin downloaded to `/tmp/jsondeps.gradle`, you should run (from within your project's working directory):

```
gradle -I/tmp/jsondeps.gradle jsonDeps
```

Usually, this output provides additional information on what is causing the build to fail.
This information is provided by Gradle and is not related to FOSSA.

### Debugging the plugin

If the plugin itself appears to not be working based on its output, please send in a support request with the following information:

1. If available, the "complete report" written by Gradle when you ran the script directly.
   This is usually linked in the Gradle output with the message "See the complete report at {file path}".
2. Send in the commands you executed to run the plugin directly, and the verbatim output of those commands.
3. Create a minimal reproduction case for us to run locally on our machines so we can debug the script on our systems.

> [!TIP]
> Support requests can be initiated at https://support.fossa.com.

### "Configuration cache problems found in this build"

The plugin may contain text like the below:

```
FAILURE: Build failed with an exception.

* Where:
Initialization script 'jsondeps.gradle' line: 190

* What went wrong:
Configuration cache problems found in this build.
```

This is a Gradle specific issue with the "[configuration cache]" feature.
When the document you're reading was written, the Gradle documentation had this to say about this feature:

> This feature [Gradle's configuration cache] is currently not enabled by default. This feature has the following limitations:
>
> - The configuration cache does not support all core Gradle plugins and features. Full support is a work in progress.
> - Your build and the plugins you depend on might require changes to fulfil the requirements.
> - IDE imports and syncs do not yet use the configuration cache.

Specific resolution steps depend on your project and Gradle version, but a possible resolution is to set `org.gradle.unsafe.configuration-cache-problems=warn` in your `gradle.properties`. This modifies configuration cache problems such that they become warnings instead of errors.

> [!TIP]
> You can read more about the [Gradle configuration cache here][configuration cache].

[init script](https://github.com/fossas/fossa-cli/blob/master/scripts/jsondeps.gradle)
[configuration cache](https://docs.gradle.org/current/userguide/configuration_cache.html)
