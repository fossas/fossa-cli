# Reachability

### What is Reachability?

Reachability Analysis is a security offering designed to enhance FOSSA's security analysis by providing context on vulnerable packages. It alleviates the constraints of traditional CVE assessments through the static analysis of application and dependency code, confirming the presence of vulnerable call paths.

### Limitations

- Reachability currently supports all Maven and Gradle projects dynamically analyzed by FOSSA CLI.
- The target jar of the project must exist, prior to the analysis. If the jar artifact is not present, or FOSSA CLI fails to
associate this jar with project, FOSSA CLI will not perform reachability analysis.
- Reachability requires that `java` is present in PATH, and `java` version must be greater than `1.8` (jdk8+).

For example,
- if you are using maven, you should run `mvn package` to ensure jar artifact exists, prior to running `fossa analyze`
- if you are using gradle, you should run `gradlew build` to ensure jar artifact exists, prior to running `fossa analyze`

### Maven Analysis

For Maven projects, FOSSA CLI performs an analysis to infer dependencies. If FOSSA CLI identifies a complete dependency graph, which may include both direct and transitive dependencies, it attempts to infer the built JAR file for reachability analysis. It looks for `./target/{artifact}-{version}.jar` from the POM directory. If the POM file provides `build.directory` or `build.finalName` attributes, they are used instead of the default target jar path. For this reason, perform `fossa analyze` after the project of interest is built (g.g. `mvn package`), and target artifact exists in the directory.

### Gradle Analysis

For Gradle projects, FOSSA CLI invokes `./gradlew -I jsonpaths.gradle jsonPaths`. Where [jsonpaths.gradle](./../../scripts/jarpaths.gradle) is gradle script, which uses `java` plugin, and `jar` task associated with gradle to infer path of the built jar file. If neither of those are present, FOSSA CLI won't be able to identify jar artifacts for analysis.

### Custom JAR locations

For both Gradle and Maven, it is possible to choose different locations for output JAR files, or to move them before FOSSA CLI runs.
For these cases, you can configure the locations directly.

First, run `fossa list-targets` inside your project. For example:
```sh
❯ fossa list-targets
Found project: gradle@./
Found target: gradle@./::app
```

This output specifies that a Gradle project was found at the root of the current directory (at `./`).
Note for which project(s) you wish to provide the paths to the built JAR files, we'll need these later.

Now, create a `.fossa.yml` file if one does not already exist ([more information here](../references/files/fossa-yml.md)).
Add the following keys to it:
```yml
reachability:
  jvmOutputs:
```

Inside the `jvmOutputs` object, create a map that describes, for each project path (noted down earlier) the paths to the JAR files that the project output.

For example, let's say that the Gradle project at the root of my scan directory (at `./`) produces a JAR file named `app.jar`, and that I configured Gradle such that when it builds the project that `app.jar` file is also written to the root of my scan directory (so its path is `./app.jar`, relative to the scan root).
My config file would look like this:

```yml
reachability:
  jvmOutputs:
    './':
    - './app.jar'
```

You can also provide the absolute paths to make this more clear, if desired. For example, if my project was at `~/projects/example-project` and I wanted to use absolute paths in the config file, the file would look like this:
```yml
reachability:
  jvmOutputs:
    '/Users/me/projects/example-project':
    - '/Users/me/projects/example-project/app.jar'
```

With this configuration, when FOSSA CLI analyzes a Maven or Gradle project at the specified path (in this case `/Users/me/projects/example-project`) instead of attempting to query the build tool for the location of the built JAR files it will instead use the values provided for that path (in this case `/Users/me/projects/example-project/app.jar`).

### How do I debug reachability from `fossa-cli`?

```bash
fossa analyze --debug

unzip -p fossa.debug.zip fossa.debug.json | jq '.bundleReachabilityRaw'
[
  {
    "callGraphAnalysis": {
      "value": [
        {
          "parsedJarContent": {
            "kind": "ContentRaw",
            "value": "some value"
          },
          "parsedJarPath": "/Users/dev/example/example-projects/maven/example-maven-project/target/example-artifact-1.1.jar"
        }
      ],
      "kind": "JarAnalysis"
    },
    "srcUnitDependencies": [
      "mvn+com.fasterxml.jackson.core:jackson-databind$2.13.0",
      "mvn+joda-time:joda-time$2.10.14",
      "mvn+com.fasterxml.jackson.core:jackson-annotations$2.13.0",
      "mvn+com.fasterxml.jackson.core:jackson-core$2.13.0"
    ],
    "srcUnitManifest": "/Users/dev/example/example-projects/maven/example-maven-project/",
    "srcUnitName": "/Users/dev/example/example-projects/maven/example-maven-project/",
    "srcUnitOriginPaths": [
      "pom.xml"
    ],
    "srcUnitType": "maven"
  },
  {
    "callGraphAnalysis": {
      "tag": "NoCallGraphAnalysis"
    },
    "srcUnitDependencies": [],
    "srcUnitManifest": "manifest",
    "srcUnitName": "name",
    "srcUnitOriginPaths": [],
    "srcUnitType": "type"
  }
]


unzip -p fossa.debug.zip fossa.debug.json | jq '.bundleReachabilityEndpoint'
{
 # content uploaded to endpoint
}
```

FOSSA CLI uses [jar-callgraph-1.0.3.jar](../../scripts/jar-callgraph-1.0.3.jar) to infer call path edges.
FOSSA CLI uses `java -jar jar-callgraph-1.0.3.jar ./path/to/your/build.jar` command to record edges from
the your target jar. If you are running into issues with reachability, please confirm that you can execute
`java -jar jar-callgraph-1.0.3.jar ./path/to/your/build.jar` on your environment.

<!--
## How do I debug reachability from endpoint?

```bash
# get what we sent to endpoint

unzip -p fossa.debug.zip fossa.debug.json | jq '.bundleReachabilityEndpoint' > rawReachabilityJob.json

# run job in dry mode
>> yarn repl
>> performReachabilityInDryMode('rawReachabilityJob.json', 'orgId', 'userRevisionId')
#
# [Info] ....
# [Info] ....

# This will upsert 'rawReachabilityJob.json' to S3, and perform
# analysis without persisting anything to database. This command is ('orgId', 'userRevisionId')
# agnostic, meaning that you can run 'rawReachabilityJob.json' for any permutation of ('orgId', 'userRevisionId').

# For example, for any customer's 'rawReachabilityJob.json' (retrieved via debug bundle), you
# can `performReachabilityInDryMode(...)` in your local environment, for your orgId, and userRevisionId.
#
# Since this does not persist any data in cache, nor in exports table - it has no consequences to
# orgId, and userRevisionId.

```
Likewise, you can also inspect analysis done in datadog, by looking at logs associated with the build id. FOSSA
performs reachability analysis as part of provided build (all variants of provided builds).
-->

## F.A.Q.

1. What data from my codebase is uploaded to endpoint?

We upload call graph of (caller, and callee) relationships, in which
caller and callee are fully qualified symbol name. We do not upload source code.

Here is example of caller, callee relationship that is uploaded to endpoint.

```txt
M:com.example.app.utils.ContextReader:<init>() (O)java.lang.Object:<init>()
M:com.example.app.utils.ContextReader:parseWithCtx(java.net.URL) (O)java.io.File:<init>(java.lang.String)
M:com.example.app.utils.ContextReader:parseWithCtx(java.net.URL) (S)com.google.common.io.Files:toString(java.io.File,java.nio.charset.Charset)
M:com.example.app.utils.ContextReader:parseWithCtx(java.net.URL) (O)org.dom4j.jaxb.JAXBReader:<init>(java.lang.String)
M:com.example.app.utils.ContextReader:parseWithCtx(java.net.URL) (M)org.dom4j.jaxb.JAXBReader:read(java.net.URL)
M:com.example.app.App:<init>() (O)java.lang.Object:<init>()
M:com.example.app.App:main(java.lang.String[]) (O)java.net.URI:<init>(java.lang.String)
M:com.example.app.App:main(java.lang.String[]) (M)java.net.URI:toURL()
M:com.example.app.App:main(java.lang.String[]) (S)com.example.app.App:parse(java.net.URL)
M:com.example.app.App:main(java.lang.String[]) (M)java.io.PrintStream:println(java.lang.Object)
M:com.example.app.App:main(java.lang.String[]) (S)com.example.app.utils.ContextReader:parseWithCtx(java.net.URL)
M:com.example.app.App:main(java.lang.String[]) (M)java.io.PrintStream:println(java.lang.Object)
M:com.example.app.App:parse(java.net.URL) (O)org.dom4j.io.SAXReader:<init>()
M:com.example.app.App:parse(java.net.URL) (M)org.dom4j.io.SAXReader:read(java.net.URL)
```

You can inspect the data by running:

```bash
; fossa analyze --output --debug # --output to not communicate with endpoint
; unzip fossa.debug.zip fossa.debug.json # extract produced debug bundle

# content in .bundleReachabilityRaw is uploaded
# to endpoint for reachability analysis.
; cat fossa.debug.json | jq '.bundleReachabilityRaw'
```

2. How do I know if reachability is supported by my organization?

FOSSA requires that `reachability` is enabled for your organization. If `reachability` is not enabled,
for your organization, you will see following message in output when `fossa analyze` is performed without
`--output or -o` mode.

```text
Organization: (your orgId) does not support reachability! skipping reachability analysis upload!
```

To enable, `reachability` please contact your FOSSA account manager, or [FOSSA support](https://support.fossa.com).
