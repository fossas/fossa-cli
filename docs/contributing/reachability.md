# Reachability

### What is Reachability?
Reachability Analysis is a security offering designed to enhance FOSSA's security analysis by providing context on vulnerable packages. It alleviates the constraints of traditional CVE assessments through the static analysis of application and dependency code, confirming the presence of vulnerable call paths. 

### Limitations
Reachability currently supports all Maven projects dynamically analyzed by fossa-cli. The target jar of the project must exist, prior to the analysis.

### Maven Analysis

For Maven projects, FOSSA CLI performs an analysis to infer dependencies. If FOSSA CLI identifies a complete dependency graph, which may include both direct and transitive dependencies, it attempts to infer the built JAR file for reachability analysis. It looks for `./target/{artifact}-{version}.jar` from the POM directory. If the POM file provides `build.directory` or `build.finalName` attributes, they are used instead of the default target jar path. For this reason, perform `fossa analyze` after the project of interest is built (g.g. `mvn package`), and target artifact exists in the directory.

### How do I debug reachability from `fossa-cli`?

```bash
fossa analyze --debug

cat fossa.debug.json | jq '.bundleReachabilityRaw'
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


cat fossa.debug.json | jq '.bundleReachabilityEndpoint'
{
 # content uploaded to endpoint
}
```

<!-- 
## How do I debug reachability from endpoint?

```bash
# get what we sent to endpoint
cat fossa.debug.json | jq '.bundleReachabilityEndpoint' > rawReachabilityJob.json

# run job in explain mode
yarn repl
explainReachability('rawReachabilityJob.json')

# [1] I was provided 'rawReachabilityJob.json'
# [2] I'm parsing file: 'rawReachabilityJob.json'
# [3] I found [X] reachability units
# [4] Working on [0] reachability unit
# -- 
# {
#   ....  
# }
#
```
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
; gunzip fossa.debug.json.gz     # extract produced debug bundle

# content in .bundleReachabilityRaw is uploaded
# to endpoint for reachability analysis.
; cat fossa.debug.json | jq '.bundleReachabilityRaw'
```
