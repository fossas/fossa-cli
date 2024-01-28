# Reachability

<!-- 
## What is reachability?



## Limitations

Reachabiility currently support maven package manager, and jar builds.

-->

## How do I debug reachability from fossa-cli?

```bash
fossa analyze --debug

cat fossa.debug.json | jq '.bundleReachabilityRaw'
[
  {
    "callGraphAnalysis": {
      "contents": [
        {
          "parsedJarContent": {
            "kind": "ContentRaw",
            "value": "some value"
          },
          "parsedJarPath": "/Users/dev/example/example-projects/maven/example-maven-project/target/example-artifact-1.1.jar"
        }
      ],
      "tag": "JarAnalysis"
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
caller and callee are fully qualified symbol name. 

You can inspect the data by running, 

```bash
; fossa analyze --output --debug # --output to not communicate with endpoint
; gunzip fossa.debug.json.gz     # extract debug bundle produced

# content in .bundleReachabilityRaw is uploaded
# to endpoint for reachability analysis.
; cat fossa.debug.json | jq '.bundleReachabilityRaw'
```
