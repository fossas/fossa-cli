# FOSSA's new Container Scanner

- [FOSSA's new Container Scanner](#fossas-new-container-scanner)
  - [What's New in Experimental Container Scanner?](#whats-new-in-experimental-container-scanner)
  - [Integration](#integration)
- [Documentation](#documentation)
  - [Container Image Source](#container-image-source)
    - [1) Exported docker archive (checks if `<IMAGE>` is local path to exported image tarball)](#1-exported-docker-archive-checks-if-image-is-local-path-to-exported-image-tarball)
    - [2) From Docker Engine](#2-from-docker-engine)
    - [3) From Registries](#3-from-registries)
  - [Container Image Analysis](#container-image-analysis)
    - [Path Exclusion and Filtering](#path-exclusion-and-filtering)
    - [Debugging Integration](#debugging-integration)
    - [Frequently Asked Questions (FAQs)](#frequently-asked-questions-faqs)
      - [How do I scan multi-platform container images with `fossa-cli`?](#how-do-i-scan-multi-platform-container-images-with-fossa-cli)
    - [How can I only scan for system dependencies (alpine, dpkg, rpm)?](#how-can-i-only-scan-for-system-dependencies-alpine-dpkg-rpm)
    - [How do I exclude specific projects from container scanning?](#how-do-i-exclude-specific-projects-from-container-scanning)
    - [Limitations & Workarounds](#limitations--workarounds)

## What's New in Experimental Container Scanner?

FOSSA's new container scanner supports compliance and vulnerability checks 
for application dependencies (e.g. javascript). These are done performantly 
and from many image sources. It also overhauls additional functionality as 
a fossa configuration file (`.fossa.yml`), and a `fossa-deps` file for 
scanning manual & vendor dependencies, as well as performing path exclusion
and filtering. Moreover, improvements in UX and reporting make it easy
for FOSSA to provide immediate support and better debugability. For example, 
in FOSSA WEB UI, images scanned with experimental scanner will show dependency's 
origin path.

Like the current container scanner, the experimental scanner fully 
supports the detection of OS dependencies (apk, deb, etc.).

## Integration

To use the new fossa container scanner, use the `--experimental-scanner` flag 
with the container analyze command like the following:

```bash
fossa container analyze <ARG> --experimental-scanner
fossa container test <ARG> # no changes to container test command  
```

For example,

```bash
fossa container analyze redis:alpine --experimental-scanner
fossa container analyze ghcr.io/fossas/haskell-dev-tools:9.0.2 --experimental-scanner

# via docker archive
fossa container analyze image.tar --experimental-scanner

# only analyze system dependencies (alpine, dpkg, rpm)
fossa container analyze redis:alpine --experimental-scanner --only-system-deps
```

Refer to following guides for integrating container scanning in your CI,

- [Walthrough: Integrating in Generic CI](./../../../walkthroughs/container-scanning-generic-ci.md)


# Documentation

With `fossa-cli's`, new container scanner scans container image's base layer, 
and other layers (squashed) to report compliance and security issues for 
operating system dependencies and application dependencies. 

To scan container image with `fossa-cli`, use container analyze command:

```bash
# Example 1
# Analyze container image `<IMAGE>` with experimental scanner
# and uploading scan results to provided endpoint (defaults to app.fossa.com)
#
# It only reports dependencies metadata, no source code or actual file
# is communicated to server endpoint. 
#
# It will use repository name as project name, and image digest as revision value.
# If you want to provide custom project name and custom revision value. You can
# use `--project` and `--revision` options, like:
#
#   >> fossa container analyze <IMAGE> --project <PROJECT-NAME> --revision <REVISION-VALUE>
#
fossa container analyze <IMAGE> --experimental-scanner 

# Example 2
#
# Similar to command example 1, but it does not report 
# dependencies to endpoint, but rather output's them in JSON format.
#
fossa container analyze <IMAGE> -o
```

Once you have scanned container image (without `-o or --output` flag), you can
check for security and compliance issues for analyzed container image. To do so,
use `fossa container test ` command.

```bash
# Example 3
#
# Runs FOSSA's security and compliance check on provided image,
# it will return non 0 exit code, if issues are detected.
#
# You can configure policy in FOSSA's webapp to resolve issue manually,
# only scan for specific issue type (e.g. security only), or based on 
# where dependency is (e.g. do not report issues on base layer). Refer
# to FOSSA's product documentation for guide: https://docs.fossa.com
#
fossa container test <IMAGE>
```

## Container Image Source

FOSSA can analyze container image from multiple sources - such as docker engine, 
podman, oci container registry (both public and private), or docker hub (default docker registry). 

By default, `fossa-cli`, first attempts to identify `<IMAGE>` source in order of:

### 1) Exported docker archive (checks if `<IMAGE>` is local path to exported image tarball)

```bash
docker save redis:alpine > redis_alpine.tar
fossa container analyze redis_alpine.tar --experimental-scanner
```

### 2) From Docker Engine

```bash
fossa container analyze redis:alpine --experimental-scanner
```

For this image source to work, `fossa-cli` will require docker to 
be running and accessible. Specifically, `fossa-cli` will use socket
connection at `/var/lib/docker.sock` and on that connection
use `http://localhost/v1.28/` to make 

a) `/_ping`: to sanity check connection to docker
b)  `images/<IMAGE>/json`: to retrieve image manifest (confirms image is present locally)
c) `images/<IMAGE>/get`: to retrieve image tarball

In summary, `fossa-cli` does equivalent to:

```bash
curl --unix-socket /var/run/docker.sock -X GET "http://localhost/v1.28/_ping"
curl --unix-socket /var/run/docker.sock -X GET "http://localhost/v1.28/images/redis:alpine/json"
curl --unix-socket /var/run/docker.sock -X GET "http://localhost/v1.28/images/redis:alpine/get" > tmp/fossa-cli-docker-engine-artifact-<random-number>/img.tar
```

Once the image tarball is retrieved, `fossa-cli` will store this tarball in temporary
location, and will defer to (1) Exported docker archive strategy to perform the analysis.

### 3) From Registries

```bash
fossa container analyze ghcr.io/fossas/haskell-dev-tools:9.0.2 --experimental-scanner
```

This step works even if you do not have docker installed or have docker engine accessible.

If `<IMAGE>` is not docker image archived, nor is accessible via docker engine API. `fossa-cli`, will
try to retrieve the image via registries based on content of `<IMAGE>`. It will 
parse `<IMAGE`>, in same fashion as `docker pull <ARG>`.

It will infer registry url, repository name, and repository reference (digest or tag). For
example,

| `<IMAGE>`                                         	| Registry                             	| Repository                 	| Manifest Reference             	|
|---------------------------------------------------	|--------------------------------------	|----------------------------	|--------------------------------	|
| `redis`                                           	| None (defaults to `index.docker.io`) 	| `library/redis`            	| None (defaults to `latest`)    	|
| `redis:alpine`                                    	| None (defaults to `index.docker.io`) 	| `library/redis`            	| `alpine` (as tag)              	|
| `bitnami/wordpress:6.0.1-debian-11-r14`           	| None (defaults to `index.docker.io`) 	| `bitnami/wordpress`        	| `6.0.1-debian-11-r14` (as tag) 	|
| `ghcr.io/fossas/haskell-dev-tools:9.0.2`          	| `ghcr.io`                            	| `fossas/haskell-dev-tools` 	| `9.0.2` (as tag)               	|
| `ghcr.io/fossas/haskell-dev-tools@sha256:e83e...` 	| `ghcr.io`                            	| `fossas/haskell-dev-tools` 	| `sha256:e83e...` (as digest)   	|
| `quay.io/org/image:tag`                           	| `quay.io`                            	| `org/image`                	| `tag`                          	|

Note that, 
- when domain is not present, we default to `index.docker.io` registry. 
- when digest or tag is not present, we default to `latest` tag. 
- 
When registry is found to `index.docker.io`, and repository does not have `/` character, 
we infer that this is official image which are stored under `library/<image>` value. 

When multi-platform image is provided (e.g. `ghcr.io/graalvm/graalvm-ce:ol7-java11-21.3.3`), 
`fossa-cli` defaults to selecting image artifacts for current runtime platform. 

If you want to analyze container image for platform that is different then your runtime platform 
(where you are running `fossa-cli`), specify image by that platform's digest. 

For instance, if `fossa-cli` is running in `amd84`, but we want to analyze `amr64` platform image,
we can use specific digest like following to analyze arm64 variant. 

```bash
fossa container analyze ghcr.io/graalvm/graalvm-ce@sha256:bdcba07acb11053fea0026b807ecf94550ace7df27b10596ca4c765165243cef --experimental-scanner
```

**Private Registries**

`fossa-cli` automatically infers credential based on host name and credential store
docker is using. This is done in following steps:

1) Identify host of image source (e.g. `quay.io` for `quay.io/org/image:tag`)
2) Parse Docker Config file: `$HOME/.docker/config.json` or `%USERPROFILE%/.docker/config.json`
3) If the specific credential helper is provided in `credHelpers` for host of interest, it will be used, 
otherwise default value of `credsStore` will be used.
4) `fossa-cli` will make invocation to `docker-credential-<store>`, similar to following command to retrieve credentials:

```bash
>> echo "index.docker.io" | docker-credential-desktop get
{
  "ServerURL": "https://index.docker.io",
  "Username": "username",
  "Secret": "secret"
}
```
If any of the step above fails, we default to connecting to registry without user credentials. 
   
If you want to explicitly provide username and password, you can use `user:pass@...` scheme to
provide explicit username ans password. This only works when host value is present in `<IMAGE>`.

```bash
fossa container analyze user:secret@quay.io/org/image:tag --experimental-scanner
```

**Retrieving Image from Registry**

`fossa-cli` uses `/v2/` registry api (per OCI distribution spec) for retrieving 
image manifests, and image artifacts from registry. It does so in following manner:

1) `HEAD <repository>/manifests/<tag-or-digest>` (to see if the manifests exists)
   a) If 401 status code is provided, and auth challenge is presented, `fossa-cli` will make request to token provider with mentioned scope and service (if username and password were provided for registry, HTTP auth will be used) to retrieve token.
2) Use token from step (1) to make `GET /v2/<repository>/manifests/<tag-or-digest>` to retrieve manifest
   a) If multi-platform image is detected (content-type indicating manifest index, or manifest list), manifest for current runtime's digest will be identified
   b) Using (2.a)'s digest value, `GET /v2/<repository>/manifests/<digest>` will be used to infer platform specific manifest
3) Identify configuration blob and layer blobs digests from (2)
4) Download all blobs using `GET /v2/<repository>/blobs/<digest>` (if blobs are tar.gzip, they will be gzip extracted)
5) From artifacts downloaded representative image tarball will be created.

All `GET` request from step 2 to step 5, will make `HEAD` call prior to confirm existence of resource. If 
401 status is received new access token will be generated using auth flow mentioned in step (1).


## Container Image Analysis

From the container image, experimental scanner will scan base layer, and rest of the
layers as squashed. 

Following package managers are supported in container scanning:

| Analysis                             | Supported?         | Docs                                                             |
| ------------------------------------ | ------------------ | ---------------------------------------------------------------- |
| Alpine                               | :white_check_mark: | [Apk Docs](./../../strategies/system/apk/apk.md)                 |
| Dpkg                                 | :white_check_mark: | [Dpkg Docs](./../../strategies/system/dpkg/dpkg.md)              |
| Rpm                                  | :x:                | This is to be implemented, and in roadmap                        |
| Python (setuptools, poetry, etc.)    | :white_check_mark: | [Python Docs](./../../strategies/languages/python/python.md)     |
| Javascript (npm, yarn, pnpm, etc.)   | :white_check_mark: | [Javascript Docs](./../../strategies/languages/nodejs/nodejs.md) |
| Ruby (bundler)                       | :white_check_mark: | [Ruby](./../../strategies/languages/ruby/ruby.md)                |
| .Net (packet, projectjson, etc.)     | :white_check_mark: | [.Net](./../../strategies/languages/dotnet/README.md)            |
| Perl                                 | :white_check_mark: | [Perl](./../../strategies/languages/perl/perl.md)                |
| Swift (xcode, swift package manager) | :white_check_mark: | [Swift](./../../strategies/platforms/ios/swift.md)               |
| Carthage                             | :white_check_mark: | [Carthage](./../../strategies/platforms/ios/carthage.md)         |
| Fortran (fpm)                        | :white_check_mark: | [Fortran](./../../strategies/languages/fortran/fortran.md)       |
| Cocoapods                            | :warning:          | [CocoaPods](./../../strategies/platforms/ios/cocoapods.md)       |
| Nim (nimble)                         | :warning:          | [Nim](./../../strategies/languages/nim/nimble.md)                |
| Dart (pub)                           | :warning:          | [Dart](./../../strategies/languages/dart/pub.md)                 |
| Maven                                | :warning:          | [Maven](./../../strategies/languages/maven/maven.md)             |
| Golang (gomod)                       | :x:                | N/A                                                              |
| Rust (cargo)                         | :x:                | N/A                                                              |
| Haskell (cabal, stack)               | :x:                | N/A                                                              |
| Gradle                               | :x:                | N/A                                                              |
| Clojure (lein)                       | :x:                | N/A                                                              |
| Scala (sbt)                          | :x:                | N/A                                                              |
| Elixir                               | :x:                | N/A                                                              |
| Erlang                               | :x:                | N/A                                                              |

Where, 
- :heavy-check-mark: - analysis is supported
- :warning: - partial analysis is supported (e.g. may not report edges among dependencies, etc.). Refer to the documentation.
- :x: - analysis is not supported in container scanning.

### Path Exclusion and Filtering



### Debugging Integration

Unlike current container scanner, `fossa-cli` supports `--debug` flag, 
and debug bundle generation with experimental-scanner. 

```bash
fossa container analyze redis:alpine --experimental-scanner --debug

# generates debug logs in stdout
# dumps comprehensive debug bundle in cwd with filename of "fossa.container.debug.json.gz"
```

For performant experience, prefer analyzing exported docker archive file, instead
of downloading image from registry. If you are building container image in CI
pipeline, consider saving this image artifact and using that for analysis. As 
this will reduce total runtime by not having to download and extract image.

If you are using docker, you can also easily save image.

```bash
docker pull <IMAGE>:<TAG> # or docker pull <IMAGE>@<DIGEST>
docker save <IMAGE>:<TAG> > image.tar

fossa container analyze image.tar --experimental scanner 

rm image.tar
```

### Frequently Asked Questions (FAQs)

#### How do I scan multi-platform container images with `fossa-cli`?

By default, when `fossa-cli` is analyzing multi-platform image, it will prefer using same
runtime architecture, of host. If the you would like to analyze for specific platform, 
prefer using digest. 

For example,

```bash
fossa container analyze ghcr.io/fossas/haskell-dev-tools@sha256:e83e... --experimental-scanner
```

Read more: 

### How can I only scan for system dependencies (alpine, dpkg, rpm)?

You can use `--only-system-deps` option,

```bash
fossa container analyze <IMAGE> --experimental-scanner --only-system-deps
```

### How do I exclude specific projects from container scanning?

You can use configuration file to perform exclusion of projects or paths. Refer to
[configuration file](./../../files/fossa-yml.md) for more details. 

For example, to only analyze `setuptools`, and `alpine` packages, we can use
following configuration file. 

```yml
# filename: .fossa.config.yaml
version: 3

targets:
  only:
    - type: setuptools
    - type: apkdb
```

```bash
fossa container analyze <IMAGE> --experimental-scanner -c .fossa.config.yaml --output
```

### Limitations & Workarounds

Currently experimental-scanner does not support already deprecated [v1 docker manifest format](https://docs.docker.com/registry/spec/manifest-v2-1/),
found for container image in some registries.

Workaround recommended is to export this image in tarball with use of docker. For example,

```bash
docker pull quay.io/org/image:tag
docker save quay.io/org/image:tag > img.tar

fossa container analyze img.tar --experimental-scanner
rm img.tar
```

You can also refer to [migration guide](https://docs.docker.com/registry/spec/deprecated-schema-v1/) provided by docker to update your image to using latest manifest version.
