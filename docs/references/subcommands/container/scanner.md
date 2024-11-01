# FOSSA's container scanner

- [FOSSA's container scanner](#fossas-container-scanner)
  - [What's supported in FOSSA's container scanner?](#whats-supported-in-fossas-container-scanner)
- [Documentation](#documentation)
  - [Container image source](#container-image-source)
    - [1) Exported docker archive](#1-exported-docker-archive)
    - [2) From Docker Engine](#2-from-docker-engine)
    - [3) From registries](#3-from-registries)
  - [Container image analysis](#container-image-analysis)
    - [Container JAR analysis](#container-jar-analysis)
    - [Distroless Containers](#distroless-containers)
    - [Supported Container Package Managers](#supported-container-package-managers)
    - [View detected projects](#view-detected-projects)
      - [Command output](#command-output)
    - [Utilize analysis target configuration](#utilize-analysis-target-configuration)
    - [Debugging](#debugging)
    - [Frequently Asked Questions (FAQs)](#frequently-asked-questions-faqs)
      - [How do I scan multi-platform container images with `fossa-cli`?](#how-do-i-scan-multi-platform-container-images-with-fossa-cli)
    - [How can I only scan for system dependencies (alpine, dpkg, rpm)?](#how-can-i-only-scan-for-system-dependencies-alpine-dpkg-rpm)
    - [How do I exclude specific projects from container scanning?](#how-do-i-exclude-specific-projects-from-container-scanning)
  - [Limitations \& Workarounds](#limitations--workarounds)

## What's supported in FOSSA's container scanner?

FOSSA's container scanner adds support for compliance and vulnerability checks for application dependencies inside of containers.
The performance of analysis and support for container image sources is improved, and is more robust for future enhancement.

FOSSA's container scanner brings support for standard FOSSA CLI features into containers:
- Support for configuration via `.fossa.yml`.
- Support for path filtering (exclusion and inclusion).

The container scanner fully supports the detection of OS dependencies (`apk`, `deb`, etc).

Refer to following guides for integrating container scanning in your CI,

- [Walthrough: Integrating in Generic CI](./../../../walkthroughs/container-scanning-generic-ci.md)

# Documentation

FOSSA's container scanner scans the base layer of the image, squashes all other layers, and scans those as well.
Scans report compliance and security issues for operating system dependencies and application dependencies.

To scan a container image with `fossa-cli`, use the `container analyze` command:

```bash
# Analyze the container image `<IMAGE>` with container scanner and upload scan results to FOSSA.
# This command only reports dependency metadata, no source code or actual file is sent to FOSSA.
#
# This command uses the repository name as project name, and image digest as the revision.
# Like standard FOSSA analysis, the project name is customizable via `--project` and revision via `--revision`:
#
#   >> fossa container analyze <IMAGE> --project <PROJECT-NAME> --revision <REVISION-VALUE>
#
fossa container analyze <IMAGE>

# Similar to the above, but instead of uploading the results they are instead written to the terminal in JSON format.
#
fossa container analyze <IMAGE> -o
```

Once the container image has been scanned and the results uploaded to FOSSA,
security and compliance issues are reported via the `fossa container test` command.

```bash
# Run FOSSA security and compliance checks on the container metadata.
# Like the standard `fossa test` command, `fossa container test` exits with a non-zero exit code if issues are detected.
#
# Container issue scanning works with standard FOSSA policies just like any other project.
# Policies are also able to surface or suppress issues based on the layer in which the issue was detected.
# Refer to FOSSA's product documentation for more information: https://docs.fossa.com
#
# This does not need the `` flag.
fossa container test <IMAGE>
```

## Container image source

FOSSA can analyze container images from multiple sources:
- Exported archive
- Docker engine
- Podman
- OCI container registry (public and private)
- Docker Hub

By default `fossa-cli` attempts to identify `<IMAGE>` source in the following order:

### 1) Exported docker archive

```bash
docker save redis:alpine > redis_alpine.tar
fossa container analyze redis_alpine.tar
```

### 2) From Docker Engine

```bash
fossa container analyze redis:alpine
```

For this image source to work, `fossa-cli` requires docker to be running and accessible.
Specifically, `fossa-cli` uses the Unix socket at `/var/lib/docker.sock`, with `http://localhost/v1.28/` as a base API endpoint.

1) `/_ping`: check connection to docker
2)  `images/<IMAGE>/json`: retrieve image manifest
3) `images/<IMAGE>/get`: retrieve image archive

In summary, `fossa-cli` performs the equivalent of:

```bash
curl --unix-socket /var/run/docker.sock -X GET "http://localhost/v1.28/_ping"
curl --unix-socket /var/run/docker.sock -X GET "http://localhost/v1.28/images/redis:alpine/json"
curl --unix-socket /var/run/docker.sock -X GET "http://localhost/v1.28/images/redis:alpine/get" > tmp/fossa-cli-docker-engine-artifact-<random-number>/img.tar
```

`fossa-cli` stores this archive in a temporary location and analyzes that archive.

### 3) From registries

```bash
fossa container analyze ghcr.io/fossas/haskell-dev-tools:9.0.2
```

This step works even if you do not have docker installed or have docker engine accessible.

If `<IMAGE>` is not a docker image archive and is not accessible via the docker engine API,
`fossa-cli` attempts to retrieve the image via registries based on the content of `<IMAGE>`.

`<IMAGE>` is parsed in a similar fashion to `docker pull <ARG>`:

| `<IMAGE>`                                         | Registry                             | Repository                 | Manifest Reference             |
| ------------------------------------------------- | ------------------------------------ | -------------------------- | ------------------------------ |
| `redis`                                           | None (defaults to `index.docker.io`) | `library/redis`            | None (defaults to `latest`)    |
| `redis:alpine`                                    | None (defaults to `index.docker.io`) | `library/redis`            | `alpine` (as tag)              |
| `bitnami/wordpress:6.0.1-debian-11-r14`           | None (defaults to `index.docker.io`) | `bitnami/wordpress`        | `6.0.1-debian-11-r14` (as tag) |
| `ghcr.io/fossas/haskell-dev-tools:9.0.2`          | `ghcr.io`                            | `fossas/haskell-dev-tools` | `9.0.2` (as tag)               |
| `ghcr.io/fossas/haskell-dev-tools@sha256:e83e...` | `ghcr.io`                            | `fossas/haskell-dev-tools` | `sha256:e83e...` (as digest)   |
| `quay.io/org/image:tag`                           | `quay.io`                            | `org/image`                | `tag`                          |

Note:
- When the domain is not present, `fossa-cli` defaults to the registry `index.docker.io`.
- When digest or tag is not present, `fossa-cli` defaults to the tag `latest`.
- When the registry is `index.docker.io`, and repository does not contain the literal `/`, `fossa-cli` infers that this is official image stored under `library/<image>`.
- When a multi-platform image is provided (e.g. `ghcr.io/graalvm/graalvm-ce:ol7-java11-21.3.3`), `fossa-cli` defaults to selecting image artifacts for current runtime platform.

Analyzing the container image for a platform other than the one currently running is possible by specifying the digest for the image on a different platform.

For example, the following command analyzes the `arm64` platform image of `ghcr.io/graalvm/graalvm-ce@sha256` regardless of the platform running `fossa container analyze`:

```bash
fossa container analyze ghcr.io/graalvm/graalvm-ce@sha256:bdcba07acb11053fea0026b807ecf94550ace7df27b10596ca4c765165243cef
```

**Private registries**

`fossa-cli` automatically infers credentials based on the host name and Docker credential store.

This is done in following steps:

1) Identify the host of image source (e.g. `quay.io` for `quay.io/org/image:tag`)
2) Parse Docker config file: `$HOME/.docker/config.json` or `%USERPROFILE%/.docker/config.json`
3) If there is an associated credential helper specified in `credHelpers` it is used. Otherwise, the default helper `credsStore` is used.
4) `fossa-cli` executes `docker-credential-<store>`, similar to following command to retrieve credentials:

```bash
>> echo "index.docker.io" | docker-credential-desktop get
{
  "ServerURL": "https://index.docker.io",
  "Username": "username",
  "Secret": "secret"
}
```

If any of the steps above fail, `fossa-cli` defaults to connecting without user credentials.

To explicitly provide a username and password, use HTTP-style authentication in the image URL.
For this to work the host value must be present in the image URL:

```bash
fossa container analyze user:secret@quay.io/org/image:tag
```

**Retrieving image from registry**

`fossa-cli` uses `/v2/` registry api (per OCI distribution spec) for retrieving
image manifests, and image artifacts from registry. It does so in following manner:

1) `HEAD <repository>/manifests/<tag-or-digest>` (to see if the manifests exists)
   1) If 401 status code is provided, and auth challenge is presented, `fossa-cli` will make request to token provider with mentioned scope and service (if username and password were provided for registry, HTTP auth will be used) to retrieve token.
2) Use token from step (1) to make `GET /v2/<repository>/manifests/<tag-or-digest>` to retrieve manifest
   1) If multi-platform image is detected (content-type indicating manifest index, or manifest list), manifest for current runtime's digest will be identified
   2) Using (2.a)'s digest value, `GET /v2/<repository>/manifests/<digest>` will be used to infer platform specific manifest
3) Identify configuration blob and layer blobs digests from (2)
4) Download all blobs using `GET /v2/<repository>/blobs/<digest>` (if blobs are tar.gzip, they will be gzip extracted)
5) From artifacts downloaded representative image tarball will be created.

All `GET` request from step 2 to step 5, will make a `HEAD` call prior to confirm existence of resource. If
401 status is received new access token will be generated using auth flow mentioned in step (1).

## Container image analysis

The container scanner scans in two steps:
1. The base layer.
2. The rest of the layers, squashed.

### Container JAR analysis

The container analyzer will try to find Java Archive (Jar) files inside each layer.
It will then report them to FOSSA which will try to match the Jar files to the project they are a build artifact from.

The container analyzer will also expand each Jar file that it encounters and report any Jar files that it finds in the expanded Jar file. This is done recursively.

This process relies on there being a back-end that can perform that analysis.
SaaS customers should have this functionality available but on-prem customers may need to contact FOSSA support to have it enabled.

### Distroless Containers

Container images where FOSSA cannot detect an operating system are supported but in a more limited way than images where FOSSA can.
These container images will not support reporting system deps (APK, DPKG, and RPM) but can support the other forms of analyses listed in the table below.

### Supported Container Package Managers
The following package managers are supported in container scanning:

| Analysis                             | Supported?         | Docs                                                             |
| ------------------------------------ | ------------------ | ---------------------------------------------------------------- |
| Alpine (APK)                         | :white_check_mark: | [APK Docs](./../../strategies/system/apk/apk.md)                 |
| Debian (DPKG)                        | :white_check_mark: | [DPKG Docs](./../../strategies/system/dpkg/dpkg.md)              |
| RedHat (RPM)                         | :white_check_mark: | [RPM Docs](../../strategies/system/rpm/rpm-container.md)         |
| Python (setuptools, poetry, etc.)    | :white_check_mark: | [Python Docs](./../../strategies/languages/python/python.md)     |
| Javascript (npm, yarn, pnpm, etc.)   | :white_check_mark: | [Javascript Docs](./../../strategies/languages/nodejs/nodejs.md) |
| Ruby (bundler)                       | :white_check_mark: | [Ruby](./../../strategies/languages/ruby/ruby.md)                |
| .Net (paket, projectjson, etc.)     | :white_check_mark: | [.Net](./../../strategies/languages/dotnet/README.md)            |
| Perl                                 | :white_check_mark: | [Perl](./../../strategies/languages/perl/perl.md)                |
| Swift (xcode, swift package manager) | :white_check_mark: | [Swift](./../../strategies/platforms/ios/swift.md)               |
| Carthage                             | :white_check_mark: | [Carthage](./../../strategies/platforms/ios/carthage.md)         |
| Fortran (fpm)                        | :white_check_mark: | [Fortran](./../../strategies/languages/fortran/fortran.md)       |
| R (renv)                             | :white_check_mark: | [Fortran](./../../strategies/languages/r/renv.md)                |
| Cocoapods                            | :warning:          | [CocoaPods](./../../strategies/platforms/ios/cocoapods.md)       |
| Nim (nimble)                         | :warning:          | [Nim](./../../strategies/languages/nim/nimble.md)                |
| Dart (pub)                           | :warning:          | [Dart](./../../strategies/languages/dart/pub.md)                 |
| Maven                                | :warning:          | [Maven](./../../strategies/languages/maven/maven.md)             |
| Java Jar Files                       | :white_check_mark: | [Container Jar Analysis](#container-jar-analysis)               |
| Golang (gomod)                       | :x:                | N/A                                                              |
| Rust (cargo)                         | :x:                | N/A                                                              |
| Haskell (cabal, stack)               | :x:                | N/A                                                              |
| Gradle                               | :x:                | N/A                                                              |
| Clojure (lein)                       | :x:                | N/A                                                              |
| Scala (sbt)                          | :x:                | N/A                                                              |
| Elixir                               | :x:                | N/A                                                              |
| Erlang                               | :x:                | N/A                                                              |

Where:
- :white_check_mark: - analysis is supported
- :warning: - partial analysis is supported. Refer to the linked strategy documentation for more details.
- :x: - analysis is not supported in container scanning.

### View detected projects

To view the list of projects detected within the container by the above strategies run `fossa container list-targets <ARG> `.
`ARG` must be the same value as provided to `fossa container analyze <ARG> ` or `fossa container test <ARG>`.

This output can be useful to understand what is going to be analyzed via `fossa container analyze <ARG> `,
and if desired can inform [analysis target configuration](../../files/fossa-yml.md#analysis-target-configuration).

#### Command output

Example output:
```bash
; fossa container list-targets ghcr.io/tcort/markdown-link-check:stable

[ INFO] Discovered image for: ghcr.io/tcort/markdown-link-check:stable (of 137610196 bytes) via docker engine api.
[ INFO] Exporting docker image to temp file: /private/var/folders/hb/pg5d0r196kq1qdswr6_79hzh0000gn/T/fossa-docker-engine-tmp-f7af2b5d1ec5173d/image.tar! This may take a while!
[ INFO] Listing targets exported docker archive: /private/var/folders/hb/pg5d0r196kq1qdswr6_79hzh0000gn/T/fossa-docker-engine-tmp-f7af2b5d1ec5173d/image.tar
[ WARN] fossa container list-targets does not apply any filtering, you may see projects which are not present in the final analysis.
[ WARN] fossa container list-targets only lists targets for experimental-scanner (when analyzed with flag).
[ INFO] Found project: apkdb@lib/apk/db/ (Base Layer)
[ INFO] Found target: apkdb@lib/apk/db/ (Base Layer)
[ INFO] Found project: setuptools@usr/local/lib/node_modules/npm/node_modules/node-gyp/gyp/ (Other Layers)
[ INFO] Found target: setuptools@usr/local/lib/node_modules/npm/node_modules/node-gyp/gyp/ (Other Layers)
[ INFO] Found project: npm@src/ (Other Layers)
[ INFO] Found project: npm@opt/yarn-v1.22.5/ (Other Layers)
[ INFO] Found target: npm@src/ (Other Layers)
[ INFO] Found target: npm@opt/yarn-v1.22.5/ (Other Layers)
[ INFO] Found project: apkdb@lib/apk/db/ (Other Layers)
[ INFO] Found target: apkdb@lib/apk/db/ (Other Layers)
```

### Utilize analysis target configuration

The container scanner supports configuring analysis targets via `.fossa.yml`, as with a standard `fossa analyze` command.
For more information on configuring analysis targets, see [analysis target configuration](../../files/fossa-yml.md#analysis-target-configuration).

For example, the following `fossa.yml` excludes all `setuptools` targets:
```yaml
exclude:
  - type: setuptools
```

### Debugging

`fossa-cli` supports the `--debug` flag and debug bundle generation with the container scanner.

```bash
fossa container analyze redis:alpine --debug

# Generates debug logs in the terminal.
# Writes the FOSSA debug bundle in the current working directory with the filename "fossa.container.debug.json.gz".
```

For best performance prefer analyzing exported docker archive file instead of downloading image from registry.
When building in CI, consider saving the image as an artifact and using that for analysis.
This improves performance by being able to skip downloading and extracting the image from a registry.

Images can be exported to archives using Docker:

```bash
docker pull <IMAGE>:<TAG> # or docker pull <IMAGE>@<DIGEST>
docker save <IMAGE>:<TAG> > image.tar

fossa container analyze image.tar --container scanner

rm image.tar
```

### Frequently Asked Questions (FAQs)

#### How do I scan multi-platform container images with `fossa-cli`?

By default when `fossa-cli` is analyzing multi-platform image it prefers using the same runtime architecture as the host.
If a specific platform is desired, use the digest for that platform:

```bash
fossa container analyze ghcr.io/graalvm/graalvm-ce@sha256:bdcba07acb11053fea0026b807ecf94550ace7df27b10596ca4c765165243cef
```

### How can I only scan for system dependencies (alpine, dpkg, rpm)?

Use the `--only-system-deps` option:

```bash
fossa container analyze <IMAGE> --only-system-deps
```

### How do I exclude specific projects from container scanning?

Use a FOSSA configuration file to perform exclusion of projects or paths.
Refer to the [configuration file](./../../files/fossa-yml.md) documentation for more details.

As an example, the following configuration file only analyzes `setuptools`, and `alpine` packages:

```yml
# filename: .fossa.yaml
version: 3

targets:
  only:
    - type: setuptools
    - type: apkdb
```

```bash
fossa container analyze <IMAGE> -c .fossa.config.yaml --output
```

## Limitations & Workarounds

`fossa-cli` does not support [v1 docker manifest format](https://docs.docker.com/registry/spec/manifest-v2-1/).
This manifest format is officially deprecated, but is still found in some registries.

The recommended workaround is to export the image to an archive, then analyze that:

```bash
docker pull quay.io/org/image:tag
docker save quay.io/org/image:tag > img.tar

fossa container analyze img.tar
rm img.tar
```

For guidance migrating off of the deprecated format, refer to Docker's [migration guide](https://docs.docker.com/registry/spec/deprecated-schema-v1/).
