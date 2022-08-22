# `fossa container`

Fossa container subcommand lets you analyze and test container for vulnerabilities, and compliance issues. 

Fossa container has following options:

- `analyze`: Scans an container image
- `test`: Check for issues from FOSSA and exit non-zero when issues are found

## `fossa container analyze <Image>`

Fossa container analyze, can scan container image from,

1) Docker archive
2) Docker Engine (accessed via unix socket /var/lib/docker.sock)
3) OCI Registry

No arguments are required to specify which kind of image you are using. Fossa will 
automatically identifies accessible image source.

For example:

```bash

# Exported Container Image in Tarball format (done via docker save redis:alpine > redis.tar)
fossa container analyze redis.tar

# Local Image (accessed via docker engine)
fossa container analyze fossa

# Resolved from hub.docker.com/_/debian
fossa container analyze debian

# Explicit remote image via docker.fossa.com
fossa container analyze docker.fossa.com/fossa/fossa
```

## `fossa container test <Image>`

Check for issues from FOSSA and exit non-zero when issues are found.

For example:

```bash
fossa container test redis:alpine
```

## `fossa container list-targets <Image>`

The list targets command lists all valid analysis targets in a directory. This output can 
be useful to understand what is going to be analyzed when `fossa container analyze <Image> --experimental-scanner` 
is run. `list-targets` can be also be used with [analysis target configuration](../files/fossa-yml.md#analysis-target-configuration) 
to limit what is ultimately analyzed.

### Command output

Example output
```bash
➜  fossa-cli git:(feat/complete-container-scanning-docs) ✗ ./fossa container list-targets ghcr.io/tcort/markdown-link-check:stable  

[ INFO] Discovered image for: ghcr.io/tcort/markdown-link-check:stable (of 137610196 bytes) via docker engine api.
[ INFO] Exporting docker image to temp file: /private/var/folders/hb/pg5d0r196kq1qdswr6_79hzh0000gn/T/fossa-docker-engine-tmp-f7af2b5d1ec5173d/image.tar! This may take a while!
[ INFO] Listing targets exported docker archive: /private/var/folders/hb/pg5d0r196kq1qdswr6_79hzh0000gn/T/fossa-docker-engine-tmp-f7af2b5d1ec5173d/image.tar
[ WARN] fossa container list-targets does not apply any filtering, you may see projects which are not present in the final analysis.
[ WARN] fossa container list-targets only lists targets for experimental-scanner (when analyzed with --experimental-scanner flag).
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

#### Utilizing [analysis target configuration](../files/fossa-yml.md#analysis-target-configuration)

Analysis target configuration through the fossa configuration file allows users to manually determine what they would like to analyze. Looking at the example above, if we know that the dependencies for the `setuptools@usr/local/lib/node_modules/npm/node_modules/node-gyp/gyp/` target are not part of the production release, we can exclude it from the analysis. Example exclusion format:

```yaml
exclude:
  - type: setuptools
```

Adding this section to your configuration file at the root of your project will ensure that when `fossa container analyze <IMAGE> --experimental-scanner` 
is run, the `setuptools` target is skipped.

### Printing results without uploading to FOSSA

The `--output` flag can be used to print projects and dependency graph information to stdout, rather than uploading to FOSSA

```sh
fossa container analyze redis:alpine --output
```

## Experimental Options

_Important: For support and other general information, refer to the [experimental options overview](../experimental/README.md) before using experimental options._

| Name                     | Description                                                       |
| ------------------------ | ----------------------------------------------------------------- |
| `--experimental-scanner` | Uses native container scanner which has better debugging support. |
