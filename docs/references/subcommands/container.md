# `fossa container`

The `fossa container` subcommand supports analysis and testing of containers for vulnerabilities and compliance issues. 

`fossa container` supports following subcommands:

- `analyze`: Scan a container image
- `test`: Check for issues in a previously scanned container image

## `fossa container analyze <ARG>`

`fossa container analyze` scans container images from:

1) Docker archive
2) Docker Engine (accessed via unix socket `/var/lib/docker.sock`)
3) OCI Registry

No arguments are required to specify the kind of image being analyzed:
`fossa-cli` automatically identifies the appropriate image source.

For example:

```bash
# Exported container image in archive format (via `docker save redis:alpine > redis.tar`)
fossa container analyze redis.tar

# Local image (via Docker engine)
fossa container analyze redis:alpine

# Resolved from hub.docker.com/_/debian
fossa container analyze debian

# Explicit remote image via docker.your-org.com
fossa container analyze docker.your-org.com/project/image
```

## `fossa container test <ARG>`

Check for issues in a previously analyzed image.
Exits non-zero if issues are found.

For example:

```bash
fossa container test redis:alpine
```

## `fossa container list-targets <ARG>`

Lists all detected analysis targets in an image.
This output can be useful to understand what is going to be analyzed via `fossa container analyze <ARG> --experimental-scanner`,
and if desired can inform [analysis target configuration](../files/fossa-yml.md#analysis-target-configuration).

### Command output

Example output:
```bash
; fossa container list-targets ghcr.io/tcort/markdown-link-check:stable  

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

Analysis targets can be configured via `.fossa.yml`.
The example above shows the project `setuptools@usr/local/lib/node_modules/npm/node_modules/node-gyp/gyp/`.

If this is not part of the production release it can be excluded from analysis:

```yaml
exclude:
  - type: setuptools
```

Adding this section to the configuration file at the root of the project ensures that when running
`fossa container analyze <ARG> --experimental-scanner` the `setuptools` target is skipped.

### Printing results without uploading to FOSSA

The `--output` flag outputs dependency graph information to the terminal rather than uploading to FOSSA.

```sh
fossa container analyze redis:alpine --output
```

## Experimental Options

_Important: For support and other general information, refer to the [experimental options overview](../experimental/README.md) before using experimental options._

| Name                     | Description                                                                                                       |
|--------------------------|-------------------------------------------------------------------------------------------------------------------|
| `--experimental-scanner` | Provides many enhancements to container scanning ([reference](../experimental/container/experimenal-scanner.md)). |
