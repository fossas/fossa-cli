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

## Printing results without uploading to FOSSA

The `--output` flag outputs dependency graph information to the terminal rather than uploading to FOSSA.

```sh
fossa container analyze redis:alpine --output
```

## Experimental Options

_Important: For support and other general information, refer to the [experimental options overview](../experimental/README.md) before using experimental options._

| Name                     | Description                                                                                                       |
|--------------------------|-------------------------------------------------------------------------------------------------------------------|
| `--experimental-scanner` | Provides many enhancements to container scanning ([reference](../experimental/container/experimental-scanner.md)). |
