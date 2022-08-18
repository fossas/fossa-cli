## `fossa container`

Fossa container subcommand lets you analyze and test container for vulnerabilities, and compliance issues. 

Fossa container has following options:

- `analyze`: Scans an container image
- `test`: Check for issues from FOSSA and exit non-zero when issues are found

### `fossa container analyze <Image>`

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

### `fossa container test <Image>`

Check for issues from FOSSA and exit non-zero when issues are found.

For example:

```bash
fossa container test redis:alpine
```

### Printing results without uploading to FOSSA

The `--output` flag can be used to print projects and dependency graph information to stdout, rather than uploading to FOSSA

```sh
fossa container analyze redis:alpine --output
```

### Experimental Options

_Important: For support and other general information, refer to the [experimental options overview](../experimental/README.md) before using experimental options._

| Name                     | Description                                                       |
| ------------------------ | ----------------------------------------------------------------- |
| `--experimental-scanner` | Uses native container scanner which has better debugging support. |
