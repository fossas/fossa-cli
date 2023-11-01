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

> Note: `fossa-cli` currently does not support exported OCI images (image archives in OCI format).
> For such scenarios, we recommend,
> you perform: `docker save <IMG>:<TAG> > img.tar`, and use `img.tar`
> for your analysis, with `fossa container analyze img.tar`

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
You can provide, `--only-system-deps` to only analyze dependencies originating from following package managers:

- dpkg
- rpm
- alpine

> Performing `fossa container analyze <IMAGE> --only-system-deps` will match the behavior of
> the previous FOSSA CLI container scanner (same as all CLI prior to v3.5.0)

You can refer to [scanner](./container/scanner.md) documentation, to learn
more about how FOSSA CLI performs scan on a container image.

## `fossa container test <ARG>`

Check for issues in a previously analyzed image.
Exits non-zero if issues are found.

For example:

```bash
fossa container test redis:alpine
```

To render results in JSON format: 

```bash
fossa container test redis:alpine --format json
```

## Printing results without uploading to FOSSA

The `--output` flag outputs dependency graph information to the terminal rather than uploading to FOSSA.

```sh
fossa container analyze redis:alpine --output
```

### F.A.Q.

1. How can I only scan system dependencies?

You can provide `--only-system-deps`, like following to scan only system dependencies.

```bash
fossa container analyze <IMAGE> --only-system-deps
```

2. How can I improve performance of fossa container scanning?

You can use docker-archive source with FOSSA CLI. This will reduce
time required to retrieve container image from registry or from docker engine.

```bash
# save archive of the image
docker save redis:alpine > redis.tar

# scan and test the image
fossa container analyze redis.tar
fossa container test redis.tar
```

3. How can I exclude certain projects or targets from container image?

You can use [fossa configuration](./../files/fossa-yml.md) file to exclude
specific directory or projects. 

Refer to [target exclusion walk-through](./../../concepts/analysis-and-analyzers.md).