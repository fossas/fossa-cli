# `fossa container`

The `fossa container` subcommand supports analysis and testing of containers for vulnerabilities and compliance issues.

`fossa container` supports following subcommands:

- `analyze`: Scan a container image
- `test`: Check for issues in a previously scanned container image

## `fossa container analyze <ARG>`

`fossa container analyze` scans container images from:

1) Docker archive, e.g. `docker save redis:alpine > redis.tar`
2) Docker Engine (accessed via unix socket `/var/lib/docker.sock`)
3) OCI Registry

No arguments are required to specify the kind of image being analyzed:
`fossa-cli` automatically identifies the appropriate image source.

For example:

```bash
# Local image via Docker Engine
#
# Note: when pulling Docker Engine, the tag is required;
# otherwise the image is inferred to be `docker.io/library/<image>`.
fossa container analyze alpine:3.16.0

# Infers to `docker.io/library/debian:latest` and pulls the image from there.
fossa container analyze debian

# Tries to use the local Docker Engine, but if that fails
# infers to `docker.io/library/debian:latest`
# and pulls the image from there.
fossa container analyze debian:latest

# Explicit remote image with a host and namespace
fossa container analyze cgr.dev/chainguard/wolfi-base:latest

# Exported container image
# `docker save redis:latest > redis.tar`
fossa container analyze redis.tar
```

You can provide `--only-system-deps` to only analyze dependencies originating from following system package managers:

- dpkg
- rpm
- alpine

You can refer to [scanner](./container/scanner.md) documentation, to learn
more about how FOSSA CLI performs scan on a container image.

> [!NOTE]
>
> FOSSA CLI uses [`circe`](https://github.com/fossas/circe), another binary
> maintained by FOSSA, to download and export container images.
>
> For more details on how this works refer to the Circe documentation;
> you can also always try running `circe` directly if you encounter any issues.

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

## Ignore default filters

Default filters are filters which `fossa-cli` applies by default. These filters,
provide sensible non-production target exclusion. As `fossa-cli` relies on manifest and lock files provided in the project's directory,
default filters, intentionally skip `node_modules/` and such directories. If `fossa-cli` discovers and
analyzes project found in `node_modules/`: `fossa-cli` will not be able to infer
the dependency's scope (development or production) and may double count dependencies.

Specifically, `fossa-cli` by default skips any targets found within the following directories:

- `dist-newstyle`
- `doc/`
- `docs/`
- `test/`
- `tests/`
- `example/`
- `examples/`
- `vendor/`
- `node_modules/`
- `.srclib-cache/`
- `spec/`
- `Godeps/`
- `.git/`
- `bower_components/`
- `third_party/`
- `third-party/`
- `Carthage/`
- `Checkouts/`

To disable default filters, provide `--without-default-filters` flag when performing `fossa container analyze`. Currently,
it is not possible to disable only a subset of default filters. If you would like to only apply a subset of default filters, you can
use `--without-default-filters` in conjunction with [exclusion filters](./../files/fossa-yml.md#analysis-target-configuration). Refer to
[exclusion filters walkthough](../../walkthroughs/analysis-target-configuration.md) for example on how to apply path and target exclusion filters.

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
