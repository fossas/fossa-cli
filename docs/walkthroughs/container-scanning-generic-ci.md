# Integrating Container Scanning in CI

- [Integrating container scanning in CI](#integrating-container-scanning-in-ci)
  - [Analyze](#analyze)
  - [Test](#test)
  - [Example](#example)

**Scenario:**

As a development team we want to analyze container image for vulnerability and compliance issues as part of the CI process.

## Analyze

Use the `fossa container analyze <IMAGE>` command.

With Docker:

```bash
docker build . -t <IMAGE>
fossa container analyze <IMAGE>

# Alternately, export the image to an archive and analyze that for maximal performance. 
#
# >> docker save -o image.tar <IMAGE>
# >> fossa container analyze image.tar
```

With Podman:

```bash
podman build . -t <IMAGE>
fossa container analyze <IMAGE>

# Alternately, export the image to an archive and analyze that for maximal performance. 
#
# >> podman save <IMAGE> --format docker-archive -o image.tar 
# >> fossa container analyze image.tar
```

With Buildah:

```bash
buildah bud --format=docker -f Dockerfile -t <IMAGE> .
buildah push <IMAGE> docker-archive:image.tar

fossa container analyze image.tar
```

## Test

Use the `fossa container test <IMAGE>` command to test a previously analyzed image.
This command exits with a non-zero exit code if issues are discovered.

```bash
fossa container test <IMAGE>
```

## Example

**Prerequisite**

Dockerfile:
```Dockerfile
FROM alpine:latest

RUN apk add tree python3 py3-pip
COPY ./app ./app

RUN pip install -r ./app/reqs.txt
```

`app/reqs.txt`:
```text
# app/reqs.txt
flask
```

**With Docker**

```bash
VERSION="1.0.0" # or git commit hash: VERSION=$(git log -1 --pretty=%h)
REPO="core-app:"
TAG="$REPO$VERSION"
BUILD_TIMESTAMP=$( date '+%F_%H:%M:%S' )

# Build
docker build -t "$TAG" -build-arg VERSION="$VERSION" --build-arg BUILD_TIMESTAMP="$BUILD_TIMESTAMP" . 

# Analyze and test image for security and compliance issues
fossa container analyze "$TAG"
fossa container test "$TAG"

# Push image to registry
docker push "$TAG"
```

**With Podman**

```bash
VERSION="1.0.0" # or git commit hash: VERSION=$(git log -1 --pretty=%h)
REPO="core-app:"
TAG="$REPO$VERSION"
BUILD_TIMESTAMP=$( date '+%F_%H:%M:%S' )

# Build
podman build . -t "$TAG" --build-arg VERSION="$VERSION" --build-arg BUILD_TIMESTAMP="$BUILD_TIMESTAMP" . 

# Analyze and test image for security and compliance issues
fossa container analyze "$TAG"
fossa container test "$TAG"

# Push image to registry
podman push "$TAG"
```

**With Buildah**

```bash
VERSION="1.0.0" # or git commit hash: VERSION=$(git log -1 --pretty=%h)
REPO="core-app:"
TAG="$REPO$VERSION"
BUILD_TIMESTAMP=$( date '+%F_%H:%M:%S' )

# Build
buildah bud --format=docker -f Dockerfile --build-arg VERSION="$VERSION" --build-arg BUILD_TIMESTAMP="$BUILD_TIMESTAMP" -t "$TAG" .
buildah push "$TAG" docker-archive:image.tar

# Analyze and test image for security and compliance issues
fossa container analyze image.tar
fossa container test image.tar
rm image.tar
```
