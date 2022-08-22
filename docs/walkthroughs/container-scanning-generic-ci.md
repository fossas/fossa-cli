# Integrating Container Scanning in CI

- [Integrating Container Scanning in CI](#integrating-container-scanning-in-ci)
  - [Analyze](#analyze)
  - [Test](#test)
  - [Example](#example)

**Scenario:**

As a development team, we want to analyze container image for vulnerability, 
and compliance issues, as part of the CI process.

## Analyze

For this, we can use `fossa container analyze <IMAGE>` command. 

For CI with docker executable,

```bash
docker build . -t <IMAGE>
fossa container analyze <IMAGE>

# You can also export image in the docker, to perform 
# on the exported image. 
#
# >> docker save -o image.tar <IMAGE>
# >> fossa container analyze image.tar
```

If you are using podman, 

```bash
podman build . -t <IMAGE>
fossa container analyze <IMAGE>

# You can also export image in the podman, to perform 
# on the exported image. 
#
# >> podman save <IMAGE> --format docker-archive -o image.tar 
# >> fossa container analyze image.tar
```

If you are using buildah, 

```bash
buildah bud --format=docker -f Dockerfile -t <IMAGE> .
buildah push <IMAGE> docker-archive:image.tar

fossa container analyze image.tar
```

## Test

For this, we can use `fossa container test <IMAGE>` command. This command, 
will exit with non 0 exit code if there are issues discovered.

```bash
fossa container test <IMAGE>
```

## Example

**Prerequisite**

Our Dockerfile is:
```Dockerfile
FROM alpine:latest

RUN apk add tree python3 py3-pip
COPY ./app ./app

RUN pip install -r ./app/reqs.txt
```

Our `app/reqs.txt` file is:
```text
# app/reqs.txt
flask
```

**With Docker**

```bash
VERSION="1.0.0" # to use git commit hash use: VERSION=$(git log -1 --pretty=%h)
REPO="core-app:"
TAG="$REPO$VERSION"
BUILD_TIMESTAMP=$( date '+%F_%H:%M:%S' )

# Build & Analyze
docker build -t "$TAG" -build-arg VERSION="$VERSION" --build-arg BUILD_TIMESTAMP="$BUILD_TIMESTAMP" . 

# Analyze and Test Image for security and compliance issues
fossa container analyze "$TAG"
fossa container test "$TAG"

# Push image to registry
docker push "$TAG"
```

**With Podman**

```bash
VERSION="1.0.0" # to use git commit hash use: VERSION=$(git log -1 --pretty=%h)
REPO="core-app:"
TAG="$REPO$VERSION"
BUILD_TIMESTAMP=$( date '+%F_%H:%M:%S' )

podman build . -t "$TAG" --build-arg VERSION="$VERSION" --build-arg BUILD_TIMESTAMP="$BUILD_TIMESTAMP" . 

# Analyze and Test Image for security and compliance issues
fossa container analyze "$TAG"
fossa container test "$TAG"

# Push image to registry
podman push "$TAG"
```

**With Buildah**

```bash
VERSION="1.0.0" # to use git commit hash use: VERSION=$(git log -1 --pretty=%h)
REPO="core-app:"
TAG="$REPO$VERSION"
BUILD_TIMESTAMP=$( date '+%F_%H:%M:%S' )

buildah bud --format=docker -f Dockerfile --build-arg VERSION="$VERSION" --build-arg BUILD_TIMESTAMP="$BUILD_TIMESTAMP" -t "$TAG" .
buildah push "$TAG" docker-archive:image.tar

# Analyze and Test Image for security and compliance issues
fossa container analyze image.tar
fossa container test image.tar
rm image.tar
```