# Makefile variables.
SHELL=/bin/bash -o pipefail
BIN=$(shell go env GOPATH)/bin

## Build tools.
DEP=$(BIN)/dep
GO_BINDATA=$(BIN)/go-bindata
GENNY=$(BIN)/genny

## Test tools.
GO_JUNIT_REPORT=$(BIN)/go-junit-report
GOVERALLS=$(BIN)/goveralls

## Release tools.
GORELEASER=$(BIN)/goreleaser
GODOWNLOADER=$(BIN)/godownloader

## Configurations.
IMAGE?=buildtools
GORELEASER_FLAGS?=--rm-dist
LDFLAGS:=-ldflags '-extldflags "-static" -X github.com/fossas/fossa-cli/cmd/fossa/version.version=$(shell git rev-parse --abbrev-ref HEAD) -X github.com/fossas/fossa-cli/cmd/fossa/version.commit=$(shell git rev-parse HEAD) -X "github.com/fossas/fossa-cli/cmd/fossa/version.goversion=$(shell go version)" -X github.com/fossas/fossa-cli/cmd/fossa/version.buildType=development'

all: build

# Installing tools.
$(DEP):
	curl https://raw.githubusercontent.com/golang/dep/master/install.sh | sh

$(GO_BINDATA):
	go get -u -v github.com/go-bindata/go-bindata/...

$(GENNY):
	go get -u -v github.com/cheekybits/genny

$(GO_JUNIT_REPORT):
	go get -u -v github.com/jstemmer/go-junit-report

$(GOVERALLS):
	go get -u -v github.com/mattn/goveralls

$(GORELEASER):
	go get -d github.com/goreleaser/goreleaser
	cd $$GOPATH/src/github.com/goreleaser/goreleaser
	dep ensure -vendor-only
	go install github.com/goreleaser/goreleaser

$(GODOWNLOADER):
	go get -d github.com/goreleaser/godownloader
	cd $$GOPATH/src/github.com/goreleaser/godownloader
	dep ensure -vendor-only
	go install github.com/goreleaser/godownloader

# Building the CLI.
.PHONY: build
build: $(BIN)/fossa

$(BIN)/fossa: $(GO_BINDATA) $(GENNY) $(DEP)
	dep check
	go generate ./...
	go build -o $@ $(LDFLAGS) github.com/fossas/fossa-cli/cmd/fossa

# Building various Docker images.
.PHONY:
docker-base: ./docker/base/Dockerfile
	sudo docker build -t fossa/fossa-cli:base -f ./docker/base/Dockerfile .

.PHONY:
docker-buildtools: docker-base ./docker/buildtools/Dockerfile
	sudo docker build -t fossa/fossa-cli:buildtools -f ./docker/buildtools/Dockerfile .

## TODO: we will deprecate this image once native integration tests are
## completely ready.
.PHONY:
docker-fixtures: docker-buildtools ./docker/fixtures/Dockerfile
	sudo docker build -t fossa/fossa-cli:fixtures -f ./docker/fixtures/Dockerfile .

# Development tasks.
.PHONY: dev
dev: docker-$(IMAGE)
	sudo docker run --rm -it \
		-v $$GOPATH/src/github.com/fossas/fossa-cli:/home/fossa/go/src/github.com/fossas/fossa-cli \
		-v $$GOPATH/bin:/home/fossa/go/bin \
		fossa/fossa-cli:$(IMAGE) /bin/bash

# We don't mount the $GOPATH/bin because the host machine's binaries are
# compiled for Darwin and won't run on Docker (Linux).
.PHONY: dev-osx
dev-osx: docker-$(IMAGE)
	docker run --rm -it \
		-v $$GOPATH/src/github.com/fossas/fossa-cli:/home/fossa/go/src/github.com/fossas/fossa-cli \
		fossa/fossa-cli:$(IMAGE) /bin/bash

.PHONY: vendor
vendor: $(DEP)
	$< ensure -v

.PHONY: clean
clean:
	rm -f $(BIN)/fossa
	find -name *_generated.go | grep -v vendor | xargs rm -f

# Testing tasks.
.PHONY: test
test:
	make unit-test
	make integration-test

.PHONY: unit-test
unit-test:
	go test -short ./...

.PHONY: ci-unit-test
ci-unit-test: $(GO_JUNIT_REPORT) $(GOVERALLS)
	if [ -z "$${COVERALLS_TOKEN}" ]; then \
		go test -short -v ./... | go-junit-report; \
	else \
		goveralls -v -service=circle-ci -repotoken=$(COVERALLS_TOKEN) -flags "-short" | go-junit-report; \
	fi

.PHONY: integration-test
integration-test:
	# Ensure the binary is recompiled before every test.
	make
	go test ./...

.PHONY: ci-integration-test
ci-integration-test:
	# Ensure the binary is recompiled before every test.
	make
	if [ -z "$${COVERALLS_TOKEN}" ]; then \
		go test -v ./... | go-junit-report; \
	else \
		goveralls -v -service=circle-ci -repotoken=$(COVERALLS_TOKEN) | go-junit-report; \
	fi

# Release tasks.
install.sh: $(GODOWNLOADER)
	# 1. Set default installation location to /usr/local/bin.
	# 2. Use default permissions for /usr/local/bin.
	# 3. Try `sudo install` when `install` fails.
	godownloader --repo=fossas/fossa-cli \
		| sed 's/\.\/bin/\/usr\/local\/bin/' \
		| sed 's/install -d/install -d -m 775/' \
		| sed 's/install "$${srcdir}\/$${binexe}" "$${BINDIR}\/"/install "$${srcdir}\/$${binexe}" "$${BINDIR}\/" 2> \/dev\/null || sudo install "$${srcdir}\/$${binexe}" "$${BINDIR}\/"/' \
		> install.sh

.PHONY: release
release: $(GORELEASER) install.sh
	GOVERSION=$$(go version) goreleaser $(GORELEASER_FLAGS)
