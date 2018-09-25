SHELL=/bin/bash -o pipefail
BIN=$(shell go env GOPATH)/bin

DEP=$(BIN)/dep
GO_BINDATA=$(BIN)/go-bindata
GENNY=$(BIN)/genny
GO_JUNIT_REPORT=$(BIN)/go-junit-report
GOVERALLS=$(BIN)/goveralls

IMAGE?=buildtools

GORELEASER_FLAGS?=--rm-dist
LDFLAGS:=-ldflags '-extldflags "-static" -X github.com/fossas/fossa-cli/cmd/fossa/version.version=$(shell git rev-parse --abbrev-ref HEAD) -X github.com/fossas/fossa-cli/cmd/fossa/version.commit=$(shell git rev-parse HEAD) -X "github.com/fossas/fossa-cli/cmd/fossa/version.goversion=$(shell go version)" -X github.com/fossas/fossa-cli/cmd/fossa/version.buildType=development'

all: build

# Various required tools.
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

# Building the CLI.
.PHONY: build
build: $(BIN)/fossa

$(BIN)/fossa: $(GO_BINDATA) $(GENNY)
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
		fossa/fossa-cli-$(IMAGE) /bin/bash

.PHONY: dev-osx
dev-osx: docker-$(IMAGE)
	docker run --rm -it \
		-v $$GOPATH/src/github.com/fossas/fossa-cli:/home/fossa/go/src/github.com/fossas/fossa-cli \
		# We don't mount the $GOPATH/bin because the host machine's binaries are
		# compiled for Darwin and won't run on Docker (Linux).
		fossa/fossa-cli-$(IMAGE) /bin/bash

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
	# make integration-test

.PHONY: unit-test
unit-test:
	go test -short ./...

.PHONY: junit-test
junit-test: $(GO_JUNIT_REPORT) $(GOVERALLS)
	goveralls -v -service=circle-ci -repotoken=$(COVERALLS_TOKEN) | go-junit-report

.PHONY: integration-test
integration-test: docker-test
	echo "native integration tests are a WIP"
	# sudo docker run --rm -it quay.io/fossa/fossa-cli-test

# Release tasks.
.PHONY: prepare-release
prepare-release:
	if [ -z "$$RELEASE" ]; then exit 1; fi
	mv install.sh install.prev.sh
	make -s installer > install.sh
	git add install.sh
	git commit -m "release($(RELEASE)): Release version $(RELEASE)" || make abort-release
	git tag $(RELEASE) || (git reset HEAD^ && make abort-release)
	rm install.prev.sh

.PHONY: release
release:
	if [ -z "$$(git tag -l --points-at HEAD)" ]; then exit 1; fi
	[ "$$(grep "^  RELEASE='$$(git tag -l --points-at HEAD)'$$" install.sh | wc -l)" = "1" ]
	GOVERSION=$$(go version) goreleaser $(GORELEASER_FLAGS)

.PHONY: abort-release
abort-release:
	git tag -d $(RELEASE)
	rm install.sh
	mv install.prev.sh install.sh
	git reset HEAD .
	exit 1

.PHONY: installer
installer:
	sed "s/# RELEASE=/RELEASE=\'$(RELEASE)\'/" install_template.sh | sed "10s;^;# THIS FILE WAS AUTOMATICALLY GENERATED ON $(shell date) FROM install_template.sh. DO NOT EDIT.\n;"
