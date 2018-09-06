SHELL=/bin/bash -o pipefail
BIN="$(shell go env GOPATH)/bin"
DEP="$(BIN)/dep"
PREFIX?=/usr/local/bin

GO_BINDATA="$(BIN)/go-bindata"
GENNY="$(BIN)/genny"
GO_JUNIT_REPORT="$(BIN)/go-junit-report"

GORELEASER_FLAGS?=--rm-dist
LDFLAGS:=-ldflags '-extldflags "-static" -X github.com/fossas/fossa-cli/cmd/fossa/version.version=$(shell git rev-parse --abbrev-ref HEAD) -X github.com/fossas/fossa-cli/cmd/fossa/version.commit=$(shell git rev-parse HEAD) -X "github.com/fossas/fossa-cli/cmd/fossa/version.goversion=$(shell go version)" -X github.com/fossas/fossa-cli/cmd/fossa/version.buildType=development'

all: build

# Various required tools.
$(DEP):
	[ -f $@ ] || go get -u github.com/golang/dep/cmd/dep

$(GO_BINDATA):
	[ -f $@ ] || go get -u github.com/go-bindata/go-bindata/...

$(GENNY):
	[ -f $@ ] || go get -u github.com/cheekybits/genny

$(GO_JUNIT_REPORT):
	[ -f $@ ] || go get -u github.com/jstemmer/go-junit-report

# Building the CLI.
.PHONY: build
build: $(BIN)/fossa

$(BIN)/fossa: $(GO_BINDATA) $(GENNY)
	go generate ./...
	go build -o $@ $(LDFLAGS) github.com/fossas/fossa-cli/cmd/fossa

$(PREFIX)/fossa: $(BIN)/fossa
	mv $< $@

# Building various Docker images.
docker-base: ./docker/base/Dockerfile
	sudo docker build -t quay.io/fossa/fossa-cli-base -f ./docker/base/Dockerfile .

docker-test-base: docker-base ./docker/test-base/Dockerfile
	sudo docker build -t quay.io/fossa/fossa-cli-test-base -f ./docker/test-base/Dockerfile .

docker: docker-base ./docker/cli/Dockerfile
	sudo docker build -t quay.io/fossa/fossa-cli -f ./docker/cli/Dockerfile .

docker-test: docker-test-base ./docker/test/Dockerfile
	sudo docker build -t quay.io/fossa/fossa-cli-test -f ./docker/test/Dockerfile .

# Development tasks.
.PHONY: dev
dev: docker-test-base
	sudo docker run --rm -it \
		-v $$GOPATH/src/github.com/fossas/fossa-cli:/home/fossa/go/src/github.com/fossas/fossa-cli \
		-v $$GOPATH/bin:/home/fossa/go/bin \
		quay.io/fossa/fossa-cli-test-base /bin/bash

.PHONY: dev-mac
dev-mac: docker-test-base
	docker run --rm -it \
		-v $$GOPATH/src/github.com/fossas/fossa-cli:/home/fossa/go/src/github.com/fossas/fossa-cli \
		quay.io/fossa/fossa-cli-test-base /bin/bash

.PHONY: install
install: $(PREFIX)/fossa

.PHONY: uninstall
uninstall:
	rm $(PREFIX)/fossa

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
	go test ./...

.PHONY: junit-test
junit-test: $(GO_JUNIT_REPORT)
	go test -v ./... | go-junit-report

.PHONY: integration-test
integration-test: docker-test
	sudo docker run --rm -it quay.io/fossa/fossa-cli-test

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
