BIN="$(shell go env GOPATH)/bin"
DEP="$(BIN)/dep"
GO_BINDATA="$(BIN)/go-bindata"
GENNY="$(BIN)/genny"
PREFIX?=/usr/local/bin
LDFLAGS:=-ldflags '-extldflags "-static" -X github.com/fossas/fossa-cli/cmd/fossa/version.version=$(shell git rev-parse --abbrev-ref HEAD) -X github.com/fossas/fossa-cli/cmd/fossa/version.commit=$(shell git rev-parse HEAD) -X "github.com/fossas/fossa-cli/cmd/fossa/version.goversion=$(shell go version)" -X github.com/fossas/fossa-cli/cmd/fossa/version.buildType=development'

all: build

# Various required tools.
$(DEP):
	[ -f $@ ] || go get -u github.com/golang/dep/cmd/dep

$(GO_BINDATA):
	[ -f $@ ] || go get -u github.com/go-bindata/go-bindata/...

$(GENNY):
	[ -f $@ ] || go get -u github.com/cheekybits/genny

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

# Useful build tasks.
.PHONY: test
test:
	make unit-test
	make acceptance-test

.PHONY: unit-test
unit-test:
	go test ./...

.PHONY: acceptance-test
acceptance-test: docker-test
	sudo docker run --rm -it quay.io/fossa/fossa-cli-test

.PHONY: dev
dev: docker-test-base
	sudo docker run --rm -it \
		-v $$GOPATH/src/github.com/fossas/fossa-cli:/home/fossa/go/src/github.com/fossas/fossa-cli \
		-v $$GOPATH/bin:/home/fossa/go/bin \
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

.PHONY: release
release:
	GOVERSION=$$(go version) goreleaser --rm-dist
	RELEASE=$(git tag -l --points-at HEAD)
	VERSION=${RELEASE#v}
	sed "s/# RELEASE=/RELEASE=\'$RELEASE\'/; s/# VERSION=/VERSION=\'$VERSION\'/" install_tpl.sh > install.sh
