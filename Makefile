BIN="$(shell go env GOPATH)/bin"
DEP="$(BIN)/dep"
GO_BINDATA="$(BIN)/go-bindata"
STRINGER="$(BIN)/stringer"
PREFIX?=/usr/local/bin
LDFLAGS:=-ldflags '-X github.com/fossas/fossa-cli/cmd/fossa/version.version=$(shell git rev-parse --abbrev-ref HEAD) -X github.com/fossas/fossa-cli/cmd/fossa/version.commit=$(shell git rev-parse HEAD) -X "github.com/fossas/fossa-cli/cmd/fossa/version.goversion=$(shell go version)" -X github.com/fossas/fossa-cli/cmd/fossa/version.buildType=development'

all: build

# Various required tools.
$(DEP):
	[ -f $@ ] || go get -u github.com/golang/dep/cmd/dep

$(GO_BINDATA):
	[ -f $@ ] || go get -u github.com/go-bindata/go-bindata/...

$(STRINGER):
	[ -f $@ ] || go get -u golang.org/x/tools/cmd/stringer

# Building the CLI.
.PHONY: build
build: $(BIN)/fossa

$(BIN)/fossa: $(GO_BINDATA) $(STRINGER)
	go generate ./...
	go build -o $@ $(LDFLAGS) github.com/fossas/fossa-cli/cmd/fossa

$(PREFIX)/fossa: $(BIN)/fossa
	mv $< $@

# Building various Docker images.
.PHONY: docker-base
docker-base: ./docker/base/Dockerfile
	sudo docker build -t quay.io/fossa/fossa-cli-base -f ./docker/base/Dockerfile .

.PHONY: docker
docker-devel: docker-base ./docker/devel/Dockerfile
	sudo docker build -t fossa-cli -f ./docker/devel/Dockerfile .
	sudo docker tag fossa-cli quay.io/fossa/fossa-cli

.PHONY: docker-test
docker-test: docker-base ./docker/test/Dockerfile
	sudo docker build -t fossa-cli-test -f ./docker/test/Dockerfile .
	sudo docker tag fossa-cli-test quay.io/fossa/fossa-cli-test

# Useful build tasks.
.PHONY: install
install: $(PREFIX)/fossa

.PHONY: uninstall
uninstall:
	rm $(PREFIX)/fossa

vendor: $(DEP)
	$< ensure -v

.PHONY: clean
clean:
	rm -f $(BIN)/fossa

.PHONY: release
release:
	GOVERSION=$(go version) goreleaser --rm-dist
