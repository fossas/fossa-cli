BIN="$(shell go env GOPATH)/bin"
DEP="$(BIN)/dep"
GO_BINDATA="$(BIN)/go-bindata"
PREFIX?=/usr/local/bin
LDFLAGS:=-ldflags '-X github.com/fossas/fossa-cli/cmd/fossa/version.version=$(shell git rev-parse --abbrev-ref HEAD) -X github.com/fossas/fossa-cli/cmd/fossa/version.commit=$(shell git rev-parse HEAD) -X "github.com/fossas/fossa-cli/cmd/fossa/version.goversion=$(shell go version)" -X github.com/fossas/fossa-cli/cmd/fossa/version.buildType=development'

all: build

$(DEP): ## Grab golang/dep utility
	go get github.com/golang/dep/cmd/dep

$(GO_BINDATA):
	go get -u github.com/go-bindata/go-bindata/...

.PHONY: build
build: $(BIN)/fossa

$(BIN)/fossa: $(GO_BINDATA)
	mkdir -p $$(dirname $@)
	$< -pkg bindata -o builders/bindata/bindata.go builders/python/bindata/pipdeptree.py
	go build -o $@ $(LDFLAGS) github.com/fossas/fossa-cli/cmd/fossa

$(PREFIX)/fossa: $(BIN)/fossa
	mv $< $@

vendor: $(DEP)
	$< ensure -v

.PHONY: install
install: $(PREFIX)/fossa

.PHONY: uninstall
uninstall:
	rm $(PREFIX)/fossa

.PHONY: clean
clean:
	rm -f $(BIN)/fossa

# TODO: release task that builds and deploys in the Dockerfile?
# TODO: test task that runs in Dockerfile?