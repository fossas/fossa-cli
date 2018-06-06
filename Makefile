BIN="$(shell go env GOPATH)/bin"
DEP="$(BIN)/dep"
GO_BINDATA="$(BIN)/go-bindata"
STRINGER="$(BIN)/stringer"
PREFIX?=/usr/local/bin
LDFLAGS:=-ldflags '-X github.com/fossas/fossa-cli/cmd/fossa/version.version=$(shell git rev-parse --abbrev-ref HEAD) -X github.com/fossas/fossa-cli/cmd/fossa/version.commit=$(shell git rev-parse HEAD) -X "github.com/fossas/fossa-cli/cmd/fossa/version.goversion=$(shell go version)" -X github.com/fossas/fossa-cli/cmd/fossa/version.buildType=development'

all: build

$(DEP):
	go get github.com/golang/dep/cmd/dep

$(GO_BINDATA):
	go get github.com/go-bindata/go-bindata/...

$(STRINGER):
	go get golang.org/x/tools/cmd/stringer

.PHONY: build
build: $(BIN)/fossa

$(BIN)/fossa: $(GO_BINDATA) $(STRINGER)
	go generate ./...
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
