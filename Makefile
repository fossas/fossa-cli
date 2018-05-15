BIN="$(shell go env GOPATH)/bin"
DEP="$(BIN)/dep"
PREFIX?=/usr/local/bin
LDFLAGS:=-ldflags '-X github.com/fossas/fossa-cli/cmd/fossa/version.Version=$(shell git rev-parse --abbrev-ref HEAD) -X github.com/fossas/fossa-cli/cmd/fossa/version.Commit=$(shell git rev-parse HEAD) -X "github.com/fossas/fossa-cli/cmd/fossa/version.GoVersion=$(shell go version)" -X github.com/fossas/fossa-cli/cmd/fossa/version.BuildType=development'

all: build

$(DEP): ## Grab golang/dep utility
	go get github.com/golang/dep/cmd/dep

.PHONY: build
build: $(BIN)/fossa

$(BIN)/fossa:
	mkdir -p $$(dirname $@)
	# TODO: detect and install go-bindata if missing
	go-bindata -pkg bindata -o builders/bindata/bindata.go builders/bindata/pipdeptree.py
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
