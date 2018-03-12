DEP="$(shell go env GOPATH)/bin/dep"
PREFIX?=/usr/local/bin
LDFLAGS:=-ldflags '-X main.version=$(shell git rev-parse --abbrev-ref HEAD) -X main.commit=$(shell git rev-parse HEAD) -X "main.goversion=$(shell go version)"'

all: build

$(DEP): ## Grab golang/dep utility
	go get github.com/golang/dep/cmd/dep

.PHONY: build
build: bin/fossa

bin/fossa:
	mkdir -p $$(dirname $@)
	go build -o $@ $(LDFLAGS) github.com/fossas/fossa-cli/cmd/fossa

$(PREFIX)/fossa: bin/fossa
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
	rm -f bin/fossa
