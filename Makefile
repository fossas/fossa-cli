.PHONY: default clean

default: 
	go install -ldflags '-X main.version=$(shell git rev-parse --abbrev-ref HEAD) -X main.commit=$(shell git rev-parse HEAD)' github.com/fossas/fossa-cli/cmd/fossa

clean:
	rm -f $(GOPATH)/bin/fossa