ifndef GOBIN
	ifeq ($(OS),Windows_NT)
		GOBIN := $(shell cmd /C "echo %GOPATH%| cut -d';' -f1")
		GOBIN := $(subst \,/,$(GOBIN))/bin
	else
		GOBIN := $(shell echo $$GOPATH | cut -d':' -f1 )/bin
	endif
endif

ifeq ($(OS),Windows_NT)
	EXE := fossa.exe
else
	EXE := fossa
endif

.PHONY: default

default: 
	GOBIN=${GOBIN} go install -ldflags '-X main.revision=$(shell git rev-parse HEAD)' github.com/fossas/fossa-cli/cmd/fossa

clean:
	rm -f $(GOPATH)/bin/fossa