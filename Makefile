# Get path to project root: https://stackoverflow.com/questions/18136918
current_dir := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

FMT_OPTS := -co -XTypeApplications -o -XImportQualifiedPost
FIND_OPTS := src test integration-test -type f -name '*.hs'
GHC_VERSION := 9.0.2
DEV_TOOLS := ghcr.io/fossas/haskell-dev-tools:${GHC_VERSION}
MOUNTED_DEV_TOOLS := docker run
MOUNTED_DEV_TOOLS += --rm
MOUNTED_DEV_TOOLS += --mount "type=bind,source=${current_dir},target=/fossa-cli"
MOUNTED_DEV_TOOLS += --workdir "/fossa-cli"
MOUNTED_DEV_TOOLS += ${DEV_TOOLS}

build:
	cabal build

test:
	cabal test unit-tests --test-show-details=streaming --test-option=--format=checks --test-option=--times --test-option=--color

# Runs a integeration test.
# To run a set of integeration spec matching specific value, use ARGS
# 	Example: make integeration-test-for ARGS="fd"
integeration-test:
ifdef ARGS
	cabal test integration-tests --test-show-details=streaming --test-option=--format=checks --test-option=--match --test-option="$(ARGS)"
else
	cabal test integration-tests --test-show-details=streaming --test-option=--format=checks
endif
	
test-all:
	cabal test

# Dogfood the dev version
analyze:
	cabal run fossa -- analyze --output --debug --only-target 'cabal@./'

# Copy the built binary into the local root
install-local: build
	cp $(shell cabal list-bin fossa) ./fossa

install-dev: build
	cp $(shell cabal list-bin fossa) /usr/local/bin/fossa-dev

check: check-fmt lint

# Run any build scripts required for test data to be generated.
build-test-data:
	./test/App/Fossa/VSI/DynLinked/testdata/build.sh

# Clean up built test data (generated with the above command)
clean-test-data:
	./test/App/Fossa/VSI/DynLinked/testdata/clean.sh

# Format everything (if this fails, update FMT_OPTS or use your IDE to format)
# `@command` does not echo the command before running
fmt:
	@echo "Running fourmolu"
	@fourmolu --version
	@fourmolu --mode inplace ${FMT_OPTS} $(shell find ${FIND_OPTS})
	@echo "Running cabal-fmt"
	@cabal-fmt spectrometer.cabal --inplace

# Run the formatter from within a docker image, with the project mounted as a volume
fmt-ci:
	docker pull ${DEV_TOOLS}
	${MOUNTED_DEV_TOOLS} make fmt

# Confirm everything is formatted without changing anything
check-fmt:
	@echo "Running fourmolu"
	@fourmolu --version
	@fourmolu --mode check ${FMT_OPTS} $(shell find ${FIND_OPTS})
	@echo "No formatting errors found"
	@echo "Running cabal-fmt"
	@cabal-fmt --check spectrometer.cabal

# Lint everything (If this fails, update .hlint.yaml or report the failure)
lint:
	@echo "Running hlint"
	@hlint --version
	@hlint src test integration-test
	@echo "No linter errors found"

# Performs markdown lint checks for dead links
# You will need to install https://github.com/tcort/markdown-link-check
check-links:
	@echo "Running markdown-link-check in docs directory"
	find ./docs/ -name \*.md -exec markdown-link-check {} \;
	@echo "Running markdown-link-check for README.md"
	markdown-link-check README.md

# Docker doesn't always check for new versions during build, so pulling ensures
# that we always have the latest.
check-ci:
	docker pull ${DEV_TOOLS}
	${MOUNTED_DEV_TOOLS} make check

.PHONY: build test integration-test analyze install-local fmt check check-fmt lint check-ci fmt-ci build-test-data clean-test-data install-dev test-all
