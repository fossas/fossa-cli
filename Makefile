# Get path to project root: https://stackoverflow.com/questions/18136918
current_dir := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

FMT_OPTS := -co -XTypeApplications -o -XImportQualifiedPost
FIND_OPTS := src test integration-test -type f -name '*.hs'
GHC_VERSION := 9.8.4
DEV_TOOLS := ghcr.io/fossas/haskell-dev-tools:${GHC_VERSION}
MOUNTED_DEV_TOOLS_OPTS := --rm
MOUNTED_DEV_TOOLS_OPTS += --mount "type=bind,source=${current_dir},target=/fossa-cli"
MOUNTED_DEV_TOOLS_OPTS += --workdir "/fossa-cli"
MOUNTED_DEV_TOOLS := ${MOUNTED_DEV_TOOLS_OPTS} ${DEV_TOOLS}
SHELL := bash

build-cli: build-cargo
	cabal build fossa

build: build-cargo
	cabal build

build-cargo:
	cargo build --release

clean:
	cabal clean && cargo clean

# Runs units tests.
# To run a set of unit tests matching a specific value, use ARGS
# For example, to only run tests whose name matches the wildcard '*Node.PackageLockV3*':
# 	make test ARGS="Node.PackageLockV3"
test: test-cargo test-cabal

test-cabal: build-cargo
ifdef ARGS
	cabal test unit-tests --test-show-details=streaming --test-option=--format=checks --test-option=--times --test-option=--color --test-option=--match --test-option="$(ARGS)"
else
	cabal test unit-tests --test-show-details=streaming --test-option=--format=checks --test-option=--times --test-option=--color
endif

test-cargo: build-cargo
	cargo test

# Runs an integration test.
# To run a set of integration tests matching a specific value, use ARGS
# For example, to only run tests whose name matches the wildcard '*fd*':
# 	make integration-test ARGS="fd"
integration-test:
ifdef ARGS
	cabal test integration-tests --test-show-details=streaming --test-option=--format=checks --test-option=--match --test-option="$(ARGS)"
else
	cabal test integration-tests --test-show-details=streaming --test-option=--format=checks
endif

test-all: test-cargo
	cabal test

# Dogfood the dev version
analyze:
	cabal run fossa -- analyze --output --debug --only-target 'cabal@./'

# Copy the built binary into the local root
install-local: build-cli
	cp $(shell cabal list-bin fossa) ./fossa

install-dev: build-cli
	cp $(shell cabal list-bin fossa) /usr/local/bin/fossa-dev

check: check-fmt lint

check-haskell: check-fmt-haskell lint-haskell

fast-check: check-fmt-haskell fast-lint

# Run any build scripts required for test data to be generated.
build-test-data:
	./test/App/Fossa/VSI/DynLinked/testdata/build.sh

# Clean up built test data (generated with the above command)
clean-test-data:
	./test/App/Fossa/VSI/DynLinked/testdata/clean.sh

# Format everything (if this fails, update FMT_OPTS or use your IDE to format)
# `@command` does not echo the command before running
fmt: fmt-haskell fmt-cargo

fmt-haskell:
	@echo "Running fourmolu"
	@fourmolu --version
	@fourmolu --mode inplace ${FMT_OPTS} $(shell find ${FIND_OPTS})
	@echo "Running cabal-fmt"
	@cabal-fmt spectrometer.cabal --inplace

fmt-cargo:
	@echo "Running rustfmt"
	@cargo fmt --version
	@cargo fmt

# Confirm everything is formatted without changing anything
check-fmt: check-fmt-haskell check-fmt-cargo

check-fmt-haskell:
	@echo "Running fourmolu"
	@fourmolu --version
	@fourmolu --mode check ${FMT_OPTS} $(shell find ${FIND_OPTS})
	@echo "No formatting errors found"
	@echo "Running cabal-fmt"
	@cabal-fmt --check spectrometer.cabal

check-fmt-cargo:
	@echo "Running rustfmt"
	@cargo fmt --version
	@cargo fmt --check

# Lint everything (If this fails, update .hlint.yaml or report the failure)
lint: lint-haskell lint-cargo

lint-haskell:
	@echo "Running hlint"
	@hlint --version
	@hlint src test integration-test --cross --timing -vj
	@echo "No linter errors found"

# Lint everything (If this fails, update clippy directives or report the failure)
lint-cargo:
	@echo "Running clippy"
	@cargo clippy -V
	@cargo clippy

# Build cargo deps needed by the CLI and move them into place for cabal.
build-embedded-rust-bins: target/release/berkeleydb target/release/millhone
	cargo build --release --bin millhone --bin berkeleydb

# Runs linter on only modified files
# 
# When running in docker, we have to tell git that the directory which doesn't belong to us is safe.
# The two git commands are for staged (cached) and unstaged files.
# We also succeed if there are no changed filers to lint.  We grep the list of files for non-whitespace,
# and if we find any non-whitespace characters, then the git search found at least one file.
fast-lint-haskell:
	@test "${current_dir}" = "/fossa-cli/" && \
		git config --global --add safe.directory /fossa-cli && \
		echo "Running in docker, added temp safe.directory entry to git config"
	@echo Collecting unstaged files
	@git diff --name-only --diff-filter=AM -- "*.hs" > /tmp/hlint-changed-files
	@echo Collecting staged files
	@git diff --name-only --diff-filter=AM --cached -- "*.hs" >> /tmp/hlint-changed-files
	@if grep -q "[^[:space:]]" /tmp/hlint-changed-files; then \
			echo "Linting changed files"; \
			xargs hlint -vj < /tmp/hlint-changed-files; \
		else \
			echo "No haskell files changed"; \
		fi

# Performs markdown lint checks for dead links
# You will need to install https://github.com/tcort/markdown-link-check
check-links:
	@echo "Running markdown-link-check in docs directory"
	find ./docs/ -name \*.md -exec markdown-link-check {} -c .markdown-link-check.json \;
	@echo "Running markdown-link-check for README.md"
	markdown-link-check README.md -c .markdown-link-check.json

# Run the formatter from within a docker image, with the project mounted as a volume
fmt-ci:
	docker pull ${DEV_TOOLS}
	docker run ${MOUNTED_DEV_TOOLS} make fmt-haskell

# Run the fast-lint target with the CI docker container
fast-lint-ci:
	docker pull ${DEV_TOOLS}
	docker run ${MOUNTED_DEV_TOOLS} make fast-lint-haskell

# Docker doesn't always check for new versions during build, so pulling ensures
# that we always have the latest.
check-ci:
	docker pull ${DEV_TOOLS}
	docker run ${MOUNTED_DEV_TOOLS} make check-haskell

# Run the fast-check target with the CI docker container
fast-check-ci:
	docker pull ${DEV_TOOLS}
	docker run ${MOUNTED_DEV_TOOLS} make fast-check

# Run bash in the CI edocker container.  Useful for debugging make with CI tools.
ci-shell:
	docker pull ${DEV_TOOLS}
	docker run -it ${MOUNTED_DEV_TOOLS} bash

# Runs benchmarks
bench:
	cabal bench --benchmark-options '+RTS -T'

.PHONY: build-cli test integration-test analyze install-local fmt check check-fmt lint check-ci fmt-ci build-test-data clean-test-data install-dev test-all bench build-cargo test-cargo fmt-cargo check-fmt-cargo lint-cargo clean
