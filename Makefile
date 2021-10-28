FMT_OPTS = -co -XTypeApplications -o -XImportQualifiedPost
FIND_OPTS = src test -type f -name '*.hs'

build:
	cabal build

test:
	cabal test

# Dogfood the dev version
analyze:
	cabal run fossa -- analyze --output --debug --filter 'cabal@./'

# Copy the built binary into the local root
install-local: build
	cp $(shell cabal list-bin fossa) ./fossa

check: check-fmt lint

# Format everything (if this fails, update FMT_OPTS or use your IDE to format)
# `@command` does not echo the command before running
fmt:
	@echo "Running fourmolu"
	@fourmolu --version
	@fourmolu --mode inplace ${FMT_OPTS} $(shell find ${FIND_OPTS})

# Confirm everything is formatted without changing anything
check-fmt:
	@echo "Running fourmolu"
	@fourmolu --version
	@fourmolu --mode check ${FMT_OPTS} $(shell find ${FIND_OPTS})
	@echo "No formatting errors found"

# Lint everything (If this fails, update .hlint.yaml or report the failure)
lint:
	@echo "Running hlint"
	@hlint --version
	@hlint src test
	@echo "No linter errors found"

# Docker doesn't always check for new versions during build, so pulling ensures
# that we always have the latest.
check-ci:
	docker pull ghcr.io/fossas/haskell-dev-tools:8.10.4
	docker build --tag delete-me -f docker/Dockerfile.lint .
	docker rmi delete-me

.PHONY: build test analyze install-local fmt check check-fmt lint
