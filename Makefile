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
fossa:
	cp $(shell cabal list-bin fossa) ./fossa

install-local: fossa

check: check-fmt lint

# Format everything (if this fails, update FMT_OPTS or use your IDE to format)
# `@command` does not echo the command before running
fmt:
	@fourmolu --mode inplace ${FMT_OPTS} $(shell find ${FIND_OPTS})

# Confirm everything is formatted without changing anything
check-fmt:
	@fourmolu --mode check ${FMT_OPTS} $(shell find ${FIND_OPTS})
	@echo "No formatting errors found"

# Lint everything (If this fails, update .hlint.yaml or report the failure)
lint:
	hlint src test

.PHONY: build test analyze install-local fmt check check-fmt lint
