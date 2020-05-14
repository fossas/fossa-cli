# Pipenv/Pipfile

This is a modern approach to defining python project dependencies, providing
very precise, complete dependency graphs for a python project.

## Project Discovery

Find all files named `Pipfile.lock`

## Analysis

We parse `Pipfile.lock` -- a json-structured file -- to find:

- `sources` - repositories/locations that can be referenced by packages
- `default` - production dependencies
- `develop` - development dependencies

Dependencies may contain an `index` field -- this is a reference to a repository
in the top-level `sources`.

Dependencies contain pinned `version` information.

Notably missing: edges between dependencies. This is where the Pipenv strategy
diverges from Pipfile.

## Pipenv-specific

When possible, we use `pipenv graph --json-tree` to hydrate the edges between
dependencies. This will fail unless `pipenv install` has been run in that directory.
