# uv

[uv](https://docs.astral.sh/uv/) is a package and project manager for Python.

The uv strategy is a static analysis strategy and does not require the use of any external tools.

## Project Discovery

Find files named `uv.lock`. uv also uses `pyproject.toml` to define dependencies, but only the presence of a lock file is used to detect uv projects.

## Analysis

We parse the `uv.lock` file, which is in the TOML format. This file contains an array named `package` which contains all the direct and transitive dependencies in the project. Each package has a field `dependencies` which is used to build the edges in the dependency graph.
