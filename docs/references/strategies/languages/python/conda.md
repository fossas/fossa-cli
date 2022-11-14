# Conda Analysis

While originally created for Python programs, Conda can package and distribute software for any language. It provides a full list of all dependencies installed in the environment, without providing a dependency graph.

## Project Discovery

A conda project is discovered if the project directory contains an `environment.yml` file.

## Analysis: conda env create

Run `conda env create --json --file environment.yml --dry-run --force` to get a complete list of packages that would be installed by `conda env create -f environment.yml`.
Using the `--dry-run` option means that `fossa-cli` does not actually modify any local environments. 
This process can take some time since `conda` may need to refresh its package data in order to run the command.

## Analysis: environment.yml

If the `conda list --json` operation does not succeed, we fall back to parsing the `environment.yml`, and getting the list of dependencies from the `dependencies` section of this file.
