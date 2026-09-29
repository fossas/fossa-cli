# Reference: conan

## Requirements

**Ideal**
- `conan` v2.0.6 or greater installed locally
- `conanfile.py` or `conanfile.txt` file present in your project
- `conan.lock` file present in your project

**Minimum**
- `conan` v2.0.6 or greater installed locally
- `conanfile.py` or `conanfile.txt` file present in your project

## Project discovery

Directories containing `conanfile.py` or `conanfile.txt` files are considered conan projects.

## Analysis

From the project root, we walk through and search all directories for a `conanfile.py` or a `conanfile.txt`. Any directory which contains these files is considered a `conan` project. We run `conan graph info -f json` and convert the resulting graph into our internal represenation. An example of the graph is shown [in the Conan docs](https://docs.conan.io/2/reference/commands/formatters/graph_info_json_formatter.html#reference-commands-graph-info-json-format).


## F.A.Q

#### 1. Why do I need Conan `v2.0.6` or greater?

This strategy uses the `conan graph info` command whose format was [standardized in v2.0.6](https://docs.conan.io/2/changelog.html#may-2023). Earlier formats can be supported, if required. Please reach out to the [FOSSA helpdesk](https://support.fossa.com/hc/en-us) if you need this support.