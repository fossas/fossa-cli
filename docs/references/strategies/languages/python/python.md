# Python Analysis

The python buildtool ecosystem consists of three major toolchains: setuptools
(requirements.txt, setup.py), pipenv, and conda.

| Strategy                                       | Direct Deps        | Transitive Deps    | Edges              | Container Scanning |
| ---------------------------------------------- | ------------------ | ------------------ | ------------------ | ------------------ |
| [pipenv](pipenv.md)                            | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :x:                |
| [pipfile](pipenv.md)                           | :heavy_check_mark: | :heavy_check_mark: | :x:                | :x:                |
| [requirements.txt & setuptools](setuptools.md) | :heavy_check_mark: | :x:                | :x:                | :heavy_check_mark: |
| [setup.py & setuptools](setuptools.md)         | :heavy_check_mark: | :x:                | :x:                | :heavy_check_mark: |
| [conda](conda.md)                              | :heavy_check_mark: | :white_check_mark: | :x:                | :x:                |
| [poetry](poetry.md)                            | :heavy_check_mark: | :white_check_mark: | :white_check_mark: | :heavy_check_mark: |
| [pdm](pdm.md)                                  | :heavy_check_mark: | :white_check_mark: | :white_check_mark: | :heavy_check_mark: |

* :heavy_check_mark: - Supported in all projects
* :white_check_mark: - Supported only when relevant data is available (e.g. lockfiles are present)
* :x: - Not Supported
