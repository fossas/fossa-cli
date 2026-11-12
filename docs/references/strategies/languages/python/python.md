# Python Analysis

The python buildtool ecosystem consists of three major toolchains: setuptools
(requirements.txt, setup.py), pipenv, and conda. pixi supports conda and PyPI packages.

| Strategy                                       | Direct Deps        | Transitive Deps    | Edges              | Container Scanning |
| ---------------------------------------------- | ------------------ | ------------------ | ------------------ | ------------------ |
| [pipenv](pipenv.md)                            | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :x:                |
| [pipfile](pipenv.md)                           | :heavy_check_mark: | :heavy_check_mark: | :x:                | :x:                |
| [requirements.txt & setuptools](setuptools.md) | :heavy_check_mark: | :white_check_mark: | :white_check_mark: | :heavy_check_mark: |
| [setup.py & setuptools](setuptools.md)         | :heavy_check_mark: | :white_check_mark: | :white_check_mark: | :heavy_check_mark: |
| [conda](conda.md)                              | :heavy_check_mark: | :white_check_mark: | :x:                | :x:                |
| [pixi](pixi.md)                                | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :x:                |
| [poetry](poetry.md)                            | :heavy_check_mark: | :white_check_mark: | :white_check_mark: | :heavy_check_mark: |
| [pdm](pdm.md)                                  | :heavy_check_mark: | :white_check_mark: | :white_check_mark: | :heavy_check_mark: |
| [uv](uv.md)                                    | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: |

* :heavy_check_mark: - Supported in all projects
* :white_check_mark: - Supported only when relevant data is available (e.g. lockfiles are present)
* :x: - Not Supported
