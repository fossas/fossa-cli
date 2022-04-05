# Python Analysis

The python buildtool ecosystem consists of three major toolchains: setuptools
(requirements.txt, setup.py), pipenv, and conda.

| Strategy                                       | Direct Deps        | Deep Deps          | Edges              |
| ---------------------------------------------- | ------------------ | ------------------ | ------------------ |
| [pipenv](pipenv.md)                            | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: |
| [pipfile](pipenv.md)                           | :heavy_check_mark: | :heavy_check_mark: | :x:                |
| [requirements.txt & setuptools](setuptools.md) | :heavy_check_mark: | :x:                | :x:                |
| [setup.py & setuptools](setuptools.md)         | :heavy_check_mark: | :x:                | :x:                |
| [piplist](piplist.md)                          | :white_check_mark: | :white_check_mark: | :x:                |
| [conda](conda.md)                              | :heavy_check_mark: | :heavy_check_mark: | :x:                |
| [poetry](poetry.md)                            | :heavy_check_mark: | :white_check_mark: | :white_check_mark: |

* :heavy_check_mark: - Supported in all projects
* :white_check_mark: - Supported only when relevant data is available (e.g. lockfiles are present)
* :x: - Not Supported
