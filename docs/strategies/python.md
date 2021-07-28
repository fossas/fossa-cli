# Python Analysis

The python buildtool ecosystem consists of three major toolchains: setuptools
(requirements.txt, setup.py), pipenv, and conda.

| Strategy                                    | Direct Deps        | Deep Deps          | Edges              |
| ------------------------------------------- | ------------------ | ------------------ | ------------------ |
| [pipenv][pipenv] (pipenv)                   | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: |
| [pipfile][pipenv] (pipenv)                  | :heavy_check_mark: | :heavy_check_mark: | :x:                |
| [requirements.txt][setuptools] (setuptools) | :heavy_check_mark: | :x:                | :x:                |
| [setup.py][setuptools] (setuptools)         | :heavy_check_mark: | :x:                | :x:                |
| [piplist][piplist]                          | :white_check_mark: | :white_check_mark: | :x:                |
| [conda][conda] (conda)                      | :heavy_check_mark: | :heavy_check_mark: | :x:                |
| [poetry][poetry] (poetry)                   | :heavy_check_mark: | :white_check_mark: | :white_check_mark: |

Where, 

* :heavy_check_mark: - Supported in all projects
* :white_check_mark: - Supported only when relevant data is available (e.g. lockfiles are present)
* :x: - Not Supported

[pipenv]: python/pipenv.md
[setuptools]: python/setuptools.md
[piplist]: python/piplist.md
[conda]: python/conda.md
[poetry]: python/poetry.md