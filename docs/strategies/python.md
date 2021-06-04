# Python Analysis

The python buildtool ecosystem consists of three major toolchains: setuptools
(requirements.txt, setup.py), pipenv, and conda.

| Strategy                                    | Direct Deps | Deep Deps | Edges |
| ---                                         | ---         | ---       | ---   |
| [pipenv][pipenv] (pipenv)                   | ✅          | ✅        | ✅    |
| [pipfile][pipenv] (pipenv)                  | ✅          | ✅        | ❌    |
| [requirements.txt][setuptools] (setuptools) | ✅          | ❌        | ❌    |
| [setup.py][setuptools] (setuptools)         | ✅          | ❌        | ❌    |
| [piplist][piplist]                          | Maybe       | Maybe     | ❌    |
| [conda][conda] (conda)                      | ✅          | ✅        | ❌    |

[pipenv]: python/pipenv.md
[setuptools]: python/setuptools.md
[piplist]: python/piplist.md
[conda]: python/conda.md