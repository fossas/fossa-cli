# Python Analysis

The python buildtool ecosystem consists of two major toolchains: setuptools
(requirements.txt, setup.py), and pipenv.

| Strategy                                    | Direct Deps | Deep Deps | Edges |
| ---                                         | ---         | ---       | ---   |
| [pipenv][pipenv] (pipenv)                   | ✅          | ✅        | ✅    |
| [pipfile][pipenv] (pipenv)                  | ✅          | ✅        | ❌    |
| [requirements.txt][setuptools] (setuptools) | ✅          | ❌        | ❌    |
| [setup.py][setuptools] (setuptools)         | ✅          | ❌        | ❌    |
| [piplist][piplist]                          | Maybe       | Maybe     | ❌    |

[pipenv]: python/pipenv.md
[setuptools]: python/setuptools.md
[piplist]: python/piplist.md
