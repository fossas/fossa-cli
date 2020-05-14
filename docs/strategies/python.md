# Python Analysis

The python buildtool ecosystem consists of two major toolchains: setuptools
(requirements.txt, setup.py), and pipenv.

| Strategy                                    | Direct Deps | Deep Deps | Edges | Tags                        |
| ---                                         | ---         | ---       | ---   | ---                         |
| [pipenv][pipenv] (pipenv)                   | ✅          | ✅        | ✅    | Environment                 |
| [pipfile][pipenv] (pipenv)                  | ✅          | ✅        | ❌    | Environment                 |
| [requirements.txt][setuptools] (setuptools) | ✅          | ❌        | ❌    | PEP-508 Environment Markers |
| [setup.py][setuptools] (setuptools)         | ✅          | ❌        | ❌    | PEP-508 Environment Markers |
| [piplist][piplist]                          | Maybe       | Maybe     | ❌    |                             |

[pipenv]: python/pipenv.md
[setuptools]: python/setuptools.md
[piplist]: python/piplist.md
