# Setuptools (requirements.txt/setup.py)

requirements.txt, alongside setup.py, is the most common -- yet imprecise --
approach to dependency management in python projects.

## Project Discovery

`requirementstxt`: Find all files named `requirements.txt`

`setuppy`: Find all files named `setup.py`

## Analysis: requirementstxt

requirements.txt contains direct dependencies, and is parsed compliant to its
[file format spec][requirements-file-format].

pip-cli options, URLs, absolute paths, and relative paths are ignored -- though
this may be revisited in the future.

Dependencies found in requirements.txt have a spec defined by
[PEP-508][pep-508]. Dependencies often have version ranges and environment
markers (e.g. python version, OS, ...). The resulting graph contains packages
tagged with environment markers.

## Analysis: setuppy

setup.py is naively scanned for its `install_requires=[...]` field, which often
fails on projects encountered in the wild. Short of implementing a robust python
parser, or running a python script in their environment (which may have
unintended consequences!), reliable output from setup.py is difficult to obtain.

Entries in the `install_requires` array are parsed compliant to the
[PEP-508][pep-508] spec, similar to requirements.txt

[requirements-file-format]: https://pip.pypa.io/en/stable/cli/pip_install/#requirements-file-format
[pep-508]: https://www.python.org/dev/peps/pep-0508/
