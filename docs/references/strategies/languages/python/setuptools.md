# Setuptools (requirements.txt/setup.py)

requirements.txt, alongside setup.py, is the most common -- yet imprecise --
approach to dependency management in python projects.

## Project Discovery

`requirements.txt`: Find all files matching the glob: `req*.txt`

`setup.py`: Find all files named `setup.py`

## Analysis: requirements.txt

requirements.txt contains direct dependencies, and is parsed compliant to its
[file format spec][requirements-file-format].

pip-cli options, URLs, absolute paths, and relative paths are ignored -- though
this may be revisited in the future.

Dependencies found in requirements.txt have a spec defined by
[PEP-508][pep-508]. Dependencies often have version ranges and environment
markers (e.g. python version, OS, ...). The resulting graph contains packages
tagged with environment markers.

## Analysis: setup.py

setup.py is naively scanned for its `install_requires=[...]` field, which often
fails on projects encountered in the wild. Short of implementing a robust python
parser, or running a python script in their environment (which may have
unintended consequences!), reliable output from setup.py is difficult to obtain.

Entries in the `install_requires` array are parsed compliant to the
[PEP-508][pep-508] spec, similar to requirements.txt

If `setup.cfg` exists in the same directory as `setup.py`, `fossa-cli` also naively 
scans for its `install_requires=[...]` attributes, similar to `setup.py`. If both `setup.cfg` and
`setup.py` exists and both have `install_requires` attribute, `fossa-cli` concatenates requirements
from both files.

[setup.cfg docs]: https://setuptools.pypa.io/en/latest/userguide/declarative_config.html
[requirements-file-format]: https://pip.pypa.io/en/stable/cli/pip_install/#requirements-file-format
[pep-508]: https://www.python.org/dev/peps/pep-0508/

## Limitations

* Python requirements files and setup.py files do not provide any data about edges between dependencies.
* Requirements files can be completely different than an existing setup.py specification, as there is no built-in
synchronization between them.
* Since we don't actually run `setup.py` or do our own interpretation of the Python code therein, `install_requires` not defined as a literal array of string literals done in the `setup.py` file will hide the
true `install_requires` list from our view. For example, we don't have a way to find `install_requires` set up this way:
```python
a = ['package1==1.0.0']
b = ['package2==2.0.0']
install_requires = a + b
```
However, we do catch *variables* named `install_requires`, as long as they are declared earlier in the file than the `install_requires` keyword argument to `setup`.
* Often, the `requirements.txt` file entirely overlaps the `setup.py` file.  This is almost always by design.

## Examples

Given the following files:

`setup.py` (manually created):

```python
setup(
    name='Foo-project',
    version='1.0',
    description='Python example project',
    author='Jeff Jefferson',
    author_email='bug-catcher@butterfly.net',
    url='https://this.url/means#nothing',
    packages=['foo'],
    # And now the important part...
    install_requires=[  
        "requests",
    ],
)
```

`requirements.txt` (obtained by running `pip install requests && pip freeze > requirements.txt`):
*Note: your requirements file may have different versions. This file is just a reference example.*

```txt
certifi==2021.5.30
chardet==4.0.0
idna==2.10
requests==2.25.1
urllib3==1.26.6
```

We will produce a list of these direct dependencies with no edges between them (see [#limitations](#limitations)):

```txt
certifi==2021.5.30
chardet==4.0.0
idna==2.10
requests==2.25.1
urllib3==1.26.6
```
