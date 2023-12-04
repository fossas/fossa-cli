# Setuptools (requirements.txt/setup.py)

requirements.txt, alongside setup.py, is the most common -- yet imprecise --
approach to dependency management in python projects.

## Project Discovery

`requirements.txt`: Find all files matching the glob: `req*.txt`

`setup.py`: Find all files named `setup.py`

## Analysis: requirements.txt

For the CLI to identify transitive dependencies from a requirements.txt file,
certain conditions must be met. First, the specified packages must be installed
using a command like `python -m pip install -r requirements.txt`. The packages
should be installed within a virtual environment, ensuring a clean and isolated
environment. Subsequently, the CLI should be executed within the same virtual
environment the packages were installed in. Finally, the environment should
include both `python` and `pip`, as the CLI parses the output from executing
`python -m pip list` and `python -m pip show` to determine transitive
dependencies. Importantly, the CLI only reports transitive dependencies for
packages explicitly listed in the requirements.txt file and installed within the
current environment.

If neither python nor pip are installed the CLI will only to reporting only direct
dependencies.

requirements.txt contains direct dependencies, and is parsed compliant to its
[file format spec][requirements-file-format].

pip-cli options, URLs, absolute paths, and relative paths are ignored -- though
this may be revisited in the future.

Dependencies found in requirements.txt have a spec defined by
[PEP-508][pep-508]. Dependencies often have version ranges and environment
markers (e.g. python version, OS, ...). The resulting graph contains packages
tagged with environment markers.

## Analysis: setup.py

### Installed packages

For the CLI to detect transitives dependencies for packages specified in a
`setup.py` file's `install_requires` field, certain conditions must be met. The
packages must be installed with command similar to `python -m pip install .`.
The packages should be installed within a virtual environment, ensuring a clean
and isolated environment. Subsequently, the CLI should be executed with the same
environment. Finally, the environment should include both `python` and `pip`, as
the CLI will parse the output from executing `python -m pip list` and `python -m
pip show` to determine the transitive dependencies. The CLI will naively scan a
setup.py file for a `name` attribute and attempt to match the name with an
installed package. If the name matches an installed package, the CLI will
exclusively report the transitive dependencies required by the found package. If
a name is not matched then the CLI will naively scan for install_requires as
explained in the next section.

If neither python nor pip are installed the CLI will report dependencies found
by naively scanning for install_requires as explained in the next section.

### Naively scanning for install_requires

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
[requirements-file-format]: https://pip.pypa.io/en/stable/reference/requirements-file-format/
[pep-508]: https://www.python.org/dev/peps/pep-0508/

## Limitations

* Python requirements files and setup.py files do not provide any data about edges between dependencies.
* Requirements files can be completely different than an existing setup.py specification, as there is no built-in
synchronization between them.
* The CLI will catch *variables* named `install_requires`, as long as they are declared earlier in the file than the `install_requires` keyword argument to `setup`.
* Because the CLI doesn't actually run `setup.py` or do its own interpretation of the Python code therein, `install_requires` not defined as a literal array of string literals done in the `setup.py` file will hide the
true `install_requires` list from the CLI's view. For example, FOSSA CLI does not have a general way to find `install_requires` set up this way:
    ```python
    a = ['package1==1.0.0']
    b = ['package2==2.0.0']
    install_requires = a + b
    ```
* Often, the `requirements.txt` file entirely overlaps the `setup.py` file.  This is almost always by design.

## Examples

Assuming no virtual environment, given the following files:

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

`requirements.txt`:
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

## F.A.Q

### Why is `fossa-cli` reporting incorrect transitive dependencies or edges?

`fossa-cli` uses `python -m pip list` and `python -m pip show` command to infer
transitive dependencies, and it's edges. It's paramount that you are in
project's [venv](https://docs.python.org/3/tutorial/venv.html) prior to invoking
`fossa analyze` command. If you are not in project's virtual environment,
`fossa-cli` will use global python environment to infer transitive dependencies,
edges, and dependency versions.

For example, you should always invoke `fossa-cli` in project's python's virtual environment.
```bash
python -m venv ~/envs/my-venv
source ~/envs/my-venv/bin/activate # this command may differ depending on your python version!
pip install -r requirements.txt

fossa analyze
```

If you are unable to get any transitive dependencies, ensure you can run following command in project's directory:
- `python -m pip list --format json`
- `python -m pip show`
