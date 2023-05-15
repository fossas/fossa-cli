# Custom Integration with Conan Package Manager

Conan is a dependency and package manager for C and C++ languages. It is free and open-source, works on all 
platforms (Windows, Linux, OSX, FreeBSD, Solaris, etc.), and can be used to develop for all targets including 
embedded, mobile (iOS, Android), and bare metal. It also integrates with all build systems like CMake, 
Visual Studio (MSBuild), Makefiles, etc., including proprietary ones.

## Prerequisite

- Conan v2 (you can check the version with running: `conan -v`)
- Python v3 (so you can run the script)

## Integration

We will use, `conan graph info` command to retrieve dependency graph, and 
source code for all of your packages. From this data, we will generate
[fossa-deps](./../references/files/fossa-deps.md) file to use 
[vendor-dependencies](./../features/vendored-dependencies.md) and [custom-dependencies](../features/manual-dependencies.md)
functionalities.

For this,
1. Download [make_fossa_deps_conan.py](./make_fossa_deps_conan.py) python script, and place it in same directory as `conanfile.txt`
2. Build your project (ensure it compiles)
3. Run `python make_fossa_deps_conan.py` (this will generate `fossa-deps.yaml` in same directory)
4. Run `fossa analyze && fossa test`

### Analysis

In this approach, `make_fossa_deps_conan.py` does the followings:

1. Retrieve project's dependency graph via `conan graph info` command
2. Use `-c tools.build:download_source=True` option to ensure Conan always [retrieves source code](https://docs.conan.io/2.0/reference/conanfile/methods/source.html#forced-retrieval-of-sources)
3. For each requirement with non `build` context, and source code directory, build vendor-dependency entry in `fossa-deps.yaml` file
4. For each requirement with non `build` context, and empty source code directory, build custom-dependency entry in `fossa-deps.yaml` file

### Limitations

### Example

```
# install conan and some prerequisites
apt-get -y install python3 pip git wget
pip3 install conan
wget https://raw.githubusercontent.com/fossas/fossa-cli/master/install-latest.sh
bash install-latest.sh

# check conan version
conan --version

# retrieve example projects
git clone https://github.com/conan-io/examples2.git
cd /examples2/tutorial/consuming_packages/simple_cmake_project

# download make_fossa_deps_conan.py
wget https://github.com/fossas/fossa-cli/blob/master/docs/walkthroughs/make_fossa_deps_conan.py

# Perform analysis
python3 make_fossa_deps_conan.py
FOSSA_API_KEY=XXX fossa analyze

# Perform test
FOSSA_API_KEY=XXX fossa test
```

## F.A.Q

#### 1. Why doesn't FOSSA offer native Conan package manager analysis?

FOSSA is actively working to develop native Conan support. We want to build functionality that
provides accurate and repeatable analysis for all version of Conan package manager.

#### 2. Why do I need Conan `v2` or greater?

This integration example uses `conan graph info` command with `--format json` and
`-c tools.build:download_source=True` option, which are only available in Conan v2.

#### 3. I want to use custom profile or provide additional options. 

You can provide any additional [`conan graph info`](https://docs.conan.io/2.0/reference/commands/graph/info.html) options, 
other than `--format`. 

To do so, provide option to the python script. For example, 

```bash
>> python3 make_fossa_deps_conan.py -s compiler=gcc
```

#### 4. How can I get help with this integration?

You can file a support ticket with [FOSSA helpdesk](https://support.fossa.com/hc/en-us).


### References

- [Conan Package Manager](https://docs.conan.io)
- [Conan graph command](https://docs.conan.io/2.0/reference/commands/graph/info.html)

