# Custom Integration with Conan Package Manager

Conan is a dependency and package manager for C and C++ languages. It is free and open-source, works on all
platforms (Windows, Linux, OSX, FreeBSD, Solaris, etc.), and can be used to develop for all targets, including
embedded, mobile (iOS, Android), and bare metal. It also integrates with all build systems like CMake,
Visual Studio (MSBuild), Makefiles, etc., including proprietary ones.

## Prerequisite

- Conan v2.0.0 or greater (you can check the version by running: `conan -v`)
- Python v3 (so you can run the script)

## Integration

This integration uses the `conan graph info` command to retrieve the dependency graph and source code for all dependencies. From this data, it generates [fossa-deps](./../references/files/fossa-deps.md) file with
[vendored-dependencies](./../features/vendored-dependencies.md) and [custom-dependencies](../features/manual-dependencies.md).

To use this integration,
1. Download [make_fossa_deps_conan.py](./make_fossa_deps_conan.py) python script, and place it in the same directory as `conanfile.txt` or `conanfile.py.`
2. Build your project (ensure it compiles)
3. Run `python make_fossa_deps_conan.py` (this will generate `fossa-deps.yaml` in the same directory)
4. Run `fossa analyze && fossa test`

### Analysis

In this approach, `make_fossa_deps_conan.py` does the followings:

1. Retrieve the project's dependency graph via the `conan graph info` command
2. Uses `-c tools.build:download_source=True` option to ensure Conan always [retrieves source code](https://docs.conan.io/2.0/reference/conanfile/methods/source.html#forced-retrieval-of-sources)
3. For each requirement with non `build` context and source code directory, it builds a vendor-dependency entry in the `fossa-deps.yaml` file
4. For each requirement with non `build` context and empty source code directory, it builds a custom-dependency entry in the `fossa-deps.yaml` file

### Limitations

This integration method uses [vendored-dependencies](./../features/vendored-dependencies.md) and [custom-dependencies](../features/manual-dependencies.md)
functionalities, and as such, it does not provide the following,

- Security functionalities (FOSSA will not be able to identify vulnerabilities, only licensing and copyright issues)
- Author information (in dependency view)

This integration example uses the best alternative mode of analysis for each dependency. It tries to
locate the source code for each dependency, and if it fails to locate the source code, it will create this dependency
as [custom-dependency](../features/manual-dependencies.md) entry in the [fossa-deps](./../references/files/fossa-deps.md) file.
In this case, it will use a declared license for this dependency.

If the script locates the source code, it will create [vendor-dependency](./../features/vendored-dependencies.md) entry in
the [fossa-deps](./../references/files/fossa-deps.md) file.

### Example

```bash
# install Conan and some prerequisites
apt-get -y install python3 pip git wget
pip3 install conan
wget https://raw.githubusercontent.com/fossas/fossa-cli/master/install-latest.sh
bash install-latest.sh

# check the Conan version
conan --version

# retrieve example projects
git clone https://github.com/conan-io/examples2.git
cd /examples2/tutorial/consuming_packages/simple_cmake_project

# build
conan profile detect --force
conan install . --output-folder=build --build=missing

# download make_fossa_deps_conan.py
wget https://raw.githubusercontent.com/fossas/fossa-cli/master/docs/walkthroughs/make_fossa_deps_conan.py

# Set your API key. Get this from the FOSSA web application.
export FOSSA_API_KEY=XXXX

# Perform analysis
python3 make_fossa_deps_conan.py
fossa analyze

# Perform test
fossa test
```

## F.A.Q

#### 1. Why doesn't FOSSA offer native Conan package manager analysis?

FOSSA is actively working to develop native Conan support. We want to build a functionality that provides accurate and repeatable analysis for all versions of the Conan package manager. This integration
script is supplied as a potential option if you want to start using FOSSA for Conan immediately.

#### 2. Why do I need Conan `v2` or greater?

This integration example uses the `conan graph info` command with `--format json` and
`-c tools.build:download_source=True` option, which are only available in Conan v2 (`v2.0.0+`).

#### 3. I want to use a custom profile or provide additional options.

You can provide any additional [`conan graph info`](https://docs.conan.io/2.0/reference/commands/graph/info.html) options (except `--format` or `-f`)

To do so, provide options to the Python script. For example,

```bash
>> python3 make_fossa_deps_conan.py -s compiler=gcc
```

#### 4. How can I get help with this integration?

You can file a support ticket with [FOSSA helpdesk](https://support.fossa.com/hc/en-us).

#### 5. How do I always use a declared license?

This can be achieved by modifying [make_fossa_deps_conan.py](./make_fossa_deps_conan.py). In the script,
you can choose to always create [custom-dependency](../features/manual-dependencies.md) entry, this will ensure
that declared license is always used.

### References

- [Conan Package Manager](https://docs.conan.io)
- [Conan graph command](https://docs.conan.io/2.0/reference/commands/graph/info.html)
