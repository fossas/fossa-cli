# Custom Integration with Conan Package Manager

Conan is a dependency and package manager for C and C++ languages. It is free and open-source, works on all 
platforms (Windows, Linux, OSX, FreeBSD, Solaris, etc.), and can be used to develop for all targets including 
embedded, mobile (iOS, Android), and bare metal. It also integrates with all build systems like CMake, 
Visual Studio (MSBuild), Makefiles, SCons, etc., including proprietary ones.

## Prerequisite

- Conan V2 (you can check the version with running `conan version` command)

## Analysis

We will use, `conan show` to retrieve dependency graph, and download source code for all of your packages
to local disk. We will use fossa-cli's [] to 

```python
# filename: mk_fossa_deps_for_conan.py
# python3 


def main():
    pass

```

To use this script, 

```bash
>> python3 mk_fossa_deps_for_conan.py
>> 

>> fossa analyze . --include node --exclude node
>> 

```

## F.A.Q

#### 1. Why doesn't FOSSA offer native conan package manager analysis?

FOSSA is actively working to develop native conan support. We want to build functionality that
provides accurate and repeatable analysis for all version of conan package manager. 

#### 2. Why do I need conan `` or greater?

FOSSA uses `` flag, to force conan to download source code of package. This is done so, 
FOSSA can perform license scan and copyright detection on the actual source code of the
package as opposed to binary artifact retrieved from the conan registry.

#### 3. With current custom integration, what functionality of FOSSA I'm unable to use?

With custom integration provided in this document, FOSSA will not be able to,

- view author information
- view homepage information
- use security features (vulnerability)

#### 4. How can I get help in this integration?

You can file a support ticket with [FOSSA helpdesk]().

