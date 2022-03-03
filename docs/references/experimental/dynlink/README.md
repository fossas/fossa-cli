## Dynamic Linked Dependency Detection

Some projects, especially C or C++ projects, result in binaries that dynamically link with system installed libaries.

### How does it work?

Dynamic Linked Dependency Detection works by using `ldd` to inspect the target binary for a list of linked binaries.
It then interacts with the local package manager (`rpm` for RedHat-based systems, `dpkg` for Debian-based systems) to determine the library which owns the binary.

If a library is found, it is reported to FOSSA, which uses our database of Linux packages to lookup licensing and vulnerability information for the package.
If a library is not found, the dependency is reported in FOSSA as an "unmanaged dynamically linked dependency". From there users may use the FOSSA UI to add a license or ignore the dependency.

### Prerequisites

Dynamic Linked Dependency Detection is supported on the following platforms:

* RedHat-based Linux systems (examples: RedHat, CentOS, Fedora)
* Debian-based Linux systems (examples: Debian, Ubuntu)

### Security

Dynamic Linked Dependency Detection is currently powered by `ldd`.

It is possible for a program to be built with its own loader which ignores the `LD_TRACE_LOADED_OBJECTS` variable used by `ldd`.
This could result in the binary being executed instead of being inspected.

For this reason, we strongly recommend against running Dynamic Linked Dependency Detection on an untrusted binary within a trusted environment.
If you are using Dynamic Linked Dependency Detection on your own binary, we recommend against running the feature on a binary with its own execution loader.
