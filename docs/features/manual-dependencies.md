<!-- referenced, custom, and remote (anything where we "add a dep that isn't there") -->

## Manually specifying dependencies

FOSSA offers a way to manually upload dependencies provided we support the dependency type. Manually specifying dependencies is very helpful in the event your package manager is unsupported or you are using a custom and nonstandard dependency management solution.

The FOSSA CLI will automatically read a `fossa-deps.yml` or a `fossa-deps.json` file in the root directory (usually the current working directory) when `fossa analyze` is run and parse dependencies from it. These dependencies will be added to the dependencies that are normally found when `fossa analyze` is run in the directory.

> Tip: Use a script to generate this file before running `fossa analyze` to keep your results updated.

To manually specify a dependency, you must provide the package type, package name, and optionally a package version, under the `referenced-dependencies` array, as shown here:

```yaml
referenced-dependencies:
- type: gem
  name: iron
- type: pypi
  name: Django
  version: "2.1.7"
```

The `name` and `type` fields are required and specify the name of the dependency and where to find it. The `version` field is optional and specifies the preferred version of dependency.

Note: When parsed, YAML considers text that could be a decimal number (such as 1.0 or 2.0) to be a number, not a string. This means that we'd parse the version 1.0 as 1. This probably isn't what you want. To avoid this, surround your version with quotes, as in "1.0".

Supported dependency types:

- `bower` - Bower dependencies that are typically found at found at [bower.io](https://registry.bower.io).
<!-- markdown-link-check-disable-next-line -->
- `cargo` - Rust dependencies that are typically found at [crates.io](https://crates.io/).
- `carthage` - Dependencies as specified by the [Carthage](https://github.com/Carthage/Carthage) package manager.
- `composer` - Dependencies specified by the PHP package manager [Composer](https://getcomposer.org/), which are located on [Packagist](https://packagist.org/).
- `cpan` - Dependencies located on the [CPAN package manager](https://www.cpan.org/).
- `cran` - Dependencies located on the [CRAN](https://www.cran.org/) like repository.
- `gem` - Dependencies which can be found at [RubyGems.org](https://rubygems.org/).
- `git` - Github projects (which appear as dependencies in many package managers). Specified as the full github repository `https://github.com/fossas/fossa-cli`.
- `go` - Go specific dependency. Many Go dependencies are located on Github, but there are some which look like the following `go.mongodb.org/mongo-driver` that have custom Go URLs.
- `hackage` - Haskell dependencies found at [Hackage](https://hackage.haskell.org/).
- `hex` - Erlang and Elixir dependencies that are found at [Hex.pm](https://hex.pm/).
- `maven` - Maven dependencies that can be found at many different sources. Specified as `name: javax.xml.bind:jaxb-api` where the convention is `groupId:artifactId`.
- `npm` - Javascript dependencies found at [npmjs.com](https://www.npmjs.com/).
- `nuget` - .NET dependencies found at [NuGet.org](https://www.nuget.org/).
- `paket` - .NET dependencies found at [fsprojects.github.io/Paket/](https://fsprojects.github.io/Paket/).
- `pub` - Dart dependencies found at [pub.dev](https://www.pub.dev/).
- `pypi` - Python dependencies that are typically found at [Pypi.org](https://pypi.org/).
- `swift` - Swift dependencies using the [Swift Package Manager](https://www.swift.org/package-manager/).
- `cocoapods` - Swift and Objective-C dependencies found at [Cocoapods.org](https://cocoapods.org/).
- `url` - The URL type allows you to specify only the download location of an archive (e.g.: `.zip`, .`tar.gz`, etc.) in the `name` field and the FOSSA backend will attempt to download and scan it. Example for a github source dependency `https://github.com/fossas/fossa-cli/archive/refs/tags/v3.3.12.tar.gz`. The `version` field will be silently ignored for `url` type dependencies.

Following dependency types are also supported but they require `arch`, `os`, and `osVersion` attributes:

- `apk` - Alpine packages. 
- `deb` - Debian packages.
- `rpm-generic` - Rpm packages.

At this moment the following `os` are supported:

- `alpine`
- `centos`
- `debian`
- `redhat`
- `ubuntu`
- `oraclelinux`
- `busybox`
- `sles`
- `fedora`

For example:

```yaml
referenced-dependencies:
 - name: musl
   version: 1.2.3-r0
   type: apk
   arch: x86_64
   os: alpine
   osVersion: 3.16.2

 - name: bash
   type: deb
   version: 5.1-6ubuntu1
   arch: amd64
   os: ubuntu
   osVersion: 22.04

 - name: bash
   type: rpm-generic
   version: 4.4.19-14.el8
   os: centos
   arch: x86_64
   osVersion: 8
```

You should choose `architecture`, `os`, and `osVersion` values based on the package
information. If you are unsure of how to identify these values for these packages
refer to the table below:

| type        | version                                                                                                                        | architecture                                                                                                                                                                    |
|-------------|--------------------------------------------------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| apk         | version value from `apk info -vv`<br>e.g. for `musl-1.2.3-r0` version is `1.2.3-r0`                                            | `A` field in alpine database, or `Architecture` field in PKGINFO.<br>You can also infer architecture from calling `arch` command on system where this <br>package is installed. |
| deb         | version value from `dpkg -p package`                                                                                           | `Architecture` field from `dpkg -p package` query                                                                                                                               |
| rpm-generic | `version-release` value from `rpm -qi package` query. <br>e.g `Version: 4.4.19 Release: 14.el-8` translates to `4.4.19-14.el8` | `Architecture` field from `rpm -qi package` query                                                                                                                               |

For `os` and `osVersion`, refer to `ID` value and `VERSION_ID` value from `/etc/os-release` of target system.


### Custom dependencies

FOSSA supports users that have dependencies that can't be automatically discovered or identified, by offering the ability to define new dependencies.

To do this, you must supply the name, version, and license of the dependency.  This creates a stub package which requires no source code or linkage to any other system, but still acts as a normal dependency in other areas of FOSSA, like reports and the dependency views.

You may also supply a description and/or url, but both are optional.  Note that these fields reference the dependency itself, and do not reference the parent project (the one at the current analysis directory), or the individual versions of the dependency.

```yaml
custom-dependencies:
# Custom dependencies need name, version, and license
- name: foo
  version: "1.2.3"
  license: "MIT or Apache-2.0"
# You can also provide a description and/or homepage. These values populate metadata fields in reports in the FOSSA web UI.
- name: foo-wrapper
  version: "1.2.3"
  license: MIT
  metadata:
    homepage: https://www.foowrapper.com/about
    description: Provides foo and a helpful interface around foo-like tasks.
```

Note: When parsed, YAML considers text that could be a decimal number (such as 1.0 or 2.0) to be a number, not a string. This means that we'd parse the version 1.0 as 1. This probably isn't what you want. To avoid this, surround your version with quotes, as in "1.0".

### Remote dependencies

FOSSA also supports dependencies that can't be automatically discovered or identified, but where the user has a URL where FOSSA can download the source code of the dependency.

To specify a remote dependency, you must provide the name, version, and download URL of the dependency. The FOSSA backend will attempt to download and scan any source code contained in an archive hosted at this URL.

For example, for a dependency released on a GitHub release, your URL might look like: `https://github.com/fossas/fossa-cli/archive/refs/tags/v3.3.12.tar.gz`.

You can also optionally add metadata fields ("description" and "homepage") to populate these fields in the FOSSA web UI (these fields can be displayed when generating reports).

```yaml
remote-dependencies:
# Remote dependencies need name, version, and url
- name: foo
  version: 1.2.3
  url: https://www.fooarchive.tar.gz
# You can also provide a description and/or homepage. These values populate metadata fields in reports in the FOSSA web UI.
- name: foo-wrapper
  version: 1.2.3
  url: https://www.foowrapper.tar.gz
  metadata:
    description: Provides foo and a helpful interface around foo-like tasks.
    homepage: https://www.foowrapper-home.com
```
