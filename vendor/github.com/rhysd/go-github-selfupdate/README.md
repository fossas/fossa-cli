Self-Update Mechanism for Go Commands using GitHub
==================================================

[![GoDoc Badge][]][GoDoc]
[![TravisCI Status][]][TravisCI]
[![AppVeyor Status][]][AppVeyor]
[![Codecov Status][]][Codecov]

[go-github-selfupdate][] is a Go library to provide self-update mechanism to command line tools.

Go does not provide the way to install/update the stable version of tools. By default, Go command line
tools are updated:

1. using `go get -u`, but it is not stable because HEAD of the repository is built
2. using system's package manager, but it is harder to release because of dpeneding on the platform
3. downloading executables from GitHub release page, but it requires users to download and put it manually

[go-github-selfupdate][] resolves the problem of 3. by detecting the latest release, downloading it and
putting it to `$GOPATH/bin` automatically.

[go-github-selfupdate][] detects the information of the latest release via [GitHub Releases API][] and
checks the current version. If newer version than itself is detected, it downloads released binary from
GitHub and replaces itself.

- Automatically detects the latest version of released binary on GitHub
- Retrieve the proper binary for the OS and arch where the binary is running
- Update the binary with rollback support on failure
- Tested on Linux, macOS and Windows (using Travis CI and AppVeyor)
- Many archive and compression formats are supported (zip, tar, gzip, xzip)
- Support private repositories
- Support [GitHub Enterprise][]

And small wrapper CLIs are provided:

- [detect-latest-release](./cmd/detect-latest-release): Detect the latest release of given GitHub repository from command line
- [go-get-release](./cmd/go-get-release): Like `go get`, but install a release binary from GitHub instead

[go-github-selfupdate]: https://github.com/rhysd/go-github-selfupdate
[GitHub Releases API]: https://developer.github.com/v3/repos/releases/

## Try Out Example

Example to understand what this library does is prepared as [CLI](./cmd/selfupdate-example/main.go).

Install it at first.

```
$ go get -u github.com/rhysd/go-github-selfupdate/tree/master/cmd/selfupdate-example
```

And check the version by `-version`. `-help` flag is also available to know all flags.

```
$ selfupdate-example -version
```

It should show `v1.2.3`.

Then run `-selfupdate`

```
$ selfupdate-example -selfupdate
```

It should replace itself and finally shows a message containing release notes.

Please check the binary version is updated to `v1.2.4` with `-version`. The binary is up-to-date.
So running `-selfupdate` again only shows 'Current binary is the latest version'.

### Real World Examples

Following tools are using this library.

- [dot-github](https://github.com/rhysd/dot-github)
- [dotfiles](https://github.com/rhysd/dotfiles)
- [github-clone-all](https://github.com/rhysd/github-clone-all)
- [pythonbrew](https://github.com/utahta/pythonbrew)
- [akashic](https://github.com/cowlick/akashic)
- [butler](https://github.com/netzkern/butler)

## Usage

### Code Usage

It provides `selfupdate` package.

- `selfupdate.UpdateSelf()`: Detect the latest version of itself and run self update.
- `selfupdate.UpdateCommand()`: Detect the latest version of given repository and update given command.
- `selfupdate.DetectLatest()`: Detect the latest version of given repository.
- `selfupdate.DetectVersion()`: Detect the user defined version of given repository.
- `selfupdate.UpdateTo()`: Update given command to the binary hosted on given URL.
- `selfupdate.Updater`: Context manager of self-upadte process. If you want to customize some behavior
  of self-update (e.g. specify API token, use GitHub Enterprise, ...), please make an instance of
  `Updater` and use its methods.

Following is the easiest way to use this package.

```go
import (
    "log"
    "github.com/blang/semver"
    "github.com/rhysd/go-github-selfupdate/selfupdate"
)

const version = "1.2.3"

func doSelfUpdate() {
    v := semver.MustParse(version)
    latest, err := selfupdate.UpdateSelf(v, "myname/myrepo")
    if err != nil {
        log.Println("Binary update failed:", err)
        return
    }
    if latest.Version.Equals(v) {
        // latest version is the same as current version. It means current binary is up to date.
        log.Println("Current binary is the latest version", version)
    } else {
        log.Println("Successfully updated to version", latest.Version)
        log.Println("Release note:\n", latest.ReleaseNotes)
    }
}
```

Following asks user to update or not.

```go
import (
    "bufio"
    "github.com/blang/semver"
    "github.com/rhysd/go-github-selfupdate/selfupdate"
    "log"
    "os"
)

const version = "1.2.3"

func confirmAndSelfUpdate() {
    latest, found, err := selfupdate.DetectLatest("owner/repo")
    if err != nil {
        log.Println("Error occurred while detecting version:", err)
        return
    }

    v := semver.MustParse(version)
    if !found || latest.Version.Equals(v) {
        log.Println("Current version is the latest")
        return
    }

    fmt.Print("Do you want to update to", latest.Version, "? (y/n): ")
    input, err := bufio.NewReader(os.Stdin).ReadString('\n')
    if err != nil || (input != "y\n" && input != "n\n") {
        log.Println("Invalid input")
        return
    }
    if input == "n\n" {
        return
    }

    if err := selfupdate.UpdateTo(latest.AssetURL, os.Args[0]); err != nil {
        log.Println("Error occurred while updating binary:", err)
        return
    }
    log.Println("Successfully updated to version", latest.Version)
}
```

If GitHub API token is set to `[token]` section in `gitconfig` or `$GITHUB_TOKEN` environment variable,
this library will use it to call GitHub REST API. It's useful when reaching rate limits or when using
this library with private repositories.

Please see [the documentation page][GoDoc] for more detail.

This library should work with [GitHub Enterprise][]. To configure API base URL, please setup `Updater`
instance and use its method instead (Actually all functions above are just a shortcuts of methods of
`Updater` instance).

Following is an example of usage with GitHub Enterprise.

```go
import (
    "log"
    "github.com/blang/semver"
    "github.com/rhysd/go-github-selfupdate/selfupdate"
)

const version = "1.2.3"

func doSelfUpdate(token string) {
    v := semver.MustParse(version)
    up, err := selfupdate.NewUpdater(selfupdate.Config{
        APIToken: token,
        EnterpriseBaseURL: "https://github.your.company.com/api/v3",
    })
    latest, err := up.UpdateSelf(v, "myname/myrepo")
    if err != nil {
        log.Println("Binary update failed:", err)
        return
    }
    if latest.Version.Equals(v) {
        // latest version is the same as current version. It means current binary is up to date.
        log.Println("Current binary is the latest version", version)
    } else {
        log.Println("Successfully updated to version", latest.Version)
        log.Println("Release note:\n", latest.ReleaseNotes)
    }
}
```

If `APIToken` field is not given, it tries to retrieve API token from `[token]` section of `.gitconfig`
or `$GITHUB_TOKEN` environment variable. If no token is found, it raises an error because GitHub Enterprise
API does not work without authentication.

If your GitHub Enterprise instance's upload URL is different from the base URL, please also set `EnterpriseUploadURL`
field

### Naming Rules of Released Binaries

go-github-selfupdate assumes that released binaries are put for each combination of platforms and archs.
Binaries for each platform can be easily built using tools like [gox][]

You need to put the binaries with the following format.

```
{cmd}_{goos}_{goarch}{.ext}
```

`{cmd}` is a name of command.
`{goos}` and `{goarch}` are the platform and the arch type of the binary.
`{.ext}` is a file extension. go-github-selfupdate supports `.zip`, `.gzip`, `.tar.gz` and `.tar.xz`.
You can also use blank and it means binary is not compressed.

If you compress binary, uncompressed directory or file must contain the executable named `{cmd}`.

And you can also use `-` for separator instead of `_` if you like.

For example, if your command name is `foo-bar`, one of followings is expected to be put in release
page on GitHub as binary for platform `linux` and arch `amd64`.

- `foo-bar_linux_amd64` (executable)
- `foo-bar_linux_amd64.zip` (zip file containing `foo-bar`)
- `foo-bar_linux_amd64.tar.gz` (tar file containing `foo-bar`)
- `foo-bar_linux_amd64.xz` (xzip file of the executable `foo-bar`)
- `foo-bar-linux-amd64.tar.gz` (`-` is also ok for separator)

To archive the executable directly on Windows, `.exe` can be added before file extension like
`foo-bar_windows_amd64.exe.zip`.

[gox]: https://github.com/mitchellh/gox


### Naming Rules of Versions (=Git Tags)

go-github-selfupdate searches binaries' versions via Git tag names (not a release title).
When your tool's version is `1.2.3`, you should use the version number for tag of the Git
repository (i.e. `1.2.3` or `v1.2.3`).

This library assumes you adopt [semantic versioning][]. It is necessary for comparing versions
systematically.

Prefix before version number `\d+\.\d+\.\d+` is automatically omitted. For example, `ver1.2.3` or
`release-1.2.3` are also ok.

Tags which don't contain a version number are ignored (i.e. `nightly`). And releases marked as `pre-release`
are also ignored.

[semantic versioning]: https://semver.org/


### Structure of Releases

In summary, structure of releases on GitHub looks like:

- `v1.2.0`
  - `foo-bar-linux-amd64.tar.gz`
  - `foo-bar-linux-386.tar.gz`
  - `foo-bar-darwin-amd64.tar.gz`
  - `foo-bar-windows-amd64.zip`
  - ... (Other binaries for v1.2.0)
- `v1.1.3`
  - `foo-bar-linux-amd64.tar.gz`
  - `foo-bar-linux-386.tar.gz`
  - `foo-bar-darwin-amd64.tar.gz`
  - `foo-bar-windows-amd64.zip`
  - ... (Other binaries for v1.1.3)
- ... (older versions)

Tags which don't contain a version number are ignored (i.e. `nightly`). And releases marked
as `pre-release` are also ignored.

### Development

#### Running tests

All library sources are put in `/selfupdate` directory. So you can run tests as following
at the top of the repository:

```
$ go test -v ./selfupdate
```

Some tests are not run without setting a GitHub API token because they call GitHub API too many times.
To run them, please generate an API token and set it to an environment variable.

```
$ export GITHUB_TOKEN="{token generated by you}"
$ go test -v ./selfupdate
```

Above command run almost all tests and it's enough to check the behavior before creating a pull request.
Some tests are still not tested because they depend on my personal API access token, though; for repositories
on GitHub Enterprise or private repositories on GitHub.

#### Debugging

This library can output logs for debugging. By default, logger is disabled.
You can enable the logger by following and can know the details of the self update.

```go
selfupdate.EnableLog()
```

#### CI

Tests run on CIs (Travis CI, Appveyor) are run with the token I generated. However, because of security
reason, it is not used for the tests for pull requests. In the tests, a GitHub API token is not set and
API rate limit is often exceeding. So please ignore the test failures on creating a pull request.

## Dependencies

This library utilizes
- [go-github][] to retrieve the information of releases
- [go-update][] to replace current binary
- [semver][] to compare versions
- [xz][] to support XZ compress format

> Copyright (c) 2013 The go-github AUTHORS. All rights reserved.

> Copyright 2015 Alan Shreve

> Copyright (c) 2014 Benedikt Lang <github at benediktlang.de>

> Copyright (c) 2014-2016  Ulrich Kunitz

[go-github]: https://github.com/google/go-github
[go-update]: https://github.com/inconshreveable/go-update
[semver]: https://github.com/blang/semver
[xz]: https://github.com/ulikunitz/xz

## What is different from [tj/go-update][]?

This library goal is the same as tj/go-update, but it's different in following points.

tj/go-update:

- does not support Windows
- only allows `v` for version prefix
- does not ignore pre-release
- has [only a few tests](https://github.com/tj/go-update/blob/master/update_test.go)
- supports Apex store for putting releases

[tj/go-update]: https://github.com/tj/go-update

## License

Distributed under the [MIT License](LICENSE)

[GoDoc Badge]: https://godoc.org/github.com/rhysd/go-github-selfupdate/selfupdate?status.svg
[GoDoc]: https://godoc.org/github.com/rhysd/go-github-selfupdate/selfupdate
[TravisCI Status]: https://travis-ci.org/rhysd/go-github-selfupdate.svg?branch=master
[TravisCI]: https://travis-ci.org/rhysd/go-github-selfupdate
[AppVeyor Status]: https://ci.appveyor.com/api/projects/status/1tpyd9q9tw3ime5u/branch/master?svg=true
[AppVeyor]: https://ci.appveyor.com/project/rhysd/go-github-selfupdate/branch/master
[Codecov Status]: https://codecov.io/gh/rhysd/go-github-selfupdate/branch/master/graph/badge.svg
[Codecov]: https://codecov.io/gh/rhysd/go-github-selfupdate
[GitHub Enterprise]: https://enterprise.github.com/home
