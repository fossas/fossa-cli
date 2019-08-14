# Go

## Support

Go support in FOSSA CLI depends on the following tools existing in your environment:

- Go (defaults to `go`, configure with `$GO_BINARY`)
- At least one of:
  1. Dep (defaults to `dep`, configure with `$DEP_BINARY`)
  2. Gomodules, the presence of `go.mod`.
  3. Glide (defaults to `glide`, configure with `$GLIDE_BINARY`)
  4. Godep (defaults to `godep`, configure with `$GODEP_BINARY`)
  5. Govendor (defaults to `govendor`, configure with `$GOVENDOR_BINARY`)
  6. Vndr (defaults to `vndr`, configure with `$VNDR_BINARY`)
  7. Gdm (defaults to `gdm`, configure with `$GDM_BINARY`)

## Configuration

### Automatic 

Run `fossa init` which detects all executable go packages in the directory and creates a configuration file. If you are trying to analyze a go library that does not contain any executables run `fossa init --include-all`. Refer to [Discovery](#Discovery) for more information on the auto-configuration logic.

### Manual

Add a module with `type: go`, `target` set to the name of the executable package, and `path` set to the location of the executable package. 

See [Options](#Options) for an in depth look at all of the available options for a Go module.

```yaml
analyze:
  modules:
    - name:   github.com/fossas/fossa-cli/cmd/fossa
      type:   go
      target: github.com/fossas/fossa-cli/cmd/fossa
      path:   cmd/fossa
      options:
        allow-unresolved: true
```

## Options

| Option                         |   Type   | Name                                                                    | Common Use Case                              |
| ------------------------------ | :------: | ----------------------------------------------------------------------- | -------------------------------------------- |
| `tags`                         | []string | [Tags](#tags-string)                                               | Project utilizes go build tags.              |
| `all-tags`                     |   bool   | [All Tags](#all-tags-bool)                                           | Make sure all OS and Arch tags are caught.   |
| `strategy`                     |  string  | [Strategy](#strategy-string)                                         | Specify a go package manager.                |
| `lockfile`                     |  string  | [Lockfile Path](#lockfilepath-string)                                | Specify a custom lockfile.                   |
| `manifest`                     |  string  | [Manifest Path](#manifestpath-string)                                | Specify a custom manifest file.              |
| `allow-unresolved`             |   bool   | [Allow Unresolved](#allow-unresolved-bool)                           | Dependency revision is not in lockfile.      |
| `allow-unresolved-prefix`      |  string  | [Allow Unresolved Prefix](#allow-unresolved-prefix-string)           | Allow a specific unresolved package.         |
| `allow-nested-vendor`          |   bool   | [Allow Nested Vendor](#allow-nested-vendor-bool)                     | Project's parent holds the desired lockfile. |
| `allow-deep-vendor`            |   bool   | [Allow Deep Vendor](#allow-deep-vendor-bool)                         | Project is deep in a vendor directory.       |
| `allow-external-vendor`        |   bool   | [Allow External Vendor](#allow-external-vendor-bool)                 | Read lockfiles of other projects.            |
| `allow-external-vendor-prefix` |  string  | [Allow External Vendor Prefix](#allow-external-vendor-prefix-string) | Read lockfiles that match the prefix.        |
<!--- In code but currently unused
| `skip-tracing`                 |   bool   | [Skip Import Tracing](#Skip-Tracing:-<bool>)                            | Skip dependency tracing.                     |
| `skip-project`                 |   bool   | [Skip Project](#Skip-Project:-<bool>)                                   | Skip project detection.                      |
--->


#### `tags: <[]string>` 

Allows you to specify different build tags and combinations for analysis. 
Example: 
```yaml
    tags:
      - windows
      - test
      - !windows
      - custom-test-build linux
```

#### `all-tags: <bool>`

Analyzes your project using a list of predefined build tags and merges the results. This analyzes your project with each tag but does not try any combinations. If you have a build that uses a combination build tag use the [Tags](#Tags:-<[]string>) option.

Predefined build tags:

- ``` "windows", "linux", "freebsd", "android", "darwin", "dragonfly", "nacl", "netbsd", "openbsd", "plan9", "solaris" "386", "amd64", "amd64p32", "arm", "armbe", "arm64", "arm64be", "ppc64", "ppc64le", "mips", "mipsle", "mips64", "mips64le", "mips64p32", "mips64p32le", "ppc", "s390", "s390x", "sparc", "sparc64"```

#### `strategy: <string>`

Manually specify the golang package manager being used. If this option is set, it is recommended [lockfile](#LockfilePath:-<string>) and [manifest](#ManifestPath:-<string>) be set as well. A list of supported strategies is as follows:
- ```list, gomodules, dep```
- `list`: Run `go list` and compare the results to the lockfile.
- `gomodules`: Parse `go.mod` and `go.sum` to find dependencies.
- `dep`: Parse `Gopkg.lock` and `Gopkg.toml` to find dependencies.

#### `lockfile: <string>`

If your project has a custom lockfile or location specify it with this flag.
Custom lockfiles interfere with the cli's ability to determine which package manager is being used and it is recommended that the [strategy](#strategy) flag also be set.

Example:
```yaml
    lockfile: config/customLockfile.lock
```
#### `manifest: <string>`                    

If your project has a custom manifest or location specify it with this flag.
Custom lockfiles interfere with the cli's ability to determine which package manager is being used and it is recommended that the [strategy](#strategy) flag also be set.

Example:
```yaml
    manifest: config/customManifest.toml
```
#### `allow-unresolved: <bool>`            

If analysis finds any dependencies from `go list` that do not have a revision specified in a lockfile, analysis will fail by default. If it is acceptable to skip these packages during analysis and upload an incomplete dependency graph set `allow-unresolved: true`. This problem is usually a result of the underlying project having build issues. 

#### `allow-unresolved-prefix: <string>`   

Specify a package that is allowed to be unresolved based on its prefix. 

Example: This will permit any dependencies with the prefix `github.com/fossas` to be unresolved. 
```yaml
allow-unresolved-prefix: github.com/fossas
```
See [Allow Unresolved](#Allow-unresolved:-<bool>) for use cases and steps to resolve the underlying issue.

#### `allow-nested-vendor: <bool>`         

Allows vendor folders to be nested and attempts to resolve using parent lockfile lookup.

#### `allow-deep-vendor: <bool>`           

Allows nested vendored dependencies to be resolved using ancestor lockfiles farther than their direct parent.

#### `allow-external-vendor: <bool>`       

Allows reading vendor lockfiles of other projects.

#### `allow-external-vendor-prefix: <string>`

If set, allow reading vendor lockfiles of projects whose import path's prefix matches. Multiple space-delimited prefixes can be specified.

## Discovery

Golang discovery runs `go list ./...` and takes all executable packages (those with package `main`) by default. To include the rest if you are trying to analyze a library without a `main` package run `fossa init --include-all`.

## Analysis

Analysis happens in 3 steps:

1. Use `go list <target>` to determine the imports of the specified package.
2. Determine the project's package manager if it has not been specified by searching for the following in order - `go.mod`, `Godeps/Godeps.json`, `vendor/vendor.conf`, `Gopkg.toml`, `vendor.conf`, `glide.yaml`, `Godeps`.
3. Match each package import to a dependency from the manifest to obtain revision information.

> note: If a revision is not found for an import, Analysis will fail. Either fix the import error or enable [Allow Unresolved](#allow-unresolved:-<bool>)

Step (3) is the most complex part of analysis. Not all import paths have revisions directly associated with them. It is possible to import a package called `github.com/author-name/project-name/package-folder/package` and depending on the tool, this import path may not have a revision associated with it. Instead, the revision is associated with the imported _project_: in this case, that would be `github.com/author-name/project-name`. 

In order to resolve the project of an import path, we check whether any of its prefixes have revisions in the Go project's manifests. If so, we assume that project contains the package we're importing and use its revision.

## Known limitations

- We do not currently support unvendored or tool-less imports.
  <!--
  Some approaches for solving this:
  - If there is no `vendor` folder, find the imported project on the filesystem by looking up its location relative to `$GOPATH`, and use `git` to find its revision.
  - If the import is vendored, get the project name from its filesystem location and check the checksum of its contents against revision hashes of known revisions published by that project.
  - Allow the user to manually specify revisions, using either top-level configuration file (e.g. `//.fossa.yaml`) or a package-level configuration file (e.g. `//vendor/github.com/author/project/.revision`)
  -->
## FAQ

### Q: Why are all dependencies listed in go.mod not found in the discovered dependency graph?

The FOSSA CLI finds all dependencies being used with `go list` and then finds their revision by parsing `go.mod`. Gomodules installs all transitive dependencies for a dependency, including the packages which are unused in the first party code. This results in many transitive dependencies listed in `go.mod` never actually being utilized. 

Example: You need package `foo` from source `company/repository`. `go.mod` will import all packages including `company/repository/bar` and its transitive dependencies. `company/repository/bar`'s dependencies will go unused in your project and will not be picked up by `fossa-cli` but will appear in `go.mod`.

### Q: I am in the process of updating to a new package manager, how can I obtain a scan for the old package manager?

The FOSSA CLI finds what package manager is being used as specified by the fallback strategy listed in [Analysis](#analysis) Step 2. If the package manager you are migrating to is earlier in the list you will need to set the correct strategy, lockfile, and manifest in your configuration file.

Example: You are migrating to `gomodules` but have not fully converted, as result fossa fails whenever it tries to find dependencies. You are however using `dep` and would like to make sure that those dependencies are detected. Add the following to the module in your configuration file:

```yaml
analyze:
  modules:
  - name: github.com/fossas/fossa-cli/cmd/fossa
    type: go
    target: github.com/fossas/fossa-cli/cmd/fossa
    path: .
    options:
      strategy: manifest:dep
      lockfile: Gopkg.lock
      manifest: Gopkg.toml
```

> note: this example is actually how to do this for the FOSSA CLI repository itself.