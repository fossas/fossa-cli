# Go

## Support

Go support in FOSSA CLI depends on the following tools existing in your environment:

- Go (defaults to `go`, configure with `$GO_BINARY`)
- At least one of:
  1. Dep (defaults to `dep`, configure with `$DEP_BINARY`)
  2. Glide (defaults to `glide`, configure with `$GLIDE_BINARY`)
  3. Godep (defaults to `godep`, configure with `$GODEP_BINARY`)
  4. Govendor (defaults to `govendor`, configure with `$GOVENDOR_BINARY`)
  5. Vndr (defaults to `vndr`, configure with `$VNDR_BINARY`)
  6. Gdm (defaults to `gdm`, configure with `$GDM_BINARY`)

## Usage

Add a `go` module with the GOPATH to your entry point.

```yaml
analyze:
  modules:
    - name:   your-go-project
      type:   go
      target: github.com/you/your-project
      path:   github.com/you/your-project
```

## Options
|            Option            |  Type   |           Name            |                 Common Use Case                  |
| ---------------------------- | ------- | ------------------------- | ------------------------------------------------ |
| os                           | string  | [BuildOS](#BuildOS)                   | Set the OS to build the go module for.           |
| arch                         | string  | [BuildArch](#BuildArch)                 | Set the architecture to build the go module for. |
| strategy                     | string  | [Strategy](#Strategy)                  | Set the fossa analysis strategy.                 |
| lockfile                     | string  | [LockfilePath](#LockfilePath)              | Path to a custom lockfile.                       |
| allow-external-vendor-prefix | boolean | [AllowExternalVendorPrefix](#AllowExternalVendorPrefix) | Unsure                                           |

#### BuildOS
#### BuildOS
#### Strategy
#### LockfilePath
#### AllowExternalVendorPrefix


## Design

The Go plugin assumes that the configured entry point is in a larger project managed by one of our 5 supported tools. It detects the project folder by looking for the nearest parent folder that also contains a tool manifest. The manifests searched for are:

1. `dep`: `Gopkg.toml`
2. `glide`: `glide.yaml`
3. `godep`: `Godeps/Godeps.json`
4. `govendor`: `vendor/vendor.json`
5. `vndr`: `vendor.conf`

When the plugin finds a project folder, it assumes that every tool with a manifest in the project folder is used to manage the current project.

### Building

Builds are run differently depending on the detected tools in the project folder:

1. `dep`: `dep ensure`
2. `glide`: `glide install`
3. `godep`: `godep save`
4. `govendor`: `govendor sync`
5. `vndr`: `vndr`

If `--force` is set, each tool also clears its cache differently:

1. `dep`: `rm -rf vendor Gopkg.lock`
2. `glide`: `rm -rf vendor glide.lock`
3. `godep`: `rm -rf vendor Godeps`
4. `govendor`: `mv vendor/vendor.json vendor.json.bak && rm -rf vendor && mkdir -p vendor && mv vendor.json.bak vendor/vendor.json`
5. `vndr`: `rm-rf vendor`

### Analysis

Analysis happens in 3 steps:

1. Use `depth` to perform import tracing on the specified entry point.
2. Read all tool manifests in the project folder.
3. Resolve all import paths into projects, and look up the project in the manifests to determine the revision of the imported package.

The most complex part here is step (3). Not all import paths have revisions directly associated with them. For example, it's possible to import a package called `github.com/author-name/project-name/package-folder/package`. Depending on the tool, this import path may not have a revision associated with it. Instead, the revision is associated with the imported _project_: in this case, that would be `github.com/author-name/project-name`.

In order to resolve the project of an import path, we check whether any of its prefixes have revisions in the Go project's manifests. If so, we assume that project contains the package we're importing and use its revision.

#### Known limitations

- We do not currently support unvendored or tool-less imports.
  <!--
  Some approaches for solving this:
  - If there is no `vendor` folder, find the imported project on the filesystem by looking up its location relative to `$GOPATH`, and use `git` to find its revision.
  - If the import is vendored, get the project name from its filesystem location and check the checksum of its contents against revision hashes of known revisions published by that project.
  - Allow the user to manually specify revisions, using either top-level configuration file (e.g. `//.fossa.yaml`) or a package-level configuration file (e.g. `//vendor/github.com/author/project/.revision`)
  -->
