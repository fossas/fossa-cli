# Swift Package Manager

## Project Discovery

Find all files named: `Package.swift` or find Xcode's project file named: `project.pbxproj`. 
We will not scan `.build` directory if the `Package.swift` or Xcode project file is discovered. 

# Swift Analysis

| Strategy                                                                 | Direct Deps        | Transitive Deps    | Edges              | Classifies Test Dependencies | Container Scanning |
| ------------------------------------------------------------------------ | ------------------ | ------------------ | ------------------ | ---------------------------- | ------------------ |
| Resolve the package graph with `swift package show-dependencies`         | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x:                          | :x:                |
| Parse dependencies from `Package.swift`                                  | :white_check_mark: | :x:                | :x:                | :x:                          | :white_check_mark: |
| Parse dependencies from `Package.swift` and `Package.resolved`           | :white_check_mark: | :white_check_mark: | :x:                | :x:                          | :white_check_mark: |
| Parse dependencies from `Package.resolved` only                          | :white_check_mark: | :x:                | :x:                | :x:                          | :white_check_mark: |
| Parse dependencies from Xcode's `project.pbxproj`                        | :white_check_mark: | :x:                | :x:                | :x:                          | :white_check_mark: |
| Parse dependencies from Xcode's `project.pbxproj` and `Package.resolved` | :white_check_mark: | :white_check_mark: | :x:                | :x:                          | :white_check_mark: |

For a Swift package (a directory with a `Package.swift`), FOSSA CLI first runs
`swift package show-dependencies --format json` in the package directory and builds the dependency graph from its output.
If the command fails (for example because `swift` is not installed, or the dependencies cannot be fetched), FOSSA CLI falls back to parsing `Package.swift` and `Package.resolved` as described below, and reports the failure as a warning.
If `Package.swift` cannot be parsed either, FOSSA CLI falls back once more to `Package.resolved` alone: every pinned package is then reported as a direct dependency, because the lockfile does not record which packages the manifest declares directly.
With `--static-only-analysis` the command is never run, and in `--strict` mode no fallback is used.

Note that `swift package show-dependencies` resolves the package like `swift build` does: it may fetch dependencies over the network into `.build/checkouts` and create or update `Package.resolved`.
It runs fastest after a build, when the checkouts already exist.

Xcode projects are always analyzed by parsing `project.pbxproj`, because `swift package` cannot read them.

- Manifest file: `Package.swift`, must begin with `// swift-tools-version:` string, followed by version number specifier. 
- We follow swift package manager's convention and presume properties of the package are defined in a single nested initializer statement and are not modified after initialization.
- Valid Xcode project for swift, is defined by the discovery of `project.pbxproj` file in ASCII plist format with at least one `XCRemoteSwiftPackageReference` object in its content.

## Limitations

- When `swift package show-dependencies` is used, a path dependency (e.g. `package(path: "./../local-pkg")`) is reported as a [path dependency](../../../experimental/path-dependency.md), which is only analyzed with `--experimental-analyze-path-dependencies`. Without that flag the local package is omitted and its remote dependencies are reported as transitive dependencies.
- When `Package.swift` is parsed, path dependencies are ignored in the analysis.
- If the Xcode project dependencies are sourced via a local path, they will be ignored in the analysis.
- Only Xcode project files in ASCII plist format with UTF-8 encoding are supported.

## Example

Create Package.swift file in the directory. Add dependencies, targets, products, and source code. Example Package.swift file is shown below. By convention, the properties of a Package are defined in a single nested initializer statement, and not modified after initialization.

```swift
// swift-tools-version:5.4.0
import PackageDescription

let package = Package(
    name: "Example",
    defaultLocalization: "en",
    products: [],
    dependencies: [
        .package(name: "grpc-swift", url: "https://github.com/grpc/grpc-swift.git", from: "1.0.0"),
    ]
)
```

We can update and resolve dependencies by performing `swift package update`. Executing this will create Package.resolved in the directory. An example file is shown below:

```json
{
  "object": {
    "pins": [
      {
        "package": "grpc-swift",
        "repositoryURL": "https://github.com/grpc/grpc-swift.git",
        "state": {
          "branch": null,
          "revision": "14e1ea3350892a864386517c037e11fb68baf818",
          "version": "1.3.0"
        }
      },
      {
        "package": "swift-log",
        "repositoryURL": "https://github.com/apple/swift-log.git",
        "state": {
          "branch": null,
          "revision": "5d66f7ba25daf4f94100e7022febf3c75e37a6c7",
          "version": "1.4.2"
        }
      }
    ]
  },
  "version": 1
}

```
Note: Only a few pins are shown above for brevity.

### `Package.swift` and `Package.resolved`

When the analysis is performed (e.g. `fossa analyze -o`), we will identify the following as direct dependencies:

- https://github.com/grpc/grpc-swift.git with version 1.3.0

If `Package.resolved` is discovered, the following transitive dependencies will be identified, however, we will not identify the edges in the dependency graph:

- https://github.com/apple/swift-log.git with version 1.4.2

If `Package.resolved` is not discovered, only direct dependencies will be reported. 

If `Package.swift` cannot be parsed but `Package.resolved` is discovered, all of the above will be reported as direct dependencies.

### Xcode Project and `Package.resolved`

For Xcode project using swift package manager to manage swift package dependencies, Xcode project file named `project.pbxproj` will be analyzed. In the Xcode project file, `XCRemoteSwiftPackageReference` objects will be used to identify swift packages that are direct dependencies. For the analysis, at least one such reference must exist in the file. If no such references are found, we will not consider the Xcode project in the swift analysis.

Excerpt from example `project.pbxproj`:

```
// !$*UTF8*$!
{
  archiveVersion = 1;
  classes = {
  };
  objectVersion = 52;
  objects = {
    
    ...

    170A463726ECEDEF002DDFB8 /* XCRemoteSwiftPackageReference "example-package-deckofplayingcards" */ = {
      isa = XCRemoteSwiftPackageReference;
      repositoryURL = "https://github.com/apple/example-package-deckofplayingcards";
      requirement = {
        branch = main;
        kind = branch;
      };
    };

    ...
  };
  rootObject = 17874CD926C46B8500D16CA8 /* Project object */;
} 
```

If the `Package.resolved` is discovered, transitive dependencies will be identified. If not, only direct dependencies listed in xcode project file will be identified. In either case, no edges among dependencies will be reported.

## F.A.Q

### How do I *only perform analysis* for swift package dependencies?

You can explicitly specify the analysis target in `.fossa.yml` file.

The example below will exclude all analysis targets except swift. 

```yaml
# .fossa.yml 

version: 3
targets:
  only:
    - type: swift
```

### Swift packages sourced from local directories are not discovered in the analysis. Is there a workaround?

When the package graph is resolved with `swift package show-dependencies`, local packages are reported as path dependencies and can be analyzed with the `--experimental-analyze-path-dependencies` flag.
Otherwise, you can use `fossa-deps.yml` file to upload the local package for license scanning and analysis.

```yaml
# in fossa-deps.yml

vendored-dependencies:
- name: MyLocalPackage
  path: /Jenkins/App/Resources/MyLocalPackage # path can be either a file or a folder.
  version: 3.4.16 # revision will be set to the MD5 hash of the file path if left unspecified.
```

Note: License scanning currently operates by uploading the files at the specified path to a secure S3 bucket. All files that do not contain licenses are then removed after 2 weeks.
Refer to [vendored dependencies](../../../../features/vendored-dependencies.md) for more details. 

### When performing `fossa list-targets`, Xcode project using swift packages are not getting discovered.

For swift, we consider the Xcode project to be a valid Xcode project, if and only if it meets the following requirements:
- Xcode project file named: `project.pbxproj` exists in the directory.
- Xcode project file must be in ASCII plist format with UTF-8 encoding.
- Xcode project file has at least one object, with isa of `XCRemoteSwiftPackageReference`. 

## References

- [Swift Package Manager](https://docs.swift.org/swiftpm/documentation/packagemanagerdocs)
- [Package.swift documentation](https://docs.swift.org/swiftpm/documentation/packagedescription/package/)
