# Swift Package Manager

## Project Discovery

Find all files named: `Package.swift` or find Xcode's project file named: `project.pbxproj`. 
We will not scan `.build` directory if the `Package.swift` or Xcode project file is discovered. 

# Swift Analysis

| Strategy                                                                 | Direct Deps        | Deep Deps          | Edges | Classifies Test Dependencies | Container Scanning (experimental) |
| ------------------------------------------------------------------------ | ------------------ | ------------------ | ----- | ---------------------------- | --------------------------------- |
| Parse dependencies from `Package.swift`                                  | :white_check_mark: | :x:                | :x:   | :x:                          | :white_check_mark:                |
| Parse dependencies from `Package.swift` and `Package.resolved`           | :white_check_mark: | :white_check_mark: | :x:   | :x:                          | :white_check_mark:                |
| Parse dependencies from Xcode's `project.pbxproj`                        | :white_check_mark: | :x:                | :x:   | :x:                          | :white_check_mark:                |
| Parse dependencies from Xcode's `project.pbxproj` and `Package.resolved` | :white_check_mark: | :white_check_mark: | :x:   | :x:                          | :white_check_mark:                |

- Manifest file: `Package.swift`, must begin with `// swift-tools-version:` string, followed by version number specifier. 
- We follow swift package manager's convention and presume properties of the package are defined in a single nested initializer statement and are not modified after initialization.
- Valid Xcode project for swift, is defined by the discovery of `project.pbxproj` file in ASCII plist format with at least one `XCRemoteSwiftPackageReference` object in its content.

## Limitations

- Path dependencies are ignored in the analysis (e.g. `package(path: "./../local-pkg")`)
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

If `Package.resolved` is discovered, the following deep dependencies will be identified, however, we will not identify the edges in the dependency graph:

- https://github.com/apple/swift-log.git with version 1.4.2

If `Package.resolved` is not discovered, only direct dependencies will be reported. 

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

If the `Package.resolved` is discovered, deep dependencies will be identified. If not, only direct dependencies listed in xcode project file will be identified. In either case, no edges among dependencies will be reported.

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

This is a current limitation. For swift package manager analysis, we only support non-path dependencies at the moment. 
To include local dependencies, you can use `fossa-deps.yml` file to upload the local package for license scanning and analysis.

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

- [Swift Package Manager](https://github.com/apple/swift-package-manager)
- [Package.swift, must begin with version specifier](https://github.com/apple/swift-package-manager/blob/main/Documentation/PackageDescription.md#about-the-swift-tools-version)
- [Package.swift, must be defined in a single nested statement, and should not be modified after initialization](https://github.com/apple/swift-package-manager/blob/main/Documentation/PackageDescription.md#package)