# Binary Discovery

FOSSA supports the ability to flag all binary dependencies discovered in your project source tree as unlicensed dependencies via an opt-in flag.

The core idea behind this feature is that some organizations wish to validate all potential sources of intellectual property rights,
and binaries are potential sources of intellectual property rights data for which we typically cannot automatically discover licensing information.

## Discovery

Find all files that contain a NUL (`0`) byte in the first 8000 bytes of the file.

## Analysis

The following strategies are implemented in a "fallback" manner, 
meaning that for any matching file extension we try strategies in the order listed here until a strategy succeeds.

| File Extension   | Analysis                       |
|------------------|--------------------------------|
| `.jar` or `.aar` | Read `pom.xml`                 |
| `.jar` or `.aar` | Read `MANIFEST.MF`             |
| Anything else    | Create user-defined dependency |

### Analyzing jar and aar dependencies

#### Read `pom.xml`

We unpack the archive and search inside the `META-INF` directory for `pom.xml` files.
We then select the `pom.xml` with the *shortest path* and use that as the representative `pom.xml` for the JAR.

From the `pom.xml` we read:

- `project.groupId` and `project.artifactId` are combined to make the dependency description.
- `project.version` is used for the dependency version.
- Entries in `project.licenses` are extracted for their `name` field, which are concatenated and used as the dependency license.

#### Read `MANIFEST.MF`

We unpack the archive and search inside for a `META-INF/MANIFEST.MF` file.

From that file we read:

- `Bundle-SymbolicName`, if present, is used for the dependency description. If `Bundle-SymbolicName` is not present, we fallback to `Implementation-Title`.
- `Implementation-Version` is used for the dependency version.

### Creating user-defined binary dependencies

Any binaries not discovered using a different feature are considered user-defined.
Binaries discovered via this feature are displayed in the FOSSA UI as `user` dependencies.

The name of the dependency is the path to the binary within the project, and the version of the dependency is the hash of the binary file that was discovered.
The description of the dependency is "Binary discovered in source tree".
