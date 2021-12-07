## Binary Discovery

### What is it?

FOSSA supports the ability to flag all binary dependencies discovered in your project source tree as unlicensed dependencies via an opt-in flag.

The core idea behind this feature is that some organizations wish to validate all potential sources of intellectual property rights, and binaries are potential sources of intellectual property rights data for which we typically cannot automatically discover licensing information.

FOSSA detects binaries in the same manner as git: we inspect the first 8000 bytes of the file (or the whole file if less than 8000 bytes).
If it contains a NUL (`0`) byte, we consider the file to be binary.

### Displaying discovered binaries

Binaries discovered via this feature are displayed in the FOSSA UI as `user` dependencies.

The name of the dependency is the path to the binary within the project, and the version of the dependency is the hash of the binary file that was discovered.
The description of the dependency is "Binary discovered in source tree".

### Correcting discovered binaries

Most binaries cannot be statically analyzed for licensing or other information. As such, users need to correct the information about the binary using the standard FOSSA corrections flow.
Users can edit the information about the binary, such as its name or licensing information. Users may also ignore binaries that are not relevant.

These corrections persist in future revisions of the project so long as the binary does not move to a different path in the project.
If it does, the binary appears in the list again without any corrections.
If the binary content changes but stays at the same location on disk, these corrections persist and a different hash is displayed in the version field of the dependency.

### Filtering discovered binaries

This feature has the potential to be quite noisy, as most projects have many binary files.

This feature supports the standard path filters used in FOSSA CLI (via `--exclude-path` and `--only-path` flags, or [.fossa.yml](../../files/fossa-yml.md#paths-)) to customize which paths are scanned.

### Inspecting binaries

Some binaries are capable of being inspected to pre-fill information.

#### JAR

We have two tactics for unpacking information from a JAR:

1. Read a `pom.xml` file from inside `META-INF`
2. Read `META-INF/MANIFEST.MF`

We prefer the `pom.xml` if present, and only fallback to `META-INF/MANIFEST.MF` if `pom.xml` is not found.

#### Read `pom.xml`

We unpack the JAR and search inside the `META-INF` directory for `pom.xml` files. We then select the `pom.xml` with the *shortest path* and use that as the representative `pom.xml` for the JAR.

From the `pom.xml` we read:

- `project.groupId` and `project.artifactId` are combined to make the dependency description.
- `project.version` is used for the dependency version.
- Entries in `project.licenses` are extracted for their `name` field, which are concatenated and used as the dependency license.

#### Read `META-INF/MANIFEST.MF`

We unpack the JAR and search inside for a `META-INF/MANIFEST.MF` file.

From that file we read:

- `Bundle-SymbolicName`, if present, is used for the dependency description. If `Bundle-SymbolicName` is not present, we fallback to `Implementation-Title`.
- `Implementation-Version` is used for the dependency version.

#### AAR

AAR files are treated identically to JAR files.
