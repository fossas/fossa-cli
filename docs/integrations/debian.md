# Debian

## Support

Debian package scanning is supported provided that the following are met:
1. Running on a Debian operating system.
2. `apt-cache` is available.
3. Premium fossa subscription to leverage the [archive uploader](#./archive.md).

## Configuration

> Note: There is no automatic discovery for Debian packages as this would cause unexpected behavior for users not interested in Debian scanning and too much information for those who are.

### Manual

Add a module with `type: debian` `target` set to the name of the debian dependency that is to be analyzed, and `path` is optional.

```yaml
analyze:
  modules:
  - name: fossa
    type: debian
    target: apt
*   path: .
```

## Analysis

Analyzing a debian package is straightforward and leverages the archive uploader in order to fully scan all licenses. Determining a dependency graph happens in these steps:
1. Find all transitive dependencies for the `target` package by running `apt-cache depends --recurse <target>`.
2. Upload all dependencies that are found to exist and store their locator information.
3. Construct a dependency graph by running `apt-cache depends <target>` on each dependency found in the previous step.