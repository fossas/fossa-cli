# Archive

FOSSA supports repositories that vendorize archives of known open source packages in other supported integration formats.

## Usage

Add a `vendoredarchives` module with an empty path in your `fossa.yml` config.

```yaml
analyze:
  modules:
    - name: your-custom-project
      path: .
      type: vendoredarchives
```

### Archive Format Support

Currently, the following archive formats are detected in the package subdirectory:

 - Tar Files (`*.tar.gz` or `*.tgz`)

### Dependency Resolution

Currently, the archive implementation supports the following package types:

 - Node Modules (`package.json` files)