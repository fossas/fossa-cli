# Archive

## Support

Repositories that vendorize archives of known open source packages in other supported integration formats are supported using the FOSSA archive uploader. This can be used to upload unsupported languages and run a full license scan on the included files.

> Note: The archive uploader by default uploads all source code of the specified file or directory to the endpoint specified in the configuration.

## Usage

> Note: Automatic configuration does not exist for archive modules.

Add a module with `type` set to `raw` and `target` set to the directory or file to be uploaded.

```yaml
analyze:
  modules:
    - name: your-custom-project
      type: raw
      path: .
      target: ./<directory-or-file>
```

### Analysis

Archive analysis is done entirely by the FOSSA backend and the FOSSA-CLI's only job is to upload source code so that it can be analyzed. The following steps are taken to ensure this is done properly:
1. A tarball is created from all files located in the `target`.
2. FOSSA-CLI asks FOSSA for a secure S3 endpoint to upload the created tarball.
3. The tarball is uploaded and FOSSA begins analyzing each file individually.