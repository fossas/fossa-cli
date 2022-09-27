### License scanning local dependencies

FOSSA offers the ability to license scan your code directly. This is used primarily if a package manager is not yet supported or if you are vendoring dependencies. Using the license scanning feature will allow you to capture the licenses for dependencies that may otherwise be missed from normal fossa analysis that relies on package manager information.

In order to specify a file path, modify your `fossa-deps.yml` file and add a `vendored-dependencies` section like the following:

```yml
# Example full `fossa-deps.yaml` file.
referenced-dependencies:
- type: gem
  name: rubyXL
  version: "3.4.16"

vendored-dependencies:
- name: Django
  path: vendor/Django-3.4.16.zip # path can be either a file or a folder.
  version: "3.4.16" # revision will be set to the MD5 hash of the filepath if left unspecified.
```

The path to a vendored dependency can either be a path to an archive or a path to a directory.

If it is a path to an archive, then we recursively unarchive and scan the archive. If it is a directory, then we scan the directory and recursively unarchive and scan any archives contained in the directory.

If the version is not specified, then we calculate the version by calculating a hash of the contents of the archive or directory. This is often what you actually want, as it means that the version will automatically change when the contents of the vendored dependency change. It also avoids conflicts across your organization when two different projects contain a vendored dependency with the same name and version, as described in [Vendored Dependency Names and Scope](#vendored-dependency-names-and-scope).

Note: When parsed, YAML considers text that could be a decimal number (such as 1.0 or 2.0) to be a number, not a string. This means that we'd parse the version 1.0 as 1. This probably isn't what you want. To avoid this, surround your version with quotes, as in "1.0".

We also support json-formatted dependencies:

```json
{
  "referenced-dependencies": [
    {
      "type": "gem",
      "name": "iron"
    }, {
      "type": "pypi",
      "name": "Django",
      "version": "2.1.7"
    }
  ],
  "custom-dependencies": [
    {
      "name": "foo",
      "version": "1.2.3",
      "license": "MIT"
    }, {
      "name": "foo-wrapper",
      "version": "1.0.2",
      "license": "MIT or Apache-2.0",
      "metadata": {
        "description": "Provides a help wrapper for foo-related tasks",
        "homepage": "https://foo-project.org/homepage"
      }
    }
  ],
  "vendored-dependencies": [
    {
      "name": "lodash",
      "path": "lodash-4.17.21"
    }, {
      "name": "winston",
      "path": "vendor/winston.tar.gz",
      "version": "5.0.0-alpha"
    }
  ],
  "remote-dependencies": [
    {
      "name": "foo-url",
      "version": "1.2.3",
      "url": "www.foo.tar.gz",
      "metadata": {
        "description": "foo archive",
        "homepage": "https://foo-url.org/homepage"
      }
    }
  ]
}
```

## Vendored Dependency Names and Scope

The name of a vendored dependency is scoped to an organization.

This means that if two different projects in your organization have the same name and version, they will be treated as the same dependency by FOSSA even if they have different contents. The one that was scanned first will be used by FOSSA. If you use the `--force-vendored-dependency-rescans` flag, then the current scan will overwrite the original one. But this will cause the data for the original scan to be incorrect now.

This can cause unexpected behavior, and we are working on changing this so that vendored dependencies are scoped to projects rather than organizations.

Our suggested workaround is to not set an explicit version on your vendored dependency. When you omit the version, FOSSA will calculate a version based on the contents of the vendored dependency, thus avoiding any conflicts. This also has the added benefit of automatically changing the version when the contents of the vendored dependency change.

## How Vendored Dependencies are scanned

There are two methods of vendored dependency scanning: "CLI license scan" and "archive upload".

The default is typically "CLI license scan", but your organization may have opted to default to "archive upload".

Both methods use the same license scanning technology and will give you the same results. The difference is where the license scan is done.

A "CLI license scan" inspects your code for licensing on the local system within the CLI, and only uploads the matched license data to FOSSA's servers.

"Archive upload" uploads the files at the specified path to a secure S3 bucket. We license scan the uploaded files on our servers. All files that do not contain licenses are then removed after 30 days.

You can change the scan method by using the `--force-vendored-dependency-scan-method` flag when invoking the CLI, or by setting the `vendoredDependencies.scanMethod` field in your `.fossa.yml` file. See the [.fossa.yml documentation](https://github.com/fossas/fossa-cli/blob/master/docs/references/files/fossa-yml.md) for details.

## Forcing rescans

If you are scanning a revision that has already been scanned by FOSSA, then by default we will not rescan that dependency. A revision has been scanned by FOSSA if you or someone else from your organization has previously analyzed a dependency with the same name and version in the `vendored-dependencies` field in a `fossa-deps.yml` file.

Avoiding rescans speeds up your CI scans and avoids unnecessary work.

However, if you have made a change to your code and do not want to change the version provided, you can force a rescan by using the `--force-vendored-dependency-rescans` flag or setting the `vendoredDependencies.forceRescans` field to true in your `.fossa.yml` file.

If you do not provide a version for the vendored dependency, then we generate a version by calculating an MD5 hash of the contents of the vendored dependency. Any changes to your code will be picked up as a new version and there is typically no need to force a rescan.

