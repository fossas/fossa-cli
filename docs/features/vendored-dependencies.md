### License scanning local dependencies

Fossa offers the ability to license scan your code directly. This is used primarily if a package manager is not yet supported or if you are vendoring dependencies. Using the license scanning feature will allow you to capture the licenses for dependencies that may otherwise be missed from normal fossa analysis that relies on package manager information.

In order to specify a file path, modify your `fossa-deps.yml` file and add a `vendored-dependencies` section like the following:

```yml
# Example full `fossa.deps.yml` file.
referenced-dependencies:
- type: gem
  name: rubyXL
  version: 3.4.16

vendored-dependencies:
- name: Django
  path: vendor/Django-3.4.16.zip # path can be either a file or a folder.
  version: 3.4.16 # revision will be set to the MD5 hash of the filepath if left unspecified.
```

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

## How Vendored Dependencies are scanned

There are two methods of vendored dependency scanning: "CLI license scan" and "archive upload".

The default is typically "CLI license scan", but your organization may have opted to default to "archive upload".

Both methods will give you exactly the same results. The difference is where the license scan is done.

A "CLI license scan" runs a license scan directly on your server, and only uploads the matched license data to FOSSA's servers. You can see the data that FOSSA will upload by using the `--output` flag.

"Archive upload" uploads the files at the specified path to a secure S3 bucket. We license scan the uploaded files on our servers. All files that do not contain licenses are then removed after 2 weeks.

You can change the scan method by using the `--force-vendored-dependency-license-scan` or `--force-vendored-dependency-archive-upload` flags when invoking the CLI, or by setting the `vendoredDependencies.scanMethod` field in your `.fossa.yml` file. See the [.fossa.yml documentation](https://github.com/fossas/fossa-cli/blob/master/docs/references/files/fossa-yml.md) for details.

## Forcing rescans

If you are scanning a revision that has already been scanned and is known to FOSSA, then by default we will not rescan that dependency.

This speeds up your CI scans and avoids unnecessary work.

However, if you have made a change to your code and do not want to change the version provided, you can force a rescan by using the `--force-vendored-dependency-rescans` flag or setting the `vendoredDependencies.forceRescans` field to true in your `.fossa.yml` file.

If you do not provide a version for the vendored dependency, then any changes to your code will be picked up as a new version and there should be no need to force a rescan.

