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

If the version is not specified, FOSSA CLI calculates the version by generating a hash of the contents of the archive or directory. This is often desired, as it means that the version automatically changes when the contents of the vendored dependency change. It also avoids conflicts across an organization when two different projects contain a vendored dependency with the same name and version, as described in [Vendored Dependency Names and Scope](#vendored-dependency-names-and-scope).

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

This means that if two different projects in an organization have the same name and version, they are treated as the same dependency by FOSSA, and the one that was scanned first is reported. As an example:

Project A defines a vendored dependency like this in its fossa-deps.yml, and the contents of that vendored dependency contain an MIT license:

```yaml
vendored-dependencies:
- name: Django
  path: vendor/Django-3.4.16.zip
  version: "3.4.16"
```

Project B has exactly the same contents in fossa-deps.yml, but the contents of Project B's vendored dependency contain an Apache 2.0 license.

If Project A is scanned first, then both Project A and Project B report that their vendored dependency has an MIT license.

If Project B is scanned first, then both Project A and Project B report that their vendored dependency has an Apache 2.0 license.

This can cause unexpected behavior. (Note: FOSSA is working on changing this so that vendored dependencies are scoped to projects rather than organizations.)

The suggested workaround is to not set a version in the vendored dependency entry. When the version is omitted, FOSSA calculates a version based on the contents of the vendored dependency, thus avoiding any conflicts. This also has the added benefit of automatically changing the version when the contents of the vendored dependency change.


## How Vendored Dependencies are scanned

There are two methods of vendored dependency scanning: "CLI license scan" and "archive upload".

The default is typically "CLI license scan", but your organization may have opted to default to "archive upload".

Both methods use the same license scanning technology and will give you the same results. The difference is where the license scan is done.

A "CLI license scan" inspects your code for licensing on the local system within the CLI, and only uploads the matched license data to FOSSA's servers.

"Archive upload" uploads the files at the specified path to a secure S3 bucket. We license scan the uploaded files on our servers. All files that do not contain licenses are then removed after 30 days.

You can change the scan method by using the `--force-vendored-dependency-scan-method` flag when invoking the CLI, or by setting the `vendoredDependencies.scanMethod` field in your `.fossa.yml` file. See the [.fossa.yml documentation](https://github.com/fossas/fossa-cli/blob/master/docs/references/files/fossa-yml.md) for details.

## Performance

The FOSSA service caches the results of a vendored dependency by the combination of its `(name, version)` fields.
Due to this caching setup, it is normal for the first analysis to take some time, especially for larger projects, but future analysis of dependencies with the same information should be fast.

If `version` is not specified, FOSSA computes a version based on the contents specified by `path`; this means that if the contents have not changed then the results are reused.

In the event caching is causing problems, FOSSA can be made to rescan this kind of dependency:
- Run `fossa analyze` with the `--force-vendored-dependency-rescans` flag, or
- Set `vendoredDependencies.forceRescans` to `true` in `.fossa.yml` at the root of the project.

## Path Filtering

> Note: This section does not apply to archive uploads. Path filtering is only available when doing a CLI License Scan. See [here](#how-vendored-dependencies-are-scanned) for more info on the difference between these two methods.

Path filtering can be used to omit some files or directories from license scanning. Path filtering is set up in the `.fossa.yml` file. Here is an example:

```yaml
version: 3
vendoredDependencies:
  licenseScanPathFilters:
    only:
      - "**/*.rb"
      - "**/LICENSE"
    exclude:
      - "**/test/**"
      - "**/test/*"
      - "**/spec/**"
      - "**/spec/*"
```

Filters are set in the `vendoredDependencies.licenseScanPathFilters` section of the file. You can provide an `only` object and an `exclude` object. Both of these objects consist of a list of file globs. You can provide both `only` and `exclude` objects or just `only` or just `exclude`.

The `only` object will scan paths that match at least one of the entries in the `only` object. The `exclude` object will exclude paths that match any of the entries in the `exclude` object.

So in the example above, we will license scan files named "LICENSE" and files that have an extension of `.rb`. We will also filter out any files in directories named `test` or `spec`, even if they match the `only` filters.

The `**`, known as a globstar, is a non-standard extension to globs. It matches one or more directories.

> Note: Some implementations of globstar treat it as matching "zero or more directories". Since different implementations differ in their globstar functionality, we have decided to treat globstars as matching "one or more directories". We did this as it is simpler to include the additional glob for the base directory case when desired than to exclude the base directory case when it is not desired.

The following table shows which files will be matched by a glob for this directory structure.

```
.
├── LICENSE
├── foo.rb
├── src
│   ├── runit.rb
│   ├── runit_external.rb
│   └── subdir
│       └── again.rb
└── test
    ├── LICENSE
    └── runit_test.rb
```

| Glob          | Meaning                                             | Files matched                                                                                   |
| ------------- | --------------------------------------------------- | ----------------------------------------------------------------------------------------------- |
| `src/*.rb`    | All .rb files directly in the root src directory    | `src/runit.rb`, `src/runit_external.rb`                                                         |
| `**/src/*.rb` | All .rb files directly in any directory named `src` | `src/runit.rb`, `src/runit_external.rb`                                                         |
| `**/*.rb`     | All .rb files                                       | `foo.rb`, `src/runit.rb`, `src/runit_external.rb`,  `src/subdir/again.rb`, `test/runit_test.rb` |
| `**/src/**`   | All files under the src directory                   | `src/subdir/again.rb`                                                                           |

- To filter out all files with a given extension, add an entry like `**/*.extension` to the `exclude` object. E.g. `**/*.ts`.

- To filter out all files with a given name, add an entry like `**/filename` to the `exclude` object. E.g `**/LICENSE` or `**/LICENSE.*`.

- To include only files with a given extension, add an entry like `**/*.extension` to the `only` object. If you want to include files with multiple extensions, you can add multiple entries to the `only` object. E.g. `**/*.ts`.

- To include all files with a given name, add an entry like `**/filename` to the `only` object. E.g `**/LICENSE` or `**/LICENSE.*`.

- To exclude all files in subdirectories of a given directory, add that directory followed by `/**` to the exclude object. E.g. `path/to/exclude/**`.

- If you also want to exclude files directly in that directory, add a second entry with the directory followed by `/*`. E.g. `path/to/exclude/*`.

- To scan only files in a subdirectory of a given directory, add that directory followed by `/**` to the exclude object.  E.g. `path/to/scan/**`.

- If you also want to scan all files directly in that directory, add a second entry with the directory followed by `/*`. E.g. `path/to/scan/*`.

