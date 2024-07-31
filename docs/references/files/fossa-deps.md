# `fossa-deps`

`fossa-deps` file is a file named `fossa-deps.{yaml, yml, json}` at the root of the project. It can be used to provide manual and vendor dependencies.

By default, the `fossa-deps.{yaml, yml, json}` file at the root of the project is used. However, if the `--fossa-deps-file` flag is present, then the provided `<name-of-file>.{yaml, yaml, json}` file will be used instead.

For more details on specifying a fossa-deps file, please refer to the [subcommand](../subcommands/analyze.md) documentation.

## Fields

### `referenced-dependencies:`

Denotes listing of dependencies, which are to be analyzed in conjunction with the analysis.

- `type`: Type of dependency. (Required)
- `name`: Name of the dependency. It should be the same name as listed in dependencies registry. (Required)
- `version`: Revision of the dependency. If left unspecified, the latest version discovered from the registry will be used.

```yaml
referenced-dependencies:
- type: gem
  name: iron
- type: pypi
  name: Django
  version: 2.1.7
```

For more details, please refer to the [feature](../../features/manual-dependencies.md) walk through.

### `custom-dependencies:`

Denotes listing of dependencies, which can't be automatically discovered or identified but are to be stubbed and included in the analysis.

- `name`: Name of the dependency. (Required)
- `version`: Revision of the dependency. (Required)
- `license`: License of the dependency. (Required)
- `metadata.homepage`: Homepage of the dependency. This metadata is used to enrich reporting provided in FOSSA's web interface.
- `metadata.description`: Description of the dependency. This metadata is used to enrich reporting provided in FOSSA's web interface.

Example:
```yaml
- name: foo-wrapper
  version: 1.2.3
  license: MIT
  metadata:
    homepage: https://www.foowrapper.com/about
    description: Provides foo and a helpful interface around foo-like tasks.
```

For more details, please refer to the [feature](../../features/manual-dependencies.md) walk through.

### `remote-dependencies:`

Denotes listing of dependencies, whose source code is to be downloaded from provided URL, and analyzed for license scanning in FOSSA backend.

- `name`: Name of the dependency. (Required)
- `version`: Revision of the dependency. (Required)
- `url`: URL of archived source code. (Required)
- `metadata.homepage`: Homepage of the dependency. This metadata is used to enrich reporting provided in FOSSA's web interface.
- `metadata.description`: Description of the dependency. This metadata is used to enrich reporting provided in FOSSA's web interface.

> Combined length of url and version has upper bound. It depends on your organization identifier. You can
find your organization identifier in FOSSA Webapp, by going to any project's "settings" page, and retrieving
numeric value from project's locator. For example, project locator of `custom+123/some-project-id`, means
`123` is your organization identifier.

> Combined length of `url`, `version`, and your `organizaion id` must be less than `241`.

For more details, please refer to the [feature](../../features/manual-dependencies.md) walk through.

### `vendored-dependencies:`

Denotes listing of files or directories, which are to be archived and uploaded to FOSSA backend for license scanning.

- `name`: Name of the dependency (Required)
- `path`: Local path to a file, or a directory (Required)
- `version`: Revision of the dependency. If not specified, the md5 hash of the file path will be used.
- `metadata.homepage`: Homepage of the dependency. This metadata is used to enrich reporting provided in FOSSA's web interface.
- `metadata.description`: Description of the dependency. This metadata is used to enrich reporting provided in FOSSA's web interface.

```yaml
vendored-dependencies:
- name: Django
  path: vendor/Django-3.4.16.zip
  version: 3.4.16
  metadata:
    homepage: django.com
    description: Django
```
> Note: License scanning currently operates by uploading the files at the specified path to a secure S3 bucket. All files that do not contain licenses are then removed after 2 weeks.

For more details, please refer to the [feature](../../features/vendored-dependencies.md) walk through.

## Errors in the `fossa-deps` file

The `fossa-deps` scanner tries to report clear error messages when fields are missing, incorrect, or invalid.  For example:

```yaml
referenced-dependencies:
- type: pypi
  name: flask
  version: "2.0.1"
  license: MIT  # Error!  "license" is only allowed for custom-dependencies

custom-dependencies:
- type: custom  # Error!  "type" is only allowed for referenced-dependencies
  name: mydep
  version: "3.14.15"
  license: GPL-3.0

remote-dependencies:
- name: mydep
  version: "3.14.15"
  license: GPL-3.0 # Error! "license" is only allowed for custom-dependencies
```

This would return an error with a message explaining what went wrong, and where.  However, we don't check for everything (yet!):

```yaml
referenced-dependencies:
- type: cargo
  name: bitflags
  some-unexpected-field: hello  # Has no effect, will be considered an error in future versions.
```

The `fossa-deps` scanner also requires at least one valid dependency if the file exists.  This prevents the file from being created with the wrong array names and us silently ignoring them.

If you see an error message that isn't clear, file an issue in this repository!  Clear error messages are a priority for us, and we want to know where we're lacking.
