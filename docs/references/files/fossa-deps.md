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
- `labels`: An optional list of labels to be added to the dependency.
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
    homepage: https://djangoproject.com
    description: Django
```
> Note: License scanning currently operates by uploading the files at the specified path to a secure S3 bucket. All files that do not contain licenses are then removed after 2 weeks.

For more details, please refer to the [feature](../../features/vendored-dependencies.md) walk through.

### `fork-aliases:`

Denotes mapping of fork dependencies to their base dependencies. This is useful when you have forked a dependency and want it to be treated as the original dependency in FOSSA.

- `my-fork`: The fork dependency entry that should be aliased to the base dependency. (Required)
  - `type`: Type of the fork dependency. (Required)
  - `name`: Name of the fork dependency. (Required)
  - `version`: Version of the fork dependency. (Optional)
- `base`: The base/original dependency entry that your fork should be aliased to. (Required)
  - `type`: Type of the base dependency. (Required)
  - `name`: Name of the base dependency. (Required)
  - `version`: Version of the base dependency. (Optional)
- `labels`: An optional list of labels to be added to the fork alias.

**Matching rules:**
- If `my-fork` version is specified, only that exact version will be translated
- If `my-fork` version is not specified, any version will match

**Translation rules:**
- If `base` version is specified, the dependency will always be translated to that version
- If `base` version is not specified, the original version from the fork is preserved

```yaml
fork-aliases:
- my-fork:
    type: cargo
    name: my-serde
  base:
    type: cargo
    name: serde
  labels:
  - label: internal
    scope: org
- my-fork:
    type: cargo
    name: my-serde
    version: 1.0.0  # Only version 1.0.0 will be translated
  base:
    type: cargo
    name: serde
    version: 2.0.0  # Will always translate to version 2.0.0
```

## Labels

Each kind of dependency referenced above can have a `labels` field, which is a list of labels to be added to the dependency.
These labels are **user-defined**; you may choose any labels. What they mean is up to you and/or your organization.

Labels have a `scope` field, which is the scope of the label. The possible scopes are:
- `org`: The label is scoped to the organization.
- `revision`: The label is scoped to the revision.
- `project`: The label is scoped to the project.

You may attach multiple labels to a single dependency.
For example:

```yaml
referenced-dependencies:
- type: pypi
  name: numpy
  version: 2.2.0
  labels:
  - label: numbers-go-brr
    scope: org
  - label: oss-approved
    scope: revision

custom-dependencies:
- name: somecorp-api-client
  version: 1.2.3
  license: Proprietary
  metadata:
    homepage: https://www.partner.somecorp.com/interface/client/wrapper/lib
    description: Gives access to the SomeCorp API.
  labels:
  - label: proprietary
    scope: version
  - label: license-paid-2024
    scope: revision

vendored-dependencies:
- name: Django
  path: vendor/Django-3.4.16.zip
  version: 3.4.16
  metadata:
    homepage: https://djangoproject.com
    description: Django
  labels:
  - label: hr-docs
    scope: project
  - label: internal-dependency
    scope: revision

fork-aliases:
- my-fork:
    type: cargo
    name: my-serde
  base:
    type: cargo
    name: serde
  labels:
  - label: internal
    scope: org
  - label: fork-approved
    scope: revision
```

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
