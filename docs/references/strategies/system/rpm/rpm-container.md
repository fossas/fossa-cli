# RPM

The RedHat Package Manager (rpm).

> This analysis is only executed when container scanning.

## Discovery

Each RPM installation may use one of several backends:

- BerkeleyDB backend: Find file named `Packages` under `var/lib/rpm`.
- NDB backend: Find file named `Packages.db` under `**/rpm/**`.
- Sqlite backend: Find file named `Packages.sqlite` or `rpmdb.sqlite` under `var/lib/rpm`.

## Analysis

Parse and analyze, reporting package name, package version, architecture, and optionally epoch.
Each of these backends share a common format for storing actual packages, but the method used to retrieve the list of packages is unique to each backend.

## FAQ

### How do I *only perform analysis* for RPM?

Explicitly specify an analysis target in `.fossa.yml` file. The example below excludes all other analysis targets:

```yaml
# .fossa.yml 

version: 3
targets:
  only:
    - type: ndb
    - type: berkeleydb
    - type: sqlitedb
```
