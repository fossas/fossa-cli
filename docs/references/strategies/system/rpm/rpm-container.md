# RPM

The RedHat Package Manager (rpm).

> This analysis is only executed when container scanning.

## Important Limitations

**License Identification**: RPM package detection is supported, but license information extraction has limitations:

- **Full Support**: Alpine (APK) and Debian (DPKG) packages include complete license information.
- **Limited Support**: RHEL and Oracle Linux (OL) RPM packages are detected but may appear as "unlicensed" because:
  - License information is not currently extracted from RPM package databases
  - Oracle Linux EPEL repositories are not automatically recognized
  - Modern RHEL9/OL9 signature formats may cause fetcher issues

**Impact**: Customers scanning RHEL/OL-based containers may see hundreds of system packages (like `perl`, `bash`, `coreutils`) marked as "unlicensed" even though license metadata exists in the RPM database.

**Workaround**: Packages can be manually licensed through the FOSSA web interface.

**Future Enhancement**: Full RHEL/OL support is on our roadmap. If this is important to you, please reach out to support@fossa.com.

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
