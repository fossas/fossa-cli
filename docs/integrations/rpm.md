# RPM

## Support

RPM package scanning is supported provided that the following are met:
1. `rpm` is available.
1. A premium fossa subscription in order to upload license files.

It is beneficial if `yum` is installed as the FOSSA CLI will leverage `yum` in order to analyze transitive dependencies which may not be installed.

## Configuration

> Note: There is no automatic discovery for RPM packages as this would cause unexpected behavior for users not interested in RPM scanning and too much information for those who are.

### Manual

Add a module with `type: rpm` and populate the `name:` field with your desired module name. The `target` value depends on your preferred analysis method:

1. System Level Analysis:
In order to analyze all RPM packages installed on your system, set `target: .` to tell FOSSA CLI that all RPMs should be scanned.
```yaml
analyze:
  modules:
  - name: system-rpms
    type: rpm
    target: .
```

2. Individual Package Analysis:
If you have a single RPM you would like to analyze, set `target:` to the name of the installed RPM you wish to scan, or the location of the RPM you wish to scan. Both examples are outlined below:

> Note: Individual RPM analysis will attempt to install any dependency RPMs which are not already installed in the environment. For this reason we recommend running RPM analysis inside of a container.

Installed RPM:
```yaml
analyze:
  modules:
  - name: libssh-rpm
    type: rpm
    target: libssh
```

Downloaded RPM:
```yaml
analyze:
  modules:
  - name: libssh-rpm
    type: rpm
    target: downloads/libssh-0.9.0-5.fc30.x86_64.rpm
```

## Analysis

Analyzing an RPM is straightforward and uploads license files directly to FOSSA in order to fully scan all licenses. 
System Level Analysis:
1. Run `rpm -qa --queryformat %{name},%{version},%{license}\n` to get all installed RPMs
1. Find and upload the best license for each dependency.

Individual Package Analysis:
1. WARNING: If the package is not installed, run `yum install <target>` to install it. This may result in additional RPMs being installed in your environment.
1. Find transitive dependencies by running `rpm -qr <target>`.
1. Repeat steps 1 & 2 for each transitive dependency found.
1. Find and upload the best license for each dependency.

Determining License File for an RPM:
1. Look inside the system folder set to `/usr/share/licenses/<rpm>` for the RPM's license files.
1. (Conditional on step 1 failing) Run `rpm -q --queryformat %{name},%{version},%{license}\n` 
1. Upload the license information to FOSSA for license scanning.