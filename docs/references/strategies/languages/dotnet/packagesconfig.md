# packages.config

The `packages.config` XML file is used in some project types to maintain the list of packages referenced by the project.

Note: projects that use `PackageReference` do not use `packages.config`.

## Project Discovery

Walk the directory and find all files names `packages.config`

## Analysis

Parse the XML file, and collect dependency data from all `package` tags within the `packages` section:

```
<?xml version="1.0" encoding="utf-8"?>
<packages>
  <package id="jQuery" version="3.1.1" targetFramework="net46" />
  <package id="NLog" version="4.3.10" targetFramework="net46" />
</packages>
```