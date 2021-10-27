# PackageReference

Package references, using the `PackageReference` node, manage NuGet dependencies directly within project files (as opposed to a separate `packages.config` file)

## Project Discovery

Walk the directory and find all NuGet project files, i.e. files with the suffix of `.csproj`, `.xproj`, `.vbproj`, `.dbproj`, or `.fsproj`.

## Analysis

Parse the XML project files, and collect dependency data from all `PackageReference` tags:

```
<ItemGroup>
    <!-- ... -->
    <PackageReference Include="Sitecore.Kernel" Version="12.0.0" />
    <!-- ... -->
</ItemGroup>
```