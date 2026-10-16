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

## Central Package Management (CPM)

Projects using [NuGet Central Package Management](https://learn.microsoft.com/en-us/nuget/consume-packages/central-package-management) define dependency versions in a `Directory.Packages.props` file rather than in individual project files. When a `PackageReference` has no `Version` attribute, FOSSA searches parent directories for `Directory.Packages.props` and resolves the version from matching `PackageVersion` entries:

```xml
<!-- Directory.Packages.props -->
<Project>
  <ItemGroup>
    <PackageVersion Include="Sitecore.Kernel" Version="12.0.0" />
  </ItemGroup>
</Project>
```

```xml
<!-- MyProject.csproj — version omitted, resolved from Directory.Packages.props -->
<ItemGroup>
    <PackageReference Include="Sitecore.Kernel" />
</ItemGroup>
```

Inline `Version` attributes on `PackageReference` always take precedence over the centrally managed version.

`PackageVersion` versions may reference MSBuild properties defined in a `<PropertyGroup>` of the same `Directory.Packages.props`. FOSSA expands `$(Name)` references (including properties that reference other properties) before reporting the version:

```xml
<!-- Directory.Packages.props -->
<Project>
  <PropertyGroup>
    <SomeVar>10.0.9</SomeVar>
  </PropertyGroup>
  <ItemGroup>
    <PackageVersion Include="Microsoft.Extensions.Http" Version="$(SomeVar)" />
  </ItemGroup>
</Project>
```

Only properties defined in `Directory.Packages.props` itself are known; `Condition` attributes on property groups are ignored. A version that references an undefined property, or uses MSBuild property functions such as `$(Foo.Trim())`, cannot be expanded and are reported without a version.
