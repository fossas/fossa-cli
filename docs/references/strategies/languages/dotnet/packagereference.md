# PackageReference

Package references, using the `PackageReference` node,
manage NuGet dependencies directly within project files
(as opposed to a separate `packages.config` file).

_Not sure how to read this reference?_
_Check the [Primer: strategies in FOSSA CLI](../../README.md#primer-strategies-in-fossa-cli) first!_

## Project Discovery

Walk the directory and find all files with a suffix of one of the below:

- `.csproj`
- `.xproj`
- `.vbproj`
- `.dbproj`
- `.fsproj`

## Analysis

| Tactic                                            | Analysis Method | Vulnerabilities | Full Graph Support | Dependency Scopes |
|---------------------------------------------------|-----------------|-----------------|--------------------|-------------------|
| [Parse PackageReference](#parse-packagereference) | Static          | :grey_question: | :grey_question:    | :grey_question:   |

### Parse PackageReference

Parse the XML project files, and collect dependency data from all `PackageReference` tags:

```
<ItemGroup>
    <!-- ... -->
    <PackageReference Include="Sitecore.Kernel" Version="12.0.0" />
    <!-- ... -->
</ItemGroup>
```
