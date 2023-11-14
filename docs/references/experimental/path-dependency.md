# Path Dependency


## What is a path dependency?

Path dependencies, are dependencies which were sourced from file-system,
as opposed to package manager registry, or internet URL. 

For example from following `go.mod` file, `fossa-cli` would consider `` to
be a path dependency.  

```
```

In FOSSA UI, path dependencies will be shown with `Local` tag. 

## How path dependencies are scanned

Path dependencies are scanned using "CLI license scan", similar to how vendor dependencies
are scanned. A "CLI license scan" inspects path for licensing on the local system within the CLI, 
and only uploads the matched license data to FOSSA's servers.

The FOSSA service caches the results of a path dependency by the combination of its `(project id, path)`.
Due to this caching setup, it is normal for the first analysis to take some time, especially for larger projects, 
but future analysis of dependencies with the same information should be fast.

In the event caching is causing problems, FOSSA can be made to rescan this kind of dependency:
- Run `fossa analyze` with the `--force-vendored-dependency-rescans` flag, or
- Set `vendoredDependencies.forceRescans` to `true` in `.fossa.yml` at the root of the project.

## Limitations

- Currently, path dependencies do not support vulnerability scanning.
- Currently, path dependencies are only supported for `golang`.

## F.A.Q

### How do I enable path dependency in analysis?

Run `fossa analyze` with the `--force-vendored-dependency-rescans` flag

### How do I disable path dependency in analysis?

By default, path dependency analysis is disabled.

### Is `fossa-cli` uploading content of my path dependency to server?

`fossa-cli` only upload findings of license scan result. 

## How are path dependencies different from Vendor dependencies?

Path dependencies, unlike vendor dependencies can be either direct
or transitive dependencies in the analysis graph. Furthermore, path
dependencies are scoped to Project, as opposed to entire organization.