## Monorepo Projects

### What is it?

FOSSA experimentally supports scanning large monorepo projects which have potentially many licenses spread out across its files. The monorepo feature also supports inferring build graph information depending on the type of monorepo project.

Currently, the only supported monorepo type is the Android Open Source Project ([AOSP](#AOSP)).

Currently, this also disables all other FOSSA strategies, and therefore does not do any dependency analysis.

### Prerequisites

Monorepo support requires a feature flag enabled in your FOSSA organization. If you're interested in using this feature please contact us!

### Interacting with Monorepo Scans

When viewing a Monorepo project in the FOSSA UI, users will a list of files and directories with metadata that was collected during the scan. In this UI, files can be filtered by metadata such as (but not limited to)
1. which licenses they contain
2. whether they appear in the build graph

### Running a Monorepo Scan

In order to run a monorepo scan, pass `--experimental-enable-monorepo aosp` to `fossa analyze`. A minimal invocation would look like:
```bash
fossa analyze \
  --experimental-enable-monorepo aosp \
  /path/to/aosp/project
```

### Filtering Specific Paths

Since monorepos have a tendency to be huge, it's sometimes useful to exclude certain paths from the scan, or to specify a single class of paths to include. The syntax for these exclusions is globbing (along with support for `**`). This feature supports the standard `--exclude-path` and `--only-path` flags to customize which paths are scanned.

### Additional Flags

Monorepo scans support the [standard set of `fossa analyze` flags](../../subcommands/analyze.md#specifying-fossa-project-details).

## AOSP

### Build Graphs

In order for build graph information to be inferred, ninja files (which are a prerequisite for a build) must be present under the directory being scanned.
