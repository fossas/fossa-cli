# diagnose

Out-of-band diagnostics for FOSSA CLI.

Usually we try to ensure that diagnostics are captured in the FOSSA CLI debug bundle.
However sometimes we need information that isn't reasonable to collect on every system
(due to performance, privacy, or other reasons).

Sometimes we also want to validate a diagnostic is useful before we include it in the debug bundle.

These diagnostics are generally used when directed by FOSSA support; as such the typical
way of getting help documentation is using the `--help` argument.

## `walk`

Enables users to get detailed diagnosis of walking the local file system for a project.

Intended to be used to help troubleshoot situations where FOSSA CLI is slow to walk over a file system
by gathering detailed information about the entries that are walked.

Information reported:
- The file paths walked.
- Whether they are on the same device as the root folder.
- Their type (symlink, file, folder).
- Whether they are read-only.
- Their depth in the directory structure (relative to the root folder).
- The deepest directory observed.
- The total count of each kind of entry walked.

File paths may be filtered with regular expressions: see `diagnose walk --help` for details.

Example:
```
diagnose walk --filter '\.git' --filter '\.md'
```

## `debug-output-format`

By default, `diagnose` reports rather verbose data. This is to minimize configuration required for
users; they can just run it with the default arguments and send FOSSA the entire output for maximal diagnostics.

However, this can be customized via the `--format`, `--trace-spans`, and `--trace-level` arguments.
This subcommand allows visualization of those customizations.
