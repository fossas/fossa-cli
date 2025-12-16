# `millhone`

This CLI is used for FOSSA container JAR analysis.

# Subcommands

Subcommand                                 | Description
-------------------------------------------|-------------------------------------------------------------------------------
[`analyze`](./docs/subcommands/analyze.md) | Match snippets in a local project against snippets in the FOSSA knowledgebase.
[`commit`](./docs/subcommands/commit.md)   | Collect matched snippet information into a `fossa-deps` file.
[`ingest`](./docs/subcommands/ingest.md)   | Manually ingest a new library into the knowledgebase.
[`ping`](./docs/subcommands/ping.md)       | Test connectivity to the FOSSA API.

For more information on possible options, run `millhone --help`.
