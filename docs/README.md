# User Manual

For most users, the fossa-cli will work out-of-the-box without any configuration. Get an API key, run `fossa analyze`, and view your results in the FOSSA web application.

If you haven't read the [Getting Started](../README.md#getting-started) section in the README yet, we highly recommend starting there first.

This manual is organized into three sections:

<!-- 1. **Concepts** explain the intent and mechanics behind FOSSA concepts (e.g. how FOSSA thinks about "projects" or "dependencies"), including important nuances and subtleties. -->

1. **Walkthroughs** explain how to accomplish common use cases, including common troubleshooting steps and follow-ups.

2. **Features** explain specific CLI features and how to use them.

3. **References** provide an exhaustive listing and explanation of all CLI functionality.

Every piece of documentation is accessible via hyperlink from this user manual. You should never need to manually explore `docs/` to find the page you need.

## Table of Contents

<!-- 1. [Concepts](#concepts) -->
- [User Manual](#user-manual)
  - [Table of Contents](#table-of-contents)
  - [Walkthroughs](#walkthroughs)
  - [Features](#features)
  - [References](#references)

<!-- ## Concepts

Concept guides explain the nuances behind how basic FOSSA primitives work. If you're looking to accomplish a specific goal, you should probably start with [Walkthroughs](#walkthroughs), but if you come across confusing behavior, understanding Concepts can help you debug what's going on.

- [The FOSSA ontology: Projects, Revisions, Analyses, and Targets](./concepts/ontology.md)
- [What is a Dependency?](./concepts/dependencies.md)
- [Locators, Project Identity, and Dependency Identity](./concepts/locators-and-identity.md)
- [Lifecycle of an Analysis](./concepts/analysis-and-analyzers.md)
-->

## Walkthroughs

Walkthrough guides explain how to accomplish specific tasks. They also include troubleshooting steps answer common questions.

- [Integrating a project](./walkthroughs/integrating.md)
- [Debugging a strategy](./walkthroughs/debugging-your-integration.md)
- [Analysis target configuration](./walkthroughs/analysis-target-configuration.md)
- [Custom integration](./walkthroughs/custom-integrating-with-bower-example.md)

## Features

Feature guides explain how to use specific features. These are most useful if there is a specific feature that a walkthrough has pointed you to.

- [Manual dependencies](./features/manual-dependencies.md)
- [Vendored dependencies](./features/vendored-dependencies.md)
- [Custom License and Keyword Searches](./features/custom-license-and-keyword-searches.md)

## References

Reference guides provide an exhaustive listing of all CLI functionality. If you can't find documentation on how something works elsewhere, it should be here.

- CLI commands
  - [`fossa init`](./references/subcommands/init.md)
  - [`fossa analyze`](./references/subcommands/analyze.md)
  - [`fossa test`](./references/subcommands/test.md)
  - [`fossa report`](./references/subcommands/report.md)
  - [`fossa list-targets`](./references/subcommands/list-targets.md)
  - [`fossa snippets`](./references/subcommands/snippets.md)
  <!-- TODO Write this README file
  - [Common flags and options] -->
- CLI configuration files
  - [`.fossa.yml`](./references/files/fossa-yml.md)
  - [`fossa-deps.{yml,json}`](./references/files/fossa-deps.md)
- [CLI analysis strategies](./references/strategies/README.md)
- Experimental
  - [Experimental options overview](./references/experimental/README.md)
  - CLI Commands
    - [`fossa experimental-link-user-defined-dependency-binary`](./references/experimental/subcommands/experimental-link-user-defined-dependency-binary.md)
- [CLI Telemetry](./telemetry.md)
- [Debugging FOSSA CLI](./references/debugging/README.md)
