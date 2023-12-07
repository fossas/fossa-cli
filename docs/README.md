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
<!-- ## Concepts

Concept guides explain the nuances behind how basic FOSSA primitives work. If you're looking to accomplish a specific goal, you should probably start with [Walkthroughs](#walkthroughs), but if you come across confusing behavior, understanding Concepts can help you debug what's going on.

- [The FOSSA ontology: Projects, Revisions, Analyses, and Targets](./concepts/ontology.md)
- [What is a Dependency?](./concepts/dependencies.md)
- [Locators, Project Identity, and Dependency Identity](./concepts/locators-and-identity.md)
- [Lifecycle of an Analysis](./concepts/analysis-and-analyzers.md)
-->

- [Quick Start](./walkthroughs/integrating.md)
- [FOSSA CLI Concepts](./concepts/analysis-and-analyzers.md)
    - [Analysis Targets](./concepts/analysis-and-analyzers.md#analysis-targets)
    - [Discovery and Analysis](./concepts/analysis-and-analyzers.md#discovery-and-analysis)
    - [Strategy Tactics](./concepts/analysis-and-analyzers.md#discovery-and-analysis)
    - [Static and Dynamic Strategies](./references/strategies/README.md#static-and-dynamic-strategies)
    - [Dynamic Strategy Command Selection](./features/strategy-command-selection.md)
<!-- Consider linking to each language, tool, or platform here -->
- [Language and Tool (Strategy) Support](./references/strategies/README.md#supported-languages)
- [Configuring Which Targets Are Analyzed](./walkthroughs/analysis-target-configuration.md)
  - [Analyzing Specific Submodules](./walkthroughs/analysis-target-configuration.md#target-filtering-for-submodules)
- [Manually Specifying Dependencies](./features/manual-dependencies.md#manually-specifying-dependencies)
  - [Deps from a Known Package Manager (Referenced Dependencies)](./features/manual-dependencies.md#referenced-dependencies)
  - [Fully Specifying a Dep and License Manually (Custom Dependencies)](./features/manual-dependencies.md#referenced-dependencies)
  - [Specifying a Source URL for Analysis (Remote Dependencies)](./features/manual-dependencies.md#referenced-dependencies)
  - [Performance Characteristics of Manual Dependencies](./features/manual-dependencies.md#referenced-dependencies-performance)
- [Analyzing Vendored Dependencies](./features/vendored-dependencies.md)
  - [Vendored Dependency Names and Scope](./features/vendored-dependencies.md#vendored-dependency-names-and-scope)
  - [How Vendored Dependencies are Scanned](./features/vendored-dependencies.md#how-vendored-dependencies-are-scanned)
  - [Filtering Paths for License Scanning in Vendored Dependencies](./features/vendored-dependencies.md#path-filtering)
  - [Path Filtering on Windows](./features/vendored-dependencies.md#path-filtering-and-windows)
  - [How to Debug Path Filters](./featuresvendored-dependencies.md#debugging-your-path-filters)
  - [Vendored Dependency Performance](./features/vendored-dependencies.md#performance)
- [Searching for Custom Keywords and Licenses](./features/custom-license-and-keyword-searches.md)
  - [Keyword Searches](./features/custom-license-and-keyword-searches.md#keyword-searches)
  - [License Text Searches](./features/custom-license-and-keyword-searches.md#custom-license-searches)
  - [Text Search Term Format](./features/custom-license-and-keyword-searches.md#regular-expression-format)
  - [Configuring Custom-License Searches Organization-Wide](./features/custom-license-and-keyword-searches.md#configuring-custom-license-searches-for-your-whole-organization)
    - [Escape characters in organization-wide custom licenses searches](./features/custom-license-and-keyword-searches.md#escape-characters-in-custom-license-searches-for-your-whole-organization)
    - [Turning Off Organization-Wide Custom-Licenses Searches](./features/custom-license-and-keyword-searches.md#turning-off-organization-wide-custom-licenses-searches)
- [Upgrading from FOSSA CLI v1 to a Supported FOSSA CLI](./differences-from-v1.md)
- Configuration
  - [Specifying Dependencies Manually with `fossa-deps.yml`](./references/files/fossa-deps.md)
  - [Specifying Project Settings with `.fossa.yml`](./references/files/fossa-yml.md)
- Troubleshooting
  - [Debugging FOSSA CLI](./references/debugging/README.md)
    - [Debugging a Missing Project](./references/debugging/README.md#debugging-a-missing-project)
    - [Debugging a Failing Strategy](./references/debugging/README.md#debugging-strategies)
    - [Debugging a FOSSA CLI Specific Error](./references/debugging/README.md#debugging-strategies)
    - [Debugging with the Debug Bundle](./references/debugging/README.md#debugging-with-the-debug-bundle)
  - [How to Debug Path Filters](./featuresvendored-dependencies.md#debugging-your-path-filters)
- Experimental Features
  - [Overview](./references/experimental/README.md)
  - [Flagging Binary Dependencies in the Project Source Tree](./references/experimenta/binary-discovery/README.md)
    - [Discovering Jar and Aar Dependencies](./references/experimenta/binary-discovery/README.md#analyzing-jar-and-aar-dependencies)
    - [Creating User-Defined Binary Dependencies](./references/experimenta/binary-discovery/README.md#creating-user-defined-binary-dependencies)
  - [Path Dependency Support](./references/experimental/path-dependency.md)
- [Contributing](contributing/README.md)

## FAQs

### When are you adding support for (some buildtool/language)?

If we don't support your choice of language/buildtool,
please [create a support ticket](https://support.fossa.com) to express interest!

## Walkthroughs

Walkthrough guides explain how to accomplish specific tasks. They also include troubleshooting steps answer common questions.

- [Integrating a project](./walkthroughs/integrating.md)
- [Debugging an Integration](./references/debugging/README.md)
- [Analysis target configuration](./walkthroughs/analysis-target-configuration.md)
- [Custom integration](./walkthroughs/custom-integrating-with-bower-example.md)

## References

Reference guides provide an exhaustive listing of all CLI functionality. If you can't find documentation on how something works elsewhere, it should be here.

- CLI commands
  - [`fossa init`](./references/subcommands/init.md)
  - [`fossa analyze`](./references/subcommands/analyze.md)
  - [`fossa test`](./references/subcommands/test.md)
  - [`fossa report`](./references/subcommands/report.md)
  - [`fossa list-targets`](./references/subcommands/list-targets.md)
  - [`fossa snippets`](./references/subcommands/snippets.md)
- [CLI analysis strategies](./references/strategies/README.md)
- Experimental
  - [Experimental options overview](./references/experimental/README.md)
  - CLI Commands
    - [`fossa experimental-link-user-defined-dependency-binary`](./references/experimental/subcommands/experimental-link-user-defined-dependency-binary.md)
- [CLI Telemetry](./telemetry.md)
- [Debugging FOSSA CLI](./references/debugging/README.md)
