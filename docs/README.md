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

### Concepts

- [FOSSA CLI Concepts](./concepts/analysis-and-analyzers.md)
- [Analysis Targets](./concepts/analysis-and-analyzers.md#analysis-targets)
- [Discovery and Analysis](./concepts/analysis-and-analyzers.md#discovery-and-analysis)
- [Strategy Tactics](./concepts/analysis-and-analyzers.md#tactics)
- [Static and Dynamic Strategies](./references/strategies/README.md#static-and-dynamic-strategies)

### Walkthroughs

- [Integrating a project](./walkthroughs/integrating.md)
- [Debugging an Integration](./references/debugging/README.md)
- [Analysis target configuration](./walkthroughs/analysis-target-configuration.md)
- [Custom integration](./walkthroughs/custom-integrating-with-bower-example.md)
- [Integrating a Conan Project](./walkthroughs/conan.md)
- [Configuring SSL/TLS Support Manually](./walkthroughs/ssl-cert.md)
- [Integrating Container Scanning in CI](./walkthroughs/container-scanning-generic-ci.md)

### Features

#### Analyzing Projects

<!-- Consider linking to each language, tool, or platform here -->
- [Language and Tool (Strategy) Support](./references/strategies/README.md)
- [Configuring Which Targets Are Analyzed](./walkthroughs/analysis-target-configuration.md)
  - [Analyzing Specific Submodules](./walkthroughs/analysis-target-configuration.md#target-filtering-for-submodules)
- [Dynamic Strategy Command Selection](./features/strategy-command-selection.md)
- [`fossa analyze` Reference](./references/subcommands/analyze.md)
- [Vulnerable Reachability](./features/vuln_reachability.md)

#### Manually Specifying Dependencies

- [Overview](./features/manual-dependencies.md)
- [Deps from a Known Package Manager (Referenced Dependencies)](./features/manual-dependencies.md#referenced-dependencies)
- [Fully Specifying a Dep and License Manually (Custom Dependencies)](./features/manual-dependencies.md#referenced-dependencies)
- [Specifying a Source URL for Analysis (Remote Dependencies)](./features/manual-dependencies.md#referenced-dependencies)
- [Performance Characteristics of Manual Dependencies](./features/manual-dependencies.md#performance)

#### Analyzing Vendored Dependencies

- [Overview](./features/vendored-dependencies.md)
- [Vendored Dependency Names and Scope](./features/vendored-dependencies.md#vendored-dependency-names-and-scope)
- [How Vendored Dependencies are Scanned](./features/vendored-dependencies.md#how-vendored-dependencies-are-scanned)
- [Filtering Paths for License Scanning in Vendored Dependencies](./features/vendored-dependencies.md#path-filtering)
- [Path Filtering on Windows](./features/vendored-dependencies.md#path-filtering-and-windows)
- [How to Debug Path Filters](./features/vendored-dependencies.md#debugging-your-path-filters)
- [Vendored Dependency Performance](./features/vendored-dependencies.md#performance)
- [Detecting Vendored Source Code with Vendored Source Identification (VSI)](./references/subcommands/analyze/detect-vendored.md)

#### Searching for Custom Keywords and Licenses

- [Overview](./features/custom-license-and-keyword-searches.md)
- [Keyword Searches](./features/custom-license-and-keyword-searches.md#keyword-searches)
- [License Text Searches](./features/custom-license-and-keyword-searches.md#custom-license-searches)
- [Text Search Term Format](./features/custom-license-and-keyword-searches.md#regular-expression-format)
- [Configuring Custom-License Searches Organization-Wide](./features/custom-license-and-keyword-searches.md#configuring-custom-license-searches-for-your-whole-organization)
  - [Escape characters in organization-wide custom licenses searches](./features/custom-license-and-keyword-searches.md#escape-characters-in-custom-license-searches-for-your-whole-organization)
  - [Turning Off Organization-Wide Custom-Licenses Searches](./features/custom-license-and-keyword-searches.md#turning-off-organization-wide-custom-licenses-searches)

#### Analyzing Containers

- [Overview](./references/subcommands/container.md)
- [How FOSSA's Container Scanner Works](./references/subcommands/container/scanner.md#documentation)
- [Container Image Sources](./references/subcommands/container/scanner.md#container-image-source)
  - [Exported docker archive](./references/subcommands/container/scanner.md#1-exported-docker-archive)
  - [From Docker Engine](./references/subcommands/container/scanner.md#2-from-docker-engine)
  - [From Container Registries](./references/subcommands/container/scanner.md#3-from-registries)
- [Supported Container Package Managers](./references/subcommands/container/scanner.md#supported-container-package-managers)
  - [Container Jar File Analysis](./references/subcommands/container/scanner.md#container-jar-analysis)
- [Viewing Detected Projects](./references/subcommands/container/scanner.md#view-detected-projects)
- [Configuring Container Analysis Targets](./references/subcommands/container/scanner.md#utilize-analysis-target-configuration)
- [Integrating Container Scanning in CI](./walkthroughs/container-scanning-generic-ci.md)
- [Frequently Asked Questions](./references/subcommands/container/scanner.md#frequently-asked-questions-faqs)
- [Debugging](./references/subcommands/container/scanner.md#debugging)
- [Limitations & Workarounds](./references/subcommands/container/scanner.md#limitations--workarounds)
- [Scanning Images using Podman (Experimental)](./references/subcommands/container/podman.md)

#### Analyzing SBOM Files

- [Overview](./references/subcommands/sbom.md)
- [`sbom analyze`](./references/subcommands/sbom.md#fossa-sbom-analyze-path-to-sbom-file)
- [`sbom test`](./references/subcommands/sbom.md#fossa-sbom-test-path-to-sbom-file)

#### Experimental Features

- [Overview and Support Policy](./references/experimental/README.md)

#### Scanning Projects for Source Snippets

- [Overview](./references/subcommands/snippets.md)
- [Quickstart](./references/subcommands/snippets.md#quickstart)
- [Frequently Asked Questions](./references/subcommands/snippets.md#faq)

##### Subcommands

- [`fossa snippets analyze`](./references/subcommands/snippets/analyze.md)
- [`fossa snippets commit`](./references/subcommands/snippets/commit.md)

### References

#### CLI commands

- [`fossa analyze`](./references/subcommands/analyze.md): Analyze a project.
- [`fossa container`](./references/subcommands/container.md): Scan a container for vulnerabilities and compliance issues.
- [`fossa init`](./references/subcommands/init.md): Generate sample config files.
- [`fossa list-targets`](./references/subcommands/list-targets.md): Retrieve a list of filterable targets in a project.
- [`fossa report`](./references/subcommands/report.md): Download a report of the most recent scan of a project.
- [`fossa snippets`](./references/subcommands/snippets.md): Analyze snippets of a project and check if they exist in other open source projects FOSSA knows about.
- [`fossa test`](./references/subcommands/test.md): View the results of the most recent scan of a project.
- [`fossa release-group`](./references/subcommands/release-group.md): Interact with FOSSA release groups.
- [`fossa project`](./references/subcommands/project.md): Interact with FOSSA projects.

#### Configuration

- [Specifying Dependencies Manually with `fossa-deps.yml`](./references/files/fossa-deps.md)
- [Specifying Project Settings with `.fossa.yml`](./references/files/fossa-yml.md)
- [Upgrading from FOSSA CLI v1 to a Supported FOSSA CLI](./differences-from-v1.md)

#### Troubleshooting

- [Debugging FOSSA CLI](./references/debugging/README.md)
  - [Debugging a Missing Project](./references/debugging/README.md#debugging-a-missing-project)
  - [Debugging a Failing Strategy](./references/debugging/README.md#debugging-strategies)
  - [Debugging a FOSSA CLI Specific Error](./references/debugging/README.md#debugging-fossa-cli-operation)
  - [Debugging with the Debug Bundle](./references/debugging/README.md#debugging-with-the-debug-bundle)
- [How to Debug Path Filters](./features/vendored-dependencies.md#debugging-your-path-filters)
- [Debugging Container Analysis](./references/subcommands/container/scanner.md#debugging)
- [Understanding SSL/TLS Certificates and FOSSA](./walkthroughs/ssl-cert.md)

## FAQs

### When are you adding support for (some buildtool/language)?

If we don't support your choice of language/buildtool,
please [create a support ticket](https://support.fossa.com) to express interest!

### What kind of data gets uploaded to FOSSA's servers?

Please see our '[What data gets uploaded?](./walkthroughs/what-data-gets-uploaded.md)' doc for more information.

### How can I contribute a change to FOSSA CLI?

Please see our [contributing](./contributing/README.md) documentation.

