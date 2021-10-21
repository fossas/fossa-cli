# Differences from FOSSA 1.x to 2.x

Upgrading from FOSSA CLI 1.x to 2.x is a major breaking change. For most users, this change should be seamless. For users who use less common flags, or who have heavily customized their CLI usage, you may need to update some scripts.

## Table of contents <!-- omit in toc -->

- [What's new in FOSSA 2.x?](#whats-new-in-fossa-2x)
  - ["Modules" are now "analysis targets"](#modules-are-now-analysis-targets)
  - [Automatic analysis target discovery](#automatic-analysis-target-discovery)
  - [Automatic analysis target strategy selection](#automatic-analysis-target-strategy-selection)
  - [Improved correctness](#improved-correctness)
  - [Improved debug logging](#improved-debug-logging)
- [How to upgrade to FOSSA 2.x](#how-to-upgrade-to-fossa-2x)
  - [Remove calls to `fossa init`](#remove-calls-to-fossa-init)
  - [Migrate your configuration file](#migrate-your-configuration-file)
  - [Migrate "archive upload" targets](#migrate-archive-upload-targets)
  - [Getting help with your migration](#getting-help-with-your-migration)

## What's new in FOSSA 2.x?

Our focus in CLI 2.x is making FOSSA integrations easier to deploy by reducing the amount of configuration needed by users. Our goal is to get as close to turnkey deployment on as many build systems and codebases as possible.

### "Modules" are now "analysis targets"

In 1.x, the CLI was configured by "modules", which were entrypoints to user programs that the CLI should analyze.

In 2.x, we've renamed "modules" to "analysis targets", and simplified how analysis targets are defined.

We renamed analysis targets to avoid confusing naming conflicts with existing language-specific "modules". Analysis targets are no longer defined by multiple, confusingly similar fields (e.g. "Dir" vs. "Path"), and are now simple `(type, target)` tuples.

### Automatic analysis target discovery

FOSSA CLI 2.x now does automatic analysis target discovery when you run `fossa analyze`.

This means that `fossa init` is no longer required. `fossa init` is now a no-op that emits a warning, and will be removed in a future release.

### Automatic analysis target strategy selection

In 1.x, modules could be manually configured with "strategies" that specified _how_ a module should be analyzed. While `fossa init` attempted to choose the best strategy, manual intervention was sometimes required depending on the project's setup.

In 2.x, the CLI now automatically selects the optimal strategy for analysis targets given the current environment (e.g. whether a build tool is available). This is possible because discovery and analysis are now one step, so we can check the suitability of analysis strategies while discovering targets.

### Improved correctness

Analysis strategies in CLI 2.x have substantive improvements in correctness and reliability across all language integrations. 2.x has much stronger compile-time correctness guarantees in its parsers, and uses build tool plugins instead of output parsing for target types where output parsing was brittle.

### Improved debug logging

CLI 2.x has much better debug logging, including a new feature called "replay logging" which allows developers to perfectly reproduce a bug report given a replay log. This is made possible by stronger compile-time guarantees that ensure all effects that occur during analysis are logged for replay.

## How to upgrade to FOSSA 2.x

### Remove calls to `fossa init`

Since analysis targets are now automatically discovered during analysis, `fossa init` is no longer a valid command. `fossa init` is currently a no-op that emits a warning. It may be removed in a future release.

### Migrate your configuration file

We've made major breaking changes in the configuration file format for CLI 2.x to improve clarity. You'll need to migrate your 1.x configuration files to the new 2.x format for their configurations to apply.

Configuration files for 1.x will be ignored when running 2.x. We determine whether a configuration file is for 1.x by examining its `version` field. Configuration file `version`s `1` and `2` are for 1.x.

<!-- TODO: Write up configuration file field docs -->

For documentation on the new configuration file format, see [here](https://github.com/fossas/spectrometer/pull/220).

### Migrate "archive upload" targets

In 1.x, ["archive upload" modules](https://github.com/fossas/fossa-cli/blob/master/docs/integrations/archive.md#archive) were a special kind of manually specified module of type `archive` that uploaded source code to the FOSSA backend for license scanning. They were also known as "raw" modules or "license scan" modules.

In 2.x, archive uploads are no longer a special analysis target type. Instead, you can use our general support for [manually specified dependencies](https://github.com/fossas/spectrometer/blob/master/docs/userguide.md#license-scanning-local-dependencies) to specify local dependencies.

### Getting help with your migration

If you run into migration issues, you can get support by opening a ticket in this repository.

If you are integrating a private project and want to share more details, or if you're a FOSSA customer with priority support, you can also email support@fossa.com or file a ticket at support.fossa.com for assistance.

### Migrate "fossa report" commands

In 1.x, `fossa report` supported `fossa report dependencies`, `fossa report licenses` and `fossa report attribution`.

In 2.x, only `fossa report attribution --json` is reported. The information in the previously supported v1 commands is contained in the output from `attribution`.

The format for this report has also changed slightly. Until the end of April 2022, FOSSA supports a compatibilty script to convert the format of the report.
For more information, including usage information, see [FOSSAv1 report compatibility](https://github.com/fossas/spectrometer/blob/master/docs/userguide.md#fossav1-report-compatibility) in the user guide.
