# Entrypoint Refactor Test Plan

## Overview

This test plan lays out the testing goals for the frontend of the fossa-cli.
The primary goal is to layout a series of tests that give us high confidence
that nothing has unintentionally changed between the master branch and the new
changes added in #781.


### How to Read this Test Plan

Most of this test plan is self-explanatory, with some points worth stating upfront:

- Unless a particular command is specified, the test case should be run on all
  commands. 
- Testing is done in comparison with previous behavior. Unless noted in the
  `Known Changes` section, assume that the same behvior should be observed in
  this version as the previous version.
- This test plan assumes significant familiarity with the CLI.

### Known Changes

- Removed `--filter` option and its deprecation warning.
- Use of `--config` now requires the config file to be valid and present.
- Removed the unused `fossa vps` subcommand structure.
- "Global" options are not actually global, and are now only shown on commands
  that use them.

### List of all commands

All of these commands should be prefixed with `fossa`, i.e.: `analyze` becomes
`fossa analyze`. Use this section as a reference for running EVERY command.

Commands annotated with `(local)` do not interact with the network at all.
When a reference is made to `online` commands, omit commands with this annotation.

Commands annotated with `(nocommon)` do not use the shared global CLI options.
This includes the `--config` and `--debug` flags.  When a reference is made to
`common` commands, omit commands with this annotation.

#### Top-Level Visible

- `analyze`
- `test`
- `report`
- `container`
- `list-targets` _(local)_
- `experimental-link-user-defined-dependency-binary`

#### Top-Level Hidden

- `dump-binaries` _(local, nocommon)_
- `log4j` _(local, nocommon)_
- `init` _(local, nocommon)_

#### Visible Container Subcomands

- `container analyze`
- `container test`

### Hidden Container Subcommands

- `container dump-scan` _(local, nocommon)_
- `container parse-file` _(local, nocommon)_

## Test Plan

### Running commands

- Run all commands at least once, on trivial inputs.

#### Common commands

- Ensure all common commands respect the common options:
  - `--debug` (Note that some commands do not actually change their behavior in debug mode.)
  - `--endpoint` (Online commands only)
  - `--project`
  - `--revision`
  - `--fossa-api-key`
  - `--config` (See [known changes](#known-changes))

#### Online

- For online commands, ensure that no work is done before confirming the existence of an API key.
  (you can disable your internet connection to test this)
- Confirm that an invalid (but non-empty) API key is treated as valid by the CLI.  (i.e.: confirm
  that we handle API keys opaquely)
- Confirm that all online commands respect the `--endpoint` option.  (Easiest way to test: set
  a fake URL and make sure the error references that fake URL.)

#### Analyze

- For `fossa analyze`, ensure that config-file filters are IGNORED when CLI
  filters are present.
- Ensure that `analyze` commands (`fossa analyze` and `fossa container analyze`)
  do not require an API key when `--output` is used.
- Test `analyze --experimental-enable-monorepo`:
  - Ensure that the command fails on windows and when using output (can be done at the same time).
  - Ensure that the command passes the correct options to the `wiggins` binary.
- Ensure that project inference works for `git`, and the fallbacks properly save to the disk.
  (test by renaming your `.git` directory, should be saved to `$TMP/.fossa.revision`)
- Ensure that using `--debug` creates a debug bundle.
- Ensure that VSI/IAT work exactly as before. (Would love a test plan for this as well)

#### Test

- Ensure that the `--timeout` is respected, within a margin of error.

### CLI Help text

- Compare help text for all commands.
  - Note the known changes section.
  - Confirm spelling of commands, descriptions of options and arguments.
