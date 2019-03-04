
# FOSSA CLI Documentation

The FOSSA CLI is designed to discover all build environments in your project and extract third party dependency information from all of them.

## [User Guide](user-guide.md#user-guide)

The user guide provides an in depth look at the FOSSA CLI's discovery and analysis process. This guide includes documentation on the available `fossa` commands, their usage, and the configuration options available for each.

1. [Configuring a Project](user-guide.md/#configuring-a-project)
     - [Configuration File](user-guide.md/#configuration-file)
     - [Argument Configuration](user-guide.md/#argument-configuration)   
2. [Analyzing a Project](user-guide.md/#)
      -  [Uploading Custom Builds](user-guide.md/#Uploading-Custom-Builds)
         - [Data Format](user-guide.md/#Data-Format)
         - [Locator Spec](user-guide.md/#Locator-Spec)
3. [CLI Reference](user-guide.md/#CLI-Reference)
      - [`fossa`](user-guide.md/#fossa)
      - [`init`](user-guide.md/#fossa-init)
      - [`analyze`](user-guide.md/#fossa-analyze)
      - [`test`](user-guide.md/#fossa-test)
      - [`upload`](user-guide.md/#fossa-upload)
      - [`report`](user-guide.md/#fossa-report)

## [How it Works](how-it-works.md#how-it-works)

This document is a walk-through of getting started with the FOSSA CLI and is aimed at first time users in order to learn the recommended workflow. The walk-through works through analyzing the fossa-cli for all third party dependencies.

- [Walkthrough](how-it-works.md#walkthrough)
    1. [Building](how-it-works.md#step-1-building)
    2. [Configuration](how-it-works.md#step-2-configuration)
    3. [Analysis](how-it-works.md#step-3-analysis)
    4. [Testing](how-it-works.md#step-4-testing)
    5. [Finalizing](how-it-works.md#step-5-finalizing)

## [Configuration File](config-file.md#fossa.yml)

## Environment Integrations
- General Layout
  - Support
  - Configuration
  - Options
  - Design
  - FAQ
- Environments
  - [ant](integrations/ant.md)
  - [archive](integrations/archive.md)
  - [bower](integrations/bower.md)
  - [buck](integrations/buck.md)
  - [carthage](integrations/carthage.md)
  - [cocoapods](integrations/cocoapods.md)
  - [composer](integrations/composer.md)
  - [debian](integrations/debian.md)
  - [golang](integrations/golang.md)
  - [gradle](integrations/gradle.md)
  - [maven](integrations/maven.md)
  - [nodejs](integrations/nodejs.md)
  - [nuget](integrations/nuget.md)
  - [okbuck](integrsations/okbuck.md)
  - [python](integrations/python.md)
  - [ruby](integrations/ruby.md)
  - [scala](integrations/sbt.md)
  