
# FOSSA CLI Documentation

The FOSSA CLI is designed to discover all build environments in your project and extract third party dependency information from all of them.

## [F.A.Q.](faq.md)

## [User Guide](user-guide.md#user-guide)

The user guide provides an in depth look at the FOSSA CLI's discovery and analysis process. This guide includes documentation on the available `fossa` commands, their usage, and the configuration options available for each.

1. [Installing the FOSSA CLI](user-guide.md/#1-installation)
1. [Configuring a Project](user-guide.md/#2-configuring-a-project)
     - [Configuration File](user-guide.md/#configuration-file)
     - [Argument Configuration](user-guide.md/#argument-configuration)   
1. [Analyzing a Project](user-guide.md/#3-analyzing-a-project)
      -  [Uploading Custom Builds](user-guide.md/#uploading-custom-builds)
         - [Data Format](user-guide.md/#data-format)
         - [Locator Spec](user-guide.md/#locator-spec)
1. [CLI Reference](user-guide.md/#cli-reference)
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

## [Configuration File](config-file.md#fossayml)

## Environment Integrations
- General Layout
  - Support
  - Configuration
  - Options
  - Design
  - FAQ
- Environments
  - [ant](integrations/ant.md#ant--ivy)
  - [archive](integrations/archive.md#archive)
  - [bower](integrations/bower.md#bower)
  - [buck](integrations/buck.md#buck)
  - [carthage](integrations/carthage.md#carthage)
  - [cocoapods](integrations/cocoapods.md#cocoapods)
  - [composer](integrations/composer.md#composer)
  - [debian](integrations/debian.md#debian)
  - [dotnet](integrations/dotnet.md#net)
  - [golang](integrations/golang.md#go)
  - [gradle](integrations/gradle.md#gradle)
  - [haskell](integrations/haskell.md#haskell)
  - [maven](integrations/maven.md#maven)
  - [nodejs](integrations/nodejs.md#nodejs)
  - [okbuck](integrsations/okbuck.md#okbuck)
  - [python](integrations/python.md#python)
  - [ruby](integrations/ruby.md#ruby)
  - [rust](integrations/rust.md#rust)
  - [scala](integrations/sbt.md#sbt)
  
