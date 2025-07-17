# Analyzing the Android Open Source Project

<!-- markdown-link-check-disable-next-line -->
[Android Open Source Project (AOSP)](https://source.android.com/) is used to make custom Android operating system distributions.
This document describes how to analyze AOSP distributions for licenses using FOSSA.
It does not describe how to analyze AOSP distributions for dependencies and security vulnerabilities.

[This FOSSA project shows the result of analyzing unmodified and unbuilt AOSP](https://app.fossa.com/projects/custom%2B1%2Faosp-subdirs-example/refs/branch/master).

## Requirements

In our testing, we have been able to analyze unmodified AOSP sources for licenses in about 2 hours and 30 minutes with 32 CPU cores and 64 GB of memory (`m5a.8xlarge` EC2 instance), with a peak system memory usage of 54 GB.
<!-- markdown-link-check-disable-next-line -->
This is in line with the [hardware requirements to build AOSP suggested by Google](https://source.android.com/docs/setup/build/requirements).

## Analyzing AOSP

Create the following `fossa-deps.yml` file in the AOSP root:

```yml
vendored-dependencies:
- name: aosp-bionic
  path: bionic
  version: master
- name: aosp-cts
  path: cts
  version: master
- name: aosp-developers
  path: developers
  version: master
- name: aosp-device
  path: device
  version: master
- name: aosp-hardware
  path: hardware
  version: master
- name: aosp-libcore
  path: libcore
  version: master
- name: aosp-packages
  path: packages
  version: master
- name: aosp-platform_testing
  path: platform_testing
  version: master
- name: aosp-sdk
  path: sdk
  version: master
- name: aosp-test
  path: test
  version: master
- name: aosp-tools
  path: tools
  version: master
- name: aosp-art
  path: art
  version: master
- name: aosp-bootable
  path: bootable
  version: master
- name: aosp-dalvik
  path: dalvik
  version: master
- name: aosp-development
  path: development
  version: master
- name: aosp-external
  path: external
  version: master
- name: aosp-frameworks
  path: frameworks
  version: master
- name: aosp-kernel
  path: kernel
  version: master
- name: aosp-libnativehelper
  path: libnativehelper
  version: master
- name: aosp-pdk
  path: pdk
  version: master
- name: aosp-prebuilts
  path: prebuilts
  version: master
- name: aosp-system
  path: system
  version: master
- name: aosp-toolchain
  path: toolchain
  version: master
```

Create this `.fossa.yml` configuration file in the same directory:

```yml
version: 3

targets:
  only:
    - type: npm
  exclude:
    - type: npm
```

Then, run `fossa analyze` in the same directory.


If you cannot create or modify a `.fossa.yml` configuration file, you can achieve the same result with this command:

```sh
fossa analyze --only-target npm --exclude-target npm
```

## Explanation

The directories listed in `fossa-deps.yml` were selected from unmodified and unbuilt AOSP.
If you need to analyze a different set of directories, you should modify this file accordingly.

`version` is actually an optional field when defining vendored dependencies in `fossa-deps.yml`.
However, omitting this field causes the FOSSA CLI to compress each vendored directory to calculate a hash to use as a placeholder version.
AOSP directory trees are too transitive for zip files, so we manually define a dummy version to avoid having to compress each subdirectory.

Running `fossa analyze` without any other flags or configuration files causes all [analysis strategies](../references/strategies/README.md) to be executed, which requires significantly more resources. This is likely to fail or take an excessive amount of time due to the size and number of subprojects discovered in the AOSP source tree.

The FOSSA CLI does not currently have a specific configuration to only run license analysis, but we can work around this by specifying intentionally contradicting exclusions, i.e. `--only-target npm --exclude-target npm`. The choice of `npm` here is arbitrary; any analysis strategy name can be used.
