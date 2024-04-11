# Ruby Analysis

Ruby projects use a buildtool called `bundler` to manage their dependencies. We
parse a lockfile or run the `bundle` cli to determine dependencies.

| Strategy    | Direct Deps                    | Transitive Deps                | Edges              | Tags | Container Scanning |
| ----------- | ------------------------------ | ------------------------------ | ------------------ | ---- | ------------------ |
| gemfilelock | :white_check_mark:             | :white_check_mark:             | :white_check_mark: |      | :white_check_mark: |
| bundleshow  | :white_check_mark: (unlabeled) | :white_check_mark: (unlabeled) | :x:                |      | :x:                |

## Project Discovery

`gemfilelock`: Find all files named `Gemfile.lock`

`bundleshow`: Find directories containing a `Gemfile` or `Gemfile.lock`

## Analysis: gemfilelock

> TLDR: A perfect dependency graph will be found and dependencies location will also be known.
 
The lockfile strategy attempts to parse Bundler's `Gemfile.lock` lockfile. This file is created by bundler itself after a build is completed and can be distributed in order to maintain reproducible builds. It contains the following information about a Ruby project:
- The location for each dependency. These locations are each separate sections and the ones of note are `GIT`, `PATH`, and `GEM` which provide their remote in the `remote: <location>` line.
  - `GIT` and `GEM` type dependencies are supported. `PATH` and any others are not and will show up as `GEM` type dependencies.
- Each dependencies required dependencies. These required dependencies are listed in the remote sections directly following each dependency from that remote.
- All direct dependencies, listed in the `DEPENDENCIES` section.
- Platforms that this ruby project is compatible with, listed in the `PLATFORMS` sections.
- Version of `bundler` that was used to create the lockfile.

The Lockfile strategy relies on parsing this `Gemfile.lock` file and extracting
information from each of these sections. This strategy takes the following steps
to create an accurate dependency graph. Example `Gemfile.lock`:

```
GIT
  remote: https://github.com/matthewd/rb-inotify.git
  revision: 856730aad4b285969e8dd621e44808a7c5af4242
  branch: close-handling
  specs:
    rb-inotify (0.9.9)
      ffi (~> 1.0)
PATH
  remote: .
  specs:
    activesupport (6.0.0.alpha)
      concurrent-ruby (~> 1.0, >= 1.0.2)
      i18n (>= 0.7, < 2)
      minitest (~> 5.1)
    rails (6.0.0.alpha)
      sprockets-rails (>= 2.0.0)
GEM
  remote: https://rubygems.org/
  specs:
    concurrent-ruby (1.0.5)
    ffi (1.9.21)
    i18n (1.0.1)
      concurrent-ruby (~> 1.0)
    minitest (5.11.3)
    rack (2.0.5)
    sprockets-rails (3.2.1)
      actionpack (>= 4.0)
      activesupport (>= 4.0)
      sprockets (>= 3.0.0)
PLATFORMS
  java
  ruby
  x64-mingw32
  x86-mingw32
DEPENDENCIES
  rails!
  rake (>= 11.1)
  rb-inotify!
BUNDLED WITH
   1.16.2
```

## Analysis: bundleshow

Running `bundle show` displays information about all dependencies used by a
project, and their pinned versions. It doesn't label which dependencies are
direct or transitive, and doesn't tell us edges between dependencies

Example output:

```
Gems included by the bundle:
  * CFPropertyList (3.0.1)
  * addressable (2.7.0)
  * ast (2.4.0)
  * atomos (0.1.3)
  * babosa (1.0.2)
  * binding_of_caller (0.8.0)
  * builder (3.2.3)
  * bundler (1.17.2)
  * byebug (11.0.1)
  * claide (1.0.3)
  * claide-plugins (0.9.2)
  * coderay (1.1.2)
```
