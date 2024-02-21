# Development Docs

This is a collection of development-related docs for working on the fossa-cli repo.

## Building and Running the project

The [HACKING](HACKING.md) doc describes our current dev tools and build practices, as well
as providing some info about our CI setup.

## Style Guide

Our [Style Guide](STYLE-GUIDE.md) contains our stylistic and idiomatic standards for all contributions.
We are in the process of fully aligning to that guide, but any new contributions should follow the style guide.

## Parsing and best practices

Parsing in haskell requires a lot less boilerplate than may other languages, but comes with some caveats.
The [best practices guide](parsing-best-practices.md) illustrates some important information about how to write
parsers correctly, and links to a few tutorials.

## Graph Hydration

If a top-level component of a graph of dependencies is a test dependency, then its direct children are also test
dependencies, as are their direct children, and so on.  In some cases, the build tool/manifest files/lock files will
tell us this information, but sometimes, we have to propagate these environments downwards ourselves.  This is called
[`graph hydration`](graph-hydration.md), and is handled by the `Graphing.Hydrate` module.

## Testing with effects

Testing with `hspec` provides several useful assertion/organization methods.  Writing business logic with
`fused-effects` allows us to write expressive, convenient, and safe code.  Testing those `fused-effects`-based
functions with `hspec` is a nightmare.  In short, `hspec` is `IO`-only, and `fused-effects` operates on free-ish
monads (not really, but it does work similarly).  To make this easier for us, we created the `Test.Effect` module,
and use that to mimic `hspec` for an effectful system.  [More details can be found here.](testing-with-effects.md)

We also have a [mocking framework specifically for API effects](api-mocking.md).

## Filtering

To allow the user to prevent us from including certain sets of results in an analysis, as well as trying to avoid the
work involved, we allow users to filter at both the discovery level and analysis level.
[More details can be in the filtering document](filtering.md).

## Errors and Warnings

Error handling and warnings are done through our [diagnostics effect](diagnostics.md).
In order to render errors and warnings through the diagnostic, refer to our [rendering guidelines](diagnostics.md#rendering-todiagnostic).

## Releases

We have a release process that manages building our archives and updating our release notes.  [Read more about our releases here](releases.md).
