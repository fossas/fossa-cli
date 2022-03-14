# Development Docs

This is a collection of development-related docs for working on the fossa-cli repo.

## Building and Running the project

The [HACKING](HACKING.md) doc describes our current dev tools and build practices, as well
as providing som info about our CI setup.

## Style Guide

Our [Style Guide](STYLE-GUIDE.md) las out stylistic and idiomatic standards for all contributions.
We are in the process of fully aligning to that guide, but any new contributions should follow the style guide.

## Parsing and best practices

Parsing in haskell requires a lot less boilerplate than may other languages, but comes with some caveats.
The [best practices guide](parsing-best-practices.md) illustrates some important information about how to write
parsers correctly, and links to a few tutorials.

## Record/Replay

Because of magic (not really, [but...][3laws]), we are able to track what files we have read (and their contents),
as well as the output of any commands executed during analysis.  This is called [`replay logging`](replay-logging.md),
and is a very useful debugging tool.

[3laws]: https://en.wikipedia.org/wiki/Clarke%27s_three_laws

## Graph Hydration

If a top-level component of a graph of dependencies is a test dependency, then its direct children are also test
dependencies, as are their direct children, and so on.  In some cases, the build tool/manifest files/lock files will
tell us this information, but sometimes, we have to propogate these environments downwards ourselves.  This is called
[`graph hydration`](graph-hydration.md), and is handled by the `Graphing.Hydrate` module.

## Testing with effects

Testing with `hspec` provides several useful assertion/organization methods.  Wrtiting business logic with
`fused-effects` allows us to write expressive, convenient, and safe code.  Testing those `fused-effects`-based
functions with `hspec` is a nightmare.  In short, `hspec` is `IO`-only, and `fused-effects` operates on free-ish
monads (not really, but it does work similarly).  To make this easier for us, we created the `Test.Effect` module,
and use that to mimic `hspec` for an effectful system.  [More details can be found here.](testing-with-effects.md)

## Releases

We have a release process that manages building our archives and updating our release notes.  [Read more about our releases here](releases.md).