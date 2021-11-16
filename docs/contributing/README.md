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

--
