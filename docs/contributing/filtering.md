# Filtering projects

For many repositories, including ourselves, our scans will reveal projects that
do not need to be analyzed at all, and could be safely ignored.  We use the
`--{only,exclude}-{path,target}` CLI arguments, or the
`{targets,paths}.{only,exclude}:` config file values to avoid giving users
information they don't care about, or to avoid doing work that will be thrown
away.  This document expresses how that filter process works, and clarfies its
semantics and limitations.

## Shorthand

For this document, we'll use a shorthand like this:

```haskell
-- `--only-path tooling`
include tooling/

-- `--only-target cabal --exclude-path test`
include cabal AND exclude test/

-- `--exclude-path foo/bar`
exclude foo/bar

-- `--exclude-target cargo@baz`
exclude cargo@baz
```

When an argument is ambiguous, trailing slashes are added to paths.  Target
syntax is presented later in the document.

## Background

First, we need to understand how the CLI runs at a broad level.  This model may
change in the future, but will still be broadly applicable for as long as we
follow a 2-phase "discover, then analyze" process.  This diagram shows how the
CLI operates, and where it interacts with the filters:

![CLI model with filtering](filters/filter-diagram.svg)

- Note the threading notes in the diagram, while some of that is important for
implementing safe concurrent code, the important takeaway for this discussion
is that **_every project is discovered and analyzed in complete isolation_**
**_from all other projects_**.

From this diagram, we can see that there are two major processes of each
strategy in the CLI:

- **Discovery**: Walk the filetree and emit "analysis targets".
  - Filtering applied here is called "Discovery exclusion".
- **Analysis**: Analyze each "analysis target" and determine it's dependencies.
  - Filtering here is called "Analysis target filtering".

These two phases are very important, and very different.  Each has different
sets of data known to it, and there are benefits to doing filtering at each
stage. It's also notable that once we emit an analysis target, we track it
until the process terminates.

## The Discovery Phase

Almost all discovery takes the same form:

1. Walk the filetree, and look for "indicator files". An indicator file is any
file that tells us that a specific type of project is rooted in the directory
that that file lives in. Mostly, these are actual project files that we scan,
or run buildtools against.
1. For each directory with indicator files, we capture exactly one "analysis
target" (more on that later), which contains enough info for us to run a
separate "scan for dependencies" task for that target.

There are exceptions to these rules: both maven and nodeJS are capable of
statically analyzing multi-directory workspaces, and therefore do not have a
1:1 relationship between directories with indicator files and analysis targets.
There may be other analyzers that do this in the future.

Discovery that follows the simple model is run via `simpleDiscover`, and makes
up almost every analyzer we currently have.

### Analysis Targets

To the user, an analysis target is a unique combination of
`DiscoveredProjectType` and directory (plus optional subtargets).
`DiscoveredProjectType` always correlates to one of 3 things:

- A single, unified tool, e.g. `cargo`
- A family of tools, e.g. `setuptools` (uses `setup.py` and `requirements.txt`)
- An isolated subset of a tool, e.g. the various `maven` strategies.

To a developer, an analysis target is a `DiscoveredProject a`, which contains
the info above, plus the data `a` found by the discovery process, which is
any extra input needed for the analysis phase.  This is the `projectData` for
that analysis target.  In some cases, we do extra work during discovery, and
we save that data for reuse in the `projectData`.  Notably, maven and nodeJS
discovery both do this, but this is not limited to non-simple discovery.

To actually run analysis, the `projectData` must have a instance of the
`AnalyzeProject` typeclass.  This is not likely to be relevant for the
purposes of filtering, but it's still worth mentioning.

### Subtargets

Some analyzers (currently only gradle) may implement support for partial
analysis, via subtargets.  This works by emitting an additional set of
qualifiers (as simple text), which are decided by the analyzer.  In gradle,
these refer to gradle subprojects.  This is used to only analyze part of a
single analysis target.  We will discuss this more in the next section.

### Syntax

When referring to the analysis targets, we use a string format of the syntax:
`type@path[:subtarget]`. Trailing slashes on the path component are optional.
We construct these string representations like so:

Given the analysis target tuple `(type, path, [list, of, subtargets])`,
we emit one target per subtarget, plus a target with no subtargets.  The target
without a subtarget component is ALWAYS present, even when no subtargets are
found.  This is the most commmon scenario, since most analyzers do not make use
of subtargets.  For example, the tuple `(cargo, foo/bar, [])` would emit the
single target `cargo@foo/bar`.  The tuple of `(gradle, foo/baz, [subA, subB])`
would emit three targets:

```text
gradle@foo/baz
gradle@foo/baz:subA
gradle@foo/baz:subB
```

## Analysis Target Filtering

After discovery, we have a set of analysis targets, which can be rendered to
the user via `fossa list-targets`, in the syntax discussed above.  Without
filters, we begin tasks for ALL analysis targets.  However, if filters exist,
and those filters match the found analysis targets, then some of the targets
will NOT begin tasks.  This avoids doing the analysis work entirely, and 
therefore omits the results from the final scan output.

### Examples

Running specific targets:

```text
-- Given the following emitted targets:
cargo@foo
cabal@bar
gomod@baz

-- And the following filters
include cargo@foo

-- Analysis will be run for:
cargo@foo
```

Only running cabal analysis:

```text
-- Given the following emitted targets:
cabal@foo
cargo@bar

-- And the following filter:
include cabal

-- Analysis will be run for:
cabal@foo
```

Excluding all targets in `bar/**/*` (including `bar/` itself):

```text
-- Given the following emitted targets:
cabal@foo
cargo@bar

-- And the following filter:
exclude bar/

-- Analysis will be run for:
cabal@foo
```

Irrelevant filters are ignored:

```text
-- Given the following emitted targets:
cabal@foo
cargo@bar

-- And the following filter:
exclude baz/

-- Analysis will be run for:
cabal@foo
cargo@bar
```

### Filter conflicts

When a target is both included and excluded, we always reject the target. This
allows for more fine-grained control, as in the following example:

Only running cargo projects found at `foo/**/*` (but not `foo/` itself).

```text
-- Given the following emitted targets:
cargo@foo
cargo@foo/bar/baz
cargo@quux

-- And the following filter:
include cargo AND include foo/ AND exclude cargo@foo

-- Analysis will be run for:
cargo@foo/bar/baz
```

### The Available Data

Because the analysis target filtering happens after discovery is completed, we
already know the full structure of the projects in the directory.  Given this
data, it is a simple task to compare the filters to the known projects, and
produce a list of projects which should ACTUALLY be run. Given this simplicity,
we will not be discussing the actual implementation details here.

This knowledge of full project structure allows us to report to users that we
have chosen to skip some projects, a fact which becomes very relevant when
discussing discovery exclusion.

## Discovery Exclusion

The primary use-cases for analysis target filtering are to remove unwanted
results from the final output, and to reduce the amount of work done by the CLI
during analysis, which can provide performance benefits.  The initial driving
example was for allowing gradle to do partial analysis, which can save a LOT of
time in massive gradle projects.  We also wanted to reduce the results from our
own codebase, as our testing includes a lot of false positive projects, and we
needed to filter them out.

However, the use case for discovery exclusion is STRICTLY performance-related,
and should have no overall effect on which analysis targets are actually run.
As a result of this, discovery exclusion must attempt to avoid unnecessary
discovery where possible, but it mamy also choose to allow discovery and rely
on the analysis target filtering to prevent unwanted results fromm leaking
through.

At a high-level, discovery exclusion is done via two processes:

- Short-circuiting an entire discovery function if the `DiscoveredProjectType`s
emitted by the function could never match the existing filters.  This short
circuiting is done by simply emitting zero analysis targets.
- Skipping subdirectory paths when the path to be scanned could never match
the existing filters.  This is done by instructing the filesystem walker to
skip all subdirectories at non-matching points.



