## `fossa snippets`

This subcommand is the home for FOSSA's snippet scanning feature.

It is made up of two subcommands:

- [`fossa snippets analyze`](./snippets/analyze.md)
- [`fossa snippets commit`](./snippets/commit.md)

See the pages linked above for more details.

## Quickstart

```shell
# Set your API key. Get this from the FOSSA web application.
# On Windows, use this instead: $env:FOSSA_API_KEY=XXXX
export FOSSA_API_KEY=XXXX

# Navigate to your project directory.
cd $MY_PROJECT_DIR

# Analyze the project for local snippet matches.
# Match data is output to the directory specified to the `-o` or `--output` argument.
# If desired, you can manually review the matches output to the directory.
fossa snippets analyze -o snippets

# Commit matched snippets to a `fossa-deps` file.
# Provide it the same directory provided to `fossa snippets analyze`.
# This creates a `fossa-deps` file in your project.
#
# Note that you can control what kinds of snippets are committed;
# see subcommand documentation for more details.
fossa snippets commit --analyze-output snippets

# Run a standard FOSSA analysis, which will also upload snippet scanned dependencies,
# since they were stored in your `fossa-deps` file.
fossa analyze
```

## FAQ

### Is my source code sent to FOSSA's servers?

**Short version: No.** More detail explaining this is below.

FOSSA CLI fingerprints your first party source code but does not send it to the server.
The fingerprint is a SHA-256 hashed representation of the content that made up the snippet.

FOSSA CLI does send the fingerprint to the server, but since SHA-256 hashes are
[cryptographically secure](https://en.wikipedia.org/wiki/SHA-2), it is effectively not possible
for FOSSA to reproduce the original code that went into the snippet.

Of course, if the fingerprint matches FOSSA could then infer that the project contains that snippet of code,
but since FOSSA CLI does not send any additional context in the file there's no way for FOSSA or anyone else
to make use of this information.

The code to perform this is open source in this CLI;
users can also utilize tooling such as [echotraffic](https://github.com/fossas/echotraffic)
to report the information being uploaded.

### How does FOSSA snippet scanning work?

FOSSA snippet scanning operates over a matrix of options:

```
Targets × Kinds × Methods
```

Valid options for `Targets` are:

Target     | Description
-----------|-----------------------------------------------------------------------
`Function` | Considers function declarations in the source code as snippet targets.

Valid options for `Kinds` are:

Kind        | Description
------------|----------------------------------------------
`Full`      | The full expression that makes up the target.
`Signature` | The function signature of `Function` targets.
`Body`      | The function body of `Function` targets.

Valid options for `Methods` are:

Method              | Description
--------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------
`Raw`               | The expression that makes up the target as written in the source code file.
`NormalizedSpace`   | The expression with any character in the Unicode [whitespace character class][] replaced with a space, and any contiguous spaces collapsed to a single space.
`NormalizedComment` | The expression with comments removed, as defined by the source code language.
`NormalizedCode`    | Equivalent to `NormalizedComment` followed by `NormalizedSpace`.

Given these options, the fully defined matrix of options is as follows:

```
{Function} × {Full, Signature, Body} × {Raw, NormalizedSpace, NormalizedComment, NormalizedCode}
```

FOSSA then scans open source projects for these snippets and records them along with their metadata,
such as where in the file the snippet originated and from what project.

Finally, when users scan their first-party projects, FOSSA extracts snippets in the same manner
and compares the fingerprints of the content of those snippets against the database.
If a match is found, FOSSA reports all open source projects in which the snippet was found,
along with recorded metadata about that snippet.

[whitespace character class]: https://en.wikipedia.org/wiki/Unicode_character_property#Whitespace
