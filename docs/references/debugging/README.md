# Debugging FOSSA CLI

FOSSA CLI is complicated, and debugging it is too.
This reference describes how to debug it and in what situations you may want to.

The intended audience for this reference is both FOSSA employees and any users
who want to debug FOSSA CLI themselves.

If you are a reader inside or outside the FOSSA organization and see anything
that could be improved in this document, please open a pull request or
drop us a note on [support.fossa.com](https://support.fossa.com)!

## Using this reference

Since this reference is likely to get quite long, keep in mind you can use
GitHub's automated ["Table of Contents" interface](https://github.blog/changelog/2021-04-13-table-of-contents-support-in-markdown-files/)
to jump around.

## Debugging a Missing Project

If your project wasn't found, make sure you meet the requirements in the
[relevant language/build-tool's quick reference](../strategies/README.md)

If your project meets the requirements, it's very likely `fossa analyze` found your project,
but dependency analysis failed.
The next step is to get more information to [debug the strategy](#debugging-strategies).

## Debugging Strategies

The most common scenario for needing to debug FOSSA CLI is when
it uses an external program as part of its analysis, and that fails
for some reason. In this case, the documentation for the strategy
generally will have more specific steps one can take for debugging.

First, see which strategies are being used in your project
with `fossa list-targets`. Here's an example:
```
; fossa list-targets
[ WARN] fossa list-targets does not apply any filtering, you may see projects which are not present in the final analysis.
[ INFO] Found project: npm@testdata/fossa-analyze/
[ INFO] Found target: npm@testdata/fossa-analyze/
[ INFO] Found project: cargo@./
[ INFO] Found target: cargo@./
```

From here you can then go to the FOSSA CLI
[strategies reference](../strategies/README.md)
to view more information (including additional debugging steps)
for the strategies that are actually in use in your project.

If these steps don't help, the next step is to move on to
[debugging with the debug bundle](#debugging-with-the-debug-bundle).

## Debugging FOSSA CLI operation

Sometimes the issue isn't with a specific strategy, but is related
instead to something FOSSA CLI is doing- for example walking the
file system or uploading results to FOSSA.

The first stop here is always to check the _full_ FOSSA CLI output,
not just the summary at the end: often detailed error messages are printed
at the time the error occurs, but by the time other actions run
and the scan summary is printed, the error messages are no longer
obvious. We recommend scrolling to the top of the FOSSA CLI output
and checking for error messages before moving on, these may
show exactly the cause of the error and even give steps to fix it.

For cases where that doesn't work, the next step is to move on to
[debugging with the debug bundle](#debugging-with-the-debug-bundle).

## Debugging with the debug bundle

The FOSSA CLI debug bundle contains a ton of data useful for
troubleshooting what FOSSA CLI does when analyzing your project.

**Important:** Note that FOSSA does not consider the format
of the FOSSA CLI debug bundle to be stable; this means that
any new release of FOSSA CLI could change this format in
a backwards incompatible way.

**Note:** This reference is written from the persective
of the current CLI version. If you're using an older
FOSSA CLI version, check that tag to see if an older
version of this reference exists. If not, some information
may be different than in this reference.

### Generating a debug bundle

To generate a debug bundle, run:
```
fossa analyze --debug
```

After this has run, a new file is created in the current
working directory (the directory from which you launched `fossa`).
This file is titled `fossa.debug.zip`. It's contents can vary depending on the exact command you are running.
But it will almost always contain the debug bundle in fossa.debug.json.

### Extracting a debug bundle

The FOSSA CLI debug bundle is a JSON file inside of fossa.debug.zip,

If you want to extract every file in the zip:

```
; unzip fossa.debug.zip
```


If you only want the debug bundle, you can use
```
; unzip fossa.debug.zip fossa.debug.json
```

If you also want the JSON to be formatted, you can do this in a single line
(if you have `jq` installed):
```
; unzip -p fossa.debug.zip fossa.debug.json | jq > fossa.debug.json
```

### Reading a debug bundle

The contents of the debug bundle are not stable, so rather
than explain the format in detail, we'll keep this high level.

If in doubt, always feel free to reach out to
[FOSSA Support](https://support.fossa.com)
(or, if you're a FOSSA employee, the Analysis team)
for assistance.

#### Tools

Since debug bundles are often quite large,
we recommend viewing the contents in a command-line tool
that is able to handle a large JSON document.

In practice, we've found `jless`
([website](https://jless.io/), [GitHub](https://github.com/PaulJuliusMartinez/jless))
to be a good tool for this purpose.
We recommend viewing its user guide.

Alternative tools are `jq` ([website](https://stedolan.github.io/jq/))
and `gron` ([GitHub](https://github.com/tomnomnom/gron)),
alongside of course graphical tools like
[Visual Studio Code](https://code.visualstudio.com/),
although we have found that these tools can have issues
parsing FOSSA CLI debug bundles due to their size.

The remainder of this section assumes use of `jless`.

#### Top level fields

When the debug bundle is first opened, select the
first field and press `c` to collapse the top-level fields.
You're left with the following:
```
bundleCLIVersion      # Describes the version of FOSSA CLI
bundleSystem          # Records system information
bundleOutput          # The output of your analysis
bundleArgs            # Records the arguments for FOSSA CLI
bundleConfig          # Records the effective config for FOSSA CLI
bundleEnvVariables    # Records the environment variables provided to FOSSA CLI
bundleScope           # Records FOSSA CLI internal operations
bundleJournals        # Records information about IO actions, such as reading files
```

##### `bundleCLIVersion`

`bundleCLIVersion` reports the version of FOSSA CLI.

##### `bundleSystem`

`bundleSystem` reports system information:

- The operating system name, e.g. `linux`.
- The architecture of the system, e.g. `x86_64`.
- The "capabilities" of the system,
  which is the number of parallel threads that may be run.
  Usually this corresponds to number of cores (for local systems)
  although this is different for Docker containers.
- The number of processors available to the system.
- System memory statistics.

##### `bundleOutput`

The raw output of what FOSSA CLI found when analyzing your project.
This is the same output generated when running `fossa analyze -o`.

##### `bundleArgs`

The arguments provided to FOSSA CLI at runtime.
These are a JSON array of strings; for example the command
```
fossa analyze --debug -p 'my cool project'
```

Would report the following list of arguments:
```
["fossa", "analyze", "--debug", "-p", "my cool project"]
```

##### `bundleConfig`

`bundleConfig` records the effective config for FOSSA CLI.

FOSSA CLI can be configured in a number of ways:
- A config file, e.g. `.fossa.yml`
- Arguments at run time, e.g. `--fossa-api-key`
- Environment variables, e.g. `FOSSA_MAVEN_CMD`

These configurations are then merged together internally,
with some sources overwriting other sources.

The config reported here is the final merged config
from all these sources that FOSSA CLI used.

##### `bundleEnvVariables`

`bundleEnvVariables` records the environment variables provided to FOSSA CLI.

FOSSA CLI only includes environment variables believed to be safe to share
(we don't want to accidentally include secrets)
and that we believe are useful for debugging a given strategy.

##### `bundleJournals`

`bundleJournals` records information about IO actions, such as reading files.

These entries are structured in the form:
```
[
  ["<IO Action Identifier>", "<IO Action Arguments ...>"],
  <IO Action Result>
]
```

Where `IO Action Result` is a variable type that may be a literal
(for example `false`), or an object.

When in object form, generally the object is in the shape
```
{
  "Left": "<Error message or null if no error>",
  "Right": <IO Action Data>
}
```

Using the above, here are a few examples:
```
# This indicates that the file `/home/me/test/package.json` does not exist.
[
  [ "Effect.ReadFS.DoesFileExist", "/home/me/test/package.json" ],
  false
]

# This indicates that FOSSA CLI failed to read the content of `package.json`.
[
  [ "Effect.ReadFS.ReadContentsBS", "/home/me/test/package.json" ],
  { "Left": "Failed to read content: file does not exist" }
]

# This indicates that `/home/me/test/Cargo.toml` does exist.
[
  [ "Effect.ReadFS.DoesFileExist", "/home/me/test/Cargo.toml" ],
  true
]

# This indicates that FOSSA CLI ran the command `cargo generate-lockfile`
# successfully, and that it did not provide any output.
[
  [ "Effect.Exec.Exec", "/home/me/test/", { "cmdName": "cargo", "cmdArgs": "generate-lockfile" } ],
  { "Right": "" }
]

# This indicates that FOSSA CLI ran the command `cargo metadata`
# successfully, and that it did provide the given output.
# However the actual output is extremely verbose and not included in this example.
[
  [ "Effect.Exec.Exec", "/home/me/test/", { "cmdName": "cargo", "cmdArgs": "metadata" } ],
  { "Right": "<omitted output for example>" }
]
```

##### `bundleScope`

`bundleScope` records FOSSA CLI internal operations.

This is the most complicated part of the debug bundle,
and is _generally_ something only useful for FOSSA employees,
since understanding this requires a pretty good understanding
of the internals of FOSSA CLI.

However, in general, the shape of this section is as follows:
```
{
  "duration": "2.613411",   # The number of seconds the operation took to complete
  "scope": "analyze-cargo", # (Optional) The name of the operation.
  "events": [],             # A nested set of objects.
}
```

The objects under `events` can be in many forms,
and different types of objects are interspersed in this array.

Some examples:
- Plain text lines, representing log output, may appear here.
- Objects similar to those in `bundleJournals` may appear here.
- Additional `bundleScope` objects may be arbitrarily nested here.

The most effective way for someone not familar with FOSSA CLI internals
to interpret this section is to skim the entries looking for text
(which represents log lines) and use that to zero in on what FOSSA CLI
is doing, using that to zero in on more critical parts of the
output. Given the structure of this section it is very information dense.
