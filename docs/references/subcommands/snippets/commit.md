## `fossa snippets commit`

This subcommand commits the analysis performed in the `analyze` subcommand into a `fossa-deps` file.
For more information on possible options, run `millhone commit --help`.

## Options

Argument                 | Required | Default                | Description
-------------------------|----------|------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------
`--analyze-output`       | Yes      | None                   | The directory to which `fossa snippets analyze` output its matches.
`--debug`                | No       | No                     | Enables debug mode. Note that debug bundles are not currently exported with `fossa snippets`, but this output is similarly useful.
`--overwrite-fossa-deps` | No       | No                     | If specified, overwrites the `fossa-deps` file if present.
`--target`               | No       | `function`             | If specified, commits matches consisting of only the specified targets. Specify multiple options by providing this argument multiple times.
`--kind`                 | No       | `full, snippet, body`  | If specified, commits matches consisting of only the specified kinds. Specify multiple options by providing this argument multiple times.
`--transform`            | No       | `space, comment, code` | If specified, commits matches consisting of only the specified transforms. Specify multiple options by providing this argument multiple times.

> [!NOTE]
> `--transform` corresponds to the `Normalized` methods [listed here](../snippets.md#how-does-fossa-snippet-scanning-work).
> The `Raw` method is always enabled and cannot be disabled.

## Input

The primary thing this subcommand requires is the path to the directory in which the output of `analyze`
was written. Users can also alter which kinds of matches to commit, and customize the output format
of the created `fossa-deps` file.

## Output

The result of this subcommand is a `fossa-deps` file written to the root of the project directory.

> [!NOTE]
> This subcommand will not overwrite an existing `fossa-deps` file by default,
> and currently does not merge its output into an existing `fossa-deps` file.
>
> However, users can customize the output format (via `--format`) and then
> perform scripted merges themselves.

## Next Steps

After running `fossa snippets commit`, the next step is to run `fossa analyze` on the project.

FOSSA CLI will then pick up the dependencies reported in that `fossa-deps` file and report them
as dependencies of the project.
