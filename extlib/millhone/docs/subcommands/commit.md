# `commit`

This subcommand commits the analysis performed in the `analyze` subcommand into a `fossa-deps` file.
For more information on possible options, run `millhone commit --help`.

# Input

The primary thing this subcommand requires is the path to the directory in which the output of `analyze`
was written. Users can also alter which kinds of matches to commit, and customize the output format
of the created `fossa-deps` file.

# Output

The result of this subcommand is a `fossa-deps` file written to the root of the project directory.

> [!NOTE]
> This subcommand will not overwrite an existing `fossa-deps` file by default,
> and currently does not merge its output into an existing `fossa-deps` file.
>
> However, users can customize the output format (via `--format`) and then
> perform scripted merges themselves.

# Next Steps

After running `millhone commit`, the next step is to run `fossa analyze` on the project.

FOSSA CLI will then pick up the dependencies reported in that `fossa-deps` file and report them
as dependencies of the project.
