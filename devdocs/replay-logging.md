# Replay Logging

[Back to development doc homepage](index.md)

Replay Logging is a feature of `fossa analyze` that can aid in reproducing/resolving analysis issues.

It leverages some unique properties of haskell that allow us to "record" an end-to-end analysis execution in a customer's environment, and "replay" the analysis in our local development environment, with all of the tools normally available to us as developers (allowing us to print-debug, make other code changes, etc).

## Recording

The `--debug` flag is used to run `fossa analyze` in record mode. The analysis will run normally (if a bit slower), and create a file called `fossa.debug.json`.

In addition to some basic metadata, `fossa.debug.json` contains the arguments and results of each method invocation from our `ReadFS` and `Exec` effects within the `journals` key:

```json
{
  "system": {...},
  "workdir": "/foo",
  "args": ["analyze", "--record"],
  "journals": {
    "ReadFS": [...]
    "Exec": [...]
  }
}
```

## Replaying

**Replay mode is temporarily unavailable.**

The `--replay <file>` flag is used to run `fossa analyze` in replay mode. Rather than reading from real files with our `ReadFS` effect or running real commands with our `Exec` effect, we stub in the recorded effect calls from `fossa.debug.json`.

For replay mode to work, we need to "run analysis" in the same directory (or a subdirectory) of the one used when recording `fossa.debug.json`. In the example above, this directory is `/foo`: `fossa analyze --replay fossa.debug.json /foo`. It's recommended to also use the same CLI commit and arguments (minus `--record`, of course).

Replay mode has some substantial benefits over normal debug logs:

- We can reproduce analysis issues as they occurred in a customer's environment, without the files and buildtools necessary for analysis.
- We can make code changes and see how they'd affect the analysis.
- For certain classes of issues, we can make code changes to resolve them, and confidently deliver bug fixes as a result.

There are some caveats:

- Any code changes that would require reading new files or running new/modified commands will not work under replay mode.
- `--unpack-archives` doesn't work with replay logging.
