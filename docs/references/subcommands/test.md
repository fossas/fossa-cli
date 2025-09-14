
## `fossa test`

The test command checks whether the most-recent scan of your FOSSA project raised license-policy or vulnerability issues. This command is usually run immediately after `fossa analyze`

- If there are issues, it prints them to stderr and fails with an exit code of 1
- If there are no issues, it prints nothing and succeeds with an exit code of 0

`fossa test` supports the [Common FOSSA Project Flags](./analyze.md#common-fossa-project-flags) supported by all commands

### Specifying a timeout

By default, `fossa test` waits a maximum of 3600 seconds (1 hour) for issue scan results. To override the default timeout, use, e.g.:

```sh
fossa test --timeout 60
```

Where `60` is the maximum number of seconds to wait for issue scan results.

### Print issues as JSON

By default, `fossa test` displays issues in a human-readable format. To instead print issues as JSON, use:

```sh
fossa test --format json
```

### Test for new issues compared to another revision

`--diff <REVISION>` configures FOSSA to only report new issues observed with the current revision that weren't already reported on the specified `<REVISION>`.

```sh
fossa test --diff revisionToCompare
```

#### Example

```sh
fossa test --revision 34021e --diff v2.0.0
```

This only reports issues that are present in `34021e` revision,
BUT are not present in revision `v2.0.0`.

For instance, 

* If the revision `v2.0.0` has issue: `A`, and the revision `34021e` has issue `A`, 
  * `fossa-cli` reports no new issues discovered and exits with status code of 0.
  

* If the revision `v2.0.0` has issue: `A`, and the revision `34021e` has 0 issues, 
  * `fossa-cli` reports no new issues discovered and exits with status code of 0.


* If the revision `v2.0.0` has issue: `A`, and the revision `34021e` has issues `A`, `B`, 
  * `fossa-cli` reports issue `B` and exits with status code of 1.

### Confidence scoring

`fossa test` can suppress low-confidence findings to reduce CI noise.

Enable with the default threshold (70):

```sh
fossa test --confidence
```

Set a custom threshold:

```sh
fossa test --confidence --confidence-threshold 80
```

Findings are scored 0–100; only findings at or above the threshold are reported.