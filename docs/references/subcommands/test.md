
## `fossa test`

The test command checks whether the most-recent scan of your FOSSA project raised license-policy or vulnerability issues. This command is usually run immediately after `fossa analyze`

- If there are issues, it prints them to stdout and fails with an exit code of 1
- If there are no issues, it prints nothing and succeeds with an exit code of 0

`fossa test` supports the [Common FOSSA Project Flags](#common-fossa-project-flags) supported by all commands

### Specifying a timeout

By default, `fossa test` waits a maximum of 3600 seconds (1 hour) for issue scan results. To override the default timeout, use, e.g.:

```sh
fossa test --timeout 60
```

Where `60` is the maximum number of seconds to wait for issue scan results.

### Print issues as JSON

By default, `fossa test` displays issues in a human-readable format. To instead print issues as JSON, use:

```sh
fossa test --json
```

### Test against previous revision

With `--diff <ARG>`, you can check for _only_ new issue comapred to some `<ARG>` revision. 

```sh
fossa test --diff revisionToCompare
```

#### Example

```sh
fossa test --revision 34021e --diff v2.0.0
```

This will only report issues that are present in `34021e` revision,
BUT are not present in revision `v2.0.0`.

For instance, 

* If the revision `v2.0.0` has issue: `A`, and the revision `34021e` has issue `A`, 
  * `fossa-cli` will report no new issues discovered, and will exit with status code of 0.
  

* If the revision `v2.0.0` has issue: `A`, and the revision `34021e` has 0 issues, 
  * `fossa-cli` will report no new issues discovered, and will exit with status code of 0.


* If the revision `v2.0.0` has issue: `A`, and the revision `34021e` has issues `A`, `B`, 
  * `fossa-cli` will report issue `B`, and will exit with status code of 1.