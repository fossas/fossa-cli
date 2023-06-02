### `fossa analyze`: Why wasn't my project found?

If your project wasn't found, make sure you meet the requirements in the
[relevant language/build-tool's quick reference](../references/strategies/README.md)

If your project meets the requirements, it's very likely `fossa analyze` found your project,
but dependency analysis failed. To show the failure reason, use the `--debug` flag:

```sh
fossa analyze --debug
```

Note that the output is likely to be very noisy: our project discovery process is very lenient,
and can produce many false-positives. 
False-positive projects almost always fail during the dependency analysis step,
so we don't show analysis failures by default.

If your project wasn't in the `--debug` output, or you believe you've encountered a bug,
please [create a support ticket](https://support.fossa.com).

In your bug report, please include:

- relevant package manifest files (e.g., `pom.xml` or `package.json`)
- the output of `fossa analyze --debug`

### When are you adding support for (some buildtool/language)?

If we don't support your choice of language/buildtool,
please [create a support ticket](https://support.fossa.com) to express interest!

### Where can I go for more in-depth debugging?

The [debugging FOSSA CLI](../references/debugging/README.md) reference may be useful.
Alternately, [create a support ticket](https://support.fossa.com)!
