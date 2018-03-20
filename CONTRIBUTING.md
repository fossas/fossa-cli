# Contributing

## Assign yourself an issue
Head over to our issue tracker and find one labelled ["good first issue"](https://github.com/fossas/fossa-cli/issues?q=is%3Aopen+is%3Aissue+label%3A%22good+first+issue%22) or ["help wanted"](https://github.com/fossas/fossa-cli/issues?q=is%3Aopen+is%3Aissue+label%3A%22help+wanted%22).  Then comment to elect yourself and one of our maintainers will assign you the issue!

## Adding language integrations

See [Adding New Languages](docs/integrations/adding-new-languages.md).

## Running tests

Since `fossa` relies on having the correct build tools in your local environment, running `fossa` tests requires being able to successfully build all projects in `test/fixtures/`. To provide these tools and prevent you from clobbering your local machine, we have run tests in a Docker container defined at `test/Dockerfile`.

## License

Check out our [DCO](DCO) for contribution certifications.

When submitting a Pull Request, make sure you sign our CLA.

This repository is licensed under the MPL-2.0.
