## `fossa init`

The `fossa init` command creates _examples_ of the following files if they do not exist in the current working directory:

- [.fossa.yml](./../files/fossa-yml.md): `fossa-cli` Configuration file
- [fossa-deps.yml](./../files/fossa-deps.md): File for stubbing and manually providing dependencies

Note that both the `.fossa.yml` and `fossa-deps.yml` files are optional.
They allow for more specificity on how fossa-cli performs dependency analysis, but they are not required for `fossa-cli` usage.
