## `fossa release-group delete`

`fossa release-group delete` allows you to delete a FOSSA release group.

## Options

Argument              | Required | Description
----------------------|----------|--------------------------------------------------------------------------------------------------------------------------------------
`--title` / `-t `     | Yes      | The name of your FOSSA release group.

> NOTE: `fossa release-group delete` does not use `.fossa.yml` configurations.

## Usage

```bash
fossa release-group delete --title example-title
```