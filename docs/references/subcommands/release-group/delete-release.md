## `fossa release-group delete-release`

`fossa release-group delete-release` allows you to delete a FOSSA release group release.

## Options

Argument              | Required | Description
----------------------|----------|--------------------------------------------------------------------------------------------------------------------------------------
`--title`   / `-t `   | Yes      | The name of your FOSSA release group.
`--release` / `-r`    | Yes      | The name of the release within your FOSSA release group.

> NOTE: `fossa release-group delete-release` does not use `.fossa.yml` configurations.

## Usage

```bash
fossa release-group delete-release --title example-title -release example-release 
```