## `fossa release-group create`

This subcommand allows you to create a FOSSA release group.

## Options

Argument                  | Required | Description
--------------------------|----------|--------------------------------------------------------------------------------------------------------------------------------------
`-config` / `-c`          | No       | The to your path to your `.fossa.yml`.
`--title` / `-t`          | Yes      | The name of the FOSSA release group.
`--release` / `-r`        | Yes      | The name of the release within your FOSSA release group.
`--project-id`            | Yes      | The id of your FOSSA project. The project ID can be found in the UI on the project settings page listed as the "Project Locator" underneath the "Project Title" setting. The full `Project Locator` value must be provided. For example, if the `Project Locator` value of `custom+1/foo` is provided in the FOSSA UI, use `custom+1/foo`. Specify multiple options by providing this argument multiple times.
`--project-revision`      | Yes      | The revision associated with your FOSSA project. Project revisions can be found in the UI on the project activity page. Refer to `Revision ID` to retrieve the specific revision you want to use for the project. Specify multiple options by providing this argument multiple times.
`--project-branch`        | No       | The name of the FOSSA project branch. Specify multiple options by providing this argument multiple times.
`--license-policy` / `l`  | No       | The name of the license policy to assign to the FOSSA release group. 
`--security-policy` / `s` | No       | The name of the security policy to assign to the FOSSA release group. 
`--team` / `T`            | No       | The name of team that will be associated with the FOSSA release group. Specify multiple options by providing this argument multiple times.

> NOTE: The arguments that are listed as `Required` can be provided through the CLI options OR through your `.fossa.yml` configuration.

## .fossa.yml Configuration

Refer to [fossa configuration](../../files/fossa-yml.md) to set up your `.fossa.yml`.

> NOTE: CLI options take precedence over the configurations in `.fossa.yml`.

### Examples 

The following command results in release group title: `example-title`. The CLI option takes precdence over the value in `.fossa.yml`.

```bash
fossa release-group add-projects -c path/to/.fossa.yml --title example-title  ...
``` 

```yaml
releaseGroup:
  title: release-group-title-config
```

The following command results in release group title: `release-group-title-config`. There is no CLI option so the config value is used.

```bash
fossa release-group add-projects -c path/to/.fossa.yml  ...
``` 

```yaml
releaseGroup:
  title: release-group-title-config
```