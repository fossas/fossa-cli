## `fossa release-group create`

This subcommand allows you to create a FOSSA release group.

## Options

Argument                  | Required | Description
--------------------------|----------|--------------------------------------------------------------------------------------------------------------------------------------
`--title` / `-t`          | Yes      | The name of the FOSSA release group.
`--release` / `-r`        | Yes      | The name of the release within your FOSSA release group.
`--project-id`            | Yes      | The id of your FOSSA project. The project ID can be found in the UI on the project settings page listed as the "Project Locator" underneath the "Project Title" setting. The full `Project Locator` value must be provided. For example, if the `Project Locator` value of `custom+1/foo` is provided in the FOSSA UI, use `custom+1/foo`. Specify multiple options by providing this argument multiple times.
`--project-revision`      | Yes      | The revision associated with your FOSSA project. Project revisions can be found in the UI on the project activity page. Refer to `Revision ID` to retrieve the specific revision you want to use for the project. Specify multiple options by providing this argument multiple times.
`--project-branch`        | No       | The name of the FOSSA project branch. Specify multiple options by providing this argument multiple times.
`--license-policy` / `l`  | No       | The name of the license policy to assign to the FOSSA release group. 
`--security-policy` / `s` | No       | The name of the security policy to assign to the FOSSA release group. 
`--team` / `T`            | No       | The name of team that will be associated with the FOSSA release group. Specify multiple options by providing this argument multiple times.

> NOTE: The arguments listed as `Required` need to be provided through CLI options OR through your `.fossa.yml` configuration.

## .fossa.yml Configuration

Refer to [fossa configuration](../../files/fossa-yml.md) to set up your `.fossa.yml`.

> NOTE: CLI options take precedence over the configurations in `.fossa.yml`.

## Example

```bash
fossa release-group create --title example-release-group --release example-release --project-id custom+1/example --project-revision 1234 --project-branch main --license-policy example-license-policy --security-policy example-security-policy --team ExampleTeam --team ExampleTeam2
``` 

Similarly, you can you achieve the same result by running the following command with the given `.fossa.yml` configuration:

```yaml
releaseGroup:
  title: example-release-group
  release: example-release
  releaseGroupProjects:
    - projectId: custom+1/example
    - projectRevision: 1234
    - projectBranch: main 
  licensePolicy: example-license-policy
  securityPolicy: example-security-policy
  teams:
    - ExampleTeam
    - ExampleTeam2
```

```bash
fossa release-group create -c path/to/config
``` 
