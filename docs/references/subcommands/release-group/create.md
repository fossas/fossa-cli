## `fossa release-group create`

`fossa release-group create` allows you to create a FOSSA release group.

## Options

Argument                  | Required | Description
--------------------------|----------|--------------------------------------------------------------------------------------------------------------------------------------
`--config` / `-c`         | No       | The to your path to your `.fossa.yml`.
`--title` / `-t`          | Yes      | The name of the FOSSA release group.
`--release` / `-r`        | Yes      | The name of the release within your FOSSA release group.
`--project-locator`       | Yes      | The FOSSA project locator defines a unique ID that the FOSSA API will use to reference this project. The project locator can be found in the UI on the project `Settings` page listed as the `Project Locator` underneath the `Project Title` setting. Specify multiple options by providing this argument multiple times.
`--project-revision`      | Yes      | The revision associated with your FOSSA project. Project revisions can be found in the UI on the project `Activity` page. Refer to `Revision ID` to retrieve the specific revision you want to use for the project. Specify multiple options by providing this argument multiple times.
`--project-branch`        | Yes      | The name of the FOSSA project branch. Specify multiple options by providing this argument multiple times.
`--license-policy` / `l`  | No       | The name of the license policy to assign to the FOSSA release group. 
`--security-policy` / `s` | No       | The name of the security policy to assign to the FOSSA release group. 
`--quality-policy` / `q`  | No       | The name of the quality policy to assign to the FOSSA release group.
`--team` / `T`            | No       | The name of team that will be associated with the FOSSA release group. Specify multiple options by providing this argument multiple times.

> NOTE: The arguments listed as `Required` need to be provided through CLI options OR through your `.fossa.yml` configuration.

## .fossa.yml Configuration

Refer to [fossa configuration](../../files/fossa-yml.md) to set up your `.fossa.yml`.

> NOTE: CLI options take precedence over the configurations in `.fossa.yml`.

## Example

```bash
fossa release-group create --title example-release-group --release example-release --project-locator custom+1/example --project-revision 1234 --project-branch main --license-policy example-license-policy --security-policy example-security-policy --quality-policy example-quality-policy --team ExampleTeam --team ExampleTeam2
``` 

Similarly, you can you achieve the same result by running the following command with the given `.fossa.yml` configuration:

```yaml
releaseGroup:
  title: example-release-group
  release: example-release
  releaseGroupProjects:
    - projectLocator: custom+1/example
    - projectRevision: 1234
    - projectBranch: main 
  licensePolicy: example-license-policy
  securityPolicy: example-security-policy
  qualityPolicy: example-quality-policy
  teams:
    - ExampleTeam
    - ExampleTeam2
```

```bash
fossa release-group create -c path/to/config
``` 
