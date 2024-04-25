## `fossa release-group create-release`

`fossa release-group create-release` allows you to create a FOSSA release group release.

## Options

Argument              | Required | Description
----------------------|----------|--------------------------------------------------------------------------------------------------------------------------------------
`--config` / `-c`     | No       | The to your path to your `.fossa.yml`.
`--title` / `-t `     | Yes      | The name of your FOSSA release group.
`--release` / `-r `   | Yes      | The name of your release within your FOSSA release group.
`--project-locator`   | Yes      | The FOSSA project locator defines a unique ID that the FOSSA API will use to reference this project. The project locator can be found in the UI on the project `Settings` page listed as the `Project Locator` underneath the `Project Title` setting. Specify multiple options by providing this argument multiple times.
`--project-revision`  | Yes      | The revision associated with your FOSSA project. Project revisions can be found in the UI on the project `Activity` page. Refer to `Revision ID` to retrieve the specific revision you want to use for the project. Specify multiple options by providing this argument multiple times.
`--project-branch`    | Yes      | The name of the FOSSA project branch. Specify multiple options by providing this argument multiple times.

> NOTE: The arguments listed as `Required` need to be provided through CLI options OR through your `.fossa.yml` configuration.

## .fossa.yml Configuration

Refer to [fossa configuration](../../files/fossa-yml.md) to set up your `.fossa.yml`.

> NOTE: CLI options take precedence over the configurations in `.fossa.yml`.

> NOTE: Project ID values will not work if provided to `releaseGroupProjects.projectLocator`. For more details on the differences between project ID and project locator refer to [documentation](../../files/fossa-yml.md#what-is-the-difference-between-project-id-and-project-locator).

## Example

```bash
fossa release-group create-release --title example-release-group --release example-release --project-locator custom+1/example --project-revision 1234 --project-branch main --project-locator custom+1/example2 --project-revision 5678 --project-branch main 
``` 
>NOTE: `--project-locator` , `--project-revision`, and `--project-branch` must all be specified when providing a releaseGroupProject. Multiple occurrences of these arguements are accepted and are grouped together based on their order. For example, `fossa release-group create-release --title example-release-group --release example-release --project-locator custom+1/example --project-revision 1234 --project-branch main --project-locator custom+1/example2 --project-revision 5678 --project-branch main` has project groupings of (project locator: custom+1/example, project revision: 1234, project branch: main) and (project locator: custom+1/example2, project revision: 5678, project branch: main).

Similarly, you can you achieve the same result by running the following command with the given `.fossa.yml` configuration:

```yaml
releaseGroup:
  title: example-release-group
  release: example-release
  releaseGroupProjects:
    - projectLocator: custom+1/example
      projectRevision: 1234
      projectBranch: main 
    - projectLocator: custom+1/example2
      projectRevision: 5678
      projectBranch: main
```

```bash
fossa release-group create-release -c path/to/config
``` 
