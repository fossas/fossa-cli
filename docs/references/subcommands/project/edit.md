## `fossa project edit`

This subcommand allows you to edite a FOSSA project's settings and configuration.

## Options

Argument                     | Required | Description
-----------------------------|----------|--------------------------------------------------------------------------------------------------------------------------------------
`--project-id`               | Yes      | The id of your FOSSA project. The project ID can be found in the UI on the project settings page listed as the "Project Locator" underneath the "Project Title" setting. The full `Project Locator` value must be provided. For example, if the `Project Locator` value of `custom+1/foo` is provided in the FOSSA UI, use `custom+1/foo`.
`--title` / `-t`             | No       | The title of the FOSSA project.
`--project-url`              | No       | The url of the project's repository.
`--jira-project-key` / `-j`  | No       | The JIRA project key to associate to the FOSSA project.
`--link` / `-L`              | No       | A link to attach to the FOSSA project. 
`--team` / `T`               | No       | The name of the team that will be associated with the FOSSA project.
`--project-label`            | No       | The labels associated with the FOSSA project. Assign up to 5 labels for a project. Specify multiple options by providing this argument multiple times.

> NOTE: The arguments listed as `Required` needs to be provided through CLI options OR through your `.fossa.yml` configuration.

## .fossa.yml Configuration

All of the previously mentioned CLI options can be provided through a `.fossa.yml`. Refer to [fossa configuration](../../files/fossa-yml.md) to set up your `.fossa.yml`.

> NOTE: CLI options take precedence over the configurations in `.fossa.yml`.

## Example

Given a project with projectId: `custom+1/example`, the following command:

- Sets the project's title to `example-title`
- Set the the project's url to `github.com/fossas/fossa-cli`
- Sets the project's JIRA key to `example-jira-key`
- Attches link: `fossa.com` to the project
- Adds the project to team: `ExampleTeam`
- Attaches policy: `example-policy` to the project
- Attaches labels: `example-label-1` , `example-label-2` to the project

```bash
fossa project edit --project-id custom+1/example --title example-title --project-url github.com/fossas/fossa-cli --jira-project-key example-jira-key --link fossa.com --team ExampleTeam --policy example-policy --project-label example-label-1 --project-label example-label-2 
``` 

Similarly, you can you achieve the same result by running the following command with the given `.fossa.yml` configuration: 

```bash
fossa project edit --config /path/to/config
``` 

```yaml
project:
  id: custom+1/example
  name: example-title
  team: ExampleTeam
  policy: example-policy
  link: fossa.com
  url: github.com/fossas/fossa-cli
  jiraProjectKey: example-jira-key
  labels:
    - example-label-1
    - example-label-2
```
