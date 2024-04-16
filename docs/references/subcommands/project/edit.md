## `fossa project edit`

`fossa project edit` allows you to edit a FOSSA project's settings and configuration.

## Options

Argument                     | Required | Description
-----------------------------|----------|--------------------------------------------------------------------------------------------------------------------------------------
`--project-locator`          | Yes      | The project Locator defines a unique ID that the FOSSA API will use to reference this project within FOSSA. The project locator can be found in the UI on the project `Settings` page listed as the "Project Locator" underneath the "Project Title" setting.
`--project-id`               | Yes      | The project ID defines an ID that is used to reference a project within your FOSSA organization. The project ID is a specific portion of the project locator and can be found in the UI on the project `Settings` page listed as the "Project Locator" underneath the "Project Title" setting. For example, if the "Project Locator" value of `custom+1/foo` is provided in the FOSSA UI, use `foo`. Project ID defaults to the .git/config file or project's remote "origin" URL (Git), "Repository Root" obtained using 'svn info' (SVN), or the name of the project's directory (No VCS), if project ID wasn't explicityly set during project creation.
`--title` / `-t`             | No       | The title of the FOSSA project.
`--project-url`              | No       | The url of the project's repository.
`--jira-project-key` / `-j`  | No       | The JIRA project key to associate to the FOSSA project.
`--link` / `-L`              | No       | A link to attach to the FOSSA project. 
`--team` / `T`               | No       | The name of the team that will be associated with the FOSSA project. Specify multiple options by providing this argument multiple times.
`--project-label`            | No       | The labels associated with the FOSSA project. Assign up to 5 labels for a project. Specify multiple options by providing this argument multiple times.

> NOTE: The arguments listed as `Required` need to be provided through CLI options OR through your `.fossa.yml` configuration.

> NOTE: Either project ID OR project locator needs to be set. Project ID takes precedence over project locator. For more details on the differences between project ID and project locator refer to [documentation](../../files/fossa-yml.md#what-is-the-difference-between-project-id-and-project-locator).

>NOTE: When updating project labels through `fossa project edit`, the transaction is all or nothing. This means that the project labels specified through this command will overwrite the existing labels that are associated with the project. Be sure to include all the labels that you want to be associated with the project, even if some labels are already currently set. 

## .fossa.yml Configuration

All of the previously mentioned CLI options can be provided through a `.fossa.yml`. Refer to [fossa configuration](../../files/fossa-yml.md) to set up your `.fossa.yml`.

> NOTE: CLI options take precedence over the configurations in `.fossa.yml`.

## Example

### Project locator example
Given a project with project locator: `custom+1/example`, the following command:

- Sets the project's title to `example-title`
- Set the the project's url to `github.com/fossas/fossa-cli`
- Sets the project's JIRA key to `example-jira-key`
- Attaches link: `fossa.com` to the project
- Adds the project to teams: `example-team-1`, `example-team-2`
- Attaches policy: `example-policy` to the project
- Attaches labels: `example-label-1` , `example-label-2` to the project

```bash
fossa project edit --project-locator custom+1/example --title example-title --project-url github.com/fossas/fossa-cli --jira-project-key example-jira-key --link fossa.com --team example-team --team example-team-2 --policy example-policy --project-label example-label-1 --project-label example-label-2 
``` 

Similarly, you can you achieve the same result by running the following command with the given `.fossa.yml` configuration: 

```bash
fossa project edit --config /path/to/config
``` 

```yaml
project:
  locator: custom+1/example
  name: example-title
  teams: 
    - example-team-1
    - example-team-2
  policy: example-policy
  link: fossa.com
  url: github.com/fossas/fossa-cli
  jiraProjectKey: example-jira-key
  labels:
    - example-label-1
    - example-label-2
```

### Project ID example
Achieve the same result as defined above (for projects created through the CLI) using project ID:

```bash
fossa project edit --project-id example --title example-title --project-url github.com/fossas/fossa-cli --jira-project-key example-jira-key --link fossa.com --team example-team --team example-team-2 --policy example-policy --project-label example-label-1 --project-label example-label-2 
``` 

Similarly, you can you achieve the same result by running the following command with the given `.fossa.yml` configuration: 

```bash
fossa project edit --config /path/to/config
``` 

```yaml
project:
  id: example
  name: example-title
  teams: 
    - example-team-1
    - example-team-2
  policy: example-policy
  link: fossa.com
  url: github.com/fossas/fossa-cli
  jiraProjectKey: example-jira-key
  labels:
    - example-label-1
    - example-label-2
```
