## `fossa project`

This `fossa project` subcommand allows users to interact with FOSSA projects.

It has the following subcommand:

- [`fossa project edit`](./project/edit.md)

See the pages linked above for more details.

### `fossa project edit`

Edits a FOSSA project's settings and configurations. 

Example:

```bash
fossa project edit --project-locator custom+1/example --title example-title --project-url github.com/fossas/fossa-cli --jira-project-key example-jira-key --link fossa.com --team ExampleTeam --policy example-policy --project-label example-label-1 --project-label example-label-2 
```

### F.A.Q.

1. Where can I find my project locator? 

The project Locator defines a unique ID that the FOSSA API will use to reference this project within FOSSA. The project locator can be found in the UI on the project `Settings` page listed as the "Project Locator" underneath the "Project Title" setting.

<img src="../../assets/project-locator-example.png">
