## `fossa release-group`

This `fossa release-group` subcommand allows users to interact with FOSSA release groups.

It has the following subcommands:

- [`fossa release-group add-projects`](./release-group/add-projects.md)
- [`fossa release-group create`](./release-group/create.md)
- [`fossa release-group create-release`](./release-group/create-release.md)
- [`fossa release-group delete`](./release-group/delete.md)
- [`fossa release-group delete-release`](./release-group/delete-release.md)

See the pages linked above for more details.

>NOTE: For all subcommands that allow adding projects to releases, the combination of `--project-locator` , `--project-revision`, and `--project-branch` are required to identify the project that will be added to the release group. Multiple occurrences of these arguements are accepted and are grouped together based on their order. For example, `fossa release-group [create, create-release, add-projects] --title example-release-group --release example-release --project-locator custom+1/example --project-revision 1234 --project-branch main --project-locator custom+1/example2 --project-revision 5678 --project-branch main ` has project groupings of (project locator: custom+1/example, project revision: 1234, project branch: main) and (project locator: custom+1/example2, project revision: 5678, project branch: main).

### `fossa release-group add-projects`

Add FOSSA projects to a FOSSA release group

Example:

```bash
fossa release-group add-projects --title example-release-group --release example-release --project-locator custom+1/example --project-revision 1234 --project-branch main --project-locator custom+1/example2 --project-revision 5678 --project-branch main 
```

### `fossa release-group create`

Create a FOSSA release group

Example:

```bash
fossa release-group create --title example-release-group --release example-release --project-locator custom+1/example --project-revision 1234 --project-branch main --project-locator custom+1/example2 --project-revision 5678 --project-branch main --license-policy example-license-policy --security-policy example-security-policy --quality-policy example-quality-policy --team ExampleTeam --team ExampleTeam2
```

### `fossa release-group create-release`

Create a release within a FOSSA release group

Example:

```bash
fossa release-group create-release --title example-release-group --release example-release --project-locator custom+1/example --project-revision 1234 --project-branch main --project-locator custom+1/example2 --project-revision 5678 --project-branch main 
```

### `fossa release-group delete`

Delete a FOSSA release group

Example:

```bash
fossa release-group delete --title example-title
```

### `fossa release-group delete-release`

Delete a release within a FOSSA release group

Example:

```bash
fossa release-group delete-release --title example-title --release example-release-title 
```

### F.A.Q.

1. Why are `.fossa.yml` configurations disabled for `fossa release-group delete` and `fossa release-group delete-release`?

This is done intentionally so that lingering configurations in `.fossa.yml` are not extracted and used to mistakenly delete release groups or release group releases.

2. Where can I find my release groups? 

[Release groups](https://app.fossa.com/release-groups) can be found in the UI on the `Release Groups` page.

<img src="../../assets//release-group-title-example.png">

3. Where can I find my release group releases?

Releases can be found in the UI on the `Releases` page for your given release group.

<img src="../../assets//release-example.png">

4. Where can I find my project locator?

The project locator defines a unique ID that the FOSSA API will use to reference a project. The project locator can be found in the UI on the project `Settings` page listed as the `Project Locator` underneath the `Project Title` setting.

<img src="../../assets//project-locator-example.png">

5. Where can I find my project revision ID?

Project revisions can be found in the UI on the project `Activity` page. Refer to `Revision ID` to retrieve the specific revision you want to use for the project.

<img src="../../assets//project-revision-example.png">