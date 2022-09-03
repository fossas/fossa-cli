# `.fossa.yml`

The fossa configuration file, `.fossa.yml`, is an optional file located at the root of a project that can be used to configure project settings.

The following example is a configuration file with all available fields filled displayed. All fields except for `version` are optional, configuration file versions 1 and 2 were used for CLI versions prior to 2.0.0.


```yaml
version: 3

server: https://app.fossa.com
apiKey: a1b2c3

project:
  id: github.com/fossas/fossa-cli
  name: fossa-cli
  team: cli-team
  policy: custom-cli-policy
  link: fossa.com
  url: github.com/fossas/fossa-cli
  jiraProjectKey: jira-key
  releaseGroup:
    name: release-group-name
    release: 123-release-candidate

revision:
  commit: "12345"
  branch: master

vendoredDependencies:
  forceRescans: false
  scanMethod: CLILicenseScan

targets:
  only:
    - type: maven
      path: foo/bar
  exclude:
    - type: bundler

paths:
  only:
    - ./vendor/django
    - ./test
  exclude:
    - ./vendor/django/test

telemetry:
  scope: full
```


## Fields

### `version:`
Specifies the version of configuration file. Versions 1 and 2 were used by CLI versions up until CLI 2.0.0 and are no longer supported. Version 3 is the current supported version for FOSSA CLI v2.

### `server:`
Sets the endpoint that the CLI will send requests to. This field should only be modified if your FOSSA account lives on a different server than app.fossa.com. This is most commonly needed with on-premise instances of FOSSA.

Default: `https://app.fossa.com`

### `apiKey:`
Sets the [FOSSA API key](https://docs.fossa.com/docs/api-reference#api-tokens) that is required for accessing the FOSSA API and uploading data (e.g. `fossa analyze`) or retrieving information (e.g. `fossa test`) about a project.

> Note: FOSSA strongly recommends setting the API key with the `$FOSSA_API_KEY` environment variable and NOT in the configuration file for security purposes.

### `telemetry:`
Sets the telemetry configurations.

#### `telemetry.scope:`
Sets the telemetry scope to value. Accepted values are 'full' or 'off'.

- When 'full' is provided - CLI will emit telemetry data to server.
- When 'off' is provided - CLI will not emit telemetry data to server.

```yaml
# Example .fossa.yml
# Does not emit telemetry to server.

version: 3
telemetry:
  scope: off
```

### `project:`
The project fields allow you to configure settings for the project you are interacting with through the FOSSA API.

> Note: `name`, `team`, `policy`, `link`, and `jiraProjectKey` can only be set when creating a project (running `fossa analyze` for the first time).  Otherwise, they will be silently ignored (we would like to make this a visible warning in the future).

#### `project.id:`
The project ID defines a unique ID that the FOSSA API will use to reference this project. The project ID can be found in the UI on the project settings page listed as the "Project Locator" underneath the "Project Title" setting. For example, if the "Project Locator" value of `custom+1/foo` is provided in the FOSSA UI, use `foo` for the `project.id`.

Default:
- Git: The CLI will look for a `.git/config` file and set the ID to the project's remote "origin" url.
- SVN: The CLI will run `svn info` and use the "Repository Root".
- No VCS (Version control system): The ID will be set to the name of the project's directory.

> Note: A project's ID cannot be modified after a project is created. If you change the ID, you will be interacting with a different project. If the new ID does not exist, a new project will be created for it.

#### `project.name:`
The name field sets the projects visible name in the FOSSA dashboard. By default, this will be set to the project's ID.

#### `project.team:`
The name of the team in your FOSSA organization to associate this project with.

#### `project.policy:`
The name of the policy in your FOSSA organization to associate this project with.

#### `project.link:`
An external link that will appear in the FOSSA UI for this specific project.

#### `project.url:`
The URL of your project that will appear in FOSSA. This URL is intended to be the URL to the repository of this project.

#### `project.jiraProjectKey:`
The Jira Project Key to associate with your project for improved issue triage. Refer to the [FOSSA docs](https://docs.fossa.com/docs/atlassian-jira#linking-fossa-projects-to-jira-projects) for more information.

#### `project.releaseGroup:`
The `name:` and `release:` of the release group's release to add your project to in the FOSSA dashboard.

If you choose to associate a project with a release group, you **must** supply both name and release.

### `revision:`
The revision fields are used to help FOSSA differentiate between one upload for a project and another, just as GitHub uses commit hashes and branch names.

#### `revision.commit:`
The commit is used to identify a specific scan for a project (determined by project.id). This is intended to be used identically to how Git treats commit hashes.

Default:
- Git: the CLI will parse the current HEAD state in the `.git` directory and use the commit hash of the HEAD branch
- SVN: The CLI will run `svn info` and use the "Revision".
- No VCS: The commit will be set to the unix timestamp.

#### `revision.branch:`
 project branch is an optional setting used for organizing project revisions in the FOSSA UI. The branch field is intended to function similar to how Git defines a branch.

Default:
- Git: the CLI will attempt to find the project's current branch from the `.git/config` file.
- SVN: The CLI will run `svn info` and compare the "URL" and "Repository Root" fields in an attempt to determine a branch.
- No VCS: The CLI will leave the branch field empty.

### `vendoredDependencies:`

The `vendoredDependencies` section is an optional section that configures how we scan vendored dependencies. Please see the [documentation for vendored dependencies](https://github.com/fossas/fossa-cli/blob/master/docs/features/vendored-dependencies.md) for more information on what they are and how to set up a scan for them.

#### `vendoredDependencies.forceRescans`

`forceRescans` is an optional setting that, if true, forces a re-scan of all vendored dependencies on every run. If false or not present, then we do not re-scan vendored dependencies that have been previously scanned. A vendored dependency has been previously scanned if a dependency with the same name and version has already been scanned by your organization. If no version is provided, then any change in the files being scanned will result in a rescan.

#### `vendoredDependencies.scanMethod`

The `scanMethod` setting determines whether your vendored dependencies are scanned using the "Archive Upload" or the "CLI License Scan" method.

The possible values are `ArchiveUpload` or `CLILicenseScan`.

If this setting is not present, then we will use the default for your organization. This is most likely "CLI License Scan", but it is possible that your organization has changed the default to "Archive Upload".

For a description of what these methods are and the difference between them, see the [documentation for vendored dependencies](https://github.com/fossas/fossa-cli/blob/master/docs/features/vendored-dependencies.md).

### `targets:`
The targets filtering section allows you to specify the exact targets which be should be scanned.

Targets are listed in the following formats for both `only` and `exclude` lists.
```yaml
    - type: maven
      path: foo/bar
    - type: pipenv (all pipenv type targets at any path)
```

#### `targets.only:`
The list of `only` targets that should be scanned. When used alongside `paths.only`, the intersection of the two lists is taken to find targets for scanning

#### `targets.exclude:`
The list of `exclude` targets which should be excluded from scanning. The targets listed in the exclude section will override the targets listed in the only sections. This feature is used most effectively to remove specific targets from a directory.

Example: You have a directory called `docker` which contains 3 different targets but you would like to omit the one with type `bundler`. You can do this with the following configuration:

```yaml
targets:
  exclude:
    - type: bundler
      path: prod/docker
```

### `paths:`
The paths filtering section allows you to specify which paths should be scanned and which should not. The paths should be listed as their location from the root of your project.

```yaml
paths:
  only:
    - prod
  exclude:
    - prod/vendor
```

#### `paths.only:`
The list of paths to only allow scanning within.

This section is most commonly used when you would like to restrict scanning to a certain list of directories from the root of your project. If you have a directory structure such as the following and would only like to scan targets located in the `production` directory, `paths.only` enables this:
```
/production
/development
/test
```

#### `paths.exclude:`
The list of paths to exclude from scanning in your directory.

This section is intended to be used as the inverse to `paths.only`. If you have a certain directory such as `development` you wish to exclude, `paths.exclude` enables you to do this.

### Analysis target configuration
Analysis target configuration allows you to select a very specific subset of your directory for scanning. The `targets` and `paths` sections allow users to configure which targets and directories should be scanned. This is useful if you have a custom test directory or development projects within the root project.

Analysis target configuration determines which targets should be scanned with the following logic:
1. Targets that match the `targets.only` and `paths.only` sections are unioned to create a list of targets to be scanned.
2. Targets remaining after the `only` step that match the `targets.exclude` and `paths.exclude` sections are removed from the list of targets to be scanned.
    -  If no targets were listed in the `only` sections, `exclude` will remove targets from the list of all available targets.
3. Analysis is run on the remaining targets.

For detailed walkthrough, and example please refer to [analysis target configuration walkthrough](../../walkthroughs/analysis-target-configuration.md)

#### Project target configuration example

Run the command `fossa list-targets` to determine the analysis targets present in your project. The output will look similar to the following with the targets in format `type@path` (You may see that duplicated lines for "Found target" and "Found project"):

```
Found target: bundler@prod/docker
Found target: bundler@prod
Found target: yarn@prod
Found target: yarn@prod/docker
Found target: yarn@prod/vendor
Found target: pipenv@prod
Found target: pipenv@prod/vendor
Found target: pipenv@dev
```

From here, we identify the parts of this example project that we want to scan:
- We ONLY want targets located in the `prod` directory. This would also ensure that the the `dev` directory is never scanned. (`paths.only: prod`)
  - Except for the vendored dependencies. (`paths.exclude: prod/vendor`)
- We want the targets inside `prod/docker`, except for `bundler@prod/docker` which is used to build the image.
    ```yaml
    exclude:
      - type: bundler
        path: prod/docker
    ```

Combining these individual filters together results in the following configuration:

```yaml
targets:
  exclude:
    - type: bundler
      path: prod/docker

paths:
  only:
    - prod
  exclude:
    - prod/vendor
```

The above configuration and list of targets will result in the following targets being scanned:
```
Found target: bundler@prod
Found target: yarn@prod
Found target: yarn@prod/docker
Found target: pipenv@prod
```

## FAQ

### Why are some configuration settings (name, team, policy, etc.) ignored by the FOSSA API after a project has already been created?

The purpose of allowing a user to set `policy`, `team`, and other settings in the configuration file is to make it easy for users to share configuration files within their teams when creating many different projects. If these configuration settings were allowed to modify a project every time they were set on the CLI they could disrupt anyone managing the project in the FOSSA UI. Example: I change a project from Team A to Team B in the FOSSA UI. The project is then scanned nightly in a CI environment and my UI team change is reverted. This behavior would be very difficult for someone managing the project only in the FOSSA UI to diagnose and fix.
