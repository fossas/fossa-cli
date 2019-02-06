# `.fossa.yml`

The fossa configuration file can be created manually by running `fossa init` which is recommended.

> Fields prefixed with `*` are optional and determined at runtime from the environment or omitted entirely.
```yaml
version: 1

cli:
* server: https://app.fossa.io
* fetcher: custom
* project: fossa-cli
* api_key: some-key-here
* revision: 1234567
* locator: custom+github.com/fossas/fossa-cli$revision

Unsure if should be included
* title:
* branch:
* project_url:
* jira_project_key:
* link:
* team:

analyze:
  modules:
    - name: fossa-cli
      type: go
      target: github.com/fossas/fossa-cli/cmd/fossa
      path: cmd/fossa
*     ignore: false
*     options:
        <option>: <value>
```
## Fields
### `version:`

Specifies the current fossa configuration file version being used. Version 1 is in production and version 2 is still in development.

### `cli:`
#### `server:` (Optional)
Sets the endpoint that the cli will send requests to. This field should only be modified when running a local on-premise instance of `fossa.com`.

Default: `https://app.fossa.io`

#### `fetcher:` (Optional)
Describes the type of project fossa is uploading, there are two options:
- `custom` - Main fetcher option utilized today. 
- `git` (deprecated) - Signifies a project on Github and attempts to create one source of truth for the specified project fossa.com. No longer used as Github integrations can now be achieved using `custom`.

Default: `custom`

#### `project:` (Optional)
Name of the project being analyzed. This is used to construct part of the project locator used to uniquely identify a project uploaded to fossa.com.

Default: Obtained from version control software (VCS) in the directory.

#### `api_key:` (Optional)
Holds a unique Fossa API Key which is used to determine which organization to associate the upload with. Fossa **strongly advises** against including this field to ensure that personal API keys are not committed to your repository. Fossa recommends running `export FOSSA_API_KEY=<your-api-key>` as an alternative. 

Default: Environment variable `FOSSA_API_KEY` will be used.

#### `revision:` (Optional)
Specifies a projects revision. This is used to construct part of the project locator used to uniquely identify a project uploaded to fossa.com. Revision can be thought of as a version number appended to each upload, used to distinguish the analysis of your present day project from all previous analysis.

Default: Obtains the commit sha from the version control software (VCS) located in the directory.

#### `locator:` (Optional)
Manually specify the project locator that is used to identify the unique project on fossa.com. Fossa does not recommend manually setting the locator.

Default: locator is created using fetcher, api_key to find organization ID, project, revision, and other information from the local VCS software.

### `analyze:`

#### `modules:`
Array of modules that will be analyzed in the order they are listed.

#### `name:`
Name of the module being analyzed. This field has no implication on analysis and is for user organization and debugging purposes only.

#### `type:`
Type of module being analyzed. Supported types can be found in the *Configuration* section of each [supported environment's page](../README.md#Supported-Environmetns) page.

#### `target:`
Build target for the specified module. Target will be used differently depending on which `type` is selected and is most heavily utilized in analysis methods that shell out to a command such as `go list <target>` or `./gradlew target`.

#### `path:`
Path to the root of the project folder or path to the location of the lockfile. Path will be used differently depending on which `type` is selected.

#### `ignore:` (Optional)
If set to `true` this module will be skipped.

Default: `false`

#### `option:` (Optional)
Most options are unique to the type of module being analyzed. Refer to the [supported environments' pages](../README.md#Supported-Environmetns) for documentation on the full list of options available.

Default: No options.
