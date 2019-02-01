# `.fossa.yml`

The fossa configuration file should be created by running `fossa init`

(Fields prefixed with `*` are optional and are either determined at runtime from the environment or omitted entirely)
```yaml
version: 1

cli:
* server: https://app.fossa.io
* fetcher: custom
* project: fossa-cli
* api_key: some-key-here
* revision: 234823483
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
Specifies the current fossa configuration file version being used.
### `cli:`
#### `server:` (Optional)
Primary endpoint that the cli sends its requests to. This field only needs to be modified when running a local instance of `fossa.com`.

Default: `https://app.fossa.io`

#### `fetcher:` (Optional)
Describes the type of project fossa is uploading, there are two options:
- `custom` - ?????????????
- `git` (deprecated??) - A unique project on Github

Default: `custom`

#### `project:` (Optional)
This is the name of the project which will be used to construct a project locator which uniquely identifies a project on the fossa projects page. Keeping this name consistent ensures that all future analysis run is correlated to the same project.

Default: Project name will be obtained from the version control software (VCS) in the project.

#### `api_key:` (Optional)
Holds a unique Fossa API Key which is used to send uploads to the correct organization. 
We reccomended not including this field to avoid committing a Fossa API key to a repository. Keeping your API Key as an environment variable is a safer approach.

Default: Environment variable `FOSSA_API_KEY` will be used.

#### `revision:` (Optional)
Manually specify a projects revision.

Default: Revision will be obtained from the version control software (VCS) in the project.

#### `locator:` (Optional)
This allows you to manually specify a project locator

Default: locator is created using fetcher, api_key, project, and revision.

### `analyze:`

#### `modules:`
Array of modules to be analyzed in the order they are listed.

#### `name:`
Name of the module being analyzed. This field has no implication on analysis and is for organizational and debugging purposes only.

#### `type:`
Type of module being analyzed, list of supported modules corresponds closely with the [supported languages list](../README.md#SupportedLanguages).

If type is set to `archive`, fossa will upload a copy of all source code inside the folder and run a full license scan on all files.

#### `target:`
Build target for the specified module. 

#### `path:`
Path to the root of the project folder.

#### `ignore:` (Optional)
If set to `true` this module will be skipped.

Default: `false`

#### `option:` (Optional)
Most options are unique to the type of module being analyzed. Refer to the [supported languages' pages](../README.md#SupportedLanguages) for documentation on the full list of options available.

Default: No options.

