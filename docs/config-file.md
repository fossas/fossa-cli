
# `.fossa.yml`
The fossa configuration file should be created by running `fossa init`

(Fields prefixed with `*` are optional and either determined at runtime from the environment or omitted entirely)

```yaml
version: 1

cli:
  server: https://app.fossa.io
  fetcher: custom
  project: fossa-cli
* api_key: some-key-here
* revision: 234823483
* locator: custom+github.com/fossas/fossa-cli$revision

analyze:
  modules:
    - name: fossa-cli
      type: go
      target: github.com/fossas/fossa-cli/cmd/fossa
      path: cmd/fossa

*     ignore: false
*     options:
        allowUnresolved: true
```
## Fields
### `version:`
Specifies the current fossa configuration file version being used. 
### `cli:`
#### `server:`
Primary endpoint that the cli sends its requests to. This field only needs to be modified when running an on-premise or local instance of `fossa.com`.

#### `fetcher:`
Describes the type of project fossa is uploading, there are two options:
- `custom` - ?????????????
- `git` (deprecated??) - A unique project on Github

#### `project:`
This is the name of the project which will be used to construct a project locator which uniquely identifies a project on the fossa projects page. Keeping this name consistent ensures that all future analysis run is correlated to the same project.

#### `api_key:` (Optional)
Holds a unique Fossa API Key which is used to send uploads to the correct organization. 
We reccomended not including this field to avoid committing a Fossa API key to a repository. Keeping your API Key as an environment variable is a safer approach.

If excluded, fossa will utilize the environment variable `FOSSA_API_KEY`.

#### `revision:`
Manually specify a projects revision.

If excluded, fossa will obtain the revision from the version control software (VCS) in the project.

#### `locator:` (Optional)
This allows you to manually specify a project locator

If excluded, fossa will create a locator using fetcher, api_key, project, and revision.

### `analyze`
