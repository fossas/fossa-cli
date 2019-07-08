# Python

## Support

Python support relies on the presence of one of the following:

- A `requirements.txt` file.
- `pip`, in order to retrieve a list of installed dependencies.
- Pipenv, used to manage a projects environment and dependencies.

## Configuration

### Automatic

Run `fossa init` to detect all python directories that contain `requirements.txt`.

### Manual

Add a module with `type: pip`, and `target` and `dir` set to the root of the Python project.

See [Options](#Options) for an in depth look at all of the available options for a Python module.

```yaml
analyze:
  modules:
    - name: github.com/fossas/fossa-cli/cmd/fossa
      type: pip
      target: python/project
      dir:  python/project
      options:
        strategy: pipenv
```

## Options

| Option         |  Type  | Name                                      | Common Use Case                           |
| -------------- | :----: | ----------------------------------------- | ----------------------------------------- |
| `strategy`     | string | [Strategy](#strategy-string)              | Specify a Python analysis strategy.       |
| `requirements` | string | [Requirements Path](#requirements-string) | Specify a custom `requirements.txt` file. |
<!--- In code but currently unused
| `venv`         | string | [Virtual Env](#All-Tags:-<bool>)                  | Make sure all OS and Arch tags are caught. |
--->

#### `strategy: <string>`

Manually specify the python analysis strategy to be used. Supported options:
- `requirements`: Parse `requirements.txt` to find all dependencies used. 
- `pip`: Run `pip list --format=json` to find all dependencies in the current environment. `pip` over report the dependencies used if your environment is used to build multiple python projects.
- `deptree`: Run a custom python script to retrieve the dependency tree from pip. This provides similar information to `pip` with enough resolution to create a dependency tree.
- `pipenv`: Run `pipenv graph --json=tree` which returns the dependency graph of a project managed by Pipenv.

Default: `requirements`

#### `requirements: <string>`

Specify the location of a `requirements.txt` file located outside of the project's root directory or a custom named file.

Example:
```yaml
    requirements: config/myrequirements.txt
```

## Analysis

The analysis strategy selected determines how analysis is completed for the Python analyzer. By default the fossa-cli will analyze a requirements.txt file to determine dependencies. Benefits and limitations of strategies are listed below.

- `requirements`: This strategy is the most basic but provides an accurate representation of all dependencies inside of `requirements.txt`. The limitations with this method include not picking up transitive dependencies unless they are explicitly added to the file.
- `pip` & `deptree`: These strategies can accurately provide a dependency graph, however they analyze all dependencies managed by pip, not just those in the project. If your project is built in a CI environment where all pip installed dependencies are used, then this strategy would be effective. If you are on a local development machine then this strategy can over report dependencies.
- `pipenv`: This is the most reliable analysis strategy but requires your project to use Pipenv as its environment and package manager.
