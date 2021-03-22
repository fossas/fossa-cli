# Clojure

## Support

Clojure support relies on the presence of one of the following:

- A `project.clj` file.
- `lein` in order to retrieve a list of installed dependencies.

## Configuration

### Automatic

Run `fossa init` to detect all clojure directories that contain `project.clj`.

### Manual

Add a module with `type: clojure`, `target` set to the project file and `dir` set to the root of the Clojure project.

See [Options](#Options) for an in depth look at all of the available options for a Clojure module.

```yaml
analyze:
  modules:
    - name: test/clojure
      type: clojure
      target: project.clj
      path:  clojure/project
      options:
        strategy: lein
```

## Options

| Option     |  Type  | Name                         | Common Use Case                      |
| ---------- | :----: | ---------------------------- | ------------------------------------ |
| `strategy` | string | [Strategy](#strategy-string) | Specify a Clojure analysis strategy. |
| `lein`     | string | [Lein](#lein-string)         | Specify the Leiningen command to use |

#### `strategy: <string>`

Manually specify the Clojure analysis strategy to be used. Supported options:
- `project.clj`: Parse `project.clj` to find all dependencies used.
- `lein`: Run `lein deps :tree` to find an accurate dependency graph.

#### `lein: <string>`

Manually specify the Leiningen command to run for analysis.

## Analysis

The analysis strategy selected determines how analysis is completed for the Clojure analyzer. By default the fossa-cli will follow this list of fallbacks

1. `lein`: This strategy attempts to run `lein deps :tree` to retrieve a dependency graph. This strategy is the most accurate and should always be the first choice. It is currently the only way to determine the full list of deep dependencies.
1. `project.clj`: This strategy attempts to parse a clojure project file, by default `project.clj`, to determine dependencies. This method is robust at determining user specified dependencies which are often only direct dependencies. 
