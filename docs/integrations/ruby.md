# Ruby

## Support

Ruby support in FOSSA CLI depends on the following tools existing in your environment:

- Ruby (defaults to `ruby`, configure with `$FOSSA_RUBY_BINARY`)
- Gem (defaults to `gem`, configure with `$GEM_BINARY`)
- Bundler (defaults to `bundle`, configure with `$FOSSA_BUNDLER_CMD`)

## Configuration

### Automatic

Run `fossa init` to walk the file tree and detect all directories which contain a valid `Gemfile` to create corresponding modules.

### Manual

Add a module with `type: gem`, and `target` and `path` set to the root of the Ruby project.

```yaml
analyze:
  modules:
    - name: your-ruby-project
      type: gem
      target: .
      path: .
```

## Options

| Option              |  Type  | Name                                     | Common Use Case                       |
| ------------------- | :----: | ---------------------------------------- | ------------------------------------- |
| `strategy`          | string | [Strategy](#strategy-string)          | Specify a Ruby analysis strategy.     |
| `gemfile-lock-path` | string | [Lockfile Path](#requirements-string) | Specify a custom `Gemfile.lock` file. |

#### `strategy: <string>`

Manually specify the python analysis strategy to be used. Supported options:
- `list`: Run `bundler list` and create a dependency graph based from the output.
- `lockfile`: Analyze the `Gemfile.lock` file to create a dependency graph.
- `list-lockfile`: Compare `Gemfile.lock` with the output from `bundler list` and use dependencies found in both to create a dependency graph. Note, if this strategy is specified, both `Gemfile.lock` and `bundler list` are required to create a dependency graph.

Default: `list-lockfile`

#### `gemfile-lock-path: <string>`

Specify the location of a `Gemfile.lock` file located outside of the project's root directory or a custom named file.

Example:
```yaml
    gemfile-lock-path: config/Gemfile.lock
```

## Analysis

Fossa analysis for ruby by default attempts to compare the `Gemfile.Lock` file to the output of `bundler list` and takes the intersection of dependencies found. If fossa fails to access either of these it will fallback to using the one which succeeds and using all dependencies to create a dependency graph.
