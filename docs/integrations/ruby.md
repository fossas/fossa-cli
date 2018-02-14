# Ruby

## Installation

Ruby support in FOSSA CLI depends on the following tools existing in your environment:

- Ruby (defaults to `ruby`, configure with `$RUBY_BINARY`)
- Gem (defaults to `gem`, configure with `$GEM_BINARY`)
- Bundler (defaults to `bundle`, configure with `$BUNDLER_BINARY`)

## Usage

Add a `ruby` module with the path to the `Gemfile` in your project.

```yaml
analyze:
  modules:
    - name: your-ruby-project
      path: Gemfile
      type: ruby
```

## Design
### Building

Builds are run using `bundle install --deployment --frozen`. If `--force` is set, the build command also runs `rm Gemfile.lock` before running the build.

### Analysis

Analysis parses the output of `bundle list`.
