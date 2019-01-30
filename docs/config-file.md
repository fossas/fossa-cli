# `.fossa.yml`

The fossa config files controls how analysis is run.

```yaml
version: 1

cli:
  server: https://app.fossa.io
  fetcher: custom
  api_key: some-key-here
  # # If `project` or `locator` are unset, infer locator from VCS.
  project: git+github.com/fossas/fossa-cli
  locator: git+github.com/fossas/fossa-cli$revision
  revision: 234823483

analyze:
  modules:
    - name: fossa-cli
      type: go
      target: github.com/fossas/fossa-cli/cmd/fossa
      path: cmd/fossa
      options:
        allowUnresolved: true