# Bower Support

Bower support in FOSSA CLI depends on the following tools existing in your environment:

- Node.js
- Bower

## Configuration

Add a `bower` module with the path to the `bower.json` in your project.

```yaml
analyze:
  modules:
    - name: your-bower-project
      path: bower.json
      type: bower
```

If you have an existing passing production build, you can run `fossa` from within the build environment and it should succeed.

Otherwise, you can run `fossa build` to execute the default build command `bower install --production`.

## Troubleshooting

FOSSA CLI parses the package manifests in `bower_components` to generate dependency IDs.  If FOSSA fails, your build might be failing.