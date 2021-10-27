### Errors in the `fossa-deps` file

The `fossa-deps` scanner tries to report clear error messages when fields are missing, incorrect, or invalid.  For example:

```yaml
referenced-dependencies:
- type: pypi
  name: flask
  version: "2.0.1"
  license: MIT  # Error!  "license" is only allowed for custom-dependencies

custom-dependencies:
- type: custom  # Error!  "type" is only allowed for referenced-dependencies
  name: mydep
  version: "3.14.15"
  license: GPL-3.0

remote-dependencies:
- name: mydep
  version: "3.14.15"
  license: GPL-3.0 # Error! "license" is only allowed for custom-dependencies
```

This would return an error with a message explaining what went wrong, and where.  However, we don't check for everything (yet!):

```yaml
referenced-dependencies:
- type: cargo
  name: bitflags
  some-unexpected-field: hello  # Has no effect, will be considered an error in future versions.
```

The `fossa-deps` scanner also requires at least one valid dependency if the file exists.  This prevents the file from being created with the wrong array names and us silently ignoring them.

If you see an error message that isn't clear, file an issue in this repository!  Clear error messages are a priority for us, and we want to know where we're lacking.
