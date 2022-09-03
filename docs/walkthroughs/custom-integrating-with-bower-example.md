# Custom Integration using `fossa-deps`

With `fossa-deps.{yml, json, yaml}` file, FOSSA CLI can be integrated to support any package manager or custom and non-standard management solution, that is yet to be supported natively by FOSSA CLI. With the fossa-deps file, we can:

- [Include manual dependencies](./../features/manual-dependencies.md)
- [Include vendored dependencies](./../features/vendored-dependencies.md)

## Example with Bower

For an example, we will look at [Bower](https://bower.io/).

We can usually identify a list of dependencies from our custom tool by looking at configuration files or executing a command. Bower provides both of these options, we can inspect (1) `bower.json` or (2) parse the output from the `bower list` command.

From an example `bower.json` file, lists direct dependencies:

```json
{
  "name": "example-project",
  "authors": [
    "user <user@example.com>"
  ],
  "description": "Example project",
  "main": "main.js",
  "license": "MIT",
  "homepage": "",
  "ignore": [
    "**/.*",
    "node_modules",
    "bower_components",
    "test",
    "tests"
  ],
  "dependencies": {
    "font-awesome": "^5.0.0",
    "jquery": "^3.6.0"
  }
}
```

We can execute `bower list` command to list all of the resolved bower dependencies:

```
example-project /Users/example-user/path
├── font-awesome#5.15.4
└── jquery#3.6.0
```

We have two dependencies (1) jquery and (2) font-awesome.

From [manual dependencies](./../features/manual-dependencies.md) documentation, we know that `bower` type dependencies are supported type as reference dependency.

We can include all listed dependencies in fossa-deps.json.

```json
{
  "referenced-dependencies": [
    {
      "type": "bower",
      "name": "jquery",
      "version": "3.6.0"
    },
    {
      "type": "bower",
      "name": "font-awesome",
      "version": "5.15.4"
    }
  ]
}
```

To programmatically add these dependencies, you can write a script in your language of choice to translate the dependency graph produced by bower to a fossa-deps file. Provided below is an example python script, which parses a dependency graph and produces a fossa-deps file.

[Here](./../../experimental-scripts/bower.py) is an example python script that parses bower list command, and prints fossa-deps file.

## Limitation

Please note that with the fossa-deps file, we can report dependencies, but we cannot:

- differentiate between direct and deep dependencies
- report edge information between dependencies
