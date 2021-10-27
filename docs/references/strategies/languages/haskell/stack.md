# Stack

## Project Discovery

Find all directories with a `stack.yaml` file.  When one is found, skip all
subdirectories.

## Analysis

From the project root, we run `stack ls dependencies json`, which outputs JSON
in the following format.

*Extra data removed for brevity*

``` json
[
    {
        "location": {
            "type": "hackage"
        },
        "name": "remote",
        "version": "remote-ver",
        "dependencies": [
            "deep"
        ]
    },
    {
        "location": {
            "type": "project package"
        },
        "name": "local",
        "version": "local-ver",
        "dependencies": [
            "remote",
            "builtin"
        ]
    },
    {
        "location": {
            "type": "hackage"
        },
        "name": "deep",
        "version": "deep-ver",
        "dependencies": []
    },
    {
        "name": "builtin",
        "version": "builtin-ver"
    }
]
```

Analysis has two notable parts: matching `name` fields from the strings in
`dependencies`, and noting the package's `location.type`.  A package with a 
missing `location.type` refers to a builtin, ghc-provided package, and a
present field takes one of two forms: remote and local, which have different
sets of string associated with them.  Local packages are references to the
current project, while remote references are pointers to package or source 
repositories.
