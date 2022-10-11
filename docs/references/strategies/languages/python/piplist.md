# Piplist

This is a really poor strategy that's used only in the worst-case: it includes
packages found in the global environment, and may not contain project
dependencies if `pip install` hasn't yet been run.

## Project Discovery

Find directories containing `setup.py` or `requirements.txt`

## Analysis

We run `pip list --format=json` and parse the output -- in the worst case, this
only provides global dependencies; in the best case, the global dependencies
will include our project's direct and transitive dependencies.

## Example

Output from the `pip list --format=json` call:
*This command was run in a fresh virtualenv, after running `pip install requests`.  You may see different versions or dependencies, this example is only provided as a reference.*

```json
[
    {
        "name": "certifi",
        "version": "2021.5.30"
    },
    {
        "name": "chardet",
        "version": "4.0.0"
    },
    {
        "name": "idna",
        "version": "2.10"
    },
    {
        "name": "pip",
        "version": "21.1.3"
    },
    {
        "name": "requests",
        "version": "2.25.1"
    },
    {
        "name": "setuptools",
        "version": "57.0.0"
    },
    {
        "name": "urllib3",
        "version": "1.26.6"
    },
    {
        "name": "wheel",
        "version": "0.36.2"
    }
]
```

Notice that `pip`, `setuptools` and `wheel` are discovered, even though they are only used to power `pip` commands.
