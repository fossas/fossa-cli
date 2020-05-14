# Piplist

This is a really poor strategy that's used only in the worst-case: it includes
packages found in the global environment, and may not contain project
dependencies if `pip install` hasn't yet been run.

## Project Discovery

Find directories containing `setup.py` or `requirements.txt`

## Analysis

We run `pip list --format=json` and parse the output -- in the worst case, this
only provides global dependencies; in the best case, the global dependencies
will include our project's direct and deep dependencies.
