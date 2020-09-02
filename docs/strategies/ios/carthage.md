# Carthage Analysis

## Project Discovery

Find any folder which contains a file named `Cartfile.resolved`.  Skip all
subdirectories if one is found.

## Analysis

The `Cartfile.resolved` file contains a list of direct dependencies, which
should each represent subfolders in the `Carthage/Checkouts` directory.  This
structure is recursively scanned and combined into a single analysis.  See
below for an example:

```
myproject
├─ Cartfile.resolved     # ref to "Quick/Nimble"
└─ Carthage/
   └─ Checkouts/
      └─ Nimble/
         ├─ Cartfile.resolved     # ref to "mattgallagher/CwlPreconditionTesting"
         └─ Carthage/
            └─ Checkouts/
               └─ CwlPreconditionTesting/   # and so on...
```