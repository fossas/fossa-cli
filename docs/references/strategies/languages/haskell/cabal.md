# Cabal

## Project Discovery

Find all directories with a `cabal.project` file or a file with a `.cabal`
extension.  When one is found, skip all subdirectories.

*NOTE: We don't actually examine the cabal files, we just use their existence*
*to determine that we are woking with a cabal project.  As a result, we can*
*handle multiple `*.cabal` files, even though `cabal` cannot.*

## Analysis

From the project root, we open the `./dist-newstyle/cache/plan.json` file,
created by `cabal`.  The relevant items in the file take the following form:

*Extra data removed for brevity*

``` json
{
    "install-plan": [
        {
            "type": "configured",
            "id": "with-components-1.0.2.3-efgh",
            "pkg-name": "with-components",
            "pkg-version": "1.0.2.3",
            "style": "global",
            "components": {
                "lib": {
                    "depends": [
                        "base-4.13.0.0"
                    ]
                },
                "setup": {
                    "depends": [
                        "rts"
                    ]
                }
            }
        },
        {
            "type": "pre-existing",
            "id": "rts",
            "pkg-name": "rts",
            "pkg-version": "1.0",
            "depends": []
        },
        {
            "type": "configured",
            "id": "spectrometer-0.1.0.0-inplace",
            "pkg-name": "spectrometer",
            "pkg-version": "0.1.0.0",
            "style": "local",
            "depends": [
                "aeson-1.5.2.0-abcd",
                "base-4.13.0.0",
                "with-components-1.0.2.3-efgh"
            ]
        }
    ]
}
```

Analysis has two notable parts: matching `id` fields from the strings in
`depends` and `components.*.depends`, and noting the package `type` and
`style`.  `pre-existing` types refer to builtin, ghc-provided packages, while
`configured` packages have two `style`s of their own: `local`, referring to the
current project, and `global`, referring to packages obtained from a remote
source like [hackage](https://hackage.haskell.org/).
