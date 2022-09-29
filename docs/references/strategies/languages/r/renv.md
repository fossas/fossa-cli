# R Analysis (renv)

Currently, we only support analysis of r project which are using [renv package manager](https://rstudio.github.io/renv/index.html).

| Files                       | Direct Deps        | Deep Deps          | Edges              | Classifies Dev & Test Deps | Container Scanning (experimental) |
| --------------------------- | ------------------ | ------------------ | ------------------ | -------------------------- | --------------------------------- |
| `DESCRIPTION`               | :white_check_mark: | :x:                | :x:                | :x:                        | :white_check_mark:                |
| `DESCRIPTION` & `renv.lock` | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x:                        | :white_check_mark:                |

## Project Discovery

Find a file named `DESCRIPTION`, and optionally look for `renv.lock` in the same directory as `DESCRIPTION`.

## Analysis

1. Parse `DESCRIPTION` file to identify direct dependencies - we look for packages in `Depends`, `Imports`, `Suggests`, `Enhances`, `LinkingTo`.
2. Parse `renv.lock` file to identify deep dependencies, and edges among them.

## Example

1. Create `DESCRIPTION` file:
```text
Type: project
Description: My project
Depends: tidyverse

```

2. Create `main.R` file:
```r
# you may need to execute following: 
# if you do not have renv installed
# >> install.packages("renv", repos = "http://cran.us.r-project.org")

# initiate project
# ref: https://rstudio.github.io/renv/reference/init.html
options(renv.config.install.verbose = TRUE)
options(renv.config.install.transactional = FALSE)
renv::init(bare = TRUE)
renv::install("glue@1.2.0")
renv::install()

# some example code
Square <- function(x) {
  return(x^2)
}
print(Square(4))

# create renv.lock
# ref: https://rstudio.github.io/renv/reference/snapshot.html
renv::snapshot()
```
3. execute `rscript main.R`
4. execute `fossa analyze --only-target renv --output` (run analysis only `renv`, but do not upload result to an endpoint)

## Limitations


- `fossa-cli` cannot identify test or development dependencies, and by default includes all dependencies in the analysis.
- `fossa-cli` will ignore version constraint if `renv.lock` file is not present.
- `fossa-cli` cannot analyze path dependencies. <!-- renv does not have docs on local packages --> 
  - Please refer to [vendored dependencies](./../../../../features/vendored-dependencies.md) for workaround. 

## FAQ

### How do I *only perform analysis* for renv?

Explicitly specify an analysis target in `.fossa.yml` file. The example below excludes all other analysis targets:

```yaml
# .fossa.yml 

version: 3
targets:
  only:
    - type: renv
```

### FOSSA's analyzed dependencies are incorrect or missing a package. 

Please file a ticket at [FOSSA support portal](https://support.fossa.com).

Make sure to attach following for quick response from support or development team.

* `DESCRIPTION` file
* `renv.lock` file (if any)
* stdout of `renv::diagnostics()`

## References

- [Renv Package Manager for R](https://rstudio.github.io/renv/index.html)