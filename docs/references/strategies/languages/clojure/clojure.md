# Clojure Analysis

When developing in Clojure, [leiningen](https://leiningen.org/) is the most common package manager. Dependencies are specified in a manifest file by users which is used by the `lein` tool to build a dependency graph and download the correct dependencies.


| Strategy    | Direct Deps        | Transitive Deps          | Edges              | Container Scanning (Experimental) |
| ----------- | ------------------ | ------------------ | ------------------ | --------------------------------- |
| `lein deps` | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x:                               |

## Project Discovery

In order to find Leiningen projects, we look for `project.clj` files which specify the root of a Leiningen project. Once we find a `leiningen` file we quit walking the file tree because the downloaded dependencies will have their own `project.clj` files in subdirectories which would create an incorrect dep graph if scanned. 

## Analysis

1. run `lein deps :tree-data` and generate output similar to:
```
{[clojure-complete "0.2.5" :exclusions [[org.clojure/clojure]]] nil,
 [koan-engine "0.2.5"] {[fresh "1.0.2"] nil},
 [lein-koan "0.1.5" :scope "test"] nil,
 [nrepl "0.6.0" :exclusions [[org.clojure/clojure]]] nil,
 [org.clojure/clojure "1.10.0"]
 {[org.clojure/core.specs.alpha "0.2.44"] nil,
  [org.clojure/spec.alpha "0.2.176"] nil}}
```
2. Parse this output to determine the full dependency graph and dependency versions.
