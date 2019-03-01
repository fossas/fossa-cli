# Buck

## Support

[Buck](#https://github.com/facebook/buck) projects are supported in a limited fashion by leveraging the [archive uploader](#./archive.md). The following is required

- `buck` is required to be installed in order to find the transitive dependency graph.
- a `.buckconfig` file.
- A functioning Buck project that has been built.

## Configuration

### Automatic

Running `fossa init` can detect Buck projects two different ways:
1. If a `.buckconfig` file is found, FOSSA can be sure it is at the root of a [Buck cell](https://buckbuild.com/about/overview.html).
   1. Search for user defined aliases in `.buckconfig`.
   2. If none are found, run `buck targets //` to find all local targets at the root.
2. No `.buckconfig` file is found but a `BUCK` file is found denoting a [Buck package](https://buckbuild.com/about/overview.html).
   1. FOSSA CLI runs `buck targets <directory>:` to find all local targets to the 

### Manual

Add a module with `type: buck`, `target` set to a valid [buck target](https://buckbuild.com/concept/build_target.html), and `dir` set to the current directory.

```yaml
analyze:
  modules:
    - name:   buckproject
      type:   buck
      target: //programs:buck
      dir:   .
```

## Analysis

Buck projects are uploaded with all of their source code broken into dependencies for license analysis by FOSSA. This project involves a few steps:
1. Run `buck audit input <target> --json` to retrieve a list of all dependencies.
2. Upload these dependencies and maintain references to their uploaded locator.
3. Create a dependency graph by recursively running `buck audit dependencies <target>` on each dependency and add it to the graph.
4. Upload the dependency graph to FOSSA which will match the dependency locators to the raw upload and analysis of each dependency.