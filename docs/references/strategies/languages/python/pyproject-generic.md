# PyProject.toml Generic Support

The FOSSA CLI provides robust support for analyzing Python projects that use the [PEP 621](https://peps.python.org/pep-0621/) `pyproject.toml` format, which is becoming the standard for Python project metadata and configuration.

## Project Discovery

The CLI searches for `pyproject.toml` files and automatically detects which build system is being used by examining the file contents.

## Project Type Detection

When analyzing a `pyproject.toml` file, the CLI will attempt to determine which build system is in use by checking for specific sections:

1. **Poetry**: Detected by the presence of `[tool.poetry]` section
2. **PDM**: Detected by the presence of `[tool.pdm]` section
3. **PEP 621**: Detected by the presence of `[project]` section (standard PEP 621 project)
4. **Unknown**: If none of the above sections are found

### Priority System

While a `pyproject.toml` file can technically contain configuration for multiple build systems, in practice, developers typically choose one primary build system per project. 

When multiple build system configurations are detected in a single `pyproject.toml` file, the CLI uses the following priority order:

1. Poetry (highest priority)
2. PDM
3. PEP 621
4. Unknown (lowest priority)

This prioritization ensures we select the most likely active build system when multiple configurations are present, avoiding confusion and duplicate analysis results. 

## Lock File Handling

For each detected project type, the CLI will look for the appropriate lock file:

- Poetry projects: `poetry.lock`
- PDM projects: `pdm.lock`

If a matching lock file is found, it will be used to provide a complete dependency graph with transitive dependencies. Otherwise, only direct dependencies from `pyproject.toml` will be reported.

## Analysis Strategy

The analysis strategy varies depending on the detected project type:

| Project Type | With Lock File | Without Lock File |
|--------------|----------------|-------------------|
| Poetry       | Complete graph using `poetry.lock` | Direct dependencies only |
| PDM          | Complete graph using `pdm.lock`    | Direct dependencies only |
| PEP 621      | Direct dependencies only           | Direct dependencies only |

## Limitations

- When multiple build system configurations are present in a single `pyproject.toml` file, only the highest-priority one will be analyzed.
- For PEP 621 projects without a specific build system, only direct dependencies are reported.
- Local path dependencies might not be fully analyzed depending on the configuration.

## References

- [PEP 621 – Storing project metadata in pyproject.toml](https://peps.python.org/pep-0621/)
- [Poetry Documentation](https://python-poetry.org/docs/pyproject/)
- [PDM Documentation](https://pdm.fming.dev/latest/usage/pyproject/)