# NodeJS Analysis

The nodejs buildtool ecosystem consists of four major toolchains: the `npm` cli, `pnpm`, `yarn`, and `bun`.

| Strategy                      | Direct Deps        | Transitive Deps    | Edges              | Container Scanning |
| ----------------------------- | ------------------ | ------------------ | ------------------ | ------------------ |
| [yarnlock](yarn.md)           | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: |
| [npmlock](npm-lockfile.md)    | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: |
| [pnpmlock](pnpm.md)           | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: |
| [bunlock](bun.md)             | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x:                |
| [packagejson](packagejson.md) | :white_check_mark: | :x:                | :x:                | :white_check_mark: |
