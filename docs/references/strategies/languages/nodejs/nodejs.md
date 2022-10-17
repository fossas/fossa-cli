# NodeJS Analysis

The nodejs buildtool ecosystem consists of three major toolchains: the `npm` cli, `pnpm` and `yarn`.

| Strategy                      | Direct Deps        | Transitive Deps    | Edges              | Container Scanning |
| ----------------------------- | ------------------ | ------------------ | ------------------ | ------------------ |
| [yarnlock](yarn.md)           | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: |
| [npmlock](npm-lockfile.md)    | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: |
| [pnpmlock](pnpm.md)           | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark: |
| [packagejson](packagejson.md) | :white_check_mark: | :x:                | :x:                | :white_check_mark: |
