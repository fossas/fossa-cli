# NodeJS Analysis

The nodejs buildtool ecosystem consists of three major toolchains: the `npm` cli, `pnpm` and `yarn`.

| Strategy                      | Direct Deps | Deep Deps | Edges |
| ----------------------------- | ----------- | --------- | ----- |
| [yarnlock](yarn.md)           | ✅           | ✅         | ✅     |
| [npmlock](npm-lockfile.md)    | ✅           | ✅         | ✅     |
| [pnpmlock](pnpm.md)           | ✅           | ✅         | ✅     |
| [packagejson](packagejson.md) | ✅           | ❌         | ❌     |
