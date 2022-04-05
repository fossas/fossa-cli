# NodeJS Analysis

The nodejs buildtool ecosystem consists of two major toolchains: the `npm` cli and `yarn`

| Strategy                      | Direct Deps | Deep Deps | Edges |
| ----------------------------- | ----------- | --------- | ----- |
| [yarnlock](yarn.md)           | ✅           | ✅         | ✅     |
| [npmlock](npm-lockfile.md)    | ✅           | ✅         | ✅     |
| [packagejson](packagejson.md) | ✅           | ❌         | ❌     |
