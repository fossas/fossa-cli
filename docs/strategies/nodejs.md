# NodeJS Analysis

The nodejs buildtool ecosystem consists of two major toolchains: the `npm` cli and `yarn`

| Strategy                   | Direct Deps              | Deep Deps | Edges | Tags        |
| ---                        | ---                      | ---       | ---   | ---         |
| [yarnlock][yarn]           | ✅ not labeled as direct | ✅        | ❌    |             |
| [npmlock][npm] (npmcli)    | ✅                       | ✅        | ✅    | Environment |
| [npmlist][npm] (npmcli)    | ✅                       | ✅        | ✅    |             |
| [packagejson][packagejson] | ✅                       | ❌        | ❌    | Environment |

[yarn]: nodejs/yarn.md
[npm]: nodejs/npmcli.md
[packagejson]: nodejs/packagejson.md
