# Maven Analysis

For maven projects, we offer a more-accurate strategy (mavenplugin), and a strategy with zero requirements (pomxml)

| Strategy    | Direct Deps | Deep Deps | Edges |
| ---         | ---         | ---       | ---   |
| [mavenplugin][mavenplugin] | ✅          | ✅        | ✅    |
| [pomxml][pomxml]           | ✅          | ❌        | ❌    |

[mavenplugin]: maven/mavenplugin.md
[pomxml]: maven/pomxml.md
