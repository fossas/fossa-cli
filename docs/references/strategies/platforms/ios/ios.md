# Swift/Objective-C Analysis

The iOS buildtool ecosystem consists of two major toolchains: `Carthage` and `Cocoapods`

| Strategy                     | Direct Deps | Deep Deps | Edges |
| ---------------------------- | ----------- | --------- | ----- |
| [Carthage](carthage.md)      | ✅           | ✅         | ✅     |
| [Podfile.lock](cocoapods.md) | ✅           | ✅         | ✅     |
| [Podfile](cocoapods.md)      | ✅           | ❌         | ❌     |
