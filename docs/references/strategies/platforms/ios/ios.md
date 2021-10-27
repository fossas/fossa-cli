# Swift/Objective-C Analysis

The iOS buildtool ecosystem consists of two major toolchains: `Carthage` and `Cocoapods`

| Strategy | Direct Deps | Deep Deps | Edges |
| --- | --- | --- | --- |
| [Carthage][cart] | ✅ | ✅ | ✅ |
| [Podfile.lock][coco] | ✅ | ✅ | ✅ |
| [Podfile][coco] | ✅ | ❌ | ❌ |

[cart](carthage.md)
[coco](cocoapods.md)
