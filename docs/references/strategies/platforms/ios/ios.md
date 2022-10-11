# Swift/Objective-C Analysis

The iOS buildtool ecosystem consists of two major toolchains: `Carthage` and `Cocoapods`

| Strategy                     | Direct Deps        | Transitive Deps          | Edges              | Container Scanning (experimental) |
| ---------------------------- | ------------------ | ------------------ | ------------------ | --------------------------------- |
| [Carthage](carthage.md)      | :white_check_mark: | :white_check_mark: | :white_check_mark: | :white_check_mark:                |
| [Podfile.lock](cocoapods.md) | :white_check_mark: | :white_check_mark: | :white_check_mark: | :x:                               |
| [Podfile](cocoapods.md)      | :white_check_mark: | :x:                | :x:                | :white_check_mark:                |
