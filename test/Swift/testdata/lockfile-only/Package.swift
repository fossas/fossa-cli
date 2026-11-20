// swift-tools-version:5.9
import PackageDescription

// The type annotation is valid Swift, but not the `let package = Package(` form the parser expects.
let package: Package = Package(
    name: "LockfileOnly",
    dependencies: [
        .package(url: "https://github.com/apple/swift-log.git", from: "1.0.0"),
        .package(url: "https://github.com/apple/swift-algorithms.git", branch: "main"),
    ],
    targets: [.target(name: "LockfileOnly")]
)
