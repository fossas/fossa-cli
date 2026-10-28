// swift-tools-version:5.7
import PackageDescription

let package = Package(
    name: "SwiftPathProj",
    dependencies: [
        .package(path: "lib"),
        .package(path: "does-not-exist"),
    ],
    targets: [
        .target(name: "SwiftPathProj"),
    ]
)
