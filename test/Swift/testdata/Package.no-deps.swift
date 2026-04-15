// swift-tools-version: 6.0
import PackageDescription

let package = Package(
    name: "MyPackage",
    platforms: [.iOS(.v15), .macOS(.v12)], // Minimum deployment targets
    products: [
        .library(name: "MyLibrary", targets: ["MyLibrary"]) // Publicly accessible modules
    ],
    targets: [
        .target(name: "MyLibrary", dependencies: ["OtherPackage"]),
        .testTarget(name: "MyLibraryTests", dependencies: ["MyLibrary"])
    ]
)
