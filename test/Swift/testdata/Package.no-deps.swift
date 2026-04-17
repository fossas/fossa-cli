// swift-tools-version: 6.0

// This Package.swift has no dependencies. This Package.swift ensures the parser doesn't get confused
// by the dependencies listed within `targets`

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
