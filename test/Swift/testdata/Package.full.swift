// swift-tools-version:5.3

// This Package.swift has values set for all valid options. This is to test that our parser is able to handle
// all fields in a Package.swift

import PackageDescription

let package = Package(
    name: "DeckOfPlayingCardsss",
    defaultLocalization: "en",
    platforms: [
        .macOS(.v10_15),
    ],
    products: [
        .executable(name: "tool", targets: ["tool"]),
        .library(name: "Paper", targets: ["Paper"]),
        .library(name: "PaperStatic", type: .static, targets: ["Paper"]),
        .library(name: "PaperDynamic", type: .dynamic, targets: ["Paper"]),
    ],
    // Random comment
    dependencies: [

        // without any contsraint
        .package(url: "https://github.com/kirualex/SwiftyGif.git"),

        // pkg version
        .package(
            name: "PlayingCard",
            url: "https://github.com/apple/example-package-playingcard.git",
            from: "3.0.0"
        ),
        .package(url: "https://github.com/kaishin/Gifu.git", .from("3.2.2")),

        // exact
        .package(url: "https://github.com/kelvin13/jpeg.git", .exact("1.0.0")),
        .package(url: "https://github.com/shogo4405/HaishinKit.swift", exact: "1.1.6"),

        // upToNextMajor
        .package(url: "https://github.com/dankogai/swift-sion", .upToNextMajor(from: "0.0.1")),

        // upToNextMinor
        .package(url: "git@github.com:behrang/YamlSwift.git", .upToNextMinor(from: "3.4.0")),

        // branch
        .package(url: "https://github.com/vapor/vapor", .branch("main")),
        .package(url: "git@github.com:vapor-community/HTMLKit.git", branch: "function-builder"),

        // revision
        .package(url: "https://github.com/SwiftyBeaver/SwiftyBeaver.git", revision: "607fc8d64388652135f4dcf6a1a340e3a0641088"),
        .package(url: "https://github.com/roberthein/TinyConstraints.git", .revision("3262e5c591d4ab6272255df2087a01bbebd138dc")),

        // range
        .package(url: "https://github.com/LeoNatan/LNPopupController.git", "2.5.0"..<"2.5.6"),
        .package(url: "https://github.com/Polidea/RxBluetoothKit.git", "3.0.5"..."3.0.7"),

        // path
        .package(path: "../.."),
        .package(name: "package-with-name", path: "../.."),
    ],
    targets: [
        .target(
            name: "DeckOfPlayingCards",
            dependencies: [
                .byName(name: "PlayingCard")
            ]),
        .testTarget(
            name: "DeckOfPlayingCardsTests",
            dependencies: [
                .target(name: "DeckOfPlayingCards")
            ]),
    ],
    swiftLanguageModes: [.v5, .v6],

    /*
     * Another comment
     */

    cLanguageStandard: .c11,
    cxxLanguageStandard: .cxx17, // Specifies the C++17 standard
    pkgConfig: "example-lib", // Searches for example-lib.pc
    providers: [
        .brew(["example-lib"]),
        .apt(["libexample-dev"])
    ], // Trailing comma!
)
