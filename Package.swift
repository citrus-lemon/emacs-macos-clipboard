// swift-tools-version: 5.9
import PackageDescription

let package = Package(
    name: "ClipboardModule",
    platforms: [.macOS(.v10_15)],
    products: [
        .library(
            name: "ClipboardModule",
            type: .dynamic,
            targets: ["ClipboardModule"]),
    ],
    dependencies: [
        .package(url: "https://github.com/citrus-lemon/emacs-swift-module.git", branch: "feature/make_unibyte_string")
    ],
    targets: [
        .target(
            name: "ClipboardModule",
            dependencies: [
                .product(name: "EmacsSwiftModule", package: "emacs-swift-module")
            ],
            plugins: [
                .plugin(name: "ModuleFactoryPlugin", package: "emacs-swift-module")
            ]
        )
    ]
)
