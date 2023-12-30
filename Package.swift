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
        .package(url: "https://github.com/SavchenkoValeriy/emacs-swift-module.git", from: "1.3.4")
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
