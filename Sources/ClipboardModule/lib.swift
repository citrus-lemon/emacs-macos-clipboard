import EmacsSwiftModule
import Cocoa

func extractPasteboard() -> List<List<ConsCell<String, DataString?>>>? {
    if let result = NSPasteboard.general.pasteboardItems?.map({ item in
        List(from: item.types.map { type in
            ConsCell(car: type.rawValue, cdr:(
                item.string(forType: type).map(DataString.String) ??
                item.data(forType: type).map(DataString.Data)))
        })
    }) {
        List(from: result)
    } else {
        nil
    }
}

func extractPasteboardWithTypes(_ types: List<String>) -> List<List<ConsCell<String, DataString>>>? {
    if let result = NSPasteboard.general.pasteboardItems?.map({ item in
        List(from: types.compactMap { type in (
            item.string(forType: NSPasteboard.PasteboardType(type)).map(DataString.String) ??
            item.data(forType: NSPasteboard.PasteboardType(type)).map(DataString.Data)).map {
                ConsCell(car: type, cdr: $0)}})
    }) {
        List(from: result)
    } else {
        nil
    }
}

func extractPasteboardOnlyTypes() -> List<List<String>>? {(
    NSPasteboard.general.pasteboardItems?.map {
        item in List(from: item.types.map {$0.rawValue} )
    }).map { List(from: $0) }
}

func clearPasteboard() {
    NSPasteboard.general.clearContents()
}

func setStringPasteboard(str: String, type: String?) {
    let pasteboardType = NSPasteboard.PasteboardType(type ?? "public.utf8-plain-text")
    NSPasteboard.general.setString(str, forType: pasteboardType)
}

func setDataPasteboard(data: Data, type: String?) {
    let pasteboardType = NSPasteboard.PasteboardType(type ?? "public.utf8-plain-text")
    NSPasteboard.general.setData(data, forType: pasteboardType)
}

func resolveAliasFile(fileURLString: String) -> String {
    if let fileURL = URL(string: fileURLString) {
        // This resolves `.file/id=...` to an actual file path
        if let resolvedURL = try? URL(resolvingAliasFileAt: fileURL, options: []) {
            return resolvedURL.path
        } else if fileURL.isFileURL {
            return fileURL.path // Works if already a normal file:// path
        }
    }
    return fileURLString
}

class ClipboardModule: Module {
    let isGPLCompatible = true
    func Init(_ env: Environment) throws {
        try env.defun("macos-clipboard--extract-pasteboard", with: """
Return the current macOS pasteboard contents as a list of items.

Each item is represented as a list of cons cells (TYPE . DATA),
where TYPE is a string UTI such as "public.utf8-plain-text" or
"public.file-url", and DATA is either a string or a data blob.
Return nil if the pasteboard is empty.
""", function: extractPasteboard)

        try env.defun("macos-clipboard--extract-pasteboard-with-types", with: """
Return the current macOS pasteboard contents for selected TYPES only.

TYPES is a list of UTI type strings (e.g., "public.file-url").
The return value is a list of items, each item being a list of
cons cells (TYPE . DATA) for the requested TYPES that are present.
DATA is either a string or a data blob.
Return nil if the pasteboard is empty or contains no matching types.
""", function: extractPasteboardWithTypes)

        try env.defun("macos-clipboard--extract-pasteboard-only-types", with: """
Return a list of type strings (UTIs) for each item in the macOS pasteboard.

The return value is a list of items, each being a list of UTI strings
present for that pasteboard item. Return nil if the pasteboard is empty.
""", function: extractPasteboardOnlyTypes)

        try env.defun("macos-clipboard--set-string", with: """
Set the macOS pasteboard to contain the given STR as text.

Optional TYPE specifies the pasteboard type (UTI). If nil,
defaults to "public.utf8-plain-text". STR must be a string.
""", function: setStringPasteboard)

        try env.defun("macos-clipboard--set-data", with: """
Set the macOS pasteboard to contain the given DATA as raw bytes.

Optional TYPE specifies the pasteboard type (UTI). If nil,
defaults to "public.utf8-plain-text". DATA must be a byte vector.
""", function: setDataPasteboard)

        try env.defun("macos-clipboard--clearPasteboard", with: """
Clear all contents from the macOS pasteboard.

After calling this, the pasteboard will be empty.
""", function: clearPasteboard)

        try env.defun("macos-clipboard-resolve-alias-file", with: """
Resolve a macOS file alias or private file reference URL to an actual POSIX path.

FILEURLSTRING should be a string beginning with "file://".
If FILEURLSTRING is in the private ".file/id=..." form, return
the resolved absolute path. If already a normal file URL,
return its path. If resolution fails, return FILEURLSTRING unchanged.
""", function: resolveAliasFile)

        try env.funcall("provide", with: Symbol(name: "macos-clipboard-nspasteboard"))
    }
}

func createModule() -> Module { ClipboardModule() }
