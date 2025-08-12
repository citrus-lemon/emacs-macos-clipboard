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
        try env.defun("macos-clipboard--extract-pasteboard", with: "", function: extractPasteboard)
        try env.defun("macos-clipboard--extract-pasteboard-with-types", with: "", function: extractPasteboardWithTypes)
        try env.defun("macos-clipboard--extract-pasteboard-only-types", with: "", function: extractPasteboardOnlyTypes)
        try env.defun("macos-clipboard--set-string", with: "", function: setStringPasteboard)
        try env.defun("macos-clipboard--set-data", with: "", function: setDataPasteboard)
        try env.defun("macos-clipboard--clearPasteboard", with: "", function: clearPasteboard)
        try env.funcall("provide", with: Symbol(name: "macos-clipboard-nspasteboard"))
    }
}

func createModule() -> Module { ClipboardModule() }
