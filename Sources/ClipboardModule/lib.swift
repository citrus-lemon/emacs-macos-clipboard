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

func setStringPasteboard(str: String, type: String?) {
    let pasteboardType = NSPasteboard.PasteboardType(type ?? "public.utf8-plain-text")
    NSPasteboard.general.setString(str, forType: pasteboardType)
}

class ClipboardModule: Module {
    let isGPLCompatible = true
    func Init(_ env: Environment) throws {
        try env.defun("macos-clipboard-extract-pasteboard", with: "", function: extractPasteboard)
        try env.defun("macos-clipboard-set-string", with: "", function: setStringPasteboard)
        try env.funcall("provide", with: Symbol(name: "macos-clipboard-nspasteboard"))
    }
}

func createModule() -> Module { ClipboardModule() }
