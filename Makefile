.PHONY : all clean
all : macos-clipboard-nspasteboard.so

macos-clipboard-nspasteboard.so: Sources/ClipboardModule/DataString.swift Sources/ClipboardModule/lib.swift
	swift build
	cp -f .build/debug/libClipboardModule.dylib macos-clipboard-nspasteboard.so

clean:
	rm -rf .build *.so *.o
