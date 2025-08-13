.PHONY : all clean objc
all : macos-clipboard-nspasteboard.so

macos-clipboard-nspasteboard.so: Sources/ClipboardModule/DataString.swift Sources/ClipboardModule/lib.swift
	swift build
	cp -f .build/debug/libClipboardModule.dylib macos-clipboard-nspasteboard.so

macos-clipboard-nspasteboard.o: macos-clipboard-nspasteboard.m
	gcc -Wall -c macos-clipboard-nspasteboard.m
objc: macos-clipboard-nspasteboard.o
	gcc -shared -framework Cocoa -o macos-clipboard-nspasteboard.so macos-clipboard-nspasteboard.o

clean:
	rm -rf .build *.so *.o
