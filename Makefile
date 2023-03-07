macos-clipboard-nspasteboard.so: macos-clipboard-nspasteboard.o
	gcc -shared -framework Cocoa -o macos-clipboard-nspasteboard.so macos-clipboard-nspasteboard.o
macos-clipboard-nspasteboard.o: macos-clipboard-nspasteboard.m
	gcc -Wall -c macos-clipboard-nspasteboard.m
