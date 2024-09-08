.PHONY: dist
all: dist

ucd/UTF-8:
	make -C ucd

dist: ucd/UTF-8 ucd/EastAsianWidth.txt
	mkdir -p dist
	./gen.py

install:
	sudo install dist/UTF-8-EAW-FULLWIDTH.gz dist/UTF-8-EAW-CONSOLE.gz /usr/share/i18n/charmaps/
	sudo locale-gen

clean:
	rm -rf dist
