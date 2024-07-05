all: UTF-8-EAW-FULLWIDTH.gz

ucd/UTF-8:
	make -C ucd

UTF-8-EAW-FULLWIDTH: UTF-8 ucd/EastAsianWidth.txt
	./gen.py

install:
	sudo install dist/UTF-8-EAW-FULLWIDTH.gz dist/UTF-8-EAW-CONSOLE.gz /usr/share/i18n/charmaps/
	sudo locale-gen

clean-data:
	rm -rf ucd

clean:
	rm -rf UTF-8 UTF-8-EAW-FULLWIDTH UTF-8-EAW-FULLWIDTH.gz
