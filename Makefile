UNICODE_VER=14.0.0
URI=http://www.unicode.org/Public/$(UNICODE_VER)

all: UTF-8-EAW-FULLWIDTH.gz

ucd:
	mkdir -p ucd ucd/emoji

ucd/UnicodeData.txt: | ucd
	test -f $@ || curl -L -o $@ $(URI)/$@

ucd/EastAsianWidth.txt: | ucd
	test -f $@ || curl -L -o $@ $(URI)/$@

ucd/PropList.txt: | ucd
	test -f $@ || curl -L -o $@ $(URI)/$@

ucd/emoji/emoji-data.txt: | ucd
	test -f $@ || curl -L -o $@ $(URI)/$@

UTF-8: ucd/UnicodeData.txt ucd/EastAsianWidth.txt ucd/PropList.txt
	./utf8_gen.py \
		-u ucd/UnicodeData.txt \
		-e ucd/EastAsianWidth.txt \
		-p ucd/PropList.txt \
		--unicode_version $(UNICODE_VER)

UTF-8-EAW-FULLWIDTH: UTF-8 ucd/EastAsianWidth.txt
	./gen.py

UTF-8-EAW-FULLWIDTH.gz: UTF-8-EAW-FULLWIDTH
	gzip -9 -c $^ > $@

install:
	sudo install dist/UTF-8-EAW-FULLWIDTH.gz dist/UTF-8-EAW-CONSOLE.gz /usr/share/i18n/charmaps/
	sudo locale-gen

clean-data:
	rm -rf ucd

clean:
	rm -rf UTF-8 UTF-8-EAW-FULLWIDTH UTF-8-EAW-FULLWIDTH.gz
