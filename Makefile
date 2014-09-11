UNICODE_VER=5.0.0
EAW_URI=http://www.unicode.org/Public/$(UNICODE_VER)/ucd/EastAsianWidth.txt

all: UTF-8-EAW-FULLWIDTH.gz

EastAsianWidth.txt:
	wget -O $@ $(EAW_URI)

UTF-8-EAW-FULLWIDTH: UTF-8 EastAsianWidth.txt
	./gen.py

UTF-8-EAW-FULLWIDTH.gz: UTF-8-EAW-FULLWIDTH
	gzip -9 -c $^ > $@

install:
	install UTF-8-EAW-FULLWIDTH.gz /usr/share/i18n/charmaps/
	locale-gen
