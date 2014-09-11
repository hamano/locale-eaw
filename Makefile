
UNICODE_VER=5.0.0
EAW_URI=http://www.unicode.org/Public/$(UNICODE_VER)/ucd/EastAsianWidth.txt

all:
	gzip -9 -c UTF-8-EAW-FULLWIDTH > UTF-8-EAW-FULLWIDTH.gz

update: EastAsianWidth.txt

EastAsianWidth.txt:
	wget -O $@ $(EAW_URI)

install:
	install UTF-8-EAW-FULLWIDTH.gz /usr/share/i18n/charmaps/
	locale-gen
