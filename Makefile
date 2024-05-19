UNICODE_VER=14.0.0
URI=http://www.unicode.org/Public/$(UNICODE_VER)/ucd

all: UTF-8-EAW-FULLWIDTH.gz

UnicodeData.txt:
	wget -O $@ $(URI)/$@

EastAsianWidth.txt:
	wget -O $@ $(URI)/$@

PropList.txt:
	wget -O $@ $(URI)/$@

emoji-data.txt:
	wget -O $@ $(URI)/emoji/$@

UTF-8: UnicodeData.txt EastAsianWidth.txt
	./utf8_gen.py -u UnicodeData.txt -e EastAsianWidth.txt --unicode_version $(UNICODE_VER)

UTF-8-EAW-FULLWIDTH: UTF-8 EastAsianWidth.txt
	./gen.py

UTF-8-EAW-FULLWIDTH.gz: UTF-8-EAW-FULLWIDTH
	gzip -9 -c $^ > $@

install:
	sudo install UTF-8-EAW-FULLWIDTH.gz /usr/share/i18n/charmaps/
	sudo locale-gen

clean-data:
	rm -rf UnicodeData.txt EastAsianWidth.txt emoji-data.txt

clean:
	rm -rf UTF-8 UTF-8-EAW-FULLWIDTH UTF-8-EAW-FULLWIDTH.gz
