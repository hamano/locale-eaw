#UNICODE_VER=15.1.0
UNICODE_VER=14.0.0
URI=http://www.unicode.org/Public/$(UNICODE_VER)/ucd

all: UTF-8

UnicodeData.txt:
	test -f $@ || curl -L -o $@ $(URI)/$@

EastAsianWidth.txt:
	test -f $@ || curl -L -o $@ $(URI)/$@

PropList.txt:
	test -f $@ || curl -L -o $@ $(URI)/$@

emoji/emoji-data.txt:
	mkdir -p emoji
	test -f $@ || curl -L -o $@ $(URI)/$@

UTF-8: UnicodeData.txt EastAsianWidth.txt PropList.txt emoji/emoji-data.txt
	./utf8_gen.py \
		-u UnicodeData.txt \
		-e EastAsianWidth.txt \
		-p PropList.txt \
		--unicode_version $(UNICODE_VER)

clean:
	rm -rf UTF-8 UnicodeData.txt EastAsianWidth.txt PropList.txt emoji/emoji-data.txt
