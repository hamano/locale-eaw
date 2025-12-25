UNICODE_VER=17.0.0
URI=https://www.unicode.org/Public/$(UNICODE_VER)/ucd

all: UTF-8

UnicodeData.txt EastAsianWidth.txt DerivedCoreProperties.txt HangulSyllableType.txt:
	test -f $@ || curl -L -o $@ $(URI)/$@

emoji/emoji-data.txt:
	mkdir -p emoji
	test -f $@ || curl -L -o $@ $(URI)/$@

utf8_gen.py:
	test -f $@ || curl -L -o $@ https://raw.githubusercontent.com/bminor/glibc/master/localedata/unicode-gen/utf8_gen.py

UTF-8: UnicodeData.txt EastAsianWidth.txt DerivedCoreProperties.txt HangulSyllableType.txt
	./utf8_gen.py \
		-u UnicodeData.txt \
		-d DerivedCoreProperties.txt \
		-e EastAsianWidth.txt \
		-k HangulSyllableType.txt \
		--unicode_version $(UNICODE_VER)

clean:
	rm -rf UTF-8 *.txt emoji/emoji-data.txt
