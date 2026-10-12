UNICODE_VERSION=18.0.0
URI=https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd

all: UTF-8 i18n_ctype

UnicodeData.txt EastAsianWidth.txt DerivedCoreProperties.txt HangulSyllableType.txt:
	test -f $@ || curl -L -O $(URI)/$@

emoji/emoji-data.txt:
	mkdir -p emoji
	test -f $@ || curl -L -o $@ $(URI)/$@

utf8_gen.py:
	test -f $@ || curl -L -O $@ https://raw.githubusercontent.com/bminor/glibc/master/localedata/unicode-gen/$@

gen_unicode_ctype.py:
	test -f $@ || curl -L -O https://raw.githubusercontent.com/bminor/glibc/master/localedata/unicode-gen/$@

i18n_ctype: gen_unicode_ctype.py UnicodeData.txt DerivedCoreProperties.txt
	python3 gen_unicode_ctype.py -u UnicodeData.txt \
	  -d DerivedCoreProperties.txt -o $@ \
	  --unicode_version $(UNICODE_VERSION)

UTF-8: UnicodeData.txt EastAsianWidth.txt DerivedCoreProperties.txt HangulSyllableType.txt
	./utf8_gen.py \
		-u UnicodeData.txt \
		-d DerivedCoreProperties.txt \
		-e EastAsianWidth.txt \
		-k HangulSyllableType.txt \
		--unicode_version $(UNICODE_VERSION)

clean:
	rm -rf UTF-8 i18n_ctype *.txt emoji/emoji-data.txt
