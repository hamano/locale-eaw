.PHONY: dist install install-user
all: dist

EAW ?= CONSOLE
LOCPATH_USER ?= $(HOME)/.locale

ucd/UTF-8:
	make -C ucd

ucd/i18n_ctype:
	make -C ucd i18n_ctype

dist: ucd/UTF-8 ucd/EastAsianWidth.txt ucd/i18n_ctype
	mkdir -p dist/locales
	./gen.py
	cp ucd/i18n_ctype dist/locales/i18n_ctype

install:
	sudo install dist/UTF-8-EAW-FULLWIDTH.gz dist/UTF-8-EAW-CONSOLE.gz /usr/share/i18n/charmaps/
	sudo install -D -m644 dist/locales/i18n_ctype /usr/local/share/i18n/locales/i18n_ctype
	sudo locale-gen

install-user:
	mkdir -p $(LOCPATH_USER)
	I18NPATH=$(CURDIR)/dist/ localedef -i ja_JP -c \
		-f $(CURDIR)/dist/UTF-8-EAW-$(EAW) $(LOCPATH_USER)/ja_JP.UTF-8
	@echo "export LOCPATH=$(LOCPATH_USER) を .zshrc 等に追加してください"

clean:
	rm -rf dist
