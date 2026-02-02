CC ?= gcc
CFLAGS = -Wall -Wextra -O2

all: fixtui

fixtui: fixtui.c
	$(CC) $(CFLAGS) -o $@ $< -lutil

install: fixtui
	install fixtui /usr/local/bin/

clean:
	rm -f fixtui

.PHONY: all clean
