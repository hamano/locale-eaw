
fixtmux.so: fixtmux.c
	gcc -shared -fPIC -ldl $< -o $@

install: fixtmux.so tmux.sh
	install -m 755 fixtmux.so /usr/local/bin
	install -m 755 tmux.sh /usr/local/bin/tmux

clean:
	rm -rf fixtmux.so
