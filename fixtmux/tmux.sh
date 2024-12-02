#!/bin/sh

BASEDIR=$(dirname $0)
LD_RELOAD="$BASEDIR/fixtmux.so" /usr/bin/tmux $@
