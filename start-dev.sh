#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin $PWD/deps/*/ebin $PWD/deps/*/deps/*/ebin -sname $1 -boot start_sasl -s reloader -s platformer -config $1 -s tv
