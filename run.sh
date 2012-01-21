#!/bin/sh

exec erl -sname "echessd" \
    -boot start_sasl \
    -noshell -noinput \
    -pa ebin \
    -pa contrib/mochiweb/ebin \
    -mnesia dir \"tmp/mnesia\" \
    -echessd_config echessd.conf \
    -s mnesia \
    -s echessd

