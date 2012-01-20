#!/bin/sh

exec erl -sname "echessd" \
    -boot start_sasl \
    -noshell -noinput \
    -pa ebin \
    -pa contrib/mochiweb/ebin \
    -echessd_config echessd.conf \
    -s echessd start

