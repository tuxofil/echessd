#!/bin/sh

exec erl -sname "echessd" \
    -boot start_sasl \
    -noshell -noinput \
    -pa ebin \
    -mnesia dir \"tmp/mnesia\" \
    -s echessd_db init \
    -s init stop

