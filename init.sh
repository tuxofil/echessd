#!/bin/sh

###-------------------------------------------------------------------
### File    : init.sh
### Author  : Aleksey Morarash <aleksey.morarash@gmail.com>
### Created : 20 Jan 2012
### License : FreeBSD
### Description : initiates echessd persistent storage
###               Warning: all existing data will be lost!
###-------------------------------------------------------------------

set -e
mkdir -p ./tmp
exec erl -sname "echessd" \
    -boot start_sasl \
    -noshell -noinput \
    -pa ./ebin \
    -mnesia dir \"./tmp/mnesia\" \
    -s echessd_db init \
    -s init stop

