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
mkdir -p ./db
exec erl -sname "echessd" \
    -noshell -noinput \
    -pa ./ebin \
    -mnesia dir \"./db/mnesia\" \
    -s echessd_db init \
    -s init stop

