#!/bin/sh

###-------------------------------------------------------------------
### File    : run.sh
### Author  : Aleksey Morarash <aleksey.morarash@gmail.com>
### Created : 20 Jan 2012
### License : FreeBSD
### Description : starts echessd server
###
###-------------------------------------------------------------------

exec erl -sname "echessd" \
    -setcookie echessd_secret_cookie \
    -noshell -noinput \
    -pa ./ebin \
    -pa ./contrib/mochiweb/ebin \
    -mnesia dir \"./db/mnesia\" \
    -echessd_config ./echessd.conf \
    -s mnesia \
    -s echessd

