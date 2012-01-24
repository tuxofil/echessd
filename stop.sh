#!/bin/sh

###-------------------------------------------------------------------
### File    : stop.sh
### Author  : Aleksey Morarash <aleksey.morarash@gmail.com>
### Created : 24 Jan 2012
### License : FreeBSD
### Description : stops echessd server
###
###-------------------------------------------------------------------

exec erl -sname "echessd_stopper" \
    -setcookie echessd_secret_cookie \
    -noshell -noinput \
    -pa ./ebin \
    -s echessd stop \
    -s init stop

