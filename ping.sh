#!/bin/sh

###-------------------------------------------------------------------
### File    : ping.sh
### Author  : Aleksey Morarash <aleksey.morarash@gmail.com>
### Created : 24 Jan 2012
### License : FreeBSD
### Description : checks if echessd server is alive
###
###-------------------------------------------------------------------

exec erl -sname "echessd_pinger" \
    -setcookie echessd_secret_cookie \
    -noshell -noinput \
    -pa ./ebin \
    -s echessd ping

