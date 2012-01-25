#!/bin/sh

###-------------------------------------------------------------------
### File    : hup.sh
### Author  : Aleksey Morarash <aleksey.morarash@gmail.com>
### Created : 24 Jan 2012
### License : FreeBSD
### Description : makes echessd re-read configuration file and re-open
###               log file (use it after logrotate)
###-------------------------------------------------------------------

exec erl -sname "echessd_hupper" \
    -setcookie echessd_secret_cookie \
    -noshell -noinput \
    -pa ./ebin \
    -s echessd hup

