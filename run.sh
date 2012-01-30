#!/bin/sh

###-------------------------------------------------------------------
### File    : run.sh
### Author  : Aleksey Morarash <aleksey.morarash@gmail.com>
### Created : 20 Jan 2012
### License : FreeBSD
### Description : starts echessd server
###
###-------------------------------------------------------------------

[ -z `echo " $* " | grep -E ' --help | -h '` ] ||
{
    echo "Usage: $0 [--sasl] [--interactive]"
    exit 1
}

SASL=""
[ -z `echo " $* " | grep ' --sasl '` ] || SASL="-boot start_sasl"
NOSHELL=""
[ -z `echo " $* " | grep ' --interactive '` ] && NOSHELL="-noshell -noinput"

exec erl -sname "echessd" \
    -setcookie echessd_secret_cookie \
    $SASL $NOSHELL \
    -pa ./ebin \
    -pa ./contrib/mochiweb/ebin \
    -mnesia dir \"./db/mnesia\" \
    -echessd_config ./echessd.conf \
    -s mnesia \
    -s echessd

