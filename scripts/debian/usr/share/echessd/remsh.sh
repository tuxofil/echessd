#!/bin/sh

###-------------------------------------------------------------------
### File    : remsh.sh
### Author  : Aleksey Morarash <aleksey.morarash@gmail.com>
### Created : 6 Jul 2012
### License : FreeBSD
### Description : connects to node with echessd server running
###
###-------------------------------------------------------------------

RANDOM=`date +%N`
exec su --login --command \
    'erl -sname "echessd_remsh'$RANDOM'" \
     -remsh "echessd@nanofag"' echessd

