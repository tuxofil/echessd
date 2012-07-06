#!/bin/sh

set -e
set -x

deluser echessd || :

rm -rvf -- \
    /etc/echessd.conf \
    /etc/init.d/echessd \
    /usr/lib/erlang/lib/echessd* \
    /usr/share/echessd \
    /usr/share/doc/echessd \
    /var/lib/echessd \
    /var/log/echessd \
    /var/run/echessd

