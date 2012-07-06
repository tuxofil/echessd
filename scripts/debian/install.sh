#!/bin/sh

set -e
set -x

APPDIR=/usr/lib/erlang/lib/echessd-0.2.0

adduser --system --group --home=/var/run/echessd echessd
install --directory --mode=0755 \
    "$APPDIR"/ "$APPDIR"/ebin "$APPDIR"/priv \
    /usr/share/echessd /usr/share/echessd/www \
    /usr/share/doc/echessd /usr/share/doc/echessd/html
install --directory --mode=0750 --owner=echessd --group=echessd \
    /var/log/echessd /var/lib/echessd

install --mode=640 --group=echessd echessd.conf /etc/
sed --in-place \
    's@^\s*#?logfile\s+.*$@logfile /var/log/echessd/echessd.log@' \
    /etc/echessd.conf
sed --in-place \
    's@^\s*#?doc_root\s+.*$@doc_root /usr/share/echessd/www@' \
    /etc/echessd.conf
install --mode=644 ebin/*.beam ebin/*.app "$APPDIR"/ebin/
install --mode=644 priv/echessd.lang priv/echessd.styles "$APPDIR"/priv/
install --mode=644 www/*.css www/*.js /usr/share/echessd/www/

# documentation
install --mode=644 README LICENSE /usr/share/doc/echessd/
install --mode=644 doc/*.html doc/*.css doc/*.png /usr/share/doc/echessd/html/

# scripts
install --mode=755 scripts/debian/etc/init.d/echessd /etc/init.d/
install --mode=750 --group=echessd \
    scripts/debian/usr/bin/echessd-* /usr/bin/

