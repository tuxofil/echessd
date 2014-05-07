#!/bin/sh

## Erlang emulator can't start without HOME environment variable.
## So, to start Echessd as system service we can't start it with
## 'su' utility without the '--login' option.
## To avoid using 'su --login' we set all vital environment here
## and then start the Echessd.
##
## Another reason to set HOME is the Erlang Distribution. When
## started, it tries to create a '.erlang.cookie' file in the
## directory pointed by HOME environment variable. If it fail,
## whole startup process will fail. Sad but true. Thats why we
## cannot set HOME to '/'.

export HOME=/var/lib/echessd
exec /usr/bin/echessd "$@"
