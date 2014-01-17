# Debian notes

## Runtime dependencies

* erlang
* erlang-inets
* erlang-mnesia
* erlang-crypto
* sendxmpp (to send notifications with Jabber)

## Build dependencies

* make
* zip
* erlang
* erlang-inets
* erlang-tools

To build HTML documentation you also have to install:

* erlang-edoc
* graphviz

## Installing/Uninstalling system-wide

    $ sudo make debian-install

Note: the target does NOT create any users or groups and does not
chown created directories and files. To accomplish this, type:

    $ sudo make debian-chown

To remove all _Echessd_ related files (except database files and logs)
from the system type:

    $ sudo make debian-uninstall

Both makefile targets mentioned understand DESTDIR environment variable.
