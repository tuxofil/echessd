# Installing on a Debian box

    $ sudo make debian-install

Note: the target does NOT create any users or groups and does not
chown created directories and files.

To remove all _Echessd_ related files (except database files and logs)
from the system type:

    $ sudo make debian-uninstall

Both makefile targets mentioned understand DESTDIR environment variable.
