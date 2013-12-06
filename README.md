# _Echessd_ - Internet Chess Server

## Contents

 1. Summary
 2. Goals
 3. Features
 4. License
 5. Build
 6. Configure
 7. Run
 8. Play
 9. Server administration
 10. Security
 11. Reporting bugs

### 1. Summary

_Echessd_ - is lightweight, minimalistic and free Internet Chess
Server written with the [Erlang](http://www.erlang.org/) programming language.

### 2. Goals

Goals of the project:
 - no usage limitations;
 - no email address collecting;
 - no social networking;
 - less network traffic:
   - no images;
   - no ads;
 - no extra software dependencies;
 - just playing chess with other people. Anytime, everywhere
   with any type of a browser - even like lynx or w3m.

### 3. Features

 - cross platform (all you need to run is Erlang installed);
 - easy configuration. Functional even with empty configuration file;
 - multi language support (as for now: English, Russian, Ukrainian);
 - switchable styles;
 - notifications with XMPP (Jabber);
 - logging to a file;
 - reconfiguration on-the-fly;
 - database backing up and restore using text file;
 - multithreaded request processing;
 - user's password encryption.

### 4. License

_Echessd_ uses a [FreeBSD License](http://www.freebsd.org/copyright/freebsd-license.html).
You can obtain the license online or in the file LICENSE on the top of Echessd source tree.

### 5. Build

Erlang must be installed to build and run the _Echessd_.
You always can obtain the latest Erlang version [here](http://www.erlang.org/download.html)
or use the one provided by your software repository.

_Echessd_ is developed and tested with Erlang R15B01 but probably
will work with older Erlang versions.

    $ make

To build developer HTML documentation type:

    $ make html

### 6. Configure

You can use an _echessd.conf_ file as example of configuration for the _Echessd_.
It contains reasonable* (see Security section) defaults and configuration
parameter explanations.

### 7. Run

    $ ./echessd /path/to/echessd.conf

The server will start as a foreground process.

### 8. Play

1. Send the browser to [http://localhost:8888/](http://localhost:8888/);
2. Register a new user;
3. Choose an opponent in user list and hit the button 'Create new game';
4. Wait while the opponent confirms the game (you will need to
 refresh the web page unless you're set 'Auto Refresh' to ON on the
 account settings page).

### 9. Server administration

A compiled binary of the _Echessd_ take some command line options.
As mentioned above, to start the server, type:

    $ ./echessd /path/to/echessd.conf

To tell the running server to reread configuration file and reopen a log file:

    $ ./echessd --hup /path/to/echessd.conf

To check if the server is alive or not:

    $ ./echessd --ping /path/to/echessd.conf

To tell the running server to terminate:

    $ ./echessd --stop /path/to/echessd.conf

To make a copy of an Echessd database to a file:

    $ ./echessd --dump /path/to/file.dump /path/to/echessd.conf

To load the dump from the file to the database of the running Echessd
(all previous data from the database will be destroyed):

    $ ./echessd --load /path/to/file.dump /path/to/echessd.conf

To initialize the database (all data will be destroyed):

    $ ./echessd --init /path/to/echessd.conf

Important note: the server must be stopped while database initialisation.

### 10. Security

1. Do not run the _Echessd_ with superuser privileges;
2. Do not allow normal users to read your echessd.conf file
   because it contains 'cookie' string. With the string any
   local user can connect to the running _Echessd_ instance and
   execute ANY code with privileges of _Echessd_ Server process;
3. As issue of the 2, you should change the 'cookie' string to
   something long and random which will be hard to guess.

### 11. Reporting bugs

_Echessd_ is a young project and I'm sure there is a lot of bugs
and inconsistencies. On the other hand, _Echessd_ is not such
big project to support mailing list or bug tracker. So, in the
case of bug you can:

 - submit an email to me with subject "echessd: ....";
 - if you're [GitHub](https://github.com/) user, create an issue on a
   [project's page](https://github.com/tuxofil/echessd/issues/new);
 - fork, patch and create a pull request.

-----------------------------------------------------------------
Aleksey Morarash <aleksey.morarash@gmail.com>, 2013
