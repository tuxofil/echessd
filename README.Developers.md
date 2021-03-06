# Developers notes

## Building the code

To compile the Echessd into a single executable file use:

    $ make

If all you want is to obtain Erlang byte code (*.beam) then use:

    $ make compile

## Building documentation

You can obtain autogenerated HTML documentation with:

    $ make html

Then you'll find it in the 'doc' directory.

## Testing

To run all tests available type:

    $ make all-tests

To run EUnit tests type:

    $ make eunit

To run [Dialyzer](http://erlang.org/doc/apps/dialyzer/users_guide.html) type:

    $ make dialyze

## Run Echessd within normal Erlang node

To start the Echessd with empty database:

    $ make compile
    $ ERL_LIBS=.. erl -echessd config_path '"echessd.conf"'
    ...
    1> ok = echessd_db:init().
    2> ok = application:start(echessd).

To start the Echessd and initialize the database from a dump file:

    $ make compile
    $ ERL_LIBS=.. erl -echessd config_path '"echessd.conf"'
    ...
    1> {ok, Dump} = file:consult("echessd.dump").
    2> ok = echessd_db:load(Dump).
    3> ok = application:start(echessd).

To start the Echessd with already existing database:

    $ make compile
    $ ERL_LIBS=.. erl -echessd config_path '"echessd.conf"'
    ...
    1> ok = mnesia:start().
    2> ok = application:start(echessd).

## Translate Echessd to a new language

All internationalisation data is defined in a _priv/echessd.lang_ file.
There is no need to make changes in other places to add support of a new language.
As far as the file is compiled in the Echessd executable, you'll need to
rebuild and restart the Echessd Server.

## Add a new CSS style

The task must be done in three stages:

 1. Add the new style declaration to a _priv/echessd.styles_ file;
 2. Add a corresponding CSS file to a _priv/www_ directory;
 3. Rebuild and restart the Echessd.
