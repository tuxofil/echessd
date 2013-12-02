#! /bin/sh
### BEGIN INIT INFO
# Provides:          echessd
# Required-Start:    $remote_fs $syslog
# Required-Stop:     $remote_fs $syslog
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# Short-Description: Internet Chess Server
# Description: Echessd is an lightweight and minimalistic
#    Internet Chess Server written with the Erlang programming language.
### END INIT INFO

# Author: Aleksey Morarash <aleksey.morarash@gmail.com>

# Do NOT "set -e"

# PATH should only include /usr/* if it runs after the mountnfs.sh script
PATH=/usr/sbin:/usr/bin
DESC="Internet Chess Server"
NAME=echessd
DAEMON=/usr/sbin/$NAME
CONFIG="/etc/echessd.conf"
PIDFILE=/var/run/$NAME.pid
SCRIPTNAME=/etc/init.d/$NAME

# Exit if the package is not installed
[ -x "$DAEMON" ] || exit 0

. /lib/init/vars.sh
. /lib/lsb/init-functions

#
# Function that starts the daemon/service
#
do_start(){
    # Return
    #   0 if daemon has been started
    #   1 if daemon was already running
    #   2 if daemon could not be started
    $DAEMON --ping $CONFIG && return 1
    start-stop-daemon --quiet --start --background --make-pidfile \
        --pidfile $PIDFILE --user $NAME --chuid $NAME \
        --exec $DAEMON -- $CONFIG || return 2
}

#
# Function that stops the daemon/service
#
do_stop(){
    # Return
    #   0 if daemon has been stopped
    #   1 if daemon was already stopped
    #   2 if daemon could not be stopped
    #   other if a failure occurred
    $DAEMON --stop $CONFIG
    RETVAL="$?"
    [ "$RETVAL" = 2 ] && return 2
    rm -f $PIDFILE
    return "$RETVAL"
}

case "$1" in
    start)
        [ "$VERBOSE" != no ] && log_daemon_msg "Starting $DESC" "$NAME"
        do_start
        case "$?" in
            0|1)
                [ "$VERBOSE" != no ] && log_end_msg 0
                ;;
            2)
                [ "$VERBOSE" != no ] && log_end_msg 1
                ;;
        esac
        ;;
    stop)
        [ "$VERBOSE" != no ] && log_daemon_msg "Stopping $DESC" "$NAME"
        do_stop
        case "$?" in
            0|1)
                [ "$VERBOSE" != no ] && log_end_msg 0
                ;;
            2)
                [ "$VERBOSE" != no ] && log_end_msg 1
                ;;
        esac
        ;;
    status)
        $DAEMON --ping $CONFIG
        ;;
    reload|force-reload)
        log_daemon_msg "Reloading $DESC" "$NAME"
        $DAEMON --hup $CONFIG
        log_end_msg $?
        ;;
    restart)
        log_daemon_msg "Restarting $DESC" "$NAME"
        do_stop
        case "$?" in
            0|1)
                do_start
                case "$?" in
                    0)
                        log_end_msg 0
                        ;;
                    1)
                        # Old process is still running
                        log_end_msg 1
                        ;;
                    *)
                        # Failed to start
                        log_end_msg 1
                        ;;
                esac
                ;;
            *)
                # Failed to stop
                log_end_msg 1
                ;;
        esac
        ;;
    *)
        echo "Usage: $SCRIPTNAME {start|stop|status|restart|reload}" >&2
        exit 3
        ;;
esac

:
