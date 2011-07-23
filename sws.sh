#!/bin/sh

SELF=`/usr/bin/basename $0`
ERL=`which erl`

SNAME="sws-001"
HOSTNAME="localhost"

SWS_DIR=$(cd ${0%/*} && pwd)
EBINS="$SWS_DIR/apps/*/ebin $SWS_DIR/deps/*/ebin"

start() {
	${ERL} -pa ${EBINS} -boot start_sasl -s reloader -s sws
}

stop() {
	${ERL} -noshell -pa ${EBINS} -sname ${SNAME}_stop -s sws stop ${SNAME}@${HOSTNAME}
}

shell() {
	${ERL} -sname ${SNAME}_shell -remsh ${SNAME}@${HOSTNAME}
}

case $1 in
	start)
		echo  "Starting SWS: $SNAME"
		start
		;;
	stop)
		echo "Stopping SWS: $SNAME"
		stop
		;;
	restart)
		echo "Restarting SWS: $SNAME"
		stop
		start
		;;
	shell)
		shell
		;;
	*)
		echo "Usage: $SELF {start|stop|restart|shell}"
		exit 1
esac

exit 0

