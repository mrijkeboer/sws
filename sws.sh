#!/bin/sh

SELF=`/usr/bin/basename $0`

SNAME="sws-001"
HOSTNAME="localhost"

SWS_DIR="/opt/sws"
SWS_USER="sws"

ERL="/usr/bin/erl"
EBINS="$SWS_DIR/ebin $SWS_DIR/deps/*/ebin"
SUDO="`which sudo` -u $SWS_USER"

export WEBMACHINE_IP=127.0.0.1
export WEBMACHINE_PORT=8000


start() {
	${SUDO} ${ERL} -pa ${EBINS} -name ${SNAME}@${HOSTNAME} -boot start_sasl -detached -s reloader -s sws
}

start_dev() {
	${ERL} -pa ${EBINS} -boot start_sasl -s reloader -s sws
}

stop() {
	${SUDO} ${ERL} -noshell -pa ${EBINS} -sname ${SNAME}_stop -s sws stop ${SNAME}@${HOSTNAME}
}

shell() {
	${SUDO} ${ERL} -sname ${SNAME}_shell -remsh ${SNAME}@${HOSTNAME}
}

case $1 in
	start)
		echo  "Starting SWS: $SNAME"
		start
		;;
	start-dev)
		start_dev
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
		echo "Usage: $SELF {start|stop|restart|shell|start-dev}"
		exit 1
esac

exit 0

