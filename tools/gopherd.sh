#! /bin/sh

GOPHERD=/home/tilatti/dev/haskell/GoCurry/src/gopherd
PIDFILE=/var/run/gopherd.pid


gopherd_stop ()
{
      PID=$(pidof ${GOPHERD})

      #Test if the program running
      if [$? = 1]; then
	echo ${GOPHERD} "doesn't running"
	exit 1
      fi

      kill -9 ${PID}
}

gopherd_start ()
{
      ${GOPHERD}
}

if [ -x $PIDFILE ]; then
  echo "PID file already exist : " $PIDFILE >&2
  exit 1
fi

case ${1} in
    start)
      gopherd_start
    ;;
    stop)
      gopherd_stop
    ;;
    restart)
     gopherd_stop
     gopherd_start
    ;;
    *)
      echo "start|stop|restart|reload"
    ;;
esac
