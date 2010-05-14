#! /bin/sh

GOPHERD=./src/gopherd
PIDFILE=/var/run/gopherd.pid


gopherd_stop ()
{
      PID=$(pidof ${GOPHERD})

      #Test if the program running
      if [ $? = 1 ] ; then
	echo ${GOPHERD} "doesn't running"
	exit 1
      fi

      kill -1 $PID
}

gopherd_start ()
{
      $GOPHERD
}

if [ -x $GOPHERD ] ; then
  echo "ERROR : Executable not found" >&2
  exit 1
fi

if [ -x $PIDFILE ] ; then
  echo "PID file already exist : " $PIDFILE >&2
  exit 1
fi

case $1 in
    start)
      gopherd_start
    ;;
    stop)
      gopherd_stop
    ;;
    reload)
    ;;
    restart)
     gopherd_stop
     gopherd_start
    ;;
    status)
    ;;

