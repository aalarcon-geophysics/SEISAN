#!/bin/bash
#
#   directory where netdet is running
#
cd /home/s2000/seismo/netdet
#
# start netdet if it is not running
#
#
# check if netdet is running
#
if ps ax | grep -v grep | grep "netdet realtime -parfile netdet.par"
then
/usr/bin/echo "netdet is running"
else
/usr/bin/echo "netdet is not running, start again"

#
#   define env variables
#
source /home/seismo/COM/SEISAN.bash
#
#  start netdet
#
/home/seismo/PRO/netdet realtime -parfile netdet.par >/dev/null
#
#  send mail that it has been started
#
/usr/bin/echo "netdet has been started" | /usr/bin/mailx -s "netdet startet" mail.adrr@gmail.com jens.havskov@gmail.com
#
#  put info in log file
#
/usr/bin/date >> /home/seismo/CRON/netdet.log
/usr/bin/echo "netdet started:" >> /home/seismo/CRON/netdet.log
/usr/bin/echo " " >> /home/seismo/CRON/netdet.log
/usr/bin/echo "        " >> /home/seismo/CRON/netdet.log

fi
#
