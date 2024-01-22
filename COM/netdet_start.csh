#!/bin/csh
#
#  directory where netdet is running 
#
cd /home/s2000/seismo/netdet
#
# start netdet if it is not running
# the file must be edited for your environment
#
#
# check if netdet is running
#
set check=`ps ax | grep -v grep | grep "netdet realtime -parfile netdet.par" | wc -l`

if ($check > 0) then
/usr/bin/date >> /home/s2000/seismo/CRON/netdet.log
/usr/bin/echo "netdet is running:" >> /home/s2000/seismo/CRON/netdet.log
/usr/bin/echo "netdet is running"
else
/usr/bin/echo "netdet is not running, start again"

#
#   define env variables
#
source /home/s2000/seismo/COM/SEISAN.csh
#
#  start netdet
#
/home/s2000/seismo/PRO/linux64/netdet realtime -parfile netdet.par  >/dev/null
#
#  send mail that it has been started
#
/usr/bin/echo "netdet has been started" | /usr/bin/mailx -s "netdet startet"  jens.havskov@uib.no
#
/usr/bin/date >> /home/s2000/seismo/CRON/netdet.log
/usr/bin/echo "netdet started:" >> /home/s2000/seismo/CRON/netdet.log
/usr/bin/echo "netdet.par" >>/home/s2000/seismo/CRON/netdet.log
/usr/bin/echo "       " >>/home/s2000/seismo/CRON/netdet.log

endif
#
