#!/usr/bin/sh
#
echo This script will dump mseed data from several seedlink servers to a SDS archive, in real time.
echo The script makes sure that all subscripts are running at all times. See crontab.
echo ----
date
echo $0
uname -a
echo ----

# 
SLA="Slarchive IRIS"
SSH="slarchive -k 704"
SSH_PID="`ps -efa | grep "$SSH" | grep -v grep | tr -s ' ' ' ' | cut -d ' ' -f 3`"
if [ $SSH_PID ]; then
echo $SLA is running
else
date
echo $SLA is NOT running, starting $SLA ...
/home/seismo/PRO/slarchive -k 704 -l /home/seismo/DAT/streamlist.iris -SDS /home/seismo/archive rtserve.iris.washington.edu:18000
fi
# 

SLA="Slarchive GEOFON"
SSH="slarchive -k 705"
SSH_PID="`ps -efa | grep "$SSH" | grep -v grep | tr -s ' ' ' ' | cut -d ' ' -f 3`"
if [ $SSH_PID ]; then
echo $SLA is running
else
date
echo $SLA is NOT running, starting $SLA ...
/home/seismo/PRO/slarchive -k 705 -l /home/seismo/DAT/streamlist.geofon -SDS /home/seismo/archive 139.17.3.177:18000
fi

## 
echo ------------------------------------------------------------------
echo REMEMBER TO remove old archive files, if needed
#/usr/bin/find /home/seismo/SDS/2022 -type f -mtime +26 -exec rm {} \;
#echo remove old archive folders
#/usr/bin/find /home/ seismo/SDS/2022 -type d -empty -delete
#echo Done $0
echo ------------------------------------------------------------------

exit 0

#EOF


