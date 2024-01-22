#!/bin/csh
#
#  directory where backup_sfiles is runningi and backup 
#  files are written by default 
#
cd /home/s2000/seismo/backup_sfiles
#
#   define env variables
#
source /home/s2000/seismo/COM/SEISAN.csh
#
#  start backup_sfiles
#  options: -start_time: time to start yyymmddhhmmss, at least yyyy
#                        if no given, use 1900
#           -end_time  : time to end, if not given, use current time
#           -base_name : data base, if not given use default data base
#           -out       : directory ending with / where files are written
#                        if not given, use working directory
#           -compress  : name of compress program, if blank no compression
#
/home/s2000/seismo/PRO/linux64/backup_sfiles -start_time 200201 -base_name ISC  >/dev/null
#
#
