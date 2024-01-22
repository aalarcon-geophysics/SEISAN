c
c  Program to create a SIMULPS2000 EQKS phase list input file from a Nordic file
c
c  Felix Halpaap, 2015
c  modified from Brian Baptie's nor2dd (2004)
c

c
c changes:
c  july 15, 2015 fh : now only prints the S-times when a P-reading 
c                     for the station/event-pair is available
c  apr  13  2021 lo : adjust to new format by using rea_* routines
c                     allow for 5 char station codes

      implicit none
      include 'seidim.inc'
      include 'rea.inc'
      include 'seisan.inc'
      CHARACTER*80 DATA(2500)                                                  
      CHARACTER*80 infile                                                     
      CHARACTER*5 stations(2500)
c      CHARACTER*4 phasSpec(2500),phasTime(2500),phasSta(2500)                                                  
      CHARACTER*1 TYPE,EXP                                                      
      character*80 station_file
      integer nstat,nphase,nhead,nrecord
      integer sta_list_count
c---number of arguments 
      integer nars
c-- arguments
      character*80 args(10)
      integer n,id,code
c--compact or not
      logical compact
      logical error

c
c print version
c
      include 'version.inc'
      call get_seisan_def
      out_version_date='July 6, 2015'
      if (version_new) out_version_date=version_date
      call print_ver

      error=.false.

c 
c   check if input from file given in argument
      call get_arguments(nars,args)
      if(nars.eq.0) then
         write(6,*)' You must give input file as argument'
         stop
      endif
c
c     case with output on standard output
c
      if(nars.gt.2) then        
         write(6,*)' Too many arguments'
         stop
      endif
c
c   output file
c
      open(2,file='EQKS',status='unknown')
c
c   get input file name
c
      infile=args(1)
      open(1,file=infile,status='old')
c
c   check that not a compact file
c
      call nortype(1,compact)
      if(compact) then
         write(6,*)' Input file is compact, cannot be used'
         stop
      endif
c     
c   read and write to end of file
c
      n=0
 10   continue
c
c   read one event in nordic format
c
c      CALL INDATA(1,NSTAT,NPHASE,NHEAD,NRECORD,TYPE,EXP,DATA,ID)
c      if(nrecord.eq.0) goto 99
      call rea_event_in(1,.true.,data,code)
      if(code.ne.0) goto 99

c
c   convert phase readings to SIMULPS2000 input format and write out 
c
      n=n+1
      call nor2simulps(2,n,stations,sta_list_count)
      goto 10
c
 99   continue
      write(6,*)
      write(6,*)' The input file had ',n,' events'
        
      close(1)
      close(2)

c
c find the appropriate station file and check its existence
c
c      call get_station_file(station_file,error)
c      if (error) then
c         stop
c      endif
c     
c find the stations corresponding to those in the station list and
c write out jhd format stalist.elev file containing station details
c        
c      call write_station_output(station_file,
c
c changed to use stat_loc to get location, lot 31/5/2005
c
      call write_station_output(
     1     stations,sta_list_count,error)
      if (error) then
         stop
      endif
c
c extract velocity model parameters from the station file and
c write out jhd format syn.vel velocity model file
c       
c      call get_velocity_model(station_file,error)
c      if (error) then
c         stop
c      endif


      stop
      end
                       

c
c
      subroutine nor2simulps(unit,nevent,stations,sta_list_count)
	  
c
c   gets one event in nordic format in array data and writes it to unit
c   in SIMULPS format
c
      implicit none
      include 'seidim.inc'
      include 'rea.inc'
      include 'seisan.inc'
      character*80 data(max_phase)
      character*5 stations(*)
      integer sta_list_count
      integer nrecord,unit,nhead,nevent,nobs
      integer year,month,day,hour,min,shortYear
      integer iweight,ilat,ilon
      real head_sec, p_sec, p_diff
      real lon,lat,depth,latmin,lonmin
      real weight
      real eh,ez,rms,mag
      double precision head_time,p_time ! abs times for header and phase
      integer j,k,m,q,qi,p,r,mAid
      integer nStaOb,staObs(max_phase)
      logical station_in_list,alsoP
      character*1 c,cweight,phasType(max_phase)
      integer ain
      character*5 phasSta(max_phase)
      character*4 phasSpec(max_phase)
      real phasTime(max_phase)

c
c   read origin time
c
c      read(data(1),'(1x,i4,1x,2i2,1x,2i2,1x,f4.1)') 
c     *year,month,day,hour,min,head_sec
      year=hyp_year(1)
      month=hyp_month(1)
      day=hyp_day(1)
      hour=hyp_hour(1)
      min=hyp_min(1)
      head_sec=hyp_sec(1)

      shortYear=year-FLOOR(year/100.0)*100

c   check if second/ minute are 60, then correct minute/hour 
125   if (head_sec .ge. 60.) then 
        head_sec = head_sec - 60.
        min = min + 1
        goto 125
      endif      
126   if (min .ge. 60.) then 
        min = min - 60.
        hour = hour + 1
        goto 126
      endif  
           
c      read(data(1)(31:38),'(f8.3)') lon 
c      read(data(1)(24:30),'(f7.3)') lat
c      read(data(1)(39:43),'(f5.1)') depth 
c      read(data(1)(53:55),'(f3.1)') rms 
c      read(data(1)(56:59),'(f4.1)') mag 
      lon=hyp_lon(1)
      lat=hyp_lat(1)
      depth=hyp_depth(1)
      rms=hyp_rms(1)
      mag=hyp_mag_all(1)

      eh=0.0
      ez=0.0
c
c search for high accuracy line
c
      do k=1,rea_nhead
        if (data(k)(80:80).eq.'H') then
          read(data(k)(17:22),'(f6.3)') head_sec
          read(data(k)(24:32),'(f9.5)') lat
          read(data(k)(34:43),'(f10.5)') lon
          read(data(k)(45:52),'(f8.3)') depth
          write(*,*) ' found high accuracy solution ',lat,lon,depth
        endif
      enddo
      
      ilat=floor(lat)
      ilon=floor(lon)
      latmin=(lat-ilat)*60
      lonmin=(lon-ilon)*60

c write out origin details
      write(unit,101) shortYear,month,day,hour,min,head_sec,ilat,
     *      latmin,ilon,lonmin,depth,mag,nevent
 101  format(3(i2.2),1x,2(i2.2),1x,f5.2,i3,1x,f5.2,1x,i3,1x,f5.2,
     *       2(f7.2),i12)
c 101  format(i2.2,i2.2,i2.2,1x,i2.0,i2.0,1x,f5.2,i3,1x,f5.2,1x,i3,1x,f5.2,f7.2,
c     *       f7.2,i12)
     
c 101  format(i2,i2,i2,1x,i2,i2,1x,f5.2,i3,1x,f5.2,1x,i3,1x,f5.2,f7.2,
c     1     f7.2)

c   get header abs time
c
      call timsec(year,month,day,hour,min,head_sec,head_time)

c   counter for number of phases with a station in the station information
      m=1
c
c   read phases to end of event
c
      do k=1,rea_nphase

c For each P-phase calculate the phase time relative to the origin time
c         if(data(k)(11:11).eq.'P'.or.data(k)(11:11).eq.'S') then
cc first arrivals only, have to be just P or S
cc         if(data(k)(11:12).eq.'P '.or.data(k)(11:12).eq.'S ') then
cc            read(data(k)(19:28),'(i2,i2,1x,f5.2)') hour,min,p_sec
          if (rea_phase(k)(1:1).eq.'P'.or.rea_phase(k)(1:1).eq.'S') then
c            read(data(k)(19:22),'(2i2)') hour,min
c            read(data(k)(23:28),'(f6.3)') p_sec
c            cweight = data(k)(15:15)
            hour=rea_hour(k)
            min=rea_min(k)
            p_sec=rea_sec(k)
            cweight=rea_weight_in(k)
c            iweight = ICHAR(cweight)
            
c            if ( (iweight.ne.1) .OR. (iweight.NE.2) .OR.
c     *           (iweight.ne.3) .OR. (iweight.ne.4) ) then
c              cweight='0'  
c            endif  
            if ( cweight.eq.' ') then
              cweight='0'
            endif  

            call timsec(year,month,day,hour,min,p_sec,p_time)
            p_diff=p_time-head_time ! phase time relative to header time
            p_sec=head_sec+p_diff   ! phase second relative to header second  

c write out the formatted phase output

c            station_in_list=.false.
c            do j=1,sta_list_count
c               if(data(k)(2:5).eq.stations(j)(1:4)) then
c                  station_in_list=.true.
c                  phasTime(m) = p_sec            ! save phase travel time for later
c                  phasSta(m) = stations(j)(1:4) ! save station for that phase
c                  phasType = data(k)(11:11)     ! phase type: P or S
c                  phasSpec(m) = '  '//phasType//cweight
c                  m=m+1
c               endif
c               phasTime(m) = p_sec            ! save phase travel time for later
c               phasSta(m) = stations(j)(1:4) ! save station for that phase
c               phasType = data(k)(11:11)     ! phase type: P or S
c               phasSpec(m) = '  '//phasType//cweight
c               m=m+1
c            enddo
c            if(.not.station_in_list) then
c               sta_list_count=sta_list_count+1
c               stations(sta_list_count)(1:5)='     '
c               stations(sta_list_count)(1:4)=data(k)(2:5)
c               phasSta(m-1) = '    '
c            endif
c            write(unit,100) data(k)(2:5),p_diff,weight,data(k)(11:11),c

            station_in_list=.false.
            do j=1,sta_list_count
               if(rea_stat(k).eq.stations(j)) then 
                  station_in_list=.true.
               endif
            enddo
            if(.not.station_in_list) then
               sta_list_count=sta_list_count+1
               stations(sta_list_count)=' '
               stations(sta_list_count)=rea_stat(k)
            endif

            phasTime(m) = p_diff
            phasType(m) = rea_phase(k)(1:1)
            phasSta(m) = rea_stat(k)
c            phasSpec(m) = '  '//phasType(m)//cweight
            phasSpec(m) = ' '//phasType(m)//cweight

c only increase index when there is a P AND S reading for the event at
c this station - SIMULPS can't do anything with just a S-reading
            alsoP=.false.
            mAid=m
            if(phasType(m).eq.'P') then
              m=m+1
            else if(phasType(mAid).eq.'S') then
              nStaOb=1
              p=1
              staObs(p)=m
              p=2
              do r=1,m-1
                if(phasSta(m).eq.phasSta(r)) then                 
                  nStaOb=nStaOb+1
                  staObs(p)=r
                  p=p+1
                endif
              enddo
              alsoP=.false.
              if(nStaOb.ge.2) then
                do q=1,nStaOb
                  qi=staObs(q)
                  if(phasType(qi).eq.'P') then
                    alsoP=.true.
                  endif
                enddo
              endif
            endif
            if(alsoP) then
              m=m+1
            endif
         endif
      enddo

c  print all phases for event, 6 per line
      write(unit,102) (phasSta(nobs),phasSpec(nobs),phasTime(nobs),
     1     nobs=1,m-1)
c  and print new empty line between events
      write(unit,*)' '
      return

 100  format(a4,1x,f6.2,1x,f4.2,1x,a1,a1)
c 102  format(6(a4,a4,f6.2))
 102  format(6(a5,a3,f6.2))

      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C GET_STATION_FILE 
C
C Finds the STATION0.HYP file either in the local directory or in the 
C SEISAN_TOP/DAT directory
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine get_station_file(station_file,error)

      integer itp
      character*80 cur_file        ! station file in current directory
      character*80 dat_file        ! station file in DAT directory
      character*80 station_file
      character*60 top_directory   ! seisan top dir
      character*1 dchar            ! '/' character
      logical error

      error=.false.
c
c   find station file to use
c
      call dir_char(dchar)
      call topdir(top_directory)
      ITP=index(top_directory,' ')-1
c
c  make file name for both file in current and dat directory
c
      cur_file(1:12)='STATION0.HYP'
      dat_file(1:itp)=top_directory(1:itp)	  
      dat_file(itp+1:itp+1)=dchar
      dat_file(itp+2:itp+4)='DAT'	  
      dat_file(itp+5:itp+5)=dchar
      dat_file(itp+6:itp+17)=cur_file(1:12)	  	  
      open(999,file=cur_file,status='old',err=1)
      write(6,2001) cur_file(1:12)
      station_file=cur_file
      goto 2
 1    continue	  
      OPEN(999,FILE=dat_file,STATUS='OLD',err=3)
      write(6,2001) dat_file(1:itp+17)
      station_file=dat_file
      goto 2
 3    continue
      write(6,*)' No station file found'
      error=.true.

 2    continue
      close(999)

 2001 format (' Using station file ', a)


      return

      end



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C WRITE_STATION_OUTPUT write out JHD stat.evel file
C
C Given a list of station names this program
C finds the corresponding entry in the STATION0.HYP file
C and writes out station data in JHD format
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c      subroutine write_station_output(station_file,
c     1     stations,sta_list_count,error)
      subroutine write_station_output(
     1     stations,sta_list_count,error)
      implicit none

      character*80 station_file
      CHARACTER*5 stat
      CHARACTER*5 stations(*)
      integer sta_list_count
      character*80 line         ! one line

      integer ilat, ilon, olatdeg, olondeg, coordFlag
      real dec_lat, dec_lon, mlat, mlon, elev, el
      real olatmin, olonmin, coRot
      character*1 dlat, dlon, file_ind

      integer i,j

      logical error
      
      error=.false.

      print*,"SIMULPS2000 requires a local coordinate system in the STNS
     *S-file, please define origin and rotation angle against standard l
     *at/lon"      
      print*,"Enter coordinate system origin latitude (degrees and minut
     *es)"
c      read (6,'(i4,f5.2)') olatdeg,olatmin
      read (5,*,ERR=900) olatdeg, olatmin
      print*,"Enter coordinate system origin longitude (degrees and minu
     *tes)"
c      read (6,'(i4,f5.2)') olondeg,olonmin
      read (5,*,ERR=900) olondeg, olonmin
      print*,"Enter rotation angle for coordinate system (anticlockwise)
     *"
      read (5,*) coRot
       print*,"Enter Coordinate Flag: 0 for Latitude North and Longitude
     *West, 1 (SE), 2 (NE), 3 (SW)"
      read (5,*) coordFlag     
c
c open input and output files
c
c      open(10,FILE=station_file,STATUS='OLD',err=98)
      open(3,file='station.dat',status='unknown',err=99)
      open(8,file='STNS',status='unknown',err=99)      
      write(8,104) olatdeg,olatmin,olondeg,olonmin,coRot,coordFlag
      write(8,'(i5)') sta_list_count

      file_ind=' '
      do i=1,sta_list_count
         call stat_loc(stations(i),file_ind,dec_lat,dec_lon,el)
         if (dec_lat.ne.0..and.dec_lon.ne.0.) then
           write(3,100) stations(i), dec_lat, dec_lon
           write(8,103) stations(i), dec_lat, dec_lon,el/1000.0,0.0,
     *                  0.0,0            
         else
           write(6,'(a)') ' station not found '//stations(i)
         endif
      enddo
      return

 900  STOP 'Error in input format (degree: integer, minutes: real)'
c 100  format(1x,a4,1x,f8.4,1x,f9.4)
 100  format(1x,a5,1x,f8.4,1x,f9.4)
 103  format(a5,1x,f9.5,1x,f9.5,1x,f5.3,1x,f4.2,1x,f4.2,2x,i1)
 104  format(i4,1x,f5.2,1x,i4,1x,f5.2,1x,f7.2,i3)
           
c         rewind(10)
c         do j=1,1000
c            read(10,'(a27)') line
c            if(stations(i)(1:4).eq.line(3:6)) then
c               read(line(7:27),'(i2,f5.2,a1,i3,f5.2,a1,f4.0)') 
c     &              ilat, mlat, dlat, ilon, mlon, dlon, elev
c               elev=elev/1000.
c               dec_lon=real(ilon)+(mlon/60.0)
c               if(dlon.eq.'W') then
c                  dec_lon=-1.0*dec_lon
c               endif
c               dec_lat=real(ilat)+(mlat/60.0)
c               if(dlon.eq.'S') then
c                  dec_lat=-1.0*dec_lat
c               endif
c               write(3,100) stations(i), dec_lat, dec_lon
c               goto 11
c            endif
c         enddo
c         
c 11      continue
c      enddo
c
c      close(3)
c      close(10)
c
c      return
c
c 100  format(1x,a4,1x,f8.4,1x,f9.4)
c
c 98   write(6,*) 'Error opening station file ', station_file
c      error=.true.
c      return
 99   write(6,*) 'Error opening output file station.dat'
      error=.true.
      return
      end


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C Subroutine to read the velocity model from a STATION0.HYP file
C
C Requires the name of the file as an input argument
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine get_velocity_model(station_file,error)


      character*80 station_file    ! station file
      character*80 line            ! one line

      real xnear,xfar,startz,vpvs
      real vel(100), z(100)

      integer nlayers
      integer i

      logical error

      error=.false.
c     
c open input and output files
c
      open(10,FILE=station_file,STATUS='OLD',err=98)
      open(4,file='syn.vel',status='unknown',err=99)

      rewind(10)
c
c Find first blank line
c
 12   read(10,'(a)') line
      if (line(1:10).ne.'          ') then
         goto 12
      endif
c
c Find second blank line
c
 13   read(10,'(a)') line
      if (line(1:10).ne.'          ') then
         goto 13
      endif
c
c read velocity model
c
      i=0
      read(10,'(a)') line
 30   continue
      i=i+1
      read(line,'(2f8.3)') vel(i),z(i)
      read(10,'(a)') line
      if(line(1:10).ne.'          ') goto 30
      nlayers=i

c
c  vp/vs, start depth and distance weight
c
      read(10,'(3f5.0,f5.2)') startz,xnear,xfar,vpvs

c write velocity structure to output file
      do i=1,nlayers
         write(4,*) z(i), vel(i), vpvs
      enddo

      close(10)
      close(4)

      return

 98   write(6,*) 'Error opening station file ', station_file
      error=.true.
      return
 99   write(6,*) 'Error opening output file syn.vel'
      error=.true.
      return

      end