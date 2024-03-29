c
c  Program to make a jhd phase input file form a Nordic file
c
c  Brian Baptie, 2004
c
c changes
c
c   26/02/2007 lot fixed problem with finding station file on Linux
c   12/04/2021 lot adjusted to new nordic, but not tested
c              put in 5 char station codes, which has to be tested with jhd program
c
      implicit none
      include 'seidim.inc'
      include 'seisan.inc'
      include 'rea.inc'
      CHARACTER*80 DATA(2500)                                                  
      CHARACTER*80 infile                                                     
      CHARACTER*1 TYPE,EXP                                                      
      CHARACTER*5 stations(2500)                                                  
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
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver
      call get_seisan_def

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
      if(nars.gt.2) then        !JAB(BGS)Mar95. Definitely too many
         write(6,*)' Too many arguments'
         stop
      endif
c
c   output file
c
      open(2,file='syn.times',status='unknown')
      write(2,'(1x,''JHD Phase Input'')') 
c
c   get input file name
c
      infile=args(1)
      open(1,file=infile,status='old')
c
c   check that not a compact file
c
c      call nortype(1,compact)
c      if(compact) then
c         write(6,*)' Input file is compact, cannot be used'
c         stop
c      endif
c     
c   read and write to end of file
c
      n=0
      sta_list_count=0
 10   continue
c
c   read one event in nordic format
c
c      CALL INDATA(1,NSTAT,NPHASE,NHEAD,NRECORD,TYPE,EXP,DATA,ID)
c      if(nrecord.eq.0) goto 99
      call rea_event_in(1,.true.,data,code)
      if (code.ne.0) goto 99
c
c   convert phase readings to jhd format and write out 
c
c      call nor2jhd(2,n,nrecord,nhead,data,stations,sta_list_count)
      n=n+1
      call nor2jhd(2,n,stations,sta_list_count)
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
      call get_station_file(station_file,error)
      if (error) then
         stop
      endif
c     
c find the stations corresponding to those in the station list and
c write out jhd format stalist.elev file containing station details
c        
c      call write_station_output(station_file,
      call write_station_output(
     1     stations,sta_list_count,error)
      if (error) then
         stop
      endif
c
c extract velocity model parameters from the station file and
c write out jhd format syn.vel velocity model file
c       
      call get_velocity_model(station_file,error)
      if (error) then
         stop
      endif


      stop
      end
                       

c
c
      subroutine nor2jhd(unit,nevent,stations,sta_list_count)
	  
c
c   gets one event in nordic format in array data and writes it to unit
c   in hypoinverse format
c
      implicit none
      include 'seidim.inc'
      include 'rea.inc'
      include 'seisan.inc'
      character*80 data(max_phase)
      character*5 stations(*)
      integer sta_list_count
      integer nrecord,unit,nhead,nevent
      integer year,month,day,hour,min
      integer nstat
      integer p_weight, s_weight
      real head_sec, p_sec, p_diff, s_sec, s_diff
      real sec
      real lon,lat,depth
      double precision head_time,p_time, s_time ! abs times for header and phase
      integer j,k,l
      logical s_found
      logical station_in_list
      character*80 line(max_phase)

c
c   write header
c

      l=0 ! counter for station lines
c      read(data(1)(49:51),'(i3)') nstat 
c
c   read reference time, put sec to zero so all following times 
c   are referred to a whole minute
c
      sec=0.0
c      read(data(1),'(1x,i4,1x,2i2,1x,2i2,1x,f4.1)') 
c     *year,month,day,hour,min,head_sec
      year=hyp_year(1)
      month=hyp_month(1)
      day=hyp_day(1)
      hour=hyp_hour(1)
      min=hyp_min(1)
      head_sec=hyp_sec(1)
c
c   get header abs time
c
      call timsec(year,month,day,hour,min,head_sec,head_time)
c
c   read phases to end of event
c
c      do k=nhead+1,nrecord-1
      do k=1,rea_nphase

         s_found=.false.
c For each P-phase calculate the phase time relative to the origin time
c         if(data(k)(11:11).eq.'P'.or.data(k)(11:11).eq.'S') then
c         if(data(k)(11:11).eq.'P') then
         if(rea_phase(k)(1:1).eq.'P') then
c            read(data(k)(19:28),'(i2,i2,f6.3)') hour,min,p_sec
            hour=rea_hour(k)
            min=rea_min(k)
            p_sec=rea_sec(k)
c            read(data(k)(15:15),'(i1)') p_weight
            read(rea_weight_in(k),'(i1)') p_weight
            call timsec(year,month,day,hour,min,p_sec,p_time)
            p_diff=p_time-head_time ! P phase time relative to header time
            p_sec=head_sec+p_diff   ! S phase second relative to header second  

            station_in_list=.false.
            do j=1,sta_list_count
c               if(data(k)(2:5).eq.stations(j)(1:4)) then
               if(rea_stat(k)(1:5).eq.stations(j)(1:5)) then
                  station_in_list=.true.
               endif
            enddo
            if(.not.station_in_list) then
               sta_list_count=sta_list_count+1
               stations(sta_list_count)=' '
               stations(sta_list_count)(1:5)=rea_stat(k)(1:5)
            endif

c check for a matching S-phase at this station
            do j=1,rea_nphase
              if(rea_phase(j)(1:1).eq.'S'.and.
     &           rea_stat(j).eq.rea_stat(k)) then
                  s_found=.true.
c                  read(data(j)(19:28),'(i2,i2,1x,f5.2)') hour,min,s_sec
                  hour=rea_hour(j)
                  min=rea_min(j)
                  s_sec=rea_sec(j)
c                  read(data(j)(15:15),'(i1)') s_weight
                  read(rea_weight_in(k),'(i1)') s_weight
                  call timsec(year,month,day,hour,min,s_sec,s_time)
                  s_diff=s_time-head_time ! S phase time relative to header time
                  s_sec=head_sec+s_diff   ! S phase second relative to header second  
               endif
            enddo

c write out the jhd format phase output
            l=l+1
            if(s_found) then
                  write(line(l),100) rea_stat(k)(1:5),p_sec,p_weight,
     1                 s_sec, s_weight
            else
                  write(line(l),100) rea_stat(k)(1:5),p_sec,p_weight,
     1                 p_sec, 4
            endif

         endif
      enddo
      write(unit,'(1x,i3,1x,i3)') nevent, l
      do k=1,l
        write(unit,'(a)') line(k)
      enddo

c      read(data(1)(31:38),'(f8.3)') lon 
c      read(data(1)(24:30),'(f7.3)') lat
c      read(data(1)(39:43),'(f5.1)') depth 
      lon=hyp_lon(1)
      lat=hyp_lat(1)
      depth=hyp_depth(1)

c write out origin details
      write(unit,'(3x,f5.2,1x,f8.3,1x,f7.3,1x,f5.1)') head_sec,lon,lat,
     *depth

c
c   write blank line
c
      write(unit,*)'       '
      return

 100  format(1x,a5,1x,f6.2,1x,i1,1x,f6.2,1x,i1)
 101  format(1x,a5,1x,f6.2,1x,i1)
c 100  format(1x,' ''',a4,''' ',1x,f6.2,1x,i1,1x,f6.2,1x,i1)
c 101  format(1x,' ''',a4,''' ',1x,f6.2,1x,i1)

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
      logical flag
      integer seiclen

      error=.false.
      flag=.false.
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

      inquire(file=cur_file,exist=flag)
      if (.not.flag) goto 1
c      open(999,file=cur_file,status='old',err=1)
      station_file=cur_file
      goto 2
 1    continue	  
      inquire(file=dat_file,exist=flag)
      if (.not.flag) goto 3
c      OPEN(999,FILE=dat_file,STATUS='OLD',err=3)
      station_file=dat_file
      goto 2
 3    continue
      write(6,*)' No station file found'
      error=.true.

 2    continue
c      close(999)

      write(6,2001) station_file(1:seiclen(station_file))
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
      subroutine write_station_output(
     1     stations,sta_list_count,error)

      implicit none
      character*80 station_file
      CHARACTER*5 stations(*)
      integer sta_list_count
      character*80 line            ! one line

      integer ilat, ilon
      real dec_lat, dec_lon, mlat, mlon, elev
      character*1 dlat, dlon, file_ind

      integer i,j

      logical error
      
      error=.false.
      file_ind=' '
c
c open input and output files
c
c      open(10,FILE=station_file,STATUS='OLD',err=98)
      open(3,file='stalist.elev',status='unknown',err=99)

      write(3,*) '''Stations Used'''
      write(3,*) sta_list_count

      do i=1,sta_list_count
         call stat_loc(stations(i),file_ind,dec_lat,dec_lon,elev)
         elev=elev/1000.
         if (dec_lat.ne.0..and.dec_lon.ne.0.) then
             write(3,100) stations(i), dec_lat,dec_lon,elev
         else
           write(6,'(a)') ' station not found '//stations(i)
         endif

      enddo
 100  format(1x,a5,1x,f7.3,1x,f8.3,1x,f5.2,1x,'0',1x,'0')
c           
c         rewind(10)
c         do j=1,1000
c            read(10,'(a27)') line
c            if(stations(i)(1:4).eq.line(3:6)) then
c               read(line(7:27),'(i2,f5.2,a1,i3,f5.2,a1,f4.0)') 
c     1              ilat, mlat, dlat, ilon, mlon, dlon, elev
c               elev=elev/1000.
c               dec_lon=real(ilon)+(mlon/60.0)
c               if(dlon.eq.'W') then
c                  dec_lon=-1.0*dec_lon
c               endif
c               dec_lat=real(ilat)+(mlat/60.0)
c               if(dlon.eq.'S') then
c                  dec_lat=-1.0*dec_lat
c               endif
c
c               if(stations(i)(4:4).eq.' ') then
c                  write(3,100) stations(i)(1:3), dec_lat,dec_lon, elev 
c               else
c                  write(3,100) stations(i)(1:4), dec_lat,dec_lon, elev 
c               endif
c               goto 11
c            endif
c         enddo
c         
c 11      continue
c      enddo

      close(3)
c      close(10)

      return

c 100  format(1x,a4,1x,f7.3,1x,f8.3,1x,f5.2,1x,'0',1x,'0')
c 100  format(1x,' ''',a4,''' ',1x,f7.3,1x,f8.3,1x,f5.2,1x,'0',1x,'0')

c 98   write(6,*) 'Error opening station file ', station_file
c      error=.true.
c      return
 99   write(6,*) 'Error opening output file stalist.elev'
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
