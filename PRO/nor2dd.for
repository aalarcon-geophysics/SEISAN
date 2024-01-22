c
c  Program to make a hypodd phase input file from a Nordic file
c
c  Brian Baptie, 2004
c

c
c changes:
c  02 nov 2011 wc - changed to allow for negative magnitude
c  03 Nov 2011 lo - added high accuracy reading
c  16 Mar 2016 fh - change seconds and minutes when they are 60
c  29 may 2019 jh - new format, some includes only
c  11 feb 2020 jh - Optiopnal if phase P or PG can be used, if weight gt 4 , 
c                   assume zero weight, 
c                 - do not use s-p phases,
c                 - use 5 char station names for phases instead of 4,
c                 - station file also to 5 chars,
c                 - use rea structure instead of local variables,
c                 - remove double entries of same phase and station
c                   and only use earliest.
c                 - optionally enter weights for given stations
c  21 dec 2020 lo - possibly use g or n phase if in distance range, when
c                   first only phase selected
c

      implicit none
      include 'seidim.inc'
      include 'seisan.inc'
      include 'rea.inc'
      CHARACTER*80 DATA(2500)                                                  
      CHARACTER*80 infile                                                   
      CHARACTER*5 stations(2500)  
      character*5 stat_weight(1000) ! station with different weight
      real weight(1000)             ! weights to assign
      integer n_stat_weight         ! number of stations                                                               
      CHARACTER*1 TYPE,EXP                                                      
      character*80 station_file
      character*180 text
      integer nstat,nphase,nhead,nrecord
c --- variabels for readin org. file
      integer year(50000),month(50000),day(50000),hour(50000),
     *        min(50000),nin
      real sec(50000),lat(50000),lon(50000),depth(50000),mag(50000)
c
      integer sta_list_count    ! number of stations in data set
      logical all_phase         ! if true use any P or S
c---number of arguments 
      integer nars
c-- arguments
      character*80 args(10)
      integer i,n,id, conv,code,k
c--compact or not
      logical compact
      logical error
      real min_n,max_g   ! min distance to assume Pn/Sn is correct
c                          first, max dist for Pg/Sg first

c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

      error=.false.

      call get_seisan_def
c
c   check if an argument, on 'n' is accept to convert back to nordic
c   using stabdard hypoDD.reloc
c
      call get_arguments(nars,args)
      if(nars.ge.0) then 
         if(args(1).eq.'n') then
            open(2,file='hypodd.out',status='unknown')
            open(1,file='hypoDD.reloc', status='old')
            goto 100
         endif
      endif
      
     
 25   continue
      write(6,*)
     *' Convert from Nordic to hypodd (1=enter) or hypodd to Nordic (2)'
      read(5,'(a)') text
      if(text.eq.' ') then 
         conv=1
      else
         read(text,*) conv
      endif
      if(conv.ne.1.and.conv.ne.2) goto 25

 26   continue

      if(conv.eq.1) write(6,*)' Give input Nordic file'
      if(conv.eq.2) write(6,*)
     *' Give input hypodd file, hypoDD.reloc is default (enter)'
      read(5,'(a)') infile
 


      

      if(conv.eq.2.and.infile.eq.' ') infile='hypoDD.reloc'
      if(infile.eq.' ') goto 26 
c
      if(conv.eq.1) then
         write(6,*) ' Use any P and S-phase (enter) or only P and S (o)'
         read(5,'(a)') text
         if(text.eq.' ') then
           all_phase=.true.
           max_g=9999.
           min_n=0.
         else
           all_phase=.false.
           write(*,*) ' Minimum distance for Pn/Sn to be valid (km): '
           read(5,*) min_n
           write(*,*) ' Maximum distance for Pg/Sg to be valid (km): '
           read(5,*) max_g
         endif
      endif
      
c
c   output file
c
      if(conv.eq.1) open(2,file='phase.dat',status='unknown')
      if(conv.eq.2) open(2,file='hypodd.out',status='unknown')
c
c   open input file name
c
      open(1,file=infile,status='old')

      if(conv.eq.2) goto 100
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   nordic to hypodd
cccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c   check that not a compact file
c
      call nortype(1,compact)
      if(compact) then
         write(6,*)' Input file is compact, cannot be used'
         stop
      endif
c
c   optinally enter individual station weights
c
      k=0
      write(6,*) 
     *' Stations and weights like AAAA 0.5, one per line'
      write(6,*)
     *' weights are 0-1 or -9 to force use, enter to terminate'
 28   continue
      read(5,'(a)') text
      if(text.eq.' ') goto 29
      k=k+1
      i=index(text,' ')
      stat_weight(k)=' '
      stat_weight(k)(1:i-1)=text(1:i-1)
      text(1:i-1)=' '
      read(text,*,err=33,end=33) weight(k)
      goto 28
 33   continue
      write(6,*)
     *' You must enter a number after the station code like AAAA 0.5'
      k=k-1
      goto  28
 29   continue
      n_stat_weight=k     
c     
c   read and write to end of file
c
      n=0
 10   continue
c
c   read one event in nordic format
c
      CALL INDATA(1,NSTAT,NPHASE,NHEAD,NRECORD,TYPE,EXP,DATA,ID)
      if(nrecord.eq.0) goto 99


c
c   convert phase readings to hypodd input format and write out 
c
      call nor2dd(2,n+1,nrecord,nhead,data,stations,
     *sta_list_count,all_phase,n_stat_weight,stat_weight,weight,
     *min_n,max_g)
      n=n+1

      goto 10   ! next event



 99   continue
      write(6,*)
      write(6,*)' The input file had ',n,' events'
      write(6,*)' Output phases in file phase.dat'
      write(6,*)' Output of station list in station.dat'
        
      close(1)
      close(2)


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
c

       goto 550
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   hypodd to nordic
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c

 100  continue

      n=0
c
c   original hypocenters
c
      open(3,file='hypoDD.loc',status='old')

 200  continue
      read(3,'(a)',end=300) text
      n=n+1
      read(text(10:42),*) lat(n),lon(n),depth(n)
      read(text(104:130),*) year(n),month(n),day(n),
     *hour(n),min(n),sec(n),mag(n)
      goto 200
 300  continue
      write(6,*)' Number of events in original file',n
      nin=n
      n=0
      close(3)
c
c   file for original events correpsonding to relocated events'
c
      open(3,file='hypodd.org',status='unknown')

 500  continue

      read(1,'(a)',end=530) text
      n=n+1     
c
c   clear variables
c
      rea_nphase=0      ! count number of phase lines
      rea_nhyp=1        ! count number of hypocenters
      rea_nwav=0        ! --------------- waveform files
      rea_ncomment=0    ! --------------- comment lines
      rea_nmacro=0      ! --------------- macroseismic data
      rea_nfault=0      ! --------------- fault plane solutions
      rea_nspec=0       ! --------------- spectral solutions
      rea_nmag=0        ! --------------- magnitudes
      rea_id_line=' '   ! no id line
      rea_locality=' '
      call rea_hyp_clear(1)

      read(text(11:20),'(f10.3)') hyp_lat(1)
      read(text(22:32),'(f11.3)') hyp_lon(1)
      read(text(35:42),'(f8.3)') hyp_depth(1)
      hyp_dist_id(1)='L'
      read(text(104:126),'(i4,4i3,f7.3)') 
     *hyp_year(1),hyp_month(1),hyp_day(1),hyp_hour(1),
     *hyp_min(1),hyp_sec(1)
      read(text(128:131),'(f4.1)') hyp_mag(1,1)

      call rea_event_out(2,.true.,data,code)
c
c   find original location and write out
c
      do i=1,nin

c         write(6,*) hyp_year(1),year(i),hyp_month(1),month(i),
c     *   hyp_day(1),day(i),hyp_hour(1),hour(i),
c     *   hyp_min(1),min(i),
c     *   hyp_sec(1),sec(i)

         if(hyp_year(1).eq.year(i).and.hyp_month(1).eq.month(i).
     *   and.hyp_day(1).eq.day(i).and.hyp_hour(1).eq.hour(i).
     *   and.hyp_min(1).eq.min(i).and.
     *   abs(hyp_sec(1)-sec(i)).lt.2.0) then

c
c   clear variables
c
         rea_nphase=0      ! count number of phase lines
         rea_nhyp=1        ! count number of hypocenters
         rea_nwav=0        ! --------------- waveform files
         rea_ncomment=0    ! --------------- comment lines
         rea_nmacro=0      ! --------------- macroseismic data
         rea_nfault=0      ! --------------- fault plane solutions
         rea_nspec=0       ! --------------- spectral solutions
         rea_nmag=0        ! --------------- magnitudes
         rea_id_line=' '   ! no id line
         rea_locality=' '
         call rea_hyp_clear(1)
 
         hyp_lat(1)=lat(i)
         hyp_lon(1)=lon(i)
         hyp_depth(1)=depth(i)
         hyp_dist_id(1)='L' 
         hyp_year(1)=year(i)
         hyp_month(1)=month(i)
         hyp_day(1)=day(i)
         hyp_hour(1)=hour(i)
         hyp_min(1)=min(i)
         hyp_sec(1)=sec(i)
         hyp_mag(1,1)=mag(i)

         call rea_event_out(3,.true.,data,code)
         goto 505
         endif
      enddo


 505  continue
c

      goto 500

 530  continue
      write(6,*)' Number of events in relocated file ',n
      write(6,*)' Output file of relocated events is hypodd.out'
      write(6,*)
     *' Output file of corresponding original events is hypodd.org'
      close(1)
      close(2)
      close(3)

 550  continue



      stop
      end
                       
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine nor2dd(unit,nevent,nrecord,nhead,data,stations,
     &     sta_list_count,all_phase,n_stat_weight,stat_weight,weight,
     &     min_n,max_g)
	  
c
c   gets one event in nordic format in array data and writes it to unit
c   in hypdd format
c
      implicit none
      include 'seidim.inc'
      include 'rea.inc'

      character*80 data(*)                                                  
      character*5 stations(*),stat
      integer sta_list_count
      integer nrecord,unit,nhead,nevent
      integer year,month,day,hour,min
      integer p_weight
      real min_n,max_g
      real head_sec, p_sec, p_diff
      real lon,lat,depth
      real out_weight
      character*5 stat_weight(*) ! station with different weight
      real weight(*)             ! weights to assign
      integer n_stat_weight      ! number of stations     
      character*5 sta(5000)      ! saving station for one event 
      real diff(5000)            ! saving diff----------------
      real wt(5000)              ! saving wt ------
      character*1 pha(5000)      ! saving phase

      double precision head_time,p_time ! abs times for header and phase
      integer i,j,k,kk
      logical station_in_list,all_phase
      character*1 c
      real rain
      integer ain
c
c write out origin details
c
      if(hyp_lat_err(1).eq.-999.0) hyp_lat_err(1)=0.0
      if(hyp_depth_err(1).eq.-999.0) hyp_depth_err(1)=0.0

c
c   assume horizontal error is the same as lat error
c      
      if(hyp_lat_err(1).gt.99.9) hyp_lat_err(1)=9.9        ! only room for 3 digits in output  
      if(hyp_depth_err(1).gt.99.9) hyp_depth_err(1)=9.9
      if(abs(hyp_rms(1)).gt.99.9) hyp_rms=99.0
      if(hyp_mag(1,1).eq.-999.0) hyp_mag(1,1)=0.0
 
      write(unit,101) hyp_year(1),hyp_month(1),hyp_day(1),hyp_hour(1),
     *hyp_min(1),hyp_sec(1),hyp_lat(1),hyp_lon(1),
     *hyp_depth(1),hyp_mag(1,1),
     *hyp_lat_err(1),hyp_depth_err(1),hyp_rms(1),nevent

c      write(unit,*) hyp_year(1),hyp_month(1),hyp_day(1),hyp_hour(1),
c     *hyp_min(1),hyp_sec(1),hyp_lat(1),hyp_lon(1),
c     *hyp_depth(1),hyp_mag(1,1),
c     *hyp_lat_err(1),hyp_depth_err(1),hyp_rms(1),nevent



 101   format('#',1x,i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,f4.1,f8.4,    ! changed precision to 4 digits after . for lat and lon
     1     f9.4,1x,f5.1,1x,f4.1,1x,f4.1,1x,f4.1,1x,f5.1,1x,i5)   ! lo 2 June 2022
c     1     f9.4,1x,f5.1,1x,f4.1,1x,f4.1,1x,f4.1,1x,f5.1,1x,i4)
c
c   get header abs time
c
       call timsec(hyp_year(1),hyp_month(1),hyp_day(1),
     * hyp_hour(1),hyp_min(1),hyp_sec(1),head_time)

c
c   weight out s-p
c
      do i=1,rea_nphase
          if(rea_weight_in(i).eq.'9') then    ! found s-p
              stat=rea_stat(i)
              do k=1,rea_nphase              ! null out all other phases for station
                 if(rea_stat(k).eq.stat) rea_weight_in(k)='4'
              enddo
          endif
      enddo
c                  
c------------------------------------------
c   read and process phases to end of event
c------------------------------------------
c
      kk=0
      do k=1,rea_nphase
c
c For each P and/or S-phase calculate the phase time relative to the origin time
c
c   

         if(all_phase) then
c
c   can be any P or S, like PG
c
            if(rea_phase(k)(1:1).eq.'P'.or.rea_phase(k)(1:1).eq.'S')
     *      then
              goto 222
            endif
         else
c
c first arrivals only, have to be just P or S
c
ccc lo          if(rea_phase(k)(1:2).eq.'P '.or.
ccc     *       rea_phase(k)(1:2).eq.'S ') then
            if(rea_phase(k)(1:2).eq.'P '.or.
     &         rea_phase(k)(1:2).eq.'S '.or.
     &         (rea_dist(k).le.max_g.and.(rea_phase(k)(1:2).eq.'Pg'
     &         .or.rea_phase(k)(1:2).eq.'Sg')).or.
     &         (rea_dist(k).ge.min_n.and.(rea_phase(k)(1:2).eq.'Pn'
     &         .or.rea_phase(k)(1:2).eq.'Sn'))) then
c        write(*,*) ' debug lo ',rea_stat(k),rea_phase(k),rea_dist(k)
                 goto 222
            endif
         endif
         goto 223    ! skip this phase

 222     continue


            write(*,*) ' debug ',
     &     rea_stat(k),rea_phase(k),rea_weight_in(k)
            read(rea_weight_in(k),'(i1)') p_weight   ! pr or s phase
            if(p_weight.eq.0) then
               out_weight=1.0
            else if(p_weight.eq.1) then
               out_weight=0.75
            else if(p_weight.eq.2) then
               out_weight=0.5
            else if(p_weight.eq.3) then
               out_weight=0.25
            else if(p_weight.ge.4) then
               out_weight=0.0
            endif


            p_diff=rea_abs_time(k)-head_time ! phase time relative to header time
c
c write out the formatted phase output
c
            station_in_list=.false.
            do j=1,sta_list_count
               if(rea_stat(k).eq.stations(j)) then
                  station_in_list=.true.
               endif
            enddo
            if(.not.station_in_list) then
               sta_list_count=sta_list_count+1
               stations(sta_list_count)(1:5)='     '
               stations(sta_list_count)=rea_stat(k)
            endif

            kk=kk+1

            sta(kk)=rea_stat(k)
            diff(kk)=p_diff
            wt(kk)=out_weight
            pha(kk)=rea_phase(k)

c         endif   ! was bug lo, 21/12/2020
 223     continue
      
c
c   end of phase loop
c
      enddo
c
c   check if more then one P or S for a particular station 
c   then weight out the latest arrival
c
      do i=1,kk
         do j=1,kk
            if(sta(i).eq.sta(j).and.pha(i).eq.pha(j).and.
     *      wt(i).gt.0.0.and.wt(j).gt.0.0.and.j.ne.i) then
c
c  find first arrival
c

               if(diff(j).gt.diff(i)) then
                  wt(j)=0.0
              else
                  wt(i)=0.0
              endif
           endif
        enddo
      enddo

c
c   write out and give weights
c

      do i= 1,kk
         if(n_stat_weight.gt.0) then
            do k=1,n_stat_weight
               if(sta(i).eq.stat_weight(k)) wt(i)=weight(k)
            enddo
         endif
         write(unit,100) sta(i),diff(i),wt(i),
     *   pha(i)
      enddo
      
      return


 100  format(a5,1x,f6.2,1x,f4.1,1x,a1,a1)

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

      integer ilat, ilon
      real dec_lat, dec_lon, mlat, mlon, elev,el
      character*1 dlat, dlon, file_ind

      integer i,j

      logical error
      
      error=.false.
c
c open input and output files
c
      open(3,file='station.dat',status='unknown',err=99)

      file_ind=' '
      do i=1,sta_list_count
         call stat_loc(stations(i),file_ind,dec_lat,dec_lon,el)
         if (dec_lat.ne.0..and.dec_lon.ne.0.) then
           write(3,100) stations(i), dec_lat, dec_lon
         else
           write(6,'(a)') ' station not found '//stations(i)
         endif
      enddo
      return
 100  format(1x,a5,1x,f8.4,1x,f9.4)
           

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
