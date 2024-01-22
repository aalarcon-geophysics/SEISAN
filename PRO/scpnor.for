c
c  find events in seiscomp data base, extract and convert to nordic
c
c  jh feb 2023
c
c  changes:
c 
c 2023 03 10 jh: last version
c
      implicit none

      include 'seidim.inc'               ! dimensions for rea block
      include 'seisan.inc'               ! dimensions for rea block
      include 'rea.inc'                  ! parameter common block

      character*80 data(max_data)        ! s-file with data in text array
      character*80 parfile               ! parameter file
      character*80 arg(5)                ! arguments to  program
      integer nargs                      ! number of arguments

      Character*160 text
      integer seiclen                    ! function
      character*60 top_directory         ! seisan top dir
      integer seiscomp_version           ! seiscomp version 3,4 or 5
      real*8 msec,msec1,msec2            ! abs times
      integer year,month,day,doy,hour,min,isec
      real sec
      integer year1,month1,day1,doy1,hour1,min1,isec1
      real sec1
      integer year2,month2,day2,doy2,hour2,min2,isec2
      real sec2
      real instant_time_back             ! minutes to go back from real time
      real xtime                         ! time interval to read
      character*21 first,last            ! time interval in text strings
      character*40 sql_user              ! data base user
      character*40 sql_pass              ! data base password
      character*40 sql_ip                ! ip of data base
      character*40 sql_database          ! data base name of sql
      real wa_gain                       ! wa gain
      logical insert_arc_line            ! if true,  arc line
      character*3 agency                 ! agency
      character*20 id(100000)            ! event id's
      character*12 p_time                ! system time
      character*14 proc_time             ! system time
      character*3 operator               ! operator for s-file
      character*5 base,second_base       ! base for s-file
      character*80 sfil_name             ! s file name
      character*80 second_sfil_name      ! second s file name
      integer nf                         ! length of--
      logical make_sfile                 ! if true, make s-file
      logical sfile_written              ! true if s-file already wrtitten
      logical instant                    ! take data now
      integer instant_nevent             ! number of instant events
      character*220 instant_event(1000)  ! info on instant events
      integer code                       ! error code for rea output
      integer i,n,k,m,l,k1,in,kk         ! counters

c
c   get seisan defaults including names of arc channels
c
      call get_seisan_def

c
c   check s-file format
c
      if(new_nordic_format) then
         rea_new_out=.true.              ! write new format
      endif 
c                                                                               
c   get path to seismo                                                          
c                                                                               
      call topdir(top_directory)              

c
c   get arguments
c
      call get_arguments(nargs,arg)

      if(nargs.gt.0) then
         do i=1,nargs
            if(arg(i)(1:78).eq.'instant') then
               instant=.true.
            endif
         enddo
      endif
c

c
c  read parameter file, for now scpnor.par
c
      parfile='scpnor.par'

      call get_scpnor_def(in,parfile,
     *sql_user,sql_pass,sql_ip,sql_database,
     *base,second_base,agency,operator,instant_time_back,
     *make_sfile,seiscomp_version, wa_gain,insert_arc_line)

      write(6,'(a,i1)')      'seiscomp version   ',seiscomp_version
      write(6,'(a,a)')       'sql user:          ',sql_user
      write(6,'(a,a)')       'sql pass:          ',sql_pass
      write(6,'(a,a)')       'sql ip  :          ',sql_ip
      write(6,'(a,a)')       'sql data base      ',sql_database
      write(6,'(a,l1)')      'make s-file        ',make_sfile
      write(6,'(a,l1)')      'insert ARC line    ',insert_arc_line
      write(6,'(a,f6.1)')    'wa gain            ',wa_gain        
      write(6,'(a,a)')       'sfile data base    ',base
      write(6,'(a,a)')       'sfile 2. data base ',second_base
      write(6,'(a,a,1x,a)' ) 'agency operator    ',agency,operator
      write(6,'(a,f6.1)')    'instant time back  ',instant_time_back
      write(6,*)

      call gmt_time(year,month,day,hour,min,sec,msec)
c
c   file with instant detections
c   the file will be in working directory which will also
c   be in the directory where the program is started from if a cron job
c
      if(instant) then
         open(8,file='scpnor.instant',status='unknown')
         k=1
 5       continue
         read(8,'(a)',end=6) instant_event(k)
         if(instant_event(k).eq.' ') goto 6
         k=k+1
         goto 5
 6       continue
         instant_nevent=k-1
         write(6,*) 'number of instant events',instant_nevent
         close(8)
      endif

 

      open(3,file='scpnor.out',status='unknown')
      if(.not.instant) then

          write(6,*)'Give start time, at least year, yyymmddhhmmss'
          read(5,'(a)') text
          read(text,'(i4,5i2)')year1,month1,day1,hour1,min1,isec1
          if(day1.eq.0) day1=1
          if(month1.eq.0) month1=1
          sec1=isec1
          call timsec(year1,month1,day1,hour1,min1,sec1,msec1)
 7        continue
          write(6,*)'Give end time, at least year, month and day,',
     *    ' yyyymmddhhmmss'
          write(6,*)'or give number of hours from start like 2 or 0.2'
          read(5,'(a)') text
          if(text.eq.' ') goto 7
          read(text,*) xtime
          if(xtime.lt.2000.0) then
             msec2=msec1+xtime*3600.0
             call sectim(msec2,year2,doy2,month2,day2,hour2,min2,sec2)
          else
             read(text,'(i4,5i2)')year2,month2,day2,hour2,min2,isec2
             sec2=isec2
          endif
      else
c
c   find initial start time, real_time_delay after real time
c
         call gmt_time(year,month,day,hour,min,sec,msec)        
         call sectim(msec-instant_time_back*60.0,
     *   year1,doy1,month1,day1,hour1,min1,sec1)
         call sectim(msec+300.0,year2,doy2,month2,day2,hour2,min2,sec2)
      endif
   
c
c   make time strings
c
       first(1:1)='"'
       write(first(2:5),'(i4)') year1
       first(6:6)='-'
       write(first(7:8),'(i2.2)') month1
       first(9:9)='-'
       write(first(10:11),'(i2.2)') day1
       first(12:12)=' '
       write(first(13:14),'(i2.2)') hour1
       first(15:15)=':'
       write(first(16:17),'(i2.2)') min1
       first(18:18)=':'
       write(first(19:20),'(i2.2)') isec1
       first(21:21)='"'
       write(6,'(a,a)') 'begin: ',first
       
       last(1:1)='"'
       write(last(2:5),'(i4)') year2
       last(6:6)='-'
       write(last(7:8),'(i2.2)') month2
       last(9:9)='-'
       write(last(10:11),'(i2.2)') day2
       last(12:12)=' '
       write(last(13:14),'(i2.2)') hour2
       last(15:15)=':'
       write(last(16:17),'(i2.2)') min2
       last(18:18)=':'
       write(last(19:20),'(i2.2)') isec2
       last(21:21)='"'
       write(6,'(a,a)') 'end:   ',last

c
c   get addresses of events in time interval using seiscom command
c
       text=' '
       text='scevtls -d mysql://'
     *//sql_user(1:seiclen(sql_user))
     *//':'//sql_pass(1:seiclen(sql_pass))//'@'
     *//sql_ip(1:seiclen(sql_ip))//'/'//
     *sql_database(1:seiclen(sql_database))//' --begin '//first//' '
     *//'--end '
     *//last//'  > newids.txt'
      write(6,'(a)') text
      call systemc(text,seiclen(text))
c
c   read the event id's
c
      open(1,file='newids.txt',status='old')
      n=0
 10   continue
      n=n+1
      read(1,'(a)',end=20) id(n)
      if(id(n).eq.' ') goto 20
      goto 10
 20   continue

      n=n-1
      write(6,*) 'Number of event ids',n
      if(n.eq.0) goto 100
c
c   read the data using the id's and write out in Nordic format
c   use a version of seiscvomp scbulletin fixed to give addional data
c   therea re 2 versions, scbul4 and scbul3 for seiscomp 4 and 5 and
c   seiscomp 3
c
      do i=1,n
         text=' '
         text='scbul4.py -d mysql://'
     *   //sql_user(1:seiclen(sql_user))
     *   //':'//sql_pass(1:seiclen(sql_pass))//'@'
     *   //sql_ip(1:seiclen(sql_ip))//'/seiscomp -E '
     *   //id(i)(1:seiclen(id(i)))//' -3 -e -p > event'
         if(seiscomp_version.eq.3) text(6:6)='3'
         write(6,'(a)') text
         call systemc(text,seiclen(text))
c
c   open file from seiscomp and read
c
         call rea_main_clear
         call rea_hyp_clear(1)
         rea_nmag=0
         open(2,file='event',status='old',err=100)

c
c   id
c
         rea_comment(1)=' '
         rea_comment(1)(2:32)='SEISCOMP ID: '//id(i)(1:19)
         rea_comment(1)(40:42)=agency
         rea_comment(1)(80:80)='3'
         rea_ncomment=1

  25     continue
         read(2,'(a)',end=100) text
c
c   locality
c
         if(text(7:12).eq.'region')then
            rea_locality=' '
            rea_locality=text(20:80)
            goto 26
         endif
         goto 25 
 26      continue

c
c  date and time
c
         read(2,'(a)') text
         read(2,'(a)') text
         read(text(28:37),'(i4,1x,i2,1x,i2)') year,month,day
         read(2,'(a)') text
         read(text(28:39),'(i2,1x,i2,2x,f6.3)') hour,min,sec
         isec=sec
         rea_nhyp=1
         rea_nphase=0
         hyp_year(1)=year
         hyp_month(1)=month
         hyp_day(1)=day
         hyp_hour(1)=hour
         hyp_min(1)=min
         hyp_sec(1)=sec
c
c   location etc
c
         hyp_agency(1)=agency
         hyp_dist_id(1)='R'
         read(2,'(a)') text
         read(text(28:36),*) hyp_lat(1)
         read(2,'(a)') text
         read(text(28:36),*) hyp_lon(1)
         read(2,'(a)') text
         read(text(28:36),*) hyp_depth(1)
         read(2,'(a)') text
         read(2,'(a)') text
         read(2,'(a)') text
         read(2,'(a)') text
         read(text(28:36),*) hyp_rms(1)
         read(2,'(a)') text
         read(text(28:36),*) hyp_gap(1)

c
c   magnitudes
c
         read(2,'(a)') text
         read(2,'(a)') text
 27      continue
         read(2,'(a)') text
         if(text(5:5).eq.'m'.or.text(5:5).eq.'M') then
            if(text(6:6).eq.'b') then
               rea_nmag=rea_nmag+1
               hyp_mag_type(rea_nmag,1)='b'
               read(text(14:18),*) hyp_mag(rea_nmag,1)
               hyp_mag_agency(rea_nmag,1)=agency
            endif
            if(text(6:6).eq.'B') then

               rea_nmag=rea_nmag+1
               hyp_mag_type(rea_nmag,1)='B'
               read(text(14:18),*) hyp_mag(rea_nmag,1)
               hyp_mag_agency(rea_nmag,1)=agency
            endif
            if(text(5:7).eq.'MLv') then 
               rea_nmag=rea_nmag+1
               hyp_mag_type(rea_nmag,1)='L'
               read(text(14:18),*) hyp_mag(rea_nmag,1)
               hyp_mag_agency(rea_nmag,1)=agency
            endif
            goto 27
         endif
c
c   id line
c
         hyp_dist_id(1)='L'
         rea_id_line(1:40)=
     *   ' ACTION:                   OP:     STATU'
         rea_id_line(41:80)=
     *   'S:               ID:               L   I'
         write(rea_id_line(61:74),'(i4,5I2)')
     *   year,month,day,hour,min,isec
         do l=61,74
            if(rea_id_line(l:l).eq.' ')
     *      rea_id_line(l:l)='0'
         enddo
         call systime(p_time,proc_time)
         write(rea_id_line(31:34),'(a)')operator(1:3)
         write(rea_id_line(13:26),'(a)')proc_time
         rea_id_line(9:11)='NEW'
c
c   phases
c
         k=0
 28      continue
         read(2,'(a)',end=50) text
         if(text(5:7).eq.'sta') then
         
c
c   number of phases
c
           backspace 2
           backspace 2
           read(2,'(a)') text
           do l=1,10
            if(text(l:l).eq.'P') m=l
           enddo
           if(text(1:m-1).ne.' ') read(text(1:m-1),*) hyp_nstat(1)
           read(2,'(a)') text
           goto 30
         endif

         goto 28
c
c   phases
c
 30      continue
         
         read(2,'(a)',end=50) text
         if(text.eq.' ') goto 45
         if(text(5:9).ne.' ') then
            k=k+1
            call rea_phase_clear(k)
            rea_stat(k)=text(5:9)
            rea_network(k)=text(11:12)
            rea_agency(k)=agency
            rea_operator(k)=operator
            rea_com(k)=text(16:18)
            read(text(45:56),'(i2,1x,i2,1x,f6.3)')
     *      rea_hour(k),rea_min(k),rea_sec(k)
            rea_year(k)=hyp_year(1)
            rea_month(k)=hyp_month(1)
            rea_day(k)=hyp_day(1)
            rea_phase(k)=text(37:44)
            read(text(19:28),*) rea_dist(k)
            rea_dist(k)=rea_dist(k)*111.2
            read(text(30:34),*) rea_az(k)
            read(text(57:64),*) rea_res(k)
            goto 30
         endif

 45      continue

c        c
c   make arc reference
c
         if(insert_arc_line) then
               rea_nwav=1
               rea_wav(1)=' '
               write(rea_wav(1)(22:38),'(i4,1x,2i2,1x,2i2,1x,i2)')
     *         year,month,day,hour,min,isec
               write(rea_wav(1)(39:44),'(a6)') '1200'
               rea_wav(1)(80:80)='6'
               rea_wav(1)(1:6)= ' ARC *'
         endif
c
c   amp and mag
c

         read(2,'(a)',end=50) text
         if(text(5:7).ne.'sta') then
             goto 45
         else
 46         continue
            read(2,'(a)',end=50) text
            if(text.ne.' ') then
               k=k+1
               call rea_phase_clear(k)
               rea_phase(K)='AMP'
               if(text(37:38).eq.'mb') rea_phase(k)='IAmb'
               if(text(37:39).eq.'MLv') rea_phase(k)='IAML'
               if(text(37:38).eq.'mB') rea_phase(k)='IVmB_BB'
               rea_stat(k)=text(5:9)
               rea_network(k)=text(11:12)
               rea_com(k)=text(16:18)
               rea_agency(k)=agency
               rea_operator(k)=operator
               read(text(49:54),*) rea_res(k)
               read(text(30:34),*) rea_az(k)
               read(text(20:28),*) rea_dist(k)
               rea_dist(k)=rea_dist(k)*111.2
               read(text(30:34),*) rea_az(k)
               read(text(49:54),*) rea_res(k)
               if(text(82:91).ne.' ') read(text(82:91),*) rea_amp(k)
               if(text(92:96).ne.' ') read(text(92:96),*) rea_per(k)
c
c   seisan must have a period, assume no surface wave and use 1.0 s
c
               if(rea_amp(k).gt.0.0.and.rea_per(k).lt.0.0) then
                   rea_per(k)=1.0
               endif
c
c   if ML, convert from mm to nm
c
               if(rea_phase(k).eq.'IAML')
     *         rea_amp(k)=(rea_amp(k)/wa_gain)*1000000.0
               read(text(58:80),'(i4,1x,i2,1x,i2,1x,i2,1X,i2,1x,f6.3)')
     *         rea_year(k),rea_month(k),rea_day(k),rea_hour(k),
     *         rea_min(k),rea_sec(k)
               goto 46
            endif
         endif
 50      continue
         rea_nphase=k
  999    continue       
c
c   write out nordic file in output file with all events
c             
         call rea_event_out(3,.true.,data,code)
         close(2)
c
c   make s-file name 
c

         call sfilname
     *   (hyp_year(1),hyp_month(1),hyp_day(1),hyp_hour(1),
     *   hyp_min(1),isec,base,'L',sfil_name,nf)

c
c   output in regulare data base
c

         if(make_sfile.and..not.instant) then
            write(*,*) ' first sfile:   ',sfil_name
            open(88,file=sfil_name,status='unknown',err=55)
            goto 60
 55         continue
            write(6,*)' Data base sub directory not made'
            stop
 60         continue
            call rea_event_out(88,.true.,data,code)
            close(88)
         endif
c
c----------------------------------------------------------------------
c  all  the following is for instant operation in events loop
c  this is intened for continues collection of data using a cron job.
c  the data collection will go back instant_time_back and check for
c  events. if event is already written it will skip a new write. 
c  earlier versions of the event will be deleted.
c----------------------------------------------------------------------

         if(instant) then
c
c   check if file already has been written out,
c   if so do not write or add file to instant list
c
            sfile_written=.false.
            if(instant_nevent.ge.1) then
               do k=1,instant_nevent
                  if(instant_event(k)
     *            (1:seiclen(instant_event(k)(1:80))).
     *            eq.sfil_name(1:seiclen(sfil_name))) then
                     sfile_written=.true.
                     write(6,*)'sfile already written'
                  endif
               enddo
            endif
c
c   output in data base if not written
c

            if(make_sfile.and..not.sfile_written) then
               write(*,*) ' first sfile:   ',sfil_name
               open(88,file=sfil_name,status='unknown',err=155)
               goto 160
 155           continue
               write(6,*)' Data base sub directory not made'
               stop
 160           continue
               call rea_event_out(88,.true.,data,code)
               close(88)
            endif
c
c   output in second data base where only data
c   instant time back are kept, only if set.
c
            if(make_sfile.and.second_base.ne.' '.
     *      and..not.sfile_written) then
               call sfilname
     *         (hyp_year(1),hyp_month(1),hyp_day(1),hyp_hour(1),
     *         hyp_min(1),isec,second_base,'L',second_sfil_name,nf)
               write(*,*) ' second sfile:  ',second_sfil_name
               open(88,file=second_sfil_name,status='unknown',err=57)
               goto 58
 57            continue
               write(6,*)' Data base sub directory not made'
               stop
 58            continue
               call rea_event_out(88,.true.,data,code)
               close(88)
            endif

c
c   save instant info if sfile has not been written before
c
           if(.not.sfile_written) then
c
c   first check if this event has an earlier version, if so
c   mark it for deletion.
c   if new, add to list
c
               do k=1,instant_nevent
                  if(id(i).eq.instant_event(k)(81:100)) then
                     instant_event(k)(120:120)='d'
                     write(6,*) 'id duplicated',instant_event(k)
                   endif
               enddo
               instant_nevent=instant_nevent+1
               instant_event(instant_nevent)=' '
c
c   save name of new event in main data base
c

               instant_event(instant_nevent)(1:seiclen(sfil_name))=
     *         sfil_name(1:seiclen(sfil_name))
c
c   save name of file in second data base if used
c
               if(second_base.ne.' ') then
                  instant_event(instant_nevent)
     *            (130:seiclen(second_sfil_name)+129)=
     *            second_sfil_name(1:seiclen(second_sfil_name))
               endif
c
c   save seiscomp id
c
     
               instant_event(instant_nevent)(81:100)=id(i)(1:20)
               call gmt_time(year,month,day,hour,min,sec,msec)
               isec=sec
               write(instant_event(instant_nevent)(101:114),'(i4,5i2)')
     *         year,month,day,hour,min,isec
            endif

         endif   ! end of instant block in loop
       
       enddo       ! end of  events loop


 100  continue

c
c   delete events too old in second data base if selected.
c   deleted dup events in both data bases.
c   write out instant events info for remaining events if any.
c   all of this only for instant operation.

c
      if(instant.and.instant_nevent.gt.0) then
         do i=1,instant_nevent
c
c   get current gmt time
c
            call gmt_time(year,month,day,hour,min,sec,msec2)
c
c   get gmt time event was recorded assuming origin time is close enough
c
            k=index(instant_event(i),'-')

            read(instant_event(i)
     *      (k-2:k+16),'(i2,1x,2i2,1x,i2,3x,i4,i2)')
     *      day,hour,min,isec,year,month
            sec=isec
            call timsec(year,month,day,hour,min,sec,msec1)
            if(msec2-msec1.gt.instant_time_back*60.0) then
               instant_event(i)(120:120)='o'    ! signal too old
               write(6,*) 'Event too old'
             endif
         enddo
c  
c   delete events in main data base which are duplicated 
c
         do i=1,instant_nevent
             if(instant_event(i)(120:120).eq.'d') then
                text='rm '//
     *          instant_event(i)(1:seiclen(instant_event(i)(1:80)))
                write(6,*) text
                call systemc(text,seiclen(text))
              endif
c
c   also delete duplicated and old events in 2. data base if used
c
              if(second_base.ne.' ') then
                 if(instant_event(i)(120:120).eq.'d'.or.
     *              instant_event(i)(120:120).eq.'o') then
                    text= 'rm '//
     *              instant_event(i)
     *              (130:seiclen(instant_event(i)(130:210))+129)
                    write(6,'(a)') text
                    call systemc(text,seiclen(text))
                 endif
              endif
         enddo
c
c   write out event list of events not deleted
c
         open(8,file='scpnor.instant',status='unknown')
c
c   make sure old lines do not remain in file if
c   no new lines are written
c
         write(8,*)
         rewind(8)

         do i=1,instant_nevent
            if(instant_event(i)(120:120).eq.' ') then
               write(8,'(a)') instant_event(i)
             endif
         enddo
         close(8)
      endif          ! end of instant section

      stop
      end


      subroutine get_scpnor_def(in,parfile,
     *sql_user,sql_pass,sql_ip,sql_database,
     *base,second_base,agency,operator,instant_time_back,
     *make _sfile,seiscomp_version,wa_gain,insert_arc_line)


      implicit none

      include 'libsei.inc'
      include 'seidim.inc'

      character*80 parfile          ! parameter file name
      integer in                    ! file unit
      integer code                  ! return code from opening def file
      integer i                     ! counter
      character*80 line,text        ! text line
      
      real var                      ! value of variables
      integer nstat                 ! station counter
      integer seiclen
      integer seiscomp_version      ! seiscomp version 3,4 or 5

      character*40 sql_user         ! data base user
      character*40 sql_pass         ! data base password
      character*40 sql_ip           ! ip of data base
      character*40 sql_database     ! data base name
      real instant_time_back        ! minutes to go back from real time
      real wa_gain                  ! if 0, gain is 20800, if 1.0 2080
      logical insert_arc_line       ! if true, insert arc line

      logical make_sfile            ! true if making an s-file


      character*3 agency

      
      character*1 dchar
      character*60 top_directory


      character*5 base,second_base        ! base for s-file
      character*3 operator                ! operator for s-file


      
      call topdir(top_directory)
      call dir_char(dchar)   ! directory separation character

   
c
c   read par file
c
      

      call sei get file( open$+ignore$,   ! Open file.
     &                      in,              ! On unit.
     &                      code,            ! Returned condition.
     &                      'DAT',           ! Alternative search directory.
     &                      parfile )        ! For this filename.

      
      if(code.ne.e_ok$) then
         write(*,*) ' Parameter file does not exist: ',parfile
         stop
      endif
c
c init values
c

      instant_time_back=5.0
      second_base=' '
      make_sfile=.false.
      operator='sc'
      agency='SCP'
      seiscomp_version=4
      insert_arc_line=.false.
      wa_gain=2800.0

100   continue

c
c read text line from file, check for code and set variables
c
      read(in,'(a80)',end=300,err=200) line

      if (line(1:6).eq.'AGENCY') then
        if(line(41:43).ne.' ') agency=line(41:43)
        if(line(51:53).ne.' ') operator=line(51:53)         
      elseif(line(1:17).eq.'SEISCOMP VERSION'.
     *  and.line(41:50).ne.' ') then
        read(line(41:50),*)seiscomp_version
      elseif (line(1:10).eq.'BASE SFILE') then
        read(line(41:45),'(a)') base
      elseif (line(1:17).eq.'SECOND BASE SFILE') then
        read(line(41:45),'(a)') second_base
      elseif (line(1:8).eq.'SQL USER') then
        read(line(41:80),'(a)') sql_user
      elseif (line(1:8).eq.'SQL PASS') then
        read(line(41:80),'(a)') sql_pass
      elseif (line(1:17).eq.'INSTANT TIME BACK') then
        read(line(41:80),*) instant_time_back
      elseif (line(1:6).eq.'SQL IP') then
        read(line(41:80),'(a)') sql_ip   
      elseif (line(1:12).eq.'SQL DATABASE') then
        read(line(41:80),'(a)') sql_database   
      elseif (line(1:10).eq.'MAKE SFILE') then
        var=0.0
        if(line.ne.' ') read(line(41:60),*) var
        if(var.eq.1.0) make_sfile=.true.  
      elseif (line(1:15).eq.'INSERT ARC LINE') then
        var=0.0
        if(line(41:60).ne.' ') read(line(41:60),*)var
        if(var.eq.1.0) insert_arc_line=.true.
      elseif (line(1:18).eq.'WOOD ANDERSON GAIN') then
        var=0.0
        if(line(41:60).ne.' ') read(line(41:60),*)var
        if(var.eq.1.0) wa_gain=2080.0
      endif
      goto 100

      if(second_base.eq.base) then
          write(6,*) 'the two bases cannot be equal'
          write(6,*) 'second base has been disabled'
          second_base=' '
      endif

200   continue
      write(*,*) ' Error in scpnor parameter file'
      return

300   continue


      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       subroutine gmt_time(year,month,day,hour,min,sec,abstime)
c
c  get gmt time, jh dec 2021
c
       implicit none
       integer date_time(8)
       integer year,month,day,hour,min,doy
       real sec
       real*8 abstime

       character*10 b(3)

       call date_and_time(b(1), b(2), b(3), date_time) ! system routine

c       print *,'date_time    array values:'
c       print *,'year=',date_time(1)
c       print *,'month_of_year=',date_time(2)
c       print *,'day_of_month=',date_time(3)
c       print *,'time difference in minutes=',date_time(4)
c       print *,'hour of day=',date_time(5)
c       print *,'minutes of hour=',date_time(6)
c       print *,'seconds of minute=',date_time(7)
c       print *,'milliseconds of second=',date_time(8)
c       print *, 'DATE=',b(1)
c       print *, 'TIME=',b(2)
c       print *, 'ZONE=',b(3)


       sec=date_time(7)+float(date_time(8))/1000.0

       call timsec(date_time(1),date_time(2),date_time(3),
     * date_time(5),date_time(6),sec,abstime)
c
c   add ofset to gmt
c
       abstime=abstime-date_time(4)*60.0
c
       call sectim(abstime,year,doy,month,day,hour,
     &       min,sec)

       return
       end

