***************************************************************************
c
c  netdet, real time event detection using an SeisComP or BUD archive
c  S-files are writtern out and a wav file can be extracted or an arc reference 
c  put into s-file. Output written locally or in data base. Location and magnitude
c  can be done
c  Default Parameter file is netdet.par
c
c  jh december 2021
c
c  changes
c
c  03 05 2022 jh: add detection of teleseismic events using a bp filter 
c  05 07 2022 jh: optionally keep triggers weighted out if using autopic
c  21 07 2022 Jh: error file agian, also locate distant, own parameter
c  02 08 2022 jh: fix bug with wrong start time for extract, was using
c                 sec of last trigger instead of first
c  11 08 2022 jh: change hyp_nstat(1) to ntrig_last at oen place
c  19 02 2023 jh: execution of 'command' was wrong
c  12 03 2023 jh: also posible to send mail with whole file, some changes
c                 in meail spec in SEISAN.DEF, possible to send in Nordic 2

c
c***************************************************************************

      implicit none 

c
c read SEISAN include files
c
      include 'seidim.inc'               ! dimensions
      include 'seisan.inc'               ! general seisan parameters including arc 
                                         ! channel names
      include 'rea.inc'
      include 'waveform.inc'             ! for waveform handling

      integer seiclen

      integer nstat                      ! number of channels posible
      parameter (nstat=100)      
      character*80 text
      logical realtime                   ! true if realtime
 
      character*5 net_code               ! code of network
      character*80 arg(5)                ! arguments to  program
      integer nargs                      ! number of arguments
      character*5 station(nstat)         ! stations to use
      character*7 component(nstat)       ! component and network and location to use

      integer n_chan_out                 ! number of channels to write out
      character*5 chan_out_stat(nstat)   ! stat for write out
      character*3 chan_out_comp(nstat)   ! component --------
      character*2 chan_out_net(nstat)    ! network ----------
      character*2 chan_out_loc(nstat)    ! location ---------

      character*3 com
      real sta(3,nstat),lta(3,nstat)     ! short and long term values
      integer n_trigger                  ! number of channels to use with trigger
      integer n_active                   ! number of channels active
      integer max_ntrig                  ! maximum number of triggers in ring buffer
      parameter (max_ntrig=1000)
      real*8 trig_abs_time(max_ntrig)    ! save channel trigger time
      character*5 trig_stat(max_ntrig)   ! ----------- trigger stat
      character*7 trig_comp(max_ntrig)   ! ------------ comp+nt+lo

      real xfilt(max_ntrig)              ! filter number used
      integer flag(max_ntrig)            ! indicator of trigger status
      integer ntrig                      ! total number of triggers, all chan

      logical debug                      ! true if debug output
      logical real_time                  ! true if real time operation
      character*5 debug_station          ! station to debug for sta lta

      character*130 last_detection       ! info on last detection 

      real*8 msec,msec1,msec2            ! abs times
      real*8 trigger_msec(3,nstat)       ! channel trigger time 
      real*8 ptrigger_msec(3,nstat)      ! channel previous -------
      logical trigger(3,nstat)           ! channel trigger status       
      integer year,month,day,doy,hour,min,isec
      real sec
      integer year1,month1,day1,doy1,hour1,min1,isec1
      real sec1
      integer year2,month2,day2,doy2,hour2,min2,isec2
      real sec2
      logical trig_out(3,nstat)            ! if true, a net trigger can be written out

      character*60 top_directory        ! seisan top dir
      character*1 dchar                  ! dir separator
      logical pc,sun,linux               ! compter type

c 
c  parameters
c
      character*80 parfile               ! parameter file, default=netdet.par
      character*80 parfile_now           ! parameter file with ending now
      character*80 parfile_html          ! parameter file with ending html
      character*80 parfile_err           ! file with errros
      character*3 agency                 ! agency
      character*14 startdate,stopdate    ! start and stop when running manually
      logical freeze                     ! true if freze lta
      integer nfilt                      ! number of filter sets
      real flow(3),fhigh(3)              ! filter to use
      real sta_duration(3),lta_duration(3)     ! durations of sta and lta
      real trigger_ratio(3),detrigger_ratio(3) ! trigger and detrigger ratio
      real trigger_min_int(3)            ! min time between channel triggers
      real trigger_min_dur(3)            ! min duration of trigger
      real pre_event                     ! preevent time when extracting
      real extduration                   ! duration of extraction
      real netwindow                     ! array propagation window  
      integer netmindet                  ! minimum number of triggers in window       
      logical wav_extract                ! if true, extract wav file  
      logical make_sfile                 ! if true, make s-file
      character*5 base                   ! base for s-file
      character*3 operator               ! operator for s-file
      character*1 copy_wav               ! copy, move or leave wav 
      character*5 copy_base              ! base to move wav to
      real real_time_delay               ! delay in s of real time operation 
      integer min_stat_location_local    ! min number of stations for location
      integer min_stat_location_dist     ! min number of stations for locatioa of dist. eq.
      integer min_stat_email             ! min ----------------  for email
      real min_mag_email                 ! min magnitude for email     
      character*40 command               ! command  after location
      character*40 netdet_dir            ! temp netdet dir
      real autopic                       ! if 1 use AUTOPIC
      real fix_depth                     ! depth to fix if ge 0
      real xtime                         ! end time or hours
      real flow_dist,fhigh_dist          ! filter limit for dist. eq.
      integer ndist_test                 ! minimum number of triggers to do
                                         ! dist test
      character*12 t                     ! for systime
      character*14 t1                    ! ----------

      integer i,l,k,j,ind,kk,terror,mm          ! counter

      last_detection=' '
      last_detection(1:16)='No detection yet'
      last_detection(61:75)='No location yet'

      parfile='netdet.par'    ! default name
      parfile_now='netdet.now'
      parfile_html='netdet.html'
      parfile_err='netdet.err'
      n_active=0

c
c   get arguments
c
      call get_arguments(nargs,arg)

      if(nargs.gt.0) then
         do i=1,nargs

         if(arg(i)(1:8).eq.'realtime') then
            realtime=.true.
         endif
c
c   could be another parameter file
c
         if(arg(i)(1:8).eq.'-parfile') then
            parfile=arg(i+1)
c
c   make 'now' html and err file names from name of parameter file
c
           k=index(parfile,'.')
            if(k.gt.0.and.parfile(k:k+3).eq.'.par') then
               parfile_now=parfile(1:k)//'now'  ! file for logging
               parfile_html=parfile(1:k)//'html'  ! file for logging
               parfile_err=parfile(1:k)//'err'   ! errors
            else
               write(6,*)'Parameter file name does not end with .par'
               stop
            endif
         endif      
         enddo
      endif


c
c   get seisan defaults including names of arc channels
c
      call get_seisan_def
c
c      write(6,*) n_alertemail_netdet,n_alertemail_sum_netdet,
c     *alertemail_format
c      do i=1,n_alertemail_netdet
c         write(6,*) alertemail_netdet(i)
c      enddo
c      write(6,*)
c      do i=1,n_alertemail_sum_netdet
c         write(6,*) alertemail_sum_netdet(i)
c      enddo

c      do i=1,n_alertemail
c         write(6,*) alertemail(i)
c      enddo
c      do i=1,n_alertemail_sum
c         write(6,*) alertemail_sum(i)
c      enddo

c      write(6,*) 'format ',alertemail_format
 
c
c   check s-file format
c
      if(new_nordic_format) then
         rea_new_out=.true.              ! write new format
      endif 
                                                                               
c   get path to seismo                                                          
c                                                                               
      call topdir(top_directory)

c
      call dir_char(dchar)   ! directory separation character
c
c   get computertype
c    
      call computer_type(sun,pc,linux)
c

c
c   initialize
c
      do j=1,3
      do i=1,100
         sta(j,i)=0.0
         lta(j,i)=0.0
         trigger(j,i)=.false.
         trigger_msec(j,i)=0.0
         ptrigger_msec(j,i)=0.0
         trig_out(j,i)=.false.
      enddo
      enddo

      do i=1,max_ntrig
         flag(i)=0
      enddo

c
c   read parameter file
c
      call get_netdet_def(realtime,parfile,n_trigger,
     *   startdate,stopdate,
     &   station,component,flow,fhigh,nfilt,
     &   sta_duration,lta_duration,trigger_ratio,
     &   detrigger_ratio,trigger_min_dur,trigger_min_int,
     &   freeze,
     &   pre_event,extduration,
     &   netwindow,netmindet,
     &   wav_extract, 
     &   base,make_sfile,copy_wav,copy_base,real_time_delay,
     &   min_stat_location_local,min_stat_location_dist,
     &   min_stat_email,min_mag_email,agency,
     &   n_chan_out,chan_out_stat,chan_out_comp,chan_out_net,
     &   chan_out_loc,debug,debug_station,
     &   operator,netdet_dir,autopic,command,fix_depth,flow_dist,
     &   fhigh_dist,ndist_test)

      write(6,'(a,3x,a3,1x,a3)')
     *                     'Agency and operator:            ',
     *agency,operator
      write(6,'(a,3x,a)')  'Netdet directory:               ',
     *netdet_dir
      write(6,'(a,5x,L1,1x,a)') 
     *                     'Debug and debug station:        ',
     *                      debug,debug_station
      write(6,'(a,i6)')    'Number of channels for trigger: ',n_trigger
      write(6,'(a,6f6.1)') 'Filter:                         ',
     *(flow(i),fhigh(i),i=1,nfilt)
      write(6,'(a,6f6.1)') 'STA and LTA duration:           ',
     *             (sta_duration(i),lta_duration(i),i=1,nfilt)
      write(6,'(a,6f6.1)') 'Trigger  and detrigger ratio:   ',
     *            (trigger_ratio(i),detrigger_ratio(i),i=1,nfilt)
      write(6,'(a,3f6.1)') 'Trigger minimum duration:       ',
     *            (trigger_min_dur(i),i=1,nfilt)
      write(6,'(a,3f6.1)')  'Minumum trigger interval:       ',
     *            (trigger_min_int(i),i=1,nfilt)     
      write(6,'(a,2f6.1)') 'Preevent time and ext duration: ',
     *            pre_event,extduration
      write(6,'(a,f6.1)' ) 'Real time delay in secs:        ',
     *            real_time_delay
      write(6,'(a,f6.1)')
     *                     'Autopic:                        ',autopic
      write(6,'(a,i6)')    'Min. numb of stats for local lo:',
     *            min_stat_location_local
      write(6,'(a,i6)')    'Min. numb of stats for dist. lo:',
     *            min_stat_location_dist
      write(6,'(a,f6.1)')  'Fix depth, if neg. do not fix   ',
     *            fix_depth
      write(6,'(a,2f6.1)') 'Filters for distant event detec ',
     *            flow_dist,fhigh_dist
      write(6,'(a,i6)')    'Min trig. for dist. event detec ',
     *            ndist_test
      write(6,'(a,i6)')    'Min. numb of stats for email:   ',
     *            min_stat_email
      write(6,'(a,f6.1)')  'Min. magnitude for email:       ',
     *            min_mag_email
      write(6,'(a,a)')
     *                     'System command after location:  ',command
      write(6,'(a,f6.1,i6)')   'Net window and min no of dets:  ',
     *            netwindow,netmindet
      write(6,'(a,5x,L1)')  'Make waveform file:             ',
     *            wav_extract 
      write(6,'(a,5x,L1,1x,a)')  'Make s-file and base of  S-file:',
     *            make_sfile,base
      write(6,'(a,5x,L1)')  'Copy wav file:                  ',copy_wav
      do i=1,5
         if(copy_base(i:i).ne.'_') text(i:i)=copy_base(i:i)
      enddo
      write(6,'(a,5x,a)')  'Base to copy wav to;            ',text(1:5)
      write(6,*)

c
c   check if debug, turn off in realtime mode
c
      if(realtime.and.debug)then
         debug=.false.
         write(6,*)' Cannot use debug in real time mode, turned off'
      endif
c
c  open debug files
c
      if(debug) then
         open(10,file='netdet_debug_trig.out',status='unknown')
         if(debug_station.ne.'     ')
     *   open(11,file='netdet_debug_sta_lta.out',status='unknown')
      endif


c
c check if channels defined, arc_nchan gives the number of channels defined
c
      if (arc_nchan.eq.0) then
         write(*,*) ' No archive channels defined in SEISAN.DEF'
         stop
      endif
c
c write out the database names
c
      write(*,*)'Archive channels are:'
      do i=1,arc_nchan
 	 write(*,*)'    ',arc_stat(i),arc_comp(i),arc_net(i),arc_loc(i)
      enddo

      write(6,*)'Archive is: ',arc_archive
c
c   initialize error file
c
      if(netdet_dir.ne.' ') then
          parfile_err=netdet_dir(1:seiclen(netdet_dir))//dchar//
     *   parfile_err(1:seiclen(parfile_err))
      endif

      open(29,file=parfile_err,status='unknown')
      write(29,*) 'Error file for ', parfile
      call systime(t,t1)
      write(29,'(a,a)') 'Start time ',t
      close(29)


c
c   real time
c
      if(realtime) then

c
c   find initial start time, real_time_delay after real time
c
         call gmt_time(year,month,day,hour,min,sec,msec)        
   
         msec=msec-real_time_delay     

         call sectim(msec,year,doy,month,day,hour,
     &       min,sec)
          isec=sec
          write(6,'(a,i4,1x,2i2,1x,2i2,1x,i2)') ' Start time: ',
     *    year,month,day,hour,min,isec
c
c   not realtime, give time interval
c
       else
          write(6,*)'Give start time, at least year and month'
          read(5,'(a)') text
c
c  nordic file with all events, only if not real time
c
          open(2,file='netdet.out',status='unknown')    
          read(text,'(i4,5i2)')year,month,day,hour,min,isec
          if(day.eq.0) day=1
          sec=isec
          call timsec(year,month,day,hour,min,sec,msec)

          write(6,*)'Give end time, at least year, month and day'
          write(6,*)'or give number of hours from start like 3 or 0.2'
          read(5,'(a)') text
          read(text,*) xtime
          if(k.lt.2000) then
             msec2=msec+xtime*3600.0
          else
             read(text,'(i4,5i2)')year2,month2,day2,hour2,min2,isec2
             sec2=isec2
             call timsec(year2,month2,day2,hour2,min2,sec2,msec2)
          endif
       endif

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      l=0         ! count minutes from start

 1    continue    ! loop for reading data

      l=l+1   
c
c   write info to file if in real time
c
      if(realtime) then
         if(netdet_dir.eq.' ') then
            open(3,file=
     *      parfile_now(1:seiclen(parfile_now)),status='unknown')
         else
            open(3,file=
     *      netdet_dir(1:seiclen(netdet_dir))//dchar//
     *      parfile_now(1:seiclen(parfile_now)),status='unknown')
         endif

         write(3,'(a)') parfile
         call sectim(msec,year,doy,month,day,hour,
     *   min,sec)
         isec=sec
         write(text,
     *   '(a,2x,i4.4,1x,i2.2,a1,i2.2,1x,i2.2,a1,i2.2,a1,i2.2)') 
     *   'Processing now starting with:',
     *   year,month,'-',day,hour,':',min,':',isec
         write(text(71:80),'(i10)') l
         write(3,'(a)') text
         day=l/(60*24)
         hour=(l-day*60*24)/60
         min=l-hour*60-day*24*60
         write(3,'(a,i3,a,i2,a,i2,a)')
     *  'Netdet has run ',day,' days ',hour,' hours ',min,' minutes'
         write(3,'(a,1x,2i5)')
     *  'Number of defined and active channels: ',n_trigger,n_active
       
         write(3,'(a)') last_detection
         close(3)

         n_active=0  ! count ok stations, reset here after w. out

  
c
c  html page
c
         if(netdet_dir.eq.' ') then
            open(4,file=
     *      parfile_html(1:seiclen(parfile_html)),status='unknown')
         else
            open(4,file=
     *      netdet_dir(1:seiclen(netdet_dir))//dchar//
     *      parfile_html(1:seiclen(parfile_html)),status='unknown')
         endif

         write(4,'(a)') "<HTML lang='en'>"
         write(4,'(a)') "<HEAD>"
         write(4,'(a)') "<title>SEISAN - NETDET STATUS</title>"
         write(4,'(a)') "</HEAD>"
         write(4,'(a)') '<META HTTP-EQUIV="refresh" CONTENT="6">'
         write(4,'(a)') parfile
         write(4,'(a)') "<BR>"
         call sectim(msec,year,doy,month,day,hour,
     *   min,sec)
         isec=sec
         write(text,
     *   '(a,2x,i4.4,1x,i2.2,a1,i2.2,1x,i2.2,a1,i2.2,a1,i2.2)') 
     *   'Processing now starting with:',
     *   year,month,'-',day,hour,':',min,':',isec
         write(4,'(a)') "<BR>"
         write(text(71:80),'(i10)') l
         write(4,'(a)') "<BR>"
         write(4,'(a)') text
         write(4,'(a)') "<BR>"
         day=l/(60*24)
         hour=(l-day*60*24)/60
         min=l-hour*60-day*24*60
         write(4,'(a,i3,a,i2,a,i2,a)')
     *  'Netdet has run ',day,' days ',hour,' hours ',min,' minutes'
         write(4,'(a)') "<BR>"
         write(4,'(a)') last_detection(1:60)
         write(4,'(a)') "<BR>"
         write(4,'(a)') last_detection(61:130)
         write(4,'(a)') "<BR>"
         write(4,'(a)') "<BR>"
         write(4,'(a)') '<img src="netdet_map.png"  '//
     *  'alt="Map with location of epicenter"  width="300" border="0">'
         write(4,'(a)') "</HTML>"
         close(4)
      endif

      n_active=0  ! count ok stations, reset here after w. out

      
c
c   add a time mark trigger so check of triggers will continue
c   into next minute
c
       ntrig=ntrig+1
       if(ntrig.gt.max_ntrig) ntrig=1
       trig_abs_time(ntrig)=msec
       flag(ntrig)=-2
       trig_stat(ntrig)='TIME '
       trig_comp(ntrig)=' '
c
c  the time interval to read
c
       cont_interval=60.0
       if(debug) then
         write(10,*) 'READ next minute  *******************************'
         write(10,*) '*************************************************'
       endif
       write(6,*) 'READ next minute  *******************************'
c
c  read the waveform data, one trace at a time
c

      do j=1,n_trigger

         kk=cont_interval+40.0       ! read 40s more, 20 before and 20 after
     
         mm=seiclen(arc_archive)
c
c   find where the data is, start 20s before required data
c
         msec1=msec-20.0

         call sectim(msec1,year1,doy1,month1,day1,hour1,
     &       min1,sec1)
         isec1=sec1

         call  getarchinfo(arc_archive,mm,arc_type
     *   ,station(j),5,component(j)(1:3),3,
     *   component(j)(4:5),2,component(j)(6:7),2,
     *   year1,month1,day1,hour1,min1,sec1,kk,
     *   wav_nsamp(1),wav_rate(1),terror,wav_year(1),
     *   wav_month(1),wav_day(1),wav_hour(1),
     *   wav_min(1),wav_sec(1))
c
c   if no data, skip the rest in loop
c
         if(wav_nsamp(1).eq.0) goto 10

c
c   assume there is data so count
c
         n_active=n_active+1

         write(6,
     *   '(1x,a5,1x,a3,1x,a2,a2,1x,i4,1x,i2.2,a1,
     *   i2.2,1x,i2.2,a1,i2.2,a1,
     *   f4.1,f7.1,i6)')
     *   station(j),component(j)(1:3),component(j)(4:5),
     *   component(j)(6:7),wav_year(1),wav_month(1),'-',wav_day(1),
     *   wav_hour(1),':',wav_min(1),':',wav_sec(1),
     *   wav_rate(1),wav_nsamp(1)

         wav_stat(1)=station(j)
         wav_comp(1)=component(j)(1:2)//' '//component(j)(3:3)
         wav_network(1)=component(j)(4:5) 
         wav_location(1)=component(j)(6:7) 
         
         if(debug) then
             write(10,*)
     *       '*************************************************'
             write(10,
     *       '(1x,a5,1x,a3,1x,a2,a2,1x,i4,1x,i2.2,a1,
     *       i2.2,1x,i2.2,a1,i2.2,a1,
     *       f4.1,f7.1,i6)')
     *       station(j),component(j)(1:3),component(j)(4:5),
     *       component(j)(6:7),wav_year(1),wav_month(1),'-',wav_day(1),
     *       wav_hour(1),':',wav_min(1),':',wav_sec(1),
     *       wav_rate(1),wav_nsamp(1)
         endif
c
c   read one channel
c  

         com=component(j)(1:3)                          
         call  getarchdata(arc_archive,mm,arc_type,
     *   wav_stat(1),5,
     *   com,3,
     *   wav_network(1),2,wav_location(1),2,
     *   wav_year(1),wav_month(1),wav_day(1),
     *   wav_hour(1),wav_min(1),wav_sec(1),kk,
     *   signal_int)
         do i=1, wav_nsamp(1)
            signal1(i)=signal_int(i)
            signal_int(i)=0.0
         enddo

         call timsec(wav_year(1),wav_month(1),wav_day(1),wav_hour(1),
     *   wav_min(1),wav_sec(1),wav_abs_time(1))
c
c   time of first sample to be used after filter
c
         wav_abs_time(1)=wav_abs_time(1)+20.0
c
c   find triggers for this channel
c
         call sta_lta(l,j,sta,lta,trigger,
     *   trigger_msec,ptrigger_msec,
     *   max_ntrig,ntrig,trig_abs_time,trig_stat,
     *   trig_comp,flag,trig_out,
     *   trigger_ratio,detrigger_ratio,trigger_min_dur,sta_duration,
     *   lta_duration,trigger_min_int,freeze,flow,fhigh,nfilt,
     *   debug,debug_station,xfilt)  

 10      continue   ! there were no data
                                         
      enddo   ! end of station loop

c
c   sort all the triggers in time
c
      call yhpsort(ntrig,trig_abs_time,trig_stat,trig_comp,flag,xfilt) 
c
c   remove triggers older then 5 min from last trigger
c
     
      k=0
      if(ntrig.gt.1) then
         do i=1,ntrig-1
            if(trig_abs_time(ntrig)-trig_abs_time(ntrig-i).gt.300.0)
     *      then
               k=ntrig-i    ! first one too old, counted from beginning
               goto 2
            endif
         enddo
 2       continue
c
c  move triggers up
c
         if(k.gt.0) then
            do i=1,ntrig-k
               trig_abs_time(i)=trig_abs_time(i+k)
               trig_stat(i)=trig_stat(i+k)
               trig_comp(i)=trig_comp(i+k)
               flag(i)=flag(i+k)
               xfilt(i)=xfilt(i+k)
            enddo
         endif
         ntrig=ntrig-k
      endif 

c
c   do network trigger and write out
c

      call net_det
     *(max_ntrig,ntrig,trig_abs_time,trig_stat,trig_comp,flag,
     *trig_out,pre_event,extduration,netwindow,netmindet,
     *make_sfile,operator,base,wav_extract,copy_wav,copy_base,
     *realtime,last_detection,
     *min_stat_location_local,min_stat_location_dist,
     *min_stat_email,min_mag_email,agency,n_chan_out,
     *chan_out_stat,chan_out_comp,chan_out_net,chan_out_loc,
     *debug,debug_station,xfilt,autopic,command,fix_depth,
     *flow_dist,fhigh_dist,ndist_test,parfile_err)
c
c   go forward 1 min
c
      msec=msec+60.0  
      call sectim(msec,year,doy,month,day,hour,min,sec)
      isec=sec
c
c   wait one minute if in real time detection
c      
      if(realtime) call wait(msec,real_time_delay)
c
c   if not real time, check if end time msec2 is reached
c
      if(.not.realtime.and.msec.gt.msec2) then
         stop
      endif
c
c  back to read next minute of data
c
      goto 1
          
      stop
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc 

      subroutine sta_lta(lm,chan,sta,lta,trigger,
     *trigger_msec,ptrigger_msec,
     *max_ntrig,ntrig,trig_abs_time,trig_stat,trig_comp,flag,trig_out,
     *trigger_ratio,detrigger_ratio,trigger_min_dur,sta_duration,
     *lta_duration,trigger_min_int,freeze,flow,fhigh,nfilt,
     *debug,debug_station,xfilt)

      implicit none

      include 'seidim.inc'   ! dimensions
      include 'seisan.inc'   ! general seisan parameters
      include 'libsei.inc'
      include 'waveform.inc' ! for waveform handling
      include 'rea.inc'     

      integer lm            ! minutes since start
      integer chan          ! channel to use
      real sta_duration(3)  ! length of sta
      real lta_duration(3)  ! length of lat
      integer nfilt         ! number of filter sets
      real flow(3),fhigh(3) ! filter limits
      integer npoles        ! number of poles in filter
      integer passes        ! how many times to pass
      character*8 filter_proto,filter_type  
      real samp_int         ! sampler interval
      integer year,month,day,doy,hour,min,isec
      real sec
      real*8 msec           ! abs sample time
      real*8 trigger_msec(3,*)! abs trigger time
      real*8 ptrigger_msec(3,*)! previous -------
      real sta(3,*),lta(3,*)   ! short and long term average
      integer nsta,nlta        ! sample in -----------------
      logical trigger(3,*)     ! true if trigger
      logical trig_out(3,*)    ! if true,trigger written
      real trigger_min_dur(3)  ! minimum trigger duration
      real trigger_min_int(3)  ! minimum interval between two triggers
      real trigger_ratio(3)    ! sta/lta ratio to trigger
      real detrigger_ratio(3)  ! sta/lta  ratio to detrigger

      real*8 trig_abs_time(*)  ! save channel trigger time
      character*5 trig_stat(*) ! ----------- trigger stat
      character*7 trig_comp(*) ! ------------ comp+nt+lo
      integer flag(*)          ! trigger status
      real xfilt(*)            ! filter number used
      integer ntrig            ! number of triggers saved
      integer max_ntrig        ! maximum number of triggers in ring buffer
      logical debug            ! if true, write out debug info
      character*5 debug_station  ! station to debug
c
      logical freeze           ! if true, freeze lta
      integer ndc              !
      integer nsamp            ! number fo samples in window used
      real rdc                 !
      integer tl               ! number of samples to taper
      real taper,pi            ! taper value
      integer j,i,k,ksec,jfilt
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   large filter loop all the way to the end
cccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      do jfilt=1,nfilt
cccccccccccccccccccccccccccccccccccccccccccccccccccc

      do i=1,wav_nsamp(1)
         signal2(i)=signal1(i)
      enddo
      
      ksec=0

c
c   do sta lta
c

      npoles=4
c
c   remove dc
c
c      call remove_dc(signal1,ndc,rdc,wav_nsamp(1))
c
c apply cosine taper 
c
c      tl=int(wav_nsamp(1)*.05)
c      pi=acos(-1.)
c      do j=1,tl
c         taper=(-cos(pi/2.+(j-1)*(pi/2.)/float(tl-1)))
c         signal1(j)=signal1(j)*taper
c         signal1(wav_nsamp(1)-j+1)=signal1(wav_nsamp(1)-j+1)*
c     &   taper
c        write(33,*) j,taper
c      enddo
c
c filter data
c
      passes=1   ! clear recfil buffer when used
      filter_proto='BU'

      samp_int=1./wav_rate(1)

      if (flow(jfilt).ne.0..and.fhigh(jfilt).ne.0.) then
        filter_type='BP'
        call recfil(signal2,wav_nsamp(1),signal2,
     &  filter_proto,0.,0.,npoles,filter_type,flow(jfilt),fhigh(jfilt),
     &  samp_int,passes)

      else
c
c apply highpass to remove dc
c
        filter_type='HP'
        flow(jfilt)=0.01
        call recfil(signal2,wav_nsamp(1),signal2,
     &  filter_proto,0.,0.,npoles,filter_type,flow(jfilt),fhigh(jfilt),
     &  samp_int,passes)

      endif


c
c   cut the signal by 20 s at each end to avoid filter effects at ends
c   nsamp is number of samples to use.
c
      k=20.0*wav_rate(1)
      nsamp=wav_nsamp(1)-k*2
      do i=1,nsamp
         signal2(i)=signal2(i+k)
      enddo

c
c  calculate nsta and nlta
c
          
      nsta=int(sta_duration(jfilt)*wav_rate(1))

      nlta=int(lta_duration(jfilt)*wav_rate(1))

c
c   loop of samples
c
      do j=1,nsamp          
c
c get sample time
c
        msec=wav_abs_time(1)+
     &  float(j-1)/wav_rate(1)
c
c add new sample and delete first sample in sta and lta
c

        sta(jfilt,chan)=sta(jfilt,chan)+(signal2(j)**2 - 
     *  sta(jfilt,chan))/float(nsta)       

        lta(jfilt,chan)=lta(jfilt,chan)+(signal2(j)**2 - 
     *  lta(jfilt,chan))/float(nlta)
c
c   debug sta/lta, write out every s for particular station
c
        if(wav_stat(1).eq.debug_station.and.debug) then
           call sectim(msec,year,
     &     doy,month,day,hour,min,sec)
           isec=sec   
           if(isec.eq.ksec) then

           write(11,
     *     '(a5,1x,a3,1x,i2.2,1x,i2.2,a1,i2.2,'//
     *     'a1,f4.1,a,f12.1,a,f12.1,a,f12.1,a,L1,a,i2)')wav_stat(1),
     *     wav_comp(1)(1:2)//wav_comp(1)(4:4),
     *     day, hour,':',min,':',sec,
     *     ' STA=',sta(jfilt,chan),' LTA=',lta(jfilt,chan), 
     *     ' R=',sta(jfilt,chan)/lta(jfilt,chan),' Trig=
     *     ',trigger(jfilt,chan),' Filt=',jfilt
              ksec=ksec+1
              if(ksec.eq.60) ksec=0
            endif
         endif
c
c   if first minute, do not check for trigger
c
         if(lm.eq.1) goto 1

c
c check for trigger based on sta/lta ratio
c

         if (sta(jfilt,chan)/lta(jfilt,chan).gt.trigger_ratio(jfilt)
     *    .and.
     &    .not.trigger(jfilt,chan)) then     ! a new trigger
            trigger_msec(jfilt,chan)=msec    ! trigger time saved
            trigger(jfilt,chan)=.true.
            if(debug) then  
               call sectim(trigger_msec(jfilt,chan),year,
     &         doy,month,day,hour,min,sec)     
               write(10,'(a,1x,i2.2,1x,i2.2,a1,i2.2,a1,f4.1,a,i1)')
     *         ' Trigger now *************  ',day, hour,':',min,':',sec,
     *         '  filt=',jfilt
            endif
         endif

c
c   check for detrigger based on sta/lta. then set flag so
c   trigger can be saved if duration ok  
c   

         if(sta(jfilt,chan)/lta(jfilt,chan).le.detrigger_ratio(jfilt)
     *   .and.
     &   trigger(jfilt,chan))  then

            if(debug) then
               call sectim(msec,year,
     &         doy,month,day,hour,min,sec)
               write(10,'(a,1x,i2.2,1x,i2.2,a1,i2.2,a1,f4.1)')
     *         ' Detrigger now due to ratio ',
     *         day, hour,':',min,':',sec
            endif
 
            trigger(jfilt,chan)=.false.
            trig_out(jfilt,chan)=.false.       ! a next trigger can now be written out
         endif   

c
c   duration and ratio, if both ok, save in trigger buffer even if
c   sta/lat still above limit so all triggers within window
c   are written as soon as possible and we do not have to wait
c   for the last detrigger
c


         if(msec-trigger_msec(jfilt,chan).gt.trigger_min_dur(jfilt)
     *   .and.
     *   sta(jfilt,chan)/lta(jfilt,chan).gt.trigger_ratio(jfilt).
     *   and..not.trig_out(jfilt,chan).and.trigger(jfilt,chan)) then
            
            if(debug) write(10,'(a)')                                     
     *      ' Trigger ok with ratio and duration   ************' 

c   a new trigger cannot be set before time trigger_min_int has
c   passed since the previous trigger in order to avoid many 
c   triggers for the same event
c
           if (trigger_msec(jfilt,chan)-ptrigger_msec(jfilt,chan).lt.
     &     trigger_min_int(jfilt).and.trigger(jfilt,chan)) then
              trigger(jfilt,chan)=.false.
              if(debug) write(10,'(a,f6.1)')' New trigger too early',
     *        trigger_msec(jfilt,chan)-ptrigger_msec(jfilt,chan)
           else
              trig_out(jfilt,chan)=.true.
c
c   save time as previous trigger
c
              ptrigger_msec(jfilt,chan)=trigger_msec(jfilt,chan)
c
c   store trigger in ring buffer of triggers from all channels
c
              ntrig=ntrig+1
              if(ntrig.ge.max_ntrig) ntrig=1
              trig_abs_time(ntrig)=trigger_msec(jfilt,chan)
              trig_stat(ntrig)=wav_stat(1)
              trig_comp(ntrig)=wav_comp(1)(1:2)//
     *        wav_comp(1)(4:4)//wav_network(1)//wav_location(1)
              flag(ntrig)=0
              xfilt(ntrig)=jfilt

c
c write out trigger if debug
c

              call sectim(trigger_msec(jfilt,chan),year,
     &        doy,month,day,hour,min,sec)
              isec=sec

              if(debug) then
                 write(10,'(a)')
     *           ' TRIGGER ACCEPTED ********************************'
                 write(10,
     *           '(1x,a5,1x,a3,1x,i4,1x,i2.2,'
     *           //'a1,i2.2,1x,i2.2,a1,i2.2,a1,f4.1)')
     &           wav_stat(1),wav_comp(1)(1:2)//wav_comp(1)(4:4), 
     *           year,month,'-',day,hour,':',min,':',sec
                 write(10,*)
              endif

            endif                              
         endif
 1       continue
      enddo         ! end of sample loop

c
c   if first minute, make sta and lta the same to initialize
c
      if(lm.eq.1) then
         lta(jfilt,chan)=sta(jfilt,chan)
      endif
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   end of large filter loop starting at the beginning
cccccccccccccccccccccccccccccccccccccccccccccccccccccc

      enddo
ccccccccccccccccccccccccccccccccccccccccccccccccccccc
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine net_det
     *(max_ntrig,ntrig,trig_abs_time,trig_stat,trig_comp,flag,
     *trig_out,pre_event,extduration,netwindow,netmindet,
     *make_sfile,operator,base,wav_extract,copy_wav,copy_base,
     *realtime,last_detection,
     *min_stat_location_local,min_stat_location_dist,
     *min_stat_email,
     *min_mag_email,agency,n_chan_out,chan_out_stat,chan_out_comp,
     *chan_out_net,chan_out_loc,debug,debug_station,xfilt,autopic,
     *command,fix_depth,flow_dist,fhigh_dist,ndist_test,parfile_err)

c
c  network detection and write out
c

      implicit none
     

      include 'seidim.inc'           ! dimensions 
      include 'seisan.inc'              
      include 'rea.inc'              ! parameter common block

      character*80 data(max_data)    ! s-file with data in text array

      logical all                    ! true: read all data, false: headers
      logical realtime               ! true if realtime operation
    
      integer code                   ! error return code

      character*3 agency 
      character*5 base_out
      real pre_event,extduration
      real netwindow                 ! arry propagation window
      integer netmindet              ! minimum number of detections in window
      character*120 text
      character*80 parfile_err       ! errror file 
      integer i,j,k,n,m,l

   
      integer seiclen                ! function
      integer flag(*)                ! status of det. used: 0=unused,1=used,2=rejected, -2 time mark
      logical rpt                    ! if true, stat is repeated
      real*8 trig_abs_time(*)        ! channel trigger time
      real*8 msec                    ! abs time
      character*5 trig_stat(*)       ! station triggered
      character*7 trig_comp(*)       ! components ------+nt+lo

      character*40 command           ! command after location
      real fix_depth                 ! depth to fix if gt 0
      real autopic                   ! if 1 use AUTOPIC

      integer year,month,day,hour,min,isec,doy
      real sec
      integer ndet                   ! number of detections in window
      integer ntrig                  ! number of triggers          
      integer ntrig_last             ! number of triggers for last event
      integer max_ntrig              ! maximum number of triggers in ring buffer
      integer last_trig_used         ! index of last station trigger used
      character*12 p_time            ! system time
      character*14 proc_time         ! system time
      character*14 start_time        ! start time for extract
      character*1 type               ! D or L

      logical wav_extract            ! if true, extract wav file
      logical make_sfile             ! if true, make s-file
      character*5 base               ! base for s-file
      character*3 operator           ! operator for s-file
      character*80 wavfile           ! waveform file name
      character*1 copy_wav           ! copy, move or leave wav files 
      character*5 copy_base          ! base to move/copy to
      integer min_stat_location_local! minimum number of stations for loc. local
      integer min_stat_location_dist ! minimum number of stat. for loc. distant
      integer min_stat_email         ! min ----------------  for email
      real min_mag_email             ! min magnitude for email   

      character*80 full_wav         ! wav file name with path

      integer n_chan_out             ! number of channels to write out
      character*5 chan_out_stat(*)   ! stat for write out
      character*3 chan_out_comp(*)   ! component --------
      character*2 chan_out_net(*)    ! network ----------
      character*2 chan_out_loc(*)    ! location ---------
      real xfilt(*)                  ! filter number used

      real flow_dist,fhigh_dist      ! filter for dist. ev. det.
      integer ndist_test             ! minimum number of triggers to do
                                     ! dist test

      character*80 sfil_name         ! s file name
      integer nf                     ! length of--

      logical debug                  ! if true, write out debug info
      character*5 debug_station      ! station to debug
      logical trig_out(3,*)          ! if true,trigger written   
      character*130 last_detection   ! info of last detection     
      integer kk

      last_trig_used=0               ! no net triggers yet


c
c now check if enough different stations, ndet, in time window
c
      ndet=0


      do i=1,ntrig
         if (flag(i).eq.0) then    ! no detection yet so start counting
            ndet=1                 ! only one
            j=i+1                  ! next detection
            if (i.lt.ntrig) then   ! stop at second last
c
c  go foreward one at a time and see if this detection in window, 
c  will find all in window,
c  all refereced to trigger i. only when trigger i is not ok, 
c  start from the next trigger
c
               do while(trig_abs_time(j).le.trig_abs_time(i)+
     *         netwindow.and.j.le.ntrig)
               rpt=.false.
c
c   a minute time marker is not counted, only used to make sure that 
c   potential following triggers still are inside window
c
                     if(flag(j).eq.-2) goto 1
c
c    verify that it's not an already-picked station
c
	             do k=i,j-1
	                if (trig_stat(j).eq.trig_stat(k)) then
			   rpt=.true.           ! repeat
		   	endif
		     enddo
		     if (.not.rpt) then
			ndet=ndet+1
			flag(j)=1               ! indicate that triggers used
			flag(i)=1
		     else
			flag(j)=2		! Rejected since repeat
		     endif
 1                   continue
                  j=j+1                         ! next trigger
               enddo
           endif                                ! no detection yet

c
c   if last trigger is within window, there could be more in next 60s block
c   so reset flags to start again in 60s
c
          if(j-1.eq.ntrig) then
             do k=i,j-1
                if(flag(k).eq.1.or.flag(k).eq.2) flag(k)=0
             enddo
             ndet=0
          endif
c
c   no more triggers in window starting with i so check if enough
c
           if (ndet.ge.netmindet) then
              call sectim(trig_abs_time(i),year,  ! time of first trigger
     &        doy,month,day,hour,min,sec)

              if(debug) then
                 write(10,*)
                 write(10,*)'NEW EVENT'
	         write(10,
     *           '(1x,i4,1x,i2.2,a1,i2.2,1x,i2.2,'//
     *           'a1,i2.2,a1,f4.1,2x,a,i3)') 

     *           year,month,'-',day,hour,':',min,':',sec,'ndet=',ndet
                 do n=1,ntrig
                    if(trig_stat(n).ne.'TIME') then
                       call sectim(
     *                 trig_abs_time(n),year,doy,month,day,hour,min,sec)
                       write(10,
     *                 '(1x,i4,1x,i2.2,a,i2.2,1x,i2.2,a,'//
     *                 'i2.2,a,f4.1,1x,a,a,i1,a,i1)')
     *                 year,month,'-',day,hour,':',min,':',
     *                 sec,trig_stat(n),
     *                 ' trig=',flag(n),' filt=',int(xfilt(n))
                    endif
                 enddo
              endif
c
c   nordic format output
c

             call rea_main_clear 
             call rea_hyp_clear(1)   

             rea_nhyp=1
             hyp_year(1)=year
             hyp_month(1)=month
             hyp_day(1)=day
             hyp_hour(1)=hour
             hyp_min(1)=min
             hyp_sec(1)=sec
             hyp_dist_id(1)='L'

c
c   output picked stations and times
c
              m=0
              do k=i,j-1
                  if (flag(k).eq.1) then
                      m=m+1
		      call sectim(trig_abs_time(k),year,
     &                doy,month,day,hour,min,sec)	      

c
c   nordic format
c
                      call rea_phase_clear(m)          ! clear variables     
                      rea_year(m)=year
                      rea_month(m)=month
                      rea_day(m)=day
                      rea_hour(m)=hour
                      rea_min(m)=min
                      rea_sec(m)=sec
                      rea_onset(m)='I'
                      rea_phase(m)='P'
                      rea_stat(m)=trig_stat(k)
                      rea_comp(m)=
     *                trig_comp(k)(1:2)//' '//trig_comp(k)(3:3)
                      rea_network(m)=trig_comp(k)(4:5)
                      rea_location(m)=trig_comp(k)(6:7) 
                      rea_auto(m)='automatic'
                      rea_operator(m)=operator
                      rea_agency(m)=agency
                      if(make_sfile.and.k.eq.i) then  ! take file time from first trigger
                        isec=sec
                        call sfilname
     *                  (year,month,day,hour,min,isec,base,'L',
     *                   sfil_name,nf)
                      endif
c
c   make id line if first trigger
c
                      if(k.eq.i) then
                         rea_id_line(1:40)= 
     *                   ' ACTION:                   OP:     STATU'
                         rea_id_line(41:80)=
     *                   'S:               ID:               L   I'
                         write(rea_id_line(61:74),'(i4,5I2)')
     *                   year,month,day,hour,min,isec
                         do l=61,74
                           if(rea_id_line(l:l).eq.' ') 
     *                     rea_id_line(l:l)='0'
                         enddo
                         call systime(p_time,proc_time)
                         write(rea_id_line(31:34),'(a)')operator(1:3)
                         write(rea_id_line(13:26),'(a)')proc_time
                         rea_id_line(9:11)='NEW'
                      endif
                   endif
               enddo
               rea_nphase=m
               hyp_nstat(1)=m

c
c   if location, fix depth to 15km
c
             if((hyp_nstat(1).ge.min_stat_location_local).or.
     *          (hyp_nstat(1).ge.min_stat_location_dist).
     *       and.fix_depth.ge.0.0) then
                hyp_depth(1)=fix_depth
                hyp_depth_flag(1)='F'
             endif       
c
c   save time and nstat of last detection
c
               isec=sec
               write(last_detection(1:60),
     *         '(a,i4.4,1x,i2.2,a1,i2.2,1x,i2.2,a1,i2.2,a1,i2.2,a,i3)')
     *        'Time of last detection      :  ', 
     *         year,month,'-',day,hour,':',min,':',isec,' nstat=',m

c
c   make arc reference
c
               msec=trig_abs_time(i)-pre_event          ! start pre_event min before first trigger
               call sectim(msec,year, 
     &         doy,month,day,hour,min,sec)
               isec=sec
               rea_nwav=1
               rea_wav(1)=' '
               write(rea_wav(1)(22:38),'(i4,1x,2i2,1x,2i2,1x,i2)')
     *         year,month,day,hour,min,isec
               write(rea_wav(1)(39:44),'(i6)') int(extduration)
               rea_wav(1)(80:80)='6'
               rea_wav(1)(1:6)= ' ARC *'
               full_wav(1:3)='ARC'    ! for auto dist. detection

c
c   extract waveform file if desired and put name in s-file
c
               if(wav_extract) then
                  write(start_time,'(i4,5i2.2)') 
     *            year,month,day,hour,min,isec
c
c   extract all channel in archive
c
                  if(n_chan_out.eq.0) then
	             write(text,'(a,f8.1,a)')
     &               'wavetool -arc -start '
     &               //start_time 
     &               // ' -duration ',
     &               extduration,
     &              ' -format MSEED  -wav_out_file SEISAN'
                  else
c
c   extract specified channels
c
                     open(27,file='cbase.inp',status='unknown')
                     do k=1,n_chan_out
                        write(27,'(a5,a3,a2,a2)')
     *                  chan_out_stat(k),chan_out_comp(k),
     *                  chan_out_net(k),chan_out_loc(k)
                     enddo
                     close(27)
	             write(text,'(a,f8.1,a)')
     &               'wavetool -arc -start '
     &               //start_time 
     &               // ' -duration ',
     &               extduration,' -cbase cbase.inp '//
     &              ' -format MSEED  -wav_out_file SEISAN'
                  endif

                  write(6,*) text
                  call systemc(text,seiclen(text))
                  open(33,file='extract.mes',status='unknown')
                  read(33,'(a)',err=3) text   ! get waveform file name
                  close(33)

                  if(text(1:2).eq.'OK') then ! if file ok, put in instead or ARC reference
                     k=index(text,' ')
                     rea_wav(1)=' '
                     rea_wav(1)(2:k-2)=text(3:k-1)
                     rea_wav(1)(80:80)='6'
                     text=' '
                     text=rea_wav(1)(2:k-2)
c
c   copy or move wav file
c
                     call copy_waveform_file(text,copy_wav,
     *               copy_base,full_wav)
                  endif

                  goto 4
 3                continue
                  open(29,file=parfile_err,status='old',
     *            position='append')
                  write(29,*)start_time,
     *            ' Something wrong with wave extract'
                  close(29)
                  write(6,*)start_time,
     *            ' Something wrong with wave extract'
 4                continue
               endif              

c
c   output all in one cat file but not if real time
c
               if(.not.realtime) call rea_event_out(2,.true.,data,code)
c
c---------------------------------------------------------------------
c--------------------------------------------------------------------
c   output in data base and do processing like pic, location mags etc
c   but only if sfile made
c--------------------------------------------------------------------
c--------------------------------------------------------------------
c
               if(make_sfile) then
                  write(*,*) ' sfile: ',sfil_name
                  open(88,file=sfil_name,status='unknown',err=5)
                  goto 6

 5                continue
                  open(29,file=parfile_err,status='old',
     *            position='append')
                  write(29,*)' Data base sub directory not made'
                  close(29)

                  write(6,*)' Data base sub directory not made'
                  stop
 6                continue
                  call rea_event_out(88,.true.,data,code)
                  close(88)
c
c   save number of triggers, could be overwritten if autopic
c
                  ntrig_last=hyp_nstat(1)

c
c   optinally output s-file in log file before any processing
c

                  open(29,file=parfile_err,
     *            status='old',position='append')
                  write(29,*) sfil_name
                  do k=1,rea_nrecord
                     write(29,'(a)') data(k)
                  enddo
                  close(29)
c
c--------------------------------------------------------------------
c   determine if a distant event, reads on 88, uses
c   several filters  to find if local event. if not it is D
c--------------------------------------------------------------------
c
                  type='L'     ! the default

                  if(flow_dist.ne.0.0.and.fhigh_dist.ne.0.0.and.
     *            ntrig_last.ge.ndist_test) then
                     open(88,file=sfil_name,status='old',err=7)
                     call event_type(flow_dist,fhigh_dist,
     *               type,full_wav)
                     write(6,*)'Event type is ',type
                     close(88)
                  endif
c
c   if distant, change type and file name, delete old file
c
                  if(type.eq.'D') then
                     open(88,file=sfil_name,status='old',err=7)
                     close(88, status='delete')
c
c   make new filename
c
                     m=index(sfil_name,'L.S')
                     sfil_name(m:m)='D'
                     open(88,file=sfil_name,status='unknown',err=7)
                     hyp_dist_id(1)='D'
                     data(rea_id_line_number)(76:76)='D'
                     call rea_event_out(88,.true.,data,code)
                     close(88)
                  endif
                  goto 8

 7                continue
                  open(29,file=parfile_err,
     *            status='old',position='append')
                  write(29,*) 'error opening ',sfil_name
                  close(29)

 8                continue
c
c   autopic only if a local event and enough triggers
c
                  if(autopic.gt.0.and.type.eq.'L'.and.
     *            ntrig_last.ge.min_stat_location_local) then
c
c
c   keep trigger times as phase x if requested
c

                    if(autopic.eq.2.0) then
                       do kk=1,rea_nphase
                         rea_phase(kk)='x'
                         rea_weight_in(kk)='4'
                         rea_onset(kk)=' '
                         rea_auto(kk)=' '
                       enddo
                    else
c
c   do not keep trigger times
c
                       rea_nphase=0
                    endif

                    open(88,file=sfil_name,status='old')
                    call rea_event_out(88,.true.,data,code)
                    close(88)

                    text='autopic '//sfil_name(1:seiclen(sfil_name))//
     *              ' -ag '//agency//' -op '//operator
                   if(copy_base.ne.' ') text=text(1:seiclen(text))//
     *             ' -wavbase '//copy_base

                    write(6,'(a)') text(1:seiclen(text))
                    call systemc(text(1:seiclen(text)),seiclen(text))
                 endif
c
c   location and magnitude if enough stations
c
                  if(
     *            (hyp_nstat(1).ge.min_stat_location_local.
     *            and.type.eq.'L').or.
     *            (hyp_nstat(1).ge.min_stat_location_dist.
     *            and.type.eq.'D')) then
                     call
     *               locate_magnitude(
     *               sfil_name,data,command,operator,agency,
     *               copy_base,type)
c
c   if a location, put in last_detection for monitor
c 
                     call read_sfile_head(sfil_name,data(1))
                     if(data(1)(25:38).ne.' ') then
                       last_detection(61:130)=data(1)(2:71)
                     endif
                  endif
c
c----------------------------------------------------------------------
c   email   
c
c   possibly send email, mail adr in SEISAN.DEF. email can be sent even 
c   if not located depending on parameters. BUT only if real time.
c   if autopic has been used, it is still the number of triggers
c   that determines if a mail is sent, not the number of stations
c   after autopic.
c   do not send if distant event
c   
c---------------------------------------------------------------------
c
               if(realtime.and.type.eq.'L') 
c
c   summary
c
     *            call netdet_email_summary
     *            (sfil_name,min_stat_email,min_mag_email,ntrig_last)
c
c   whole file
c
                  call netdet_email
     *            (sfil_name,min_stat_email,min_mag_email)
               endif

               last_trig_used=j-1

            endif     ! if net trigger
          endif       ! no trig loop so check

      enddo           ! all trigger loop

c
c  remove station triggers used but only if a net trigger since else
c  they might be used when more data comes in
c fix 
      if(last_trig_used.gt.0) then        ! there were at least one net trigger
c         ntrig=ntrig-last_trig_used
      endif
      
      

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine wait(abs_time,real_time_delay)
c
c  wait processing until after abs time, accuracy 1 sec
c
      implicit none
      real*8 abs_time,msec
      integer year,month,day,hour,min
      real sec
      real real_time_delay                ! delay in s of real time operation      


  1   continue
      call gmt_time(year,month,day,hour,min,sec,msec)    
      msec=msec-real_time_delay    

      if(msec.gt.abs_time) then
         return
      else
         call sleep(1)
         goto 1
      endif
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

C HEAPSORT ALGORITHM, FROM NUMERICAL RECIPES
C	Sorts an array ra(1:n) into ascending numerical order using the Heapsort algorithm. n is 
C	input; ra is replaced on output by its sorted rearrangement. 
C   MODIFIED FOR CONDET: ADD accompanying ARRAYS cas cac raa and rab,
C   and make ra double precision
c
c   nov 2021  changed xhpsort to yhpsort, raa is now integer, cac 7 chars 

	SUBROUTINE yhpsort(n,ra,cas,cac,raa,rab)
        implicit none 
	INTEGER n             ! number of triggers 
	double precision ra(n)
	integer raa(n)   
      	reAl rab(n)     
	character*5 cas(n)    ! station
	character*7 cac(n)    ! componenet
	INTEGER i,ir,j,l 
	double precision rra  ! abs trigger time
        integer rraa
	real rrab
	character*5 ccas     
	character*7 ccac
	integer iia
	
	if (n.lt.2) return 
C	The index l will be decremented from its initial value down to 1 during the
C   hiring (heap creation) phase. Once it reaches 1, the index ir will be
C   decremented from its initial value down to 1 during the
C   Òretirement-and-promotionÓ (heap selection) phase. 
	l=n/2+1 
	ir=n 
10 	continue 
	if (l.gt.1) then
		l=l-1 
		rra=ra(l) 
		ccas=cas(l)
		ccac=cac(l)
		rraa=raa(l)
		rrab=rab(l)
	else
		rra=ra(ir) 
		ccas=cas(ir)
		ccac=cac(ir)
		rraa=raa(ir)
		rrab=rab(ir)
		ra(ir)=ra(1)  
		cas(ir)=cas(1)
		cac(ir)=cac(1)
		raa(ir)=raa(1)
		rab(ir)=rab(1)
		ir=ir-1 
		if (ir.eq.1) then  
			ra(1)=rra 
			cas(1)=ccas
			cac(1)=ccac
			raa(1)=rraa
			rab(1)=rrab
			return 
		endif 
	endif 
	i=l
	j=l+l 
20	if(j.le.ir)then
		if (j.lt.ir) then 
		if (ra(j).lt.ra(j+1)) j=j+1 
		endif 
		if(rra.lt.ra(j)) then 
			ra(i)=ra(j) 
			cas(i)=cas(j)
			cac(i)=cac(j)
			raa(i)=raa(j)
			rab(i)=rab(j)
			i=j 
			j=j+j 
		else
			j=ir+1 
		endif 
		goto 20 
	endif 
	ra(i)=rra
	cas(i)=ccas
	cac(i)=ccac
	raa(i)=rraa
	rab(i)=rrab
	goto 10 
	END 

         
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine get_netdet_def(realtime,parfile,nstat,
     *   startdate,stopdate,
     &   station,component,flow,fhigh,nfilt,
     &   sta_duration,lta_duration,trigger_ratio,
     &   detrigger_ratio,trigger_min_dur,trigger_min_int,
     &   freeze,
     &   pre_event,extduration,
     &   netwindow,netmindet,
     &   wav_extract, 
     &   base,make_sfile,copy_wav,copy_base,real_time_delay,
     &   min_stat_location_local,min_stat_location_dist,
     &   min_stat_email,min_mag_email,agency,
     &   n_chan_out,chan_out_stat,chan_out_comp,chan_out_net,
     &   chan_out_loc,debug,debug_station,
     &   operator,netdet_dir,autopic,command,fix_depth,
     &   flow_dist,fhigh_dist,ndist_test)

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
      logical realtime              ! true if realtime
      integer seiclen

      logical debug                      ! true if debug output
      character*5 debug_station          ! station to debug for sta lta

      character*3 agency
      character*(*) startdate,stopdate
      character*5 station(*)              ! station code
      character*7 component(*)            ! component, network and location
      logical freeze                      ! true if freze lta
      integer nfilt                       ! number of filter sets
      real flow(3),fhigh(3)               ! filter to use
      real flow_dist,fhigh_dist           ! filter for det. dist. ev.
      integer ndist_test                  ! minimum number of triggers to do
                                          ! dist test
      real sta_duration(3),lta_duration(3)      ! durations of sta and lta
      real trigger_ratio(3),detrigger_ratio(3)  ! trigger and detrigger ratio
      real trigger_min_int(3)             ! min time between channel triggers
      real trigger_min_dur(3)             ! min duration of trigger
      real pre_event                      ! preevent time when extracting
      real extduration                    ! duration of extraction
      real netwindow
      integer netmindet                   ! minimum number of triggers in window 
      real real_time_delay                ! delay in s of real time operation 
      integer min_stat_location_local     ! min number of stations for location local
      integer min_stat_location_dist      ! min number of stations for location dist
      integer min_stat_email              ! min ----------------  for email
      real min_mag_email                  ! min magnitude for email

      integer n_chan_out                  ! number of channels to write out
      character*5 chan_out_stat(*)        ! stat for write out
      character*3 chan_out_comp(*)        ! component --------
      character*2 chan_out_net(*)         ! network ----------
      character*2 chan_out_loc(*)         ! location ---------

      character*1 dchar
      character*60 top_directory


      logical wav_extract                 ! if true, extract wav file on the fly after net detection
      logical make_sfile                  ! if true, make s-file
      character*5 base                    ! base for s-file
      character*3 operator                ! operator for s-file
      character*1 copy_wav                ! copy, move or leave wav 
      character*5 copy_base               ! base to move wav to

      character*40 command                ! command  after location
      character*40 netdet_dir             ! temp netdet dir
      real autopic                        ! if 1.0 use AUTOPIC
      real fix_depth                      ! depth to fix
 
      call topdir(top_directory)
      call dir_char(dchar)   ! directory separation character

     
c
c   read par file, if real time operation, it must be in DAT
c

      if(.not.realtime) then
         call sei get file( open$+ignore$,   ! Open waveform file.
     &                      in,              ! On unit.
     &                      code,            ! Returned condition.
     &                      'DAT',           ! Alternative search directory.
     &                      parfile )        ! For this filename.

      
         if(code.ne.e_ok$) then
           write(*,*) ' Parameter file does not exist: ',parfile
           stop
         endif
      else
         text=top_directory
     *   (1:seiclen(top_directory))//dchar//'DAT'//dchar//
     *    parfile(1:seiclen(parfile))
         open(50,file=text(1:seiclen(text)),status='old',err=50)
         goto 60
 50      continue
         write(*,*) ' Parameter file does not exist: ',parfile

         stop
 60      continue
         in=50
      endif      
c
c init values
c

      agency=' '
      startdate=' '
      stopdate=' '
      station(1)=' '
      component(1)=' '
      do i=1,3
         sta_duration(i)=0.
         lta_duration(i)=0.
         trigger_ratio(i)=10.
         detrigger_ratio(i)=10.
         trigger_min_int(i)=0.
         trigger_min_dur(i)=10.
      enddo
      freeze=.false.
      debug=.false.
      debug_station=' '

      nstat=0  ! station counter
      pre_event=60.
      extduration=180.
      netwindow=0.
      netmindet=1
      
      operator=' '
      netdet_dir=' '
      autopic=0.0
      command=' '
      fix_depth=-10.0

      do i=1,3
        flow(i)=0.0
        fhigh(i)=0.0
      enddo

      wav_extract=.false.
      make_sfile=.false.
      copy_wav=' '
      base=' '
      copy_base=' '
      n_chan_out=0

      min_stat_email=999
      min_mag_email=-999.0

      min_stat_location_local=999
      min_stat_location_dist=999

      flow_dist=7.0
      fhigh_dist=14.0
      ndist_test=4


100   continue

c
c read text line from file, check for code and set variables
c
      read(in,'(a80)',end=300,err=200) line

      if (line(1:10).eq.'START DATE') then
        read(line(41:54),'(a)') startdate
      elseif (line(1:9).eq.'STOP DATE') then
        read(line(41:54),'(a)') stopdate
      elseif (line(1:6).eq.'AGENCY') then
        agency=line(41:43)
        operator=line(51:53)
      elseif (line(1:10).eq.'NETDET DIR') then
        netdet_dir=line(41:80)
      elseif (line(1:12).eq.'CHANNEL TRIG') then
        nstat=nstat+1
        read(line(41:45),'(a)') station(nstat)
        read(line(51:57),'(a)') component(nstat)  ! includes network and location 
      elseif (line(1:11).eq.'CHANNEL OUT') then
        n_chan_out=n_chan_out+1
        read(line(41:45),'(a)') chan_out_stat(n_chan_out)
        read(line(51:53),'(a)') chan_out_comp(n_chan_out)  
        read(line(54:55),'(a)') chan_out_net(n_chan_out) 
        read(line(56:57),'(a)') chan_out_loc(n_chan_out) 
      elseif (line(1:10).eq.'FILTER LOW') then
        read(line(41:70),'(3f10.1)')flow(1),flow(2),flow(3) 
      elseif (line(1:11).eq.'FILTER HIGH') then
        read(line(41:70),'(3f10.1)') fhigh(1),fhigh(2),fhigh(3)
      elseif (line(1:10).eq.'STA LENGTH') then
        read(line(41:70),'(3f10.1)')sta_duration(1),sta_duration(2),
     *  sta_duration(3)
      elseif (line(1:10).eq.'LTA LENGTH') then
        read(line(41:70),'(3f10.1)') lta_duration(1),lta_duration(2),
     *  lta_duration(3)
      elseif (line(1:13).eq.'TRIGGER RATIO') then
        read(line(41:70),'(3f10.1)') trigger_ratio(1),trigger_ratio(2),
     *  trigger_ratio(3)
      elseif (line(1:15).eq.'DETRIGGER RATIO') then
        read(line(41:70),'(3f10.1)')
     * detrigger_ratio(1),detrigger_ratio(2),detrigger_ratio(3)
      elseif (line(1:10).eq.'FREEZE LTA') then
        read(line(41:50),'(f10.1)') var
        if (var.eq.1.) freeze=.true.
      elseif (line(1:17).eq.'MIN TRIG DURATION') then
        read(line(41:70),'(3f10.1)') trigger_min_dur(1),
     *  trigger_min_dur(2),trigger_min_dur(3)
      elseif (line(1:17).eq.'MIN TRIG INTERVAL') then
        read(line(41:70),'(3f10.1)') trigger_min_int(1),
     *  trigger_min_int(2),trigger_min_int(3)
      elseif (line(1:14).eq.'PRE EVENT TIME') then
        read(line(41:50),'(f10.1)') var
        if (var.gt.0.) pre_event=var
      elseif (line(1:14).eq.'NET WINDOW SEC') then
        read(line(41:50),'(f10.1)') var
        if (var.gt.0.) netwindow=var
      elseif (line(1:11).eq.'NET MIN DET') then
        read(line(41:50),'(f10.1)') var
        if (var.gt.0.) netmindet=int(var)
      elseif (line(1:16).eq.'EXTRACT DURATION') then
        read(line(41:50),'(f10.1)') var
        if (var.gt.0.) extduration=var
      elseif (line(1:11).eq.'WAV EXTRACT') then
        read(line(41:50),'(f10.1)') var
        if (var.gt.0.) wav_extract=.true.
      elseif (line(1:8).eq.'COPY WAV') then
        read(line(41:41),'(a1)') copy_wav
      elseif (line(1:10).eq.'MAKE SFILE') then
        read(line(41:50),'(f10.1)') var
        if (var.gt.0.) make_sfile=.true.
      elseif (line(1:10).eq.'BASE SFILE') then
        read(line(41:45),'(a)') base
      elseif (line(1:8).eq.'WAV BASE') then
        read(line(41:45),'(a)') copy_base
      elseif (line(1:15).eq.'REAL TIME DELAY') then
        read(line(41:50),'(f10.1)') real_time_delay
      elseif (line(1:23).eq.'MIN STAT LOCATION LOCAL') then
        read(line(41:50),'(f10.1)') var
        min_stat_location_local=var
      elseif (line(1:22).eq.'MIN STAT LOCATION DIST') then
        read(line(41:50),'(f10.1)') var
        min_stat_location_dist=var
      elseif (line(1:14).eq.'MIN STAT EMAIL') then
        read(line(41:50),'(f10.1)') var
        min_stat_email=var
      elseif (line(1:13).eq.'MIN MAG EMAIL') then
        read(line(41:50),'(f10.1)') min_mag_email
      elseif (line(1:5).eq.'DEBUG') then
        read(line(41:60),'(f10.1,a5)') var,debug_station
        if(var.eq.1.0) debug=.true.
      elseif (line(1:7).eq.'COMMAND') then
        read(line(41:80),'(a)') command
      elseif (line(1:7).eq.'AUTOPIC') then
        read(line(41:50),'(f10.1)') autopic
      elseif (line(1:8).eq.'FIX DEPT'.and.line(41:50).ne.' ') then
        read(line(41:50),'(f10.1)') fix_depth
      elseif (line(1:14).eq.'FILTER DISTANT'.and.line(41:70).ne.' ')
     *then
        read(line(41:70),'(3f10.1)') flow_dist,fhigh_dist,var
        ndist_test=var
      endif

      goto 100

200   continue
      write(*,*) ' Error in netdet.par file'
      return

300   continue


      if(copy_base.ne.' ') then
         do i=2,5
            if(copy_base(i:i).eq.' ') copy_base(i:i)='_'
         enddo
      endif

      if(.not.realtime) then
         call sei close( close$, in, code )
      else
         close(in)
      endif
c
c   find how many filter sets
c
      nfilt=0
      do i=1,3
        if(flow(i).ne.0.0.or.fhigh(i).ne.0.0) nfilt=nfilt+1
      enddo
      if(nfilt.eq.0) then
         write(6,*)'no filters, stop'
         stop
      endif
c
c   make sure other paramters are set, if not give
c   the ones from filt=1 are used
c
      if(nfilt.eq.2.or.nfilt.eq.3) then
         if(sta_duration(2).eq.0.0) sta_duration(2)=sta_duration(1)
         if(lta_duration(2).eq.0.0) lta_duration(2)=lta_duration(1)
         if(trigger_ratio(2).eq.0.0) trigger_ratio(2)=trigger_ratio(1)
         if(detrigger_ratio(2).eq.0.0) 
     *   detrigger_ratio(2)=detrigger_ratio(1)
         if(trigger_min_int(2).eq.0.0)
     *   trigger_min_int(2)=trigger_min_int(1)
         if(trigger_min_dur(2).eq.0.0)
     *   trigger_min_dur(2)=trigger_min_dur(1)
      endif

      if(nfilt.eq.3) then
         if(sta_duration(3).eq.0.0) sta_duration(3)=sta_duration(1)
         if(lta_duration(3).eq.0.0) lta_duration(3)=lta_duration(1)
         if(trigger_ratio(3).eq.0.0) trigger_ratio(3)=trigger_ratio(1)
         if(detrigger_ratio(3).eq.0.0) 
     *   detrigger_ratio(3)=detrigger_ratio(1)
         if(trigger_min_int(3).eq.0.0)
     *   trigger_min_int(3)=trigger_min_int(1)
         if(trigger_min_dur(3).eq.0.0)
     *   trigger_min_dur(3)=trigger_min_dur(1)
      endif

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine copy_waveform_file(wav_file,copy_wav,
     *copy_base,full_wav)
c
c   mv or copy waveform file to WAV or WAV data base
c   assume standard name
c
      implicit none
      include 'libsei.inc'                ! Open file definitions
      include 'seidim.inc'
      include 'waveform.inc'
      include 'seisan.inc'
    
      integer seiclen
      character   long_text*200
      character*80 text
      integer year,month
      character*80 wav_file      ! waveform file to copy
      character*80 full_wav     ! ------------- name with path
c-- file existence
      logical exist
c returned code
      integer code
      character*1 copy_wav       ! copy, move or leave wav 
      character*5 copy_base      ! base to move wav to
c
c  for waveform copy
c
       character*4 cwavyear
       character*2 cwavmon
c-- path to seismo                             
       character*60 top_directory
c-- directory separator
      character*1 dchar
c-- computer type
      logical pc,sun,linux

                                                                               
c   get path to seismo                                                          
c                                                                               
      call topdir(top_directory)

c
      call dir_char(dchar)   ! directory separation character
c
c   get computertype
c    
      call computer_type(sun,pc,linux)
c
c   save full wav names, initally without path if local
c   directory
c
cnew
      full_wav=' '
cnew      full_wav= wav_file(:seiclen(wav_file))
      full_wav= wav_file

      if(copy_wav(1:1).eq.'m'.or.copy_wav(1:1).eq.'c' )then

        read(wav_file(1:18),'(i4,1x,i2)') year,month 
        write(cwavyear,'(i4.4)') year
        write(cwavmon,'(i2.2)') month

        text(1:5)='move '
        if(copy_wav.eq.'c') text(1:5)='copy '

        if( pc ) then
           if(copy_base.eq.' ') then   ! copy to WAV
              long_text = text(1:5)// wav_file(:seiclen(wav_file)) //
     &        ' '                                    //
     &        top_directory(:seiclen(top_directory)) //
     &        dchar//'WAV'//dchar                    //
     &        wav_file(:seiclen(wav_file))
           else
              long_text = text(1:5)// wav_file(:seiclen(wav_file)) //
     &        ' '                                    //
     &        top_directory(:seiclen(top_directory)) //
     &        dchar//'WAV'//dchar                    //
     &        copy_base//dchar//cwavyear(1:4)//dchar //
     &        cwavmon(1:2)//dchar
           endif

        else if( sun.or.linux ) then
           text(1:5)='mv   '
           if(copy_wav(1:1).eq.'c') text(1:5)='cp   '
           if(copy_base.eq.' ') then
              long_text = text(1:5)// wav_file(:seiclen(wav_file)) //
     &        ' '                                    //
     &        top_directory(:seiclen(top_directory)) //
     &        dchar//'WAV'//dchar                    //
     &        wav_file(:seiclen(wav_file))
c
c   save full wav name
c
              full_wav=top_directory(:seiclen(top_directory)) //
     &        dchar//'WAV'//dchar                    //
     &        wav_file(:seiclen(wav_file))

           else
              long_text = text(1:5)// wav_file(:seiclen(wav_file)) //
     &        ' '                                    //
     &        top_directory(:seiclen(top_directory)) //
     &        dchar//'WAV'//dchar                    //
     &        copy_base//dchar//cwavyear(1:4)//dchar //
     &        cwavmon(1:2)//dchar
c
c   save full wavname
c
              full_wav=top_directory(:seiclen(top_directory)) //
     &        dchar//'WAV'//dchar                    //
     &        copy_base//dchar//cwavyear(1:4)//dchar //
     &        cwavmon(1:2)//dchar//wav_file(:seiclen(wav_file))


           endif
        endif

        write(6,*) long_text
        write(6,*) full_wav
    
c
c   now copy or move
c
        call systemc( long_text,seiclen(long_text) )
c
c  check that file got there
c
        call sei open( check$,             ! Check file exists.
     &                ' ',                 ! No prompt.
     &  long_text(7+seiclen(wav_file):seiclen(long_text)),
     &                0,                   ! Unit (n/a).
     &                exist,               ! File exists?.
     &                code )               ! Condition (n/a).
        if(exist) then
           if(copy_base.eq.' ') then
              write(6,*)' File transferred to WAV **********'
           else
              write(6,*)' File transferred to WAV base ',
     *        copy_base,' **********'
           endif
        else
           if(copy_base.eq.' ') then
              write(6,*)' Failure to transfer to WAV base ',
     *        copy_base,' **********'
           endif
         endif
  
      endif

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

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc   

c
c   subroutine to locate and calculate magnitude for one event. 
c   the input file is infile and it is overwritten at output.
c   the event is first located with outlier removal and then
c   magnitudes are determined and the event is updated again.
c
c   
c   jh december 2021
c
      subroutine locate_magnitude
     *(infile,data,command,operator,agency,copy_base,type)
c
c  infile is s-file
c


      implicit none                       
      include 'seisan.inc'                
      include 'seidim.inc'
      include 'rea.inc'
      integer seiclen                     ! function

      character*200 text                  ! general text     
      character*80 infile                 ! input file and output file

      logical outlier                     ! if true, remove outlier
      logical do_mag                      ! if true, do magnitudes

      character*1 type                    ! event type L or D

      character*40 command                ! optional command after location
      character*3 operator
      character*3 agency
      character*5 copy_base               ! base to move/copy to


      character*1 comp_select             ! component to use
      character*1 spectrum_type           ! P or S spectrum, defualt P
      real wa_window                      ! window for ml
      character*7 wa_window_text          ! -- in text
      real spec_window                    ! window for spectrum
      character*7 spec_window_text        ! -- in text
      integer nstat                       ! number of stations in
      character*80 data(max_data)
      integer i,code                      ! counter



c  set default values
c

      wa_window=50.0     ! window for ml
      spec_window=20.0   ! window for mw

      outlier=.true.     ! remove outliers
      do_mag=.true.      ! calculate magnitudes

      spectrum_type=' '  ! default S
      comp_select=' '    ! default use Z

c
c   check s-file format
c
      if(new_nordic_format) then
         rea_new_out=.true.              ! write new format
      endif 
c
c  save number of stations, will be removed if location fails
c
      nstat=hyp_nstat(1)    
c
c-----------------------------------------------------------------
c  locate
c------------------------------------------------------------------
c       
      text='hyp '//infile//' -reject -overwrite -update -op '//operator
      write(6,'(a)')text(1:seiclen(text))
      call systemc(text,seiclen(text))
c
c   check if location, if not, restore header to what it was so
c   number of stations is shown and no agency etc
c
      open(88,file=infile,status='old',err=5)
      goto 6
 5    continue
      write(6,*)'File not there:', infile
      goto 7
 6    continue
      call rea_event_in(88,.true.,data,code)   
      if(hyp_lat(1).lt.-900.0) then
         hyp_depth(1)=-999.0
         hyp_nstat(1)=nstat
         hyp_agency(1)=' '
         hyp_depth_flag(1)=' '
         rewind(88)
         call rea_event_out(88,.true.,data,code)      
      endif
      close(88)
 7    continue
c
c--------------------------------------------------------------------
c   magnitudes, any previous results of same stat-comp are owerwritten
c   only for local quakes
c--------------------------------------------------------------------
c

      if(do_mag.and.type.eq.'L') then  ! for now hardwired to do it
c
c   check if location
c
         if(hyp_lat(1).lt.-900.0) return

         text=' '
         write(spec_window_text,'(f7.1)') spec_window
         write(wa_window_text,'(f7.1)') wa_window
         text='automag '//infile(1:seiclen(infile))//' s '
     *   //spec_window_text//
     *   ' w '//wa_window_text//' overwrite '//
     *   ' '//comp_select//' '//spectrum_type//' -op '//
     *   operator//' -ag '//agency
         if(copy_base.ne.' ') text=text(1:seiclen(text))//
     *   ' -wavbase '//copy_base 

         write(6,'(a)') text(1:seiclen(text))
         call systemc(text,seiclen(text))
c
c   locate to update magnitudes
c
         text='hyp '//infile//' -update -overwrite -op '//operator
         write(6,'(a)') text(1:seiclen(text))
         call systemc(text,seiclen(text))
c
c   execute local script if given 
c
         if(command.ne.' ') then
             write(6,'(a)') command
             call systemc(command,seiclen(command))
          endif
      endif

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


       subroutine netdet_email(evfile,minstat,minmag)
     
c
c   send email if given criteria are fullfilled: there must be
c   minstat stations and magnitude larger than minmag.
c   if minmag is -1000, magnitude is not used as a limiting critria.
c   so even when no location and magnitude, email can be sent
c   since magnitud 	is initilized to  -999.
c   magnitude is first magnitude on line.
c   this routine sends the whole file, format is nordic
c
      implicit none
      include 'seidim.inc'
      include 'rea.inc'
      include 'seisan.inc'
      include 'libsei.inc'
      integer minstat
      real minmag
      character*80 data(max_data)   ! s-file content
      character*80 text
      character*80 evfile      ! s file
      integer seiclen
      integer code             ! error code
      logical b_old 
      integer i, kfile,k  

c
c   check if any mail defined
c    
      if(n_alertemail_netdet.eq.0) return

      call sei open(old$+warn$,             ! Open a existing file.
     &               ' ',                   ! Prompt file name (n/a).
     &               evfile(1:seiclen(evfile)), ! File name
     &               kfile,                 ! Write unit kfile
     &               b_old,                 ! Already exists?  (n/a).
     &               code)                  ! Returned condition.
c
c   read all parameters for one event from file unit kfile
c
      call rea_event_in(kfile,.true.,data,code)
      call sei close (close$,kfile,code) 

      if(hyp_mag(1,1).ge.minmag.and.hyp_nstat(1).ge.minstat) then
      
c
c   convert format to nordic if needed
c
           if(alertemail_format.eq.0) then
              call systemc('nor2nor2 -format 1 -file '
     *        //evfile(1:seiclen(evfile)),25+seiclen(evfile))
           else
              call systemc('cp '//evfile(1:seiclen(evfile))//
     *        ' nor2nor2.out',3+seiclen(evfile)+13)
           endif
          
           do k=1,n_alertemail_netdet

              text=
     &        mailx(1:seiclen(mailx))//" "//
     &        alertemail_netdet(k)
     *        (1:seiclen(alertemail_netdet(k)))//"<"//
     *        'nor2nor2.out'
              write(6,*) text

             call systemc(
     &       mailx(1:seiclen(mailx))//" "//
     &       alertemail_netdet(k)
     *       (1:seiclen(alertemail_netdet(k)))//"<"//
     *       'nor2nor2.out',
     *        seiclen(mailx)+seiclen(alertemail_netdet(k))+12+2)
           enddo

      endif

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine netdet_email_summary(evfile,minstat,minmag,ntrig_last)
     
c
c   send email if given criteria are fullfilled: there must be
c   minstat stations and magnitude larger than minmag.
c   if minmag is -1000, magnitude is not used as a limiting critria.
c   so even when no location and magnitude, email can be sent
c   since magnitud 	is initilized to  -999.
c   magnitude is first magnitude on line.
c
      implicit none
      include 'seidim.inc'
      include 'rea.inc'
      include 'seisan.inc'
      include 'libsei.inc'
      integer minstat
      real minmag
      integer ntrig_last       ! number fo triggers in event
      character*80 data(max_data)   ! s-file content
      character*80 text
      character*80 evfile      ! s file
      character*140 long_text
      integer seiclen
      integer code             ! error code
      logical b_old 
      character*1 ew,ns
      integer i, kfile,k    

      if(n_alertemail_sum_netdet.eq.0) return
  

      call sei open(old$+warn$,             ! Open a existing file.
     &               ' ',                   ! Prompt file name (n/a).
     &               evfile(1:seiclen(evfile)), ! File name
     &               kfile,                 ! Write unit kfile
     &               b_old,                 ! Already exists?  (n/a).
     &               code)                  ! Returned condition.
c
c   read all parameters for one event from file unit 1
c
      call rea_event_in(kfile,.true.,data,code)
      call sei close (close$,kfile,code) 

      if(hyp_mag(1,1).ge.minmag.and.hyp_nstat(1).ge.minstat) then
      
c
c   make summary file
c
     
           open(121,file='summary.out', status='unknown')
           write(121,'(a)')'Netdet automatic location'
           
           write(121,'(a)')
           if(hyp_mag(1,1).gt.0.0) then
              write(121,'(a,f4.1,a,a)')  'Magnitude:          ',
     *        hyp_mag(1,1),' M',hyp_mag_type(1,1)
           else
              write(121,'(a)')  'Magnitude:          '
           endif 

           write(text,'(i4,a1,i2,a1,i2,1x,i2,a1,i2,a1,i2)')
     *     hyp_year(1),'-',hyp_month(1),'-',hyp_day(1),hyp_hour(1),':'
     *     ,hyp_min(1),':',int(hyp_sec(1))
           if(text(6:6).eq.' ') text(6:6)='0'
           if(text(9:9).eq.' ') text(9:9)='0'   
           if(text(12:12).eq.' ') text(12:12)='0'
           if(text(15:15).eq.' ') text(15:15)='0' 
           if(text(18:18).eq.' ') text(18:18)='0'          
           write(121,'(a,a)')
     *     'Date and time UTC:   ',text(1:19)

           if(hyp_lat(1).gt.-900.0) then
              ns='N'
              if(hyp_lat(1).lt.0.0) ns='S'
              ew='W'
              if(hyp_lon(1).lt.0.0) ew='W'
              write(121,'(a,f7.3,1x,a1,f8.3,1x,a1)') 
     *        'Location:           ',
     *        abs(hyp_lat(1)),ns, abs(hyp_lon(1)),ew 
           else
              write(121,'(a)') 
     *        'Location:           '
           endif
          
           if(hyp_depth(1).gt.-900) then                     
              if(hyp_depth(1).ge.100.0) write(121,'(a,f8.1,a)')
     *        'Depth:            ',hyp_depth(1),' km'  
              if(hyp_depth(1).lt.100.0.and.hyp_depth(1).ge.10.0) 
     *        write(121,'(a,f7.1,a)')
     *        'Depth:            ',hyp_depth(1),' km'  
              if(hyp_depth(1).lt.10.0) 
     *        write(121,'(a,f6.1,a)')
     *        'Depth:            ',hyp_depth(1),' km'
           else
              write(121,'(a)')
     *        'Depth:            '
           endif  

           write(121,'(a,i4)')
     *     'Number of stations:  ',hyp_nstat(1)  
           write(121,'(a,i4)')
     *     'Number of triggers:  ',ntrig_last  
                               
                               
           if(rea_locality.ne.' ') then
              do i=1,10
                if(rea_locality(i:i).ne.' ') then
                   k=i
                   goto 1
                endif
              enddo
 1            continue
 
              write(121,'(a,a)')  'Region:              ',
     *        rea_locality(k:seiclen(rea_locality))
           endif 
             
           rewind(121)        

           write(6,*)
           write(6,'(a)') 'Text to send'
           write(6,*)

           do i=1,20
             read(121,'(a)',end=5) text
             write(6,'(a)') text
           enddo  
 5         continue
           close(121)

           if(n_alertemail_sum_netdet.eq.0) then
              write(6,*)'No email defined'
              goto 10
           endif
c
c   send email to list
c
           do k=1,n_alertemail_sum_netdet
                long_text=
     &          mailx(1:seiclen(mailx))//" "//
     &          alertemail_sum_netdet(k)
     *          (1:seiclen(alertemail_sum_netdet(k)))//"<"//
     *          'summary.out'        
             write(6,*) long_text
             call systemc(long_text,seiclen(long_text))
           enddo
      endif

 10   continue

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine read_sfile_head(sfile,text)
c
c   read first line in file sfile
c
      implicit none
      character*80 sfile,text
      open(120,file=sfile,status='old', err=1)
      read(120,'(a)')text
 1    continue
      close(120)
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine read_s_file(sfile)

c
c   open and read s-file
c

      implicit none
      include 'seidim.inc'
      include 'libsei.inc'
      include 'rea.inc'
      character*(*) sfile       
      integer read_unit
      logical b_flag
      integer code
      character*80 data(max_data)
      
              call sei open( old$,            ! Open old file.
     &                  ' ',                  ! No prompt.
     &                  sfile,                ! This file.
     &                  read_unit,            ! On this unit.
     &                  b_flag,               ! Existance?.
     &                  code )                ! Condition (n/a).
             call rea_event_in(read_unit,.true.,data,code)
             call sei close(close$,read_unit,code)    ! Close (stop on error).
       return
       end
     
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc



      subroutine add_to_err_file(unit,filename,text)
c
c  add text to error file filename on unit unit
c
      implicit none
      integer unit
      character*80 text,filename

      open(unit,file=filename,status='old',position='append')
      write(unit,'(a)') text
      close(unit)
      return
      end 


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


     


      subroutine event_type(f1,f2,type,full_wav)
c
c   routine filters the signal in pass f1 to f2. it then compares the average 
c   noise level to the average P-signal level in a 5s window. The intention is to
c   identify a distant event by filtring at a high frequecy like 5-10hz. if the
c   s/n is less than 2 for a particular station, the event is considereda distant
c   event for that station. to delare the event to be distant, at least 4 different 
c   stations must be used and 80% of the s/n must have a s/n less than 2. 
c   in addtion, the s/n level must be more than 1.6 in the band 1.3 to 2.5 for
c   at least half the stations.
c
c   ******  condiitons to be experimented with ************
c
c   f1,f2 : hf filter
c   type  : default L, D if declared distant
c   full_wav: wav name with path

      implicit none                       ! force delcaration of all variables
      include 'seidim.inc'                ! dimensions for rea block
      include 'seisan.inc'                ! seisan parameters
      include 'waveform.inc'              ! waveform data
      include 'rea.inc'                   ! parameter common bliock
      include 'libsei.inc'                ! for all seisan subroutines

      character*80 data(max_data)         ! s-file with data in text array
      character*80 err_text               ! error text
      character*80 full_wav               ! wav_name with path
      integer npresent                    ! number of wav files present
      
      character*80 text                   ! general text string
      
      integer seiclen                     ! function

      integer nsamp                       ! number of samples
c-- filter coefficients                                       
      real      cof(8)          
c-- filter gain                                                  
      real      gain

      logical all                         ! true: read all data, false: select
      integer code                        ! error return code
      integer year,month,day,hour,min,doy
      real sec
      double precision p_time
      character*5 stat(1000),statsel(1000)
      character*4 comp(1000),compsel(1000)
      character*2 location(1000)
      character*2 network(1000)
      character*2 co(1000)
      character*1 type       ! distance id
      
      real av                          ! average
      real signal,noise
         
      integer nstat_process            ! number of channels to process
   
      real f1,f2                       ! fband to use
      integer nbad_hf,ngood_hf,nbad_lf,ngood_lf      ! count bad and good s/n
      integer nbad_vlf,ngood_vlf
      integer i,k,l,m,n,j  ! counters etc

c
c   get seisan parameters, here attenuation parameters
c
c     call get_seisan_def

      type='L'
      nbad_hf=0
      ngood_hf=0
      nbad_lf=0
      ngood_lf=0
      nbad_vlf=0
      ngood_vlf=0

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc      




      all=.true.        ! read all parameters from s-file
 
      k=0      

c
c  initialize wav reading
c
      call wav_init
      call wav_mem_init
c
c   read all parameters for one event from file unit 88
c
      call rea_event_in(88,all,data,code)
c
c   check if from archive or the given wav name
c
      if(full_wav(1:3).eq.'ARC') then
         call auto_tr(data,rea_nhead,rea_nrecord,
     *   wav_nfiles,wav_filename)
         do i=1,wav_nfiles
            call  get_full_wav_name(wav_filename(i),text)
            if(text.ne.' ') then
               npresent=npresent+1
               wav_filename(npresent)=text
            endif
          enddo
      else
         wav_filename(1)=full_wav
         wav_nfiles=1
      endif

      write(6,'(1x,a)') wav_filename(1)
         
c
c   read all headers of files 
c

      do i=1,wav_nfiles
         call read_wav_header(i)
      enddo
c
c   output possible errors
c
      if(wav_error_message.ne.' ') then
          write(6,'(1x,a)')  wav_error_message
      endif
      write(6,*)' Total number of channels available:',wav_nchan
c
c   terminate if no channels
c
      if(wav_nchan.eq.0) then
        write(6,*)'No channels found'
        return
      endif
c
c   write some input info for each channnel on screen
c
      write(6,*)
      write(6,'(a,a)')
     *'CHA STAT  COMP  YEAR MO DA HR MI    SEC   NSAMP ',
     *'    RATE    DELAY DURATION'
      do i=1,wav_nchan
         write(6,'(i3,1x,a,1x,a,1x,i5,4i3,f7.3,i8,3f9.3)')
     *   i, wav_stat(i),wav_comp(i),
     *   wav_year(i),wav_month(i),wav_day(i),wav_hour(i),wav_min(i),
     *   wav_sec(i),wav_nsamp(i),wav_rate(i),wav_delay(i),
     *   wav_duration(i)
      enddo

c
c   write the whole first header line
c
       write(6,'(a)') data(1)(1:79)


c
ccccccccccccccccccccccccccc select stations cccccccccccccccccccccccc
c
c
c   find stations in file with P  readings, 
c    "Spectral phases" are not checked.
c


       k=0
       do i=1,rea_nphase
          if((rea_phase(i)(1:1).eq.'P').
     *    and.rea_phase(i)(1:4).ne.'SPEC') then
             k=k+1
             stat(k)=rea_stat(i)
             co(k)=rea_co(i)
          endif
       enddo
       write(6,'(7(a5,1x,a2,1x))')(stat(i),co(i),i=1,k)
c
c   remove duplicates
c
       do i=1,k
          do j=i+1,k
            if(stat(i).eq.stat(j)) stat(j)=' '
          enddo
       enddo
       m=0
       do i=1,k
          if(stat(i).ne.' ') then
             m=m+1
             stat(m)=stat(i)
             co(m)=co(i)
          endif
       enddo


c
c  find which of these stations have comp_select in wav file and 
c  select only those, make sure same type of channel (e.g. S, B) 
c  as used to read phase
c 

       k=0 
       do i=1,m
         do j=1,wav_nchan
            if(stat(i).eq.wav_stat(j).and.
     *      wav_comp(j)(1:1).eq.co(i)(1:1)) then
               k=k+1
               statsel(k)=stat(i)
               compsel(k)=wav_comp(j)
               location(k)=wav_location(j)
               network(k)=wav_network(j)
               goto 42
            endif
         enddo
 42      continue
       enddo
       nstat_process=k

      do i=1,k
        stat(i)=statsel(i)
        comp(i)=compsel(i)
      enddo

      write(6,*)
      write(6,*) 'Components auto selected to use'
      write(6,'(7(a5,1x,a4,1x))')(stat(i),comp(i),i=1,k)
                    
c-------------------------------------------------------------------
c   loop over channels selected for analysis
c-------------------------------------------------------------------

      do m=1,nstat_process
         write(6,*)
         write(6,'(a)')'**********************************************'
         write(6,'(a,a5,1x,a4,1x,a)')'***** process ',stat(m),comp(m),
     *   ' ********************'
c
c   look for P-phase
c
         p_time=0.0
         do i=1,rea_nphase
           if(rea_phase(i)(1:1).eq.'P'.and.rea_stat(i).eq.stat(m).and.
     *     rea_phase(i)(1:4).ne.'SPEC'.and.rea_weight_in(i).ne.'4') then
              p_time=rea_abs_time(i)  ! abs p-time

              write(6,'(a,a)')' P time on component ',rea_co(i)
              call sectim(p_time, year,doy,month,day,hour,min,sec)

              write(6,'(a,i4,1x,2i2,1x,2i2,f6.1)') ' Time of P ',
     *        year,month,day,hour,min,sec
              goto 222   ! jump to process
           endif
         enddo
         write(6,'(a,a)')' No P-phase or weight 4 for station ',stat(m)

c
c   no phase found for stations
c   

      goto 500    ! next channel


222     continue      

c
c  find channel
c
         call wav_find_chan(stat(m),comp(m),k)
         if(wav_error_message.ne.' ')write(6,*) wav_error_message
 
         if(k.eq.0) then
           write(6,'(a,a5,1x,a4)') 'Channel not found: ',stat(m),comp(m)
           goto 500   ! try next
         endif
c
c   read whole channel
c
         call wav_read_channel(k)
c
c   check if sample rate is ok
c
c       write(6,*)'rate',wav_rate(k)
         if(wav_rate(k).le.f2*2.0) goto 500
      
c
c   select out time window, only for one channel
c
         wav_out_nchan=1        ! one channel
         wav_out_chan(1)=k      ! channel number
c
c  start time relative to start of particular channel
c
         wav_out_start(k)=p_time-wav_abs_time(wav_first)

c
c   look for amp in a 5s window
c
         wav_out_duration(k)=5.0 
c
c   find if data is available
c
         call wav_get_interval

          write(6,*) 'Select status, 4 is ok',wav_out_status(k)
          write(6,*) 'Start sample',wav_out_first_sample(k)
          write(6,*) 'Duration available', wav_out_duration(k)
          write(6,*) 'Number of samples', wav_out_nsamp(k)

c
c   skip if not enough data
c
         if(wav_out_status(k).ne.4) then
            write(6,*)' Not enough data, skip channel'
            goto 500
         endif
c
c  subtract dc
c
         av=0.0
         do i=1,wav_nsamp(k)
            av=av+signal1(i)
         enddo

         av=av/wav_nsamp(k)
         do i=1,wav_nsamp(k)
            signal1(i)=signal1(i)-av
         enddo
c
c   save a copy for more  test of s/n
c
         do i=1,wav_nsamp(k)
           signal2(i)=signal1(i)
           signal3(i)=signal1(i)
         enddo
        
c
c  filter for hf
c
         call bndpas(f1,f2,1000.0/wav_rate(k),  
     *   cof,gain) 
c                                                  
c-- apply filter
c
         call filter(signal1,wav_nsamp(k),cof,gain,2) 
       

         nsamp=wav_out_nsamp(k)


         av=0.0
c
c   average of hf signal
c
         do i=wav_out_first_sample(k),
     *   wav_out_first_sample(k)+wav_out_nsamp(k)-1
          av=av+abs(signal1(i))
         enddo

         av=av/nsamp
         write(6,*)'av hf filter signal',av  
         signal=av 
c
c   average of noise, 3 nsamp after start to avoid tapering
c
       
         av=0.0
         do i=3*nsamp,4*nsamp-1
            av=av+abs(signal1(i))
         enddo

         av=av/nsamp
         write(6,*)'av hf filter noise',av 
         noise=av  
      
c
c   calculate s/n and count bad ones
c
         if(signal/noise.lt.2.0) then
            nbad_hf=nbad_hf+1
         else
            ngood_hf=ngood_hf+1
         endif
         write(6,*)'ratio hf ',signal/noise
              
c
c  filter for lf
c
         call bndpas(1.6,3.0,1000.0/wav_rate(k),
     *   cof,gain)
c
c-- apply filter
c
         call filter(signal2,wav_nsamp(k),cof,gain,2)


         av=0.0
c
c   average of lf signal
c
         do i=wav_out_first_sample(k),
     *      wav_out_first_sample(k)+wav_out_nsamp(k)-1
            av=av+abs(signal2(i))
         enddo

         av=av/nsamp
         write(6,*)'av lf filter signal',av
         signal=av
c
c   average of noise, 4  nsamp after start to avoid tapering and
c   longer window to get stable result
c

         av=0.0
         do i=4*nsamp,7*nsamp-1
            av=av+abs(signal2(i))
         enddo

         av=av/(3*nsamp)
         write(6,*)'av lf filter noise',av
         noise=av

c
c   calculate s/n and count good and bad ones
c
         if(signal/noise.lt.1.7) then
            nbad_lf=nbad_lf+1 
         else
            ngood_lf=ngood_lf+1
         endif
         write(6,*)'ratio lf ',signal/noise
c
c   could be very low frequancy
c

c
c  filter for vlf
c
         call bndpas(0.1,1.0,1000.0/wav_rate(k),
     *   cof,gain)
c
c-- apply filter
c
         call filter(signal3,wav_nsamp(k),cof,gain,2)


         av=0.0
c
c   average of vlf signal, larger window
c
         do i=wav_out_first_sample(k),
     *      wav_out_first_sample(k)+nsamp*2-1
            av=av+abs(signal3(i))
         enddo

         av=av/(nsamp*2)
         write(6,*)'av vlf filter signal',av
         signal=av
c
c   average of noise, 4  nsamp after start to avoid tapering and
c   longer window to get stable result
c

         av=0.0
         do i=4*nsamp,7*nsamp-1
            av=av+abs(signal3(i))
         enddo

         av=av/(3*nsamp)
         write(6,*)'av vlf filter noise',av
         noise=av

c
c   calculate s/n and count good and bad ones
c
         if(signal/noise.lt.1.7) then
            nbad_vlf=nbad_vlf+1
         else
            ngood_vlf=ngood_vlf+1
         endif
         write(6,*)'ratio vlf ',signal/noise


  
c
c   skip to here if missing data
c             

 500      continue     
    
       enddo               ! end of channel loop


       write(6,*) 'bad good  hf',nbad_hf,ngood_hf
       write(6,*) 'bad good  lf',nbad_lf,ngood_lf
       write(6,*) 'bad good vlf',nbad_vlf,ngood_vlf


c
c   evaluate if distant or local
c
       type='L'

c
c  first test if energy at low frequency, if not skip test at hf
c
      
       if((float(ngood_lf)/float(nbad_lf+ngood_lf).gt.0.49.  
     * and.(nbad_lf+ngood_lf).ge.4)
     *.or.
     *(float(ngood_vlf)/float(nbad_vlf+ngood_vlf).gt.0.49.   
     * and.(nbad_vlf+ngood_vlf).ge.4)) then
          if(float(nbad_hf)/float(nbad_hf+ngood_hf).gt.0.74.   ! 3 out of 4 is ok
     *    and.(nbad_hf+ngood_hf).ge.4) then
             write(6,*)'Distant event '
             type='D'
          endif
       endif
          
      return
      end


