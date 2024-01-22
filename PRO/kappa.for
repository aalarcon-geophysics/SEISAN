***********************************888888888********************************* 
C                                                                               
c	  PROGRAM KAPPA  
c                                                                                   
c  calculatesles kappa
c  the program is a simplification and improvemnt of the kappa option in
c  program SPEC.
c  there is no option for graphics and no options for output file of
c  the spectra.
c  kappa is calculted from the P or S spectra and both displacment and 
c  acceleration can be used, however displacment is the most useful option.
c  Kappe is calcualted from the slope of the flat par of the displacemnt 
c  or acceleration spectrum. It is generally assumed the distance is
c  short enough that the spectrum is not affected by Q which generally is
c  around 50km. However if Q is accurately known, the spectrum can be 
c  corrected for Q and larger distances might be used.

c  When using displacmeent spectra, small events should be be used in order to 
c  get a high corner frequency, Ml 1.5 will give a corner fraquecy generally 
c  above 20 Hz. The frequency range for analysis could then be 3-16 Hz. A 
c  lower frequency than 3 Hz is not recommend since the macroseismic noise
c  will affect the signal to noise ratio. 
c
c  procedure:
c  
c  When using acceleration spectra, there is the same restriction on distance,
c  however now larger events should be used to get a large enougt
c  frequency range above the corner frequency.
c
c
c  Input: The parameter file (default kappa.par) gives the processing parameters
c  and the channels to process. The events are given in a Nordic file
c  (default kappa.inp), the file must be updated to have distances.
c
c
c
      implicit none         
      include 'mulplt.inc'  ! mulplt variables
      include 'seiplot.inc' ! general plotting variables
      include 'seisan.inc'
      include 'rea.inc'
c
c  parameters
c
      real          av,av1           ! average
      integer       nkappa           ! number of kappa values
      integer       nkappa_all       ! total ----------------
      integer seiclen
c
      logical       clipped          ! true if signal clipped
      character*4   component        ! component to use                                                 
      character*4   old_comp         ! component read in
      logical stat_found             ! true if station is found
      character*1   exp              ! explosion indicator
      logical       pc,linux,sun
      integer       i,ierr,nstart,j
      character*80  infile           ! wav input file
      integer       k,l,kk,kkk ! help variables
      real          mincor           ! minumum correlation coef. t*
      real          minsn            ! minimum signal to noise ratio, t*
      character*80  name             ! Sfile name
      real aa,bb                     ! for lsq

      integer nsmooth                ! number of times smoothed
      real max_distance              ! max_distance for kappa

      integer       nspec            ! number of points for spectrum
      integer       nsamp,nsignal    ! number of samples
      integer nstat,nphase           ! see indata routine

      real          sd,sd1           ! standard deviation
      real          y_sn(10000)      ! data for sn, extra smoothed
      real          y_sn_noise(10000) ! noise for sn, extra --
      integer       sel_crit         ! seletion criteria for start time of window
      logical       sn_ok            ! indicate if sn is ok
      integer do_rotate              ! rotation or not (1 or 0)
      real          start            ! start of window in time or # of times
      character*5   station          ! station to use

      real          delta,bazim      ! distance, back azimuth
      real          t0               ! origin time
      real          tstart           ! start of window in secs after orig. time
      real          pre_start        ! number of secs before wind. to start
      character*1   type             ! event type
      character*120 text             ! help text
      real          corr,rms         ! correlation and rmd for qq0 and qqalpha
      character*80  err_text         ! error text
      real          wlen             ! window length to analyse      
      real          x_work(max_sample)

      real          skappa(100000)  !  kappa values, variable kappa in mulplt.inc 
      real          kappa_distance(100000) ! corresponding distances
      real          kappa_distance_all(500000) ! all calcualtions


      integer nars                   ! number of arguments
      character*80 arg(100)          ! arguments
      logical batch                  ! true if batch mode

      character*80 parfile           ! parameter file name
      character*80 cat_file          ! file with s-files

      logical parfile_select         ! true of file from argument
      logical cat_file_select        ! true if file from argument
      real slat,slon,elev            ! location and altitude
      real av_z(100000),av_n(100000),av_e(100000)  !  kappa, all stat
      integer n_av_z,n_av_n,n_av_e           ! number of --------------
      integer n_av_all_z, n_av_all_n,n_av_all_e  ! ---- all channels

      equivalence(x_work,com)
      include 'version.inc'
    
c
c   kappa averages pr channel
c

      n_av_z=0
      n_av_n=0
      n_av_e=0
      n_av_all_z=0
      n_av_all_n=0
      n_av_all_e=0

      start=1   ! start at phase pick
      pre_start=0.0   ! seconds before phase start

      parfile_select=.false.
      cat_file_select=.false.

c
c   print version
c
      call print_ver
c
c   get seisan defaults
c
      call get_seisan_def
c
c   get arguments
c
      call get_arguments(nars,arg)
c
c   if argument -batch, then no interactive operation
c
      batch=.false.
      parfile='kappa.par'
      cat_file='kappa.inp'
      kappa=0             ! no kappa correction

      do i=1,nars
         if(arg(i)(1:6).eq.'-batch') then
            batch=.true.
         endif
        if(arg(i)(1:4).eq.'-par') then
           parfile=arg(i+1)
           parfile_select=.true.
        endif
        if(arg(i)(1:4).eq.'-cat') then
           cat_file=arg(i+1)
           cat_file_select=.true.
           write(6,*) cat_file
        endif
      enddo
c
c
c   open output file with all results
c
       open(3,file='kappa.out',status='unknown')
c
c   open file with average results per channel
c
       open(9,file='kappa_average.out',status='unknown')
c
c   events used, nordic, pr channel
c
       open(20,file='kappa_event.z',status='unknown')
       open(21,file='kappa_event.n',status='unknown')
       open(22,file='kappa_event.e',status='unknown')
c
c   open file for a nordic file with average for each channel 
c
       open(59,file='kappa.z', status='unknown')
       open(60,file='kappa.n', status='unknown')
       open(61,file='kappa.e', status='unknown')
c
c   file with detailed results for each channel
c   
       open(62,file='kappa_chan.all',status='unknown')
c
c   get inputs
c
       if(.not.batch) then

       if(.not.parfile_select) then
          write(6,*)'Parameter file, kappa.par is default (return)'           
          read (*,'(a)') parfile                                         
          if(parfile(1:2).eq.'  ') parfile='kappa.par'
       endif
       if(.not.cat_file_select) then
         write(6,'(a,a)')                                                       
     *  ' CAT File file with events, ',
     *  'name kappa.inp is default (return)'
         read(5,'(a)') cat_file
         if(cat_file(1:2).eq.' ') cat_file='kappa.inp'
       endif                   
       endif              
C                                                                               
c   open parameter filer giving specs for processing                           
C                                                                               
      open(1,file=parfile,status='old', err=4848)
      goto 4849
 4848 continue
      write(6,*)' No such file ', parfile
      write(6,*)' Give parameter file'
      read(5,'(a)') parfile
 4849 continue
c                                                                               
c   read parameters shared by all data sets                                     
c   each data line is separated by a comment line read by text                
c                                                                               
        read(1,'(a)') text
        read(1,*,err=4850) sel_crit
        write(3,'(1x,a59,i10)') text(1:59),sel_crit
        read(1,'(a)') text
        read(1,*,err=4850) wlen,pre_start
        write(3,'(1x,a59,2f10.1)') 
     *  text(1:59),wlen,pre_start
        read(1,'(a)') text
        read(1,*,err=4850) nsmooth
        write(3,'(1x,a59,i10)') text(1:59),nsmooth
        read(1,'(a)') text
        read(1,*,err=4850) ffmin,ffmax
        write(3,'(1x,a59,2f10.2)') text(1:59),ffmin,ffmax
        read(1,'(a)') text
        read(1,*,err=4850) max_distance
        write(3,'(1x,a59,f10.2)') text(1:59),max_distance
        read(1,'(a)') text
        read(1,*,err=4850) remove_response
        write(3,'(1x,a59,i10)') text(1:59),remove_response
        read(1,'(a)') text
        read(1,*,err=4850) do_rotate
        write(3,'(1x,a59,i10)') text(1:59),do_rotate
        read(1,'(a)') text
        read(1,*,err=4850) q0,qalpha
        write(3,'(1x,a59,2f10.2)') text(1:59),q0,qalpha,kappa

        read(1,'(a)') text
        read(1,*,err=4850) mincor,minsn
        write(3,'(1x,a59,2f10.2)') text(1:59),mincor,minsn
        minsn=alog10(minsn)

        read(1,'(a)') text

        goto 4851
 4850   continue
        write(6,*)' Error in parameter file for parameter line:'
        write(6,'(1x,a)') text
        write(6,*)' Return to stop'
        read(5,'(a)')i
        stop
 4851   continue

c
c   check resp parameter
c
      if(remove_response.ne.1.and.remove_response.ne.3) then
          write(6,*)'Wrong response value'
          stop
      endif

c
c   set rotation flag
c
         rotate=.false.
         if(do_rotate.eq.1) rotate=.true.

c                                                                               
c   open file with list of event's, process until end of file                          
c
      if(batch) goto 503   ! skip interactive input
      
      if(.not.cat_file_select) then
         write(6,'(a,a)')                                                              
     *  ' CAT File or filenr.lis type file with events, ',
     *  'name kappa.inp is default (return)'
        if(cat_file(1:2).eq.' ') cat_file='kappa.inp'
      endif

 503    continue  ! skippped interactive input
c
c   open cat file
c                                   
        open(2,file=cat_file,status='old')

c
c----------------------------------------------------------------------
c   loop for channels, come back here  for next channel
c----------------------------------------------------------------------
c
 1    continue
c
c-----------------------------------------------------------------------
      nkappa=0        ! count kappa
      nkappa_all=0
c
c   read next channel from parameter file
c 
      write(3,*)
      read(1,'(a5,1x,a4)',end=99) 
     *station,component
c
c   get station location
c
      call stat_loc(station,' ',slat,slon,elev)


      if(station.eq.' '.or.component.eq.' ') goto 99                                  
c
c   save component in case changed by rotation
c
      old_comp=component
c
c   reset parameters
c
      rewind 2     ! start again in event file
c
C-----------------------------------------------------------------------
C
C   reading loop for events  to process starts here *********************        
C                                                                               
c-----------------------------------------------------------------------
c
 10   continue

c
c   initialize memory handling
c
      call wav_mem_init                                                                     
c
c   reset component in case of rotation
c
      component=old_comp
C                                                                               
C   Read next event from file until eof.                                      
C                                   
      call indata
     *(2,nstat,nphase,nhead,nrecord,type,exp,data,i)

c
c   go to average for that channel if end of events
c
      if(nrecord.eq.0) goto 60
c
c   check if station is within distance range
c
           sdistance=0.0
           stat_found=.false.
           do i=1,rea_nphase
              if(rea_stat(i).eq.station) then
                 stat_found=.true.
                 if(rea_dist(i).ge.0.0) then
                    sdistance=sqrt
     *              (rea_dist(i)*rea_dist(i)+hyp_depth(1)*hyp_depth(1))
                    if(sdistance.le.max_distance) goto 720    ! dist ok
                 endif
              endif
           enddo
           if(stat_found) then
              write(6,'(a,a,f6.1)')
     *        station ,' Distance too far ', sdistance
           else
               write(6,'(a,a)')station,' not found'
           endif

           goto 10   ! if here station not found or distance wrong, next event

 720       continue

c
c   output
c
      write(3,'(1x,a)') data(1)(1:79)
      write(6,'(1x,a)') data(1)(1:79)
      write(3,'(1x,a,1x,a)') station,component
c
c   select data in CAT file, read from waveform file
c
      call spec_select(data,nhead,nrecord,rotate,                                                        
     *station,component,
     *sel_crit,start,tstart,t0,nsamp,ierr,err_text)  
c                                                                               
c   check for errors                                                            
C
      do i=1,100
        if(signal1(i).ne.0.0) goto 640
      enddo
      ierr=1
      err_text='Zero amp wav data'
 640  continue  
                                                                             
      if(ierr.ne.0) then                                                                write(3,'(a)') err_text
         write(6,'(a)') err_text
         write(6,700) station,component,' Error CAT'
         write(3,700) station,component,' Error CAT'
 700     format(1x,a,1x,a,1x,a)                                     
         goto 10   ! next event                                       
      endif
c
c   put in sample rate
c
      if(wav_current_chan(1).le.1) wav_current_chan(1)=1 ! bug fix
      rate=wav_rate(wav_current_chan(1))
      current_chan=wav_current_chan(1)        ! maybe needed for mulplt routines

      do i=1,nsamp
        y(i)=signal1(i)
      enddo               

c
c   select window and do spectral analysis
c
      nsignal=(wlen+pre_start)*rate                                                 

      nstart=(tstart-pre_start)*rate                                                    
c
c   check that not too much data has been selected or start is before first
c   sample
c
      if(nstart+nsignal.gt.nsamp.or.nstart.le.0) then
         if(nstart+nsignal.gt.nsamp) then
            write(6,*)' Window too late, skip trace'
            write(3,*)' Window too late, skip trace'
          endif
          if(nstart.le.0) then
             write(6,*)' Window starts before start of trace'
             write(3,*)' Window starts before start of trace'
          endif
          goto 10   ! next event                                                             
      endif
c
c  response removal, check if response curve available and read, 
c  if not go to next event
c
      if(remove_response.gt.0) then
         wav_resp_file=' '
         call read_resp                 ! read response file
         if(wav_resp_status(1:1).eq.'9') then
            write(6,'(1x,a,1x,a,1x,a)') 
     *      'No response info ***',station,component
            write(3,'(1x,a,1x,a,1x,a)') 
     *      'No response info ***',station,component
             goto 10   ! next event                                                             
         endif
      endif

c
c   travel time for q correction
c
      travel_time=tstart-t0

c
c   set first and last point to use
c
      first=nstart
      last=nstart+nsignal
c                                                                               
c    put part of the signal into local datavector                           
c                                                                               
      j =  0                                                                
      do i = first,last                                                     
        j = j + 1                                                           
        y(j) = signal1(i)                                                    
      enddo
c
c   check for clipped signals
c
      call check_clipped(y,j,clipped)
      clipped=.false.
      if(clipped) then
         write(6,*) 'Signal clipped'
         write(3,*) 'Signal clipped'
         goto 10
      endif
c
c   spectrum 
c
      call amp_spec_gen(j,nspec,x_work,.false.)
c
c   write out travel time used for q correction 
c
      write(3,*)' Travel time=',travel_time
c
c   smooth
c
      call smooth(y,nspec,nsmooth)  
c
c   save data for later calculation of t*
c
      do i=1,nspec
        wav_y1(i)=y(i)
      enddo                                        
c
c   smooth 5 times for s/n
c
      do i=1,nspec
        y_sn(i)=y(i)
      enddo
      call smooth(y_sn,nspec,5)
c
c---------------------------------------------------------------------------
c   now do noise spectrum, take noise from same window in start of signal
c---------------------------------------------------------------------------
c
      first=1
      last=nsignal

c                                                                               
c   put part of the signal into local datavector                           
c                                                                               
      j =  0                                                                    
      do i = first,last                                                         
         j = j + 1                                                               
         y(j) = signal1(i)                                                            
      enddo                                                                     
                                                                                
      call amp_spec_gen(j,nspec,x_work,.false.)                                                 
c
c   smooth 5 times for s/n
c
      do i=1,nspec
        y_sn_noise(i)=y(i)
      enddo
      call smooth(y_sn_noise,nspec,5)
c
c   smooth oginal too if required
c
      call smooth(y,nspec,nsmooth)                                           
c
c   check s/n ratio
c
      sn_ok=.true.        ! initially ok

      do i=1,nspec
        if((y_sn(i)-y_sn_noise(i)).lt.minsn) sn_ok=.false.
      enddo

      if(.not.sn_ok) then
         write(3,*) ' Removed, bad s/n'
         goto  10
      endif  
c                           
c-----------------------------------------------------------------------
c   calculate kappa, s/n is now ok
c------------------------------------------------------------------------
c

      do i=1,nspec
         x_work(i)=10.0**x_work(i)    ! get linear frequencies
      enddo

      call lsqlin(nspec,x_work,wav_y1,aa,bb,corr,rms)

      bb=bb*0.733     ! 0.733=1/(pi*log10(e))

      if(abs(corr).ge.mincor) then   ! corr is negative or positive
          nkappa=nkappa+1
          skappa(nkappa)=-bb
          kappa_distance(nkappa)=sdistance  ! save hypocentral distance
          nkappa_all=nkappa_all+1
          kappa_distance_all(nkappa_all)=sdistance

          write(3,'(a,3f10.4)') 
     *   'Selected       kappa,corr, rms ',-bb,corr,rms
          if(component(4:4).eq.'Z') n_av_all_z=n_av_all_z+1
          if(component(4:4).eq.'N') n_av_all_n=n_av_all_n+1
          if(component(4:4).eq.'E') n_av_all_e=n_av_all_e+1
c
c   calcualte BAZ
c

          call azibazi(hyp_lat(1),hyp_lon(1),slat,slon,
     *    delta,azim,bazim)
          
c
c   save all in a file
c
          text=' '
          text(1:5)=station
          text(7:10)=component
          text(12:13)='k='
          write(text(15:20),'(f6.3)') -bb
          text(22:23)='h='
          write(text(25:30),'(f6.1)') hyp_depth(1)
          text(32:34)='hd='
          write(text(35:40),'(f6.1)')sdistance
          text(42:44)='ed='
          write(text(45:50),'(f6.1)') sqrt(sdistance*sdistance-
     *    hyp_depth(1)*hyp_depth(1))
          text(52:54)='lt='
          write(text(55:61),'(f7.3)') hyp_lat(1)
          text(63:65)='lo='
          write(text(66:73),'(f8.3)')hyp_lon(1)
          text(75:78)='baz='
          write(text(80:84),'(f5.1)') bazim
          write(62,'(a,a)') text(1:84),data(1)(1:20)     
c
c   nordic header line
c
          if(component(4:4).eq.'Z') write(20,'(a)') data(1)
          if(component(4:4).eq.'N') write(21,'(a)') data(1)
          if(component(4:4).eq.'E') write(22,'(a)') data(1)
      else      
          write(3,'(a,3f10.4)') 
     *    'Not selected   kappa,corr, rms ',-bb,corr,rms
      endif
c
c   back for next event
c
      goto 10

c-------------------------------------------------------------------
c------------------------------------------------------------------                                                                               
c   here after finishing with one channel, many events
c------------------------------------------------------------------
c------------------------------------------------------------------
c                                                                
 60   continue
               
c
c   calculate average kappa for this channel
c

      write(6,*)
      if(nkappa.gt.1) then
         call sdv(nkappa,skappa,av,sd)
      else
         av=skappa(1)
         sd=0.0
      endif
      write(3,*)' Number of values for kappa',nkappa

      if(nkappa.gt.0) then
          write(3,'(a,2f10.3)') 'Average kappa and sd',av,sd
          write(9,'(a5,1x,a3,1x,2f7.3,i5)')
     *    station,component(1:2)//component(4:4),
     *    av,sd,nkappa
c
c   make file with coordinates and values, one 'event' for each aveage 
c   for this component, used for plotting kappa locations 
c
           if(component(4:4).eq.'Z') then
              n_av_z=n_av_z+1
              av_z(n_av_z)=av
              text=' '
              text=' 2000 0101 0101  0.0 L'
              write(text(24:38),'(f7.3,f8.3)')slat,slon
              write(text(56:59),'(f4.1)') av*100
              text(80:80)='1'
              k=nkappa
              if(nkappa.gt.1000) k=999
              write(text(49:51),'(i3)') k
              write(59,'(a)')text
              text=' '
              text(2:6)=station
              text(7:9)=component(1:2)//component(4:4)
              write(text(11:26),'(2f8.3)') av,sd
              text(80:80)='3'
              write(59,'(a)') text
              write(59,*)
           endif

           if(component(4:4).eq.'N'.or.component.eq.'R') then
              n_av_n=n_av_n+1
              av_n(n_av_n)=av
              text=' '
              text=' 2000 0101 0101  0.0 L'
              write(text(24:38),'(f7.3,f8.3)')slat,slon
              if(av.gt.0.01) write(text(56:59),'(f4.1)') av*100
              text(80:80)='1'
              k=nkappa
              if(nkappa.gt.1000) k=999
              write(text(49:51),'(i3)') k
              write(60,'(a)')text
              text=' '
              text(2:6)=station
              text(7:9)=component(1:2)//component(4:4)
              write(text(11:26),'(2f8.3)') av,sd
              text(80:80)='3'
              write(60,'(a)') text
              write(60,*)
           endif

           if(component(4:4).eq.'E'.or.component.eq.'T') then
              n_av_e=n_av_e+1
              av_e(n_av_e)=av                
              text=' '
              text=' 2000 0101 0101  0.0 L'
              write(text(24:38),'(f7.3,f8.3)')slat,slon
              if(av.gt.0.01) write(text(56:59),'(f4.1)') av*100
              text(80:80)='1'
             k=nkappa
             if(nkappa.gt.1000) k=999
             write(text(49:51),'(i3)') k
              write(61,'(a)')text
              text=' '
              text(2:6)=station
              text(7:9)=component(1:2)//component(4:4)
             write(text(11:26),'(2f8.3)') av,sd
              text(80:80)='3'
              write(61,'(a)') text
              write(61,*)
            endif
            
         call sdv(nkappa,kappa_distance,av1,sd1)
         write(3,'(a,2f7.1)') 'Average distance and sd',av1,sd1

      endif
c
c   end of average block
c

c
c---------------------------------------------------------------------
c   back for next channel, goto 1
c---------------------------------------------------------------------
c
      goto 1
c
c   end of program
c

 99   continue                                                                

c
c   avrage of the average kappa for each channel
c
      write(6,'(a)') 
     *'Average of the averages each channel and total number of values'
      if(n_av_z.gt.0) then
         call sdv(n_av_z,av_z,av,sd)
         write(6,'(a,2f8.3,i8)')'Av kappa z: ', av,sd,n_av_all_z
      endif
      if(n_av_n.gt.0) then
         call sdv(n_av_n,av_n,av,sd)
         write(6,'(a,2f8.3,i8)')'Av kappa n: ', av,sd,n_av_all_n
      endif
      if(n_av_e.gt.0) then
         call sdv(n_av_e,av_e,av,sd)
         write(6,'(a,2f8.3,i8)')'Av kappa e: ', av,sd,n_av_all_e
      endif

       write(3,'(a)')
     *'Average of the averages each channel and total number of values'
      if(n_av_z.gt.0) then
         call sdv(n_av_z,av_z,av,sd)
         write(3,'(a,2f8.3,i8)')'Av kappa z: ', av,sd,n_av_all_z
      endif
      if(n_av_n.gt.0) then
         call sdv(n_av_n,av_n,av,sd)
         write(3,'(a,2f8.3,i8)')'Av kappa n: ', av,sd,n_av_all_n
      endif
      if(n_av_e.gt.0) then
         call sdv(n_av_e,av_e,av,sd)
         write(3,'(a,2f8.3,i8)')'Av kappa e: ', av,sd,n_av_all_e
      endif

      call sdv(nkappa_all,kappa_distance_all,av1,sd1)
      write(6,'(a,2f7.1)') 
     *'Average distance and sd for all kappa value',av1,sd1


      write(6,*)
      write(6,'(a)')'Output files are:'
      write(6,'(a)')'Output of run:                kappa.out'
      write(6,'(a)')'Average for each channel:     kappa_average.out'
      write(6,'(a)')'Ok events each channel:       kappa_chan.all'
      write(6,'(a)')'Average each channel, nordic: kappa.z,n.e'
      write(6,'(a)')'Ok events each channel, --- : kappa_event:z,n,e'

      stop
      end
c
                                                                                
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc             
      subroutine amp_spec_gen(nsamp,nspec,x_work,power)                                                 
c                                   
c
c     routine to make a real spectrum, more general than routine in
c     mul_spec                
c     power: true if power density spectrum
c     The tapering of  the data is fixed to 10%                                 
c                                                                               
c     Jh april 94                                         
c                                                                               
                                                                                
c-- common block     
c
      implicit none
      include 'mulplt.inc'			
      logical power
c
c-- percentage of tapering (fixed)                       
      real taper/10./				
c-- length of input vector                                 
      integer nsamp				
c-- length after padding                                    
      integer ipow				
c-- vector padded with npad zeros                           
      integer npad				
c-- indicator for response removal or not (1 or 0).                        
      integer rem_resp			
c-- indicator for type of spectrum                  
c     integer disp_vel! now in mulplt.inc			
c-- number of points in spectrum
      integer nspec
c-- frequencies
      real x_work(*)
c-- counters and help variables                                               
      integer i,j				
      real ff                    ! help variables
c
c  paz, not used
c

      complex pole(100),zero(100)  ! complex PAZ
      integer npole,nzero          ! number of poles and zeros
      real norm                    ! normalization constant for poles and zeros
		
c
c   no frequency domain filters
c
      filt=0
      npole=0
      nzero=0
c
c   check if response is removed 
c
      if(remove_response.gt.0) then
         rem_resp=1                   ! response is removed
         disp_vel=remove_response     ! type of response
c        opmode=0                     ! get origin time form data array
      else
         rem_resp=0                   ! response is not removed
         disp_vel=0
c        opmode=1
      endif
c                                                                               
c--------put part of the signal into local datavector                           
c                                                                               
c      j =  0                                                                    
c      do i = first,last                                                         
c        j = j + 1                                                               
c        y(j) = signal1(i)                                                            
c      enddo                                                                     
c                                                                                
c      nsamp = j                                                                 
c                                                                               
c------- prepare data.     Pad with zeros and taper 10%.                        
c                                                                               
      call prepare(nsamp,y,taper,ipow,npad,com)                                
c
c   check for max number of points
c
      if(ipow.ge.max_sample/2) then
c          call clear_to_alpha
           write(6,*)' Too many points in spectrum'
           write(6,*)' Max number is ', max_sample/2
           stop
      endif                                
c
c-------- Calculate the spectrum, and remove the system response
c         if desired, no frequency domain filters, therfore the zeros                
c

      call spectrum(com,ipow,disp_vel,rem_resp,rate,0.0,0.0,0,0,
     +     q0,qalpha,kappa,travel_time,zero,pole,nzero,npole,norm)
c                                                                               
c
c   calculate real spectrum
c
      j=0
      do i = 2,((ipow/2) + 1)
        ff=(i-1)*rate/ipow
        if(ff.ge.ffmin.and.ff.le.ffmax) then
           j=j+1
           if(power) then
             y(j) = 2.0*(com(i)*conjg(com(i)))/(nsamp*rate)    ! power spectral density
           else
             y(j) = ((1/rate)**2)*(com(i)*conjg(com(i)))    ! transient signal
           endif

           if(.not.power)y(j) = sqrt(y(j))                ! j.h. change
           y(j) = log10( y(j))                       ! take the logarithm
           if(power) y(j)=(y(j)-18.0)*10.0  ! db (m/s**2)**2
           x_work(j)=log10(ff)
        endif
      enddo
      nspec=j                                                                 
      return                                                                    
      end                                                                       



