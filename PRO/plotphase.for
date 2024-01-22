      program plotphase
c
c program to plot seismogram around P/S for quick inspection 
c usage: plotphase <sfilename>
c 
c Lars Ottemoller
c  4 Mar 2020

c changes:
c   12 Apr 2021 lo checked that works with new Nordic
c
c
      implicit none

c 
c include common stuff
c
      include 'seidim.inc'                ! dimensions for rea block
      include 'seisan.inc'                ! seisan parameters
      include 'waveform.inc'              ! waveform data
      include 'rea.inc'                   ! parameter common bliock
      include 'libsei.inc'                ! for all seisan subroutines
      include 'seiplot.inc'               ! plotting
      integer seiclen

      integer nars                        ! number of arguments
      character*80 arg(100)               ! arguments
      integer npresent                    ! number of wav files found
      logical all                         ! true: read all data, false: select
      integer code                        ! error return code
      integer i,j,k,m,l                   ! counters
      character*80 text                   ! general text string
      character*80 infile                 ! input file
      character*80 data(5000)             ! s-file with data in text array
      double precision hyp_abs_time(10),p_time(1000)
      character*5 stat(1000)
      character*4 comp(1000)
      character*2 co(1000)
      character*1 pol(1000)
      real dist(1000),az(1000)
      character*9 phase(1000)
      integer nstat                       ! number of stations
      real time_before,duration
      real onset_pos             ! P-onset position in plot     
      real av
      integer nsamp
      real x0,y0,x00,y00         ! start of plot
      real xsize, ysize          ! size of plot
      real xx,yy                 ! help variable
      character*30 xtext,ytext
      real cx(100),cy(100)
      integer ich,i1,i2
      real windowx(299),windowy(299) ! coordinates of each plot, bottom left
      integer index(5000)         ! window index of phase
      logical changes
      character*1 choice
      integer nav
      real xxtime
      real flow,fhigh                     ! filters
      integer recfil_pole                 ! poles for recfil filter
      character*8 filt_name               ! recfil filter type
      integer doy

c
c   get seisan parameters, here attenuation parameters
c
      call get_seisan_def
c
c init
c
      changes=.false.
c
c   get arguments
c
      call get_arguments(nars,arg)
      if (nars.lt.1) then
        write(*,*) ' wrong arguments: use plotphase <sfile-name>'//
     &       ' [all/pol]'
        stop
      endif

      do i=1,299
        windowx(i)=9999.
        windowy(i)=9999.
      enddo
c
c open sfile
c
      if(nars.ge.1) then
        infile=arg(1)
        write(6,'(a,a)')' Input S-file  ',arg(1)
        open(1,file=infile,status='old',err=5)
        goto 6
 5      continue
        write(6,*)'File does not exist'
        stop
 6      continue
      endif

      write(*,*) ' total window duration (default 8s): '
      read(5,'(a)') text
      duration=8.
      if (seiclen(text).gt.0) read(text,*) duration
            
c      write(*,*) ' P-onset position as percentage of total duration '//
c     &       '(10-90%; def 50% = middle):'
c      read(5,'(a)') text
      onset_pos=40
c      if (seiclen(text).gt.0.and.seiclen(text).lt.3) then 
c        read(text,*) onset_pos
c      endif 
c      if (onset_pos.lt.10.) onset_pos=10.
c      if (onset_pos.gt.90.) onset_pos=90.
      onset_pos=onset_pos/100.
c      time_before=duration*onset_pos/(1.-onset_pos)       
      time_before=duration*onset_pos       
      duration=duration-time_before

c
c   read all parameters for one event from file unit 1
c
      all=.true.        ! read all parameters from s-file
      call rea_event_in(1,all,data,code)
      close(1)
c
c   check if end of file (code=1), if so jump to stop
c
      if(code.eq.1) goto 1000     

c
c clear memory for waveform data
c
c      call wav_mem_init    ! needed ????
c
c  initialize wav reading
c
      call wav_init
c
c   get waveform file names, could be several
c
      call auto_tr(data,rea_nhead,rea_nrecord,wav_nfiles,wav_filename)
      npresent=0                      ! no files initially
      do i=1,wav_nfiles
        call  get_full_wav_name(wav_filename(i),text)
        if(text.ne.' ') then
          npresent=npresent+1
          wav_filename(npresent)=text
         endif
      enddo
c
c   terminate if no waveform files
c
      if(wav_nfiles.eq.0) then
         write(6,*)'No waveform files this event, got to next event'
         stop
      endif
c
c
c   print how many files were found
c
      wav_nfiles=npresent
      write(6,*)' Number of wav-files present', wav_nfiles
c
c   loop to read all headers of all wav files
c
      do i=1,wav_nfiles
         write(6,'(1x,a)') wav_filename(i)
c
c   read all headers of file i
c
         call read_wav_header(i)
c
c   output possible errors
c
         if(wav_error_message.ne.' ') write(6,'(1x,a)')
     *   wav_error_message
      enddo

c
c   terminate if no channels
c
      if(wav_nchan.eq.0) then
        write(6,*)'No channels found, go to next event'
        stop
      endif
      write(6,*)' Total number of channels available:',wav_nchan

c 
c  abs origin time
c
      call timsec(hyp_year(1),hyp_month(1),hyp_day(1),hyp_hour(1),
     *hyp_min(1),hyp_sec(1),hyp_abs_time(1))
c
c   write the whole first header line
c
      write(6,'(a)') data(1)(1:79)

c
c check for stations with P or S phase
c
      write(*,*) ' available components: '
      k=0
      do i=1,rea_nphase
        if(rea_phase(i)(1:1).eq.'P'.or.
     &     (rea_phase(i)(1:1).eq.'S'.and.
     &      rea_phase(i)(1:4).ne.'SPEC')) then
          k=k+1
          stat(k)=rea_stat(i)
          co(k)=rea_co(i)
          dist(k)=rea_dist(i)
          az(k)=rea_az(i)
          p_time(k)=rea_abs_time(i)  ! abs p-time
          phase(k)=rea_phase(i)
          index(k)=i
        endif
      enddo
      write(6,'(7(a5,1x,a2,1x))')(stat(i),co(i),i=1,k)
c
c   remove duplicates
c
      do i=1,k
        do j=i+1,k
          if(stat(i).eq.stat(j).and.
     &       phase(i).eq.phase(j)) stat(j)=' '
        enddo
      enddo
      m=0
      do i=1,k
        if(stat(i).ne.' ') then
          m=m+1
          stat(m)=stat(i)
          co(m)=co(i)
          dist(m)=dist(i)
          az(m)=az(i)
          pol(m)=pol(i)
          p_time(m)=p_time(i)
          phase(m)=phase(i)
          index(m)=index(i)
        endif
      enddo
      nstat=m
c
c find wave component
c
      k=0
      do i=1,m
        do j=1,wav_nchan
          if(stat(i).eq.wav_stat(j).and.
     &    wav_comp(j)(4:4).eq.co(i)(2:2).and.
     *    wav_comp(j)(1:1).eq.co(i)(1:1)) then
            k=k+1
            stat(k)=stat(i)
            comp(k)=wav_comp(j)
            dist(k)=dist(i)
            az(k)=az(i)
            pol(k)=pol(i)
            p_time(k)=p_time(i)
            phase(k)=phase(i)
            index(k)=index(i)
            goto 42
          endif
        enddo
 42     continue
      enddo
      nstat=k
      write(6,*)
      write(6,*) 'Components auto selected to use'
      write(6,'(7(a5,1x,a4,1x))')(stat(i),comp(i),i=1,k)
c
c open graphics
c
c   set default size in % of sreen size of plot
c           
      wsize=80         ! common block variable
      call get_window_size
      if(size_sample_graphics.gt.0) wsize=size_sample_graphics ! from color.def
c
c   assign file unit for postscriptt file
c
      plotunit=65      ! common block variable
      plotoption=1     ! common block variable

c
c   open postscript output file
c
      open(plotunit,file='phase.eps',status='unknown')
      write(*,*) ' open postscript file '
c
c axis text
c
      xtext='time'
      ytext='amp'

c
c plot size
c
      xsize=240.0
      ysize=140.0

      x00=10.0
c      y00=780.0-ysize-5.0
      y00=780.0-ysize-45.0
      x0=x00
      y0=y00

c
c  color of graphs
c
      cx(1)=6.0
      cx(2)=1.0
      cx(3)=3.0

c
c   open plotter (display and initial output in postscript file)
c
      flow=3.
      fhigh=8.
      call open_display

10    continue    

      call XCHARS(data(1)(2:79),104,x00,765.0)
      write(text,'(a10,f5.1,15x,a)') 'Win (s) = ',duration+time_before,
     &      'change: p=pick  r=remove    filter: b v n .' 
      call XCHARS(text,75,x00,745.)

c  find channel
c
      do m=1,nstat
c        write(6,'(a,a5,1x,a4)') 'Channel found: ',stat(m),comp(m)
        call wav_find_chan(stat(m),comp(m),k)
        if(wav_error_message.ne.' ')write(6,*) wav_error_message
        if(k.eq.0) then
          write(6,'(a,a5,1x,a4)') 'Channel not found: ',stat(m),comp(m)
          goto 800
c          stop                   
        endif
c        write(6,'(a,a5,1x,a4)') 'Channel found: ',stat(m),comp(m)

c
c   read whole channel
c
        call wav_read_channel(k)
c
c possibly filter whole signal
c

c remove dc

          call applytaper(signal1,wav_nsamp(k),10.)   ! apply taper, width 10% of half the samples

c
c   do filtering
c
        if(flow.ne.0.and.fhigh.ne.0) then
            recfil_pole=4        ! the default
            filt_name='BP      ' ! default a bp filter
c
c   use recfil to filter
c
            call recfil(signal1,wav_nsamp(k),signal1,'BU      ',
     *     0.0,0.0,recfil_pole,
     *     filt_name,flow,fhigh,1.0/wav_rate(k),1)
        endif


c
c   select out time window, only for one channel
c
        wav_out_nchan=1        ! one channel
        wav_out_chan(1)=k      ! channel number
c
c  start time relative to start of particular channel
c
        wav_out_start(k)=p_time(m)-wav_abs_time(wav_first)-time_before
        wav_out_duration(k)=duration + time_before

c
c   find if data is available, wav_get_interval fills wav_out_* variables
c
        call wav_get_interval
c
c   move data interval to beginning of array
c
        nsamp=wav_out_nsamp(k)

        l=1
        do i=wav_out_first_sample(k),
     *      wav_out_first_sample(k)+wav_out_nsamp(k)-1
          wav_y1(l)=signal1(i)
          l=l+1
        enddo
c
c  subtract dc
c
        av=0.0
        do i=1,wav_out_nsamp(k)
          av=av+wav_y1(i)
        enddo
        av=av/wav_out_nsamp(k)
        do i=1,wav_out_nsamp(k)
          wav_y1(i)=wav_y1(i)-av
c          write(99,*) i,wav_y1(i)
        enddo
c
c plot signal
c
        i1=1
        i2=nsamp
        call plot_trace(nsamp,wav_y1,wav_rate(k),
     &    i1,i2,xsize,ysize,x0,y0,onset_pos)
        windowx(m)=x0
        windowy(m)=y0
        if (dist(m).lt.1000.) then
          write(text,'(a6,f5.1)') 'dist= ',dist(m)
        else
          write(text,'(a6,i5)') 'dist= ',int(dist(m))
        endif
        call XCHARS(text,11,x0+5,y0+5)
        write(text,'(a6,f5.1)') 'az  = ',az(m)
        call XCHARS(text,11,x0+100,y0+5)
        text='stat= '//stat(m)(1:5)
        call XCHARS(text,11,x0+5,y0+130)
        text='comp= '//comp(m)(1:2)//comp(m)(4:4)
        call XCHARS(text,11,x0+100,y0+130)
        text=phase(m)(1:2)
        call XCHARS(text,2,x0+5,y0+120)


c--------------------------------------------------------------
c   prepare position of next plot
c--------------------------------------------------------------
        x0=x0+xsize+10.0
        if(x0.gt.1024.0-xsize) then
           x0=x00
           y0=y0-ysize-5.0
        endif
50      continue
        text=' '
        write(text,'(a,f4.1,a,f4.1,a)') 'filter ',flow,' - ',fhigh,' Hz'
        call tchars(text,80,400.0,5.0)
        if (m.eq.nstat) then
          text='s to start, any other key to quit'
          call tchars(text,80,15.0,5.0)
        elseif (y0.lt.0.) then
          text='s to start, f for forward, other key to quit'
          call tchars(text,80,15.0,5.0)
        endif
        if (m.eq.nstat.or.y0.lt.0.) then
          call xscursr(ich,xx,yy)
c
c close plot files
c
c          write(*,*) ' debug ',char(ich)
          if((char(ich).eq.'f'.and.
     &      m.lt.nstat).or.
     &      char(ich).eq.'b'.or.
     &      char(ich).eq.'n'.or.
     &      char(ich).eq.'.'.or.
     &      char(ich).eq.'s'.or.
     &      char(ich).eq.'v') then   ! new page
            x0=x00
            y0=y00
            do i=1,nstat
              windowx(i)=9999.
              windowy(i)=9999.
            enddo
            call close_post
            call clear_to_alpha
            call open_display
            if(char(ich).eq.'b') then
              flow=3.
              fhigh=8.
              goto 10
            elseif(char(ich).eq.'s') then
              goto 10
            elseif(char(ich).eq.'v') then
              flow=1.
              fhigh=5.
              goto 10
            elseif(char(ich).eq.'.') then
              write(*,*) ' enter filter (flow fhigh) '
              read(5,*) flow,fhigh
              goto 10
            elseif(char(ich).eq.'n') then
              flow=0.
              fhigh=0.
              goto 10
            endif
            call xnewpag
          elseif(char(ich).eq.'p'.or.
c read phase
     &      char(ich).eq.'r') then
            do i=1,nstat
              if (xx.ge.windowx(i).and.xx.le.windowx(i)+xsize.and.
     &            yy.ge.windowy(i).and.yy.le.windowy(i)+ysize) then
c
c get time in window
c
                xxtime=(xx-windowx(i))/xsize*(duration+time_before)
c                write(*,*) ' in window ',i

                if (char(ich).eq.'p') then
                  call xset_color(6)  ! make pick black
                  call xmovabs(xx,windowy(i))
                  call xdrwabs(xx,windowy(i)+ysize)
                  call xset_color(6)  ! make pick  black
c
c set absolute phase time
c
                  rea_abs_time(index(i))=p_time(i)-time_before+xxtime
                  write(*,*) ' pick adjusted ',rea_stat(index(i)),
     &             rea_phase(index(i)),rea_abs_time(index(i))-p_time(i),
     &             ' relative to initial pick '
                  call sectim(rea_abs_time(index(i)),    ! fill in rea time
     &                rea_year(index(i)),
     &                doy,
     &                rea_month(index(i)),
     &                rea_day(index(i)),
     &                rea_hour(index(i)),
     &                rea_min(index(i)),
     &                rea_sec(index(i)))
c                  write(*,*) rea_abs_time(index(i))-p_time(i)
                elseif (char(ich).eq.'r') then
                  write(*,*) ' removing ',rea_stat(index(i)),
     &              rea_phase(index(i))
                  rea_phase(index(i))(1:6)='DELETE'
                endif
                changes=.true.
c                call xset_color(color_box_letter)
              endif
            enddo 
            goto 50
          else
            goto 900
          endif
        endif
800     continue
      enddo    ! station loop
c      text='any key to quit'
c      call tchars(text,80,15.0,5.0)
c      call xscursr(ich,xx,yy)
900   continue
      call close_post
      call clear_to_alpha
c      write(*,*) ' closing postscript file '

1000  continue
      close(plotunit)
c
c write out sfile
c
      if (changes) then
        write(*,'(a)') ' save changes (y/n)?'
        read(5,'(a)') choice
        if (choice.eq.'y'.or.choice.eq.'Y') then
          write(*,*) ' changes saved '
          open(1,file=infile,status='old')
          call rea_event_out(1,all,data,code)
          close(1)
        endif
      endif

      stop
      end


      subroutine plot_trace(n,y,rate,i1,i2,xlength,ylength,x0,y0,
     & onset_pos)
c
c   plot wa seismogram indicating where pics were made with different
color
c
      implicit none
      integer n      ! number of points
      real y(*)      ! points to plot
      real x         ! x position
      real xstep     ! plot x-step
      real rate      ! sample rate
      integer i1,i2  ! sample number corresponding to wa pick
      real xlength   ! x length of plot
      real ylength   ! y length of plot
      real x0,y0     ! position of plot, lower left hand corner
      real onset_pos ! P-onset position
      real max       ! max amplitude
      integer i,k1,k2,k,nsignal
      character*80 text


c      write(text,'(a,f5.1)') 'Win= ',n/rate
c      call XCHARS(text,10,x0+10.,y0+30.)
c
c   plot frame
c
      call xmovabs(x0,y0)
      call xdrwabs(x0+xlength,y0)
      call xdrwabs(x0+xlength,y0+ylength)
      call xdrwabs(x0,y0+ylength)
      call xdrwabs(x0,y0)
      call xset_color(3)  ! make pick red
      call xmovabs(x0+xlength*onset_pos,y0)
      call xdrwabs(x0+xlength*onset_pos,y0+ylength)
      call xset_color(6)  ! make pick  black


c
c   plot signal, only use part of signal
c
      max=0.0
      k=0
c      nsignal=10.0*rate   ! number of points for 10 sec
      nsignal=n           ! number of points for 10 sec
      k1=i1-4.0*rate      ! try 4 seconds before pick
      if(k1.le.0) k1=1
      k2=k1+nsignal       ! try fixed  10 s window
      if(k2.gt.n) k2=n
c
c  find max
c
      do i=k1,k2
        if(abs(y(i)).gt.max) max=abs(y(i))
      enddo
c
c   scale
c
      do i=k1,k2
        y(i)=(y(i)/max)*(ylength/2.0)+y0+ylength/2.0
      enddo

      xstep=xlength/nsignal  ! scale for fixed length

      x=x0
      call xmovabs(x0,y(k1))

      do i=k1+1,k2
         x=x+xstep
         call xdrwabs(x,y(i))
      enddo

      return
      end

