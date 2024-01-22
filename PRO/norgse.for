c--------------------------------------------------------------------------
c  norgse, from nordic to gse/ims
c  limited conversion, missing: mt solution,  hypocenter errros
c
c  jh jan 2021
c
c--------------------------------------------------------------------------
c
c  For detail on parameters and variables names, see rea.inc
c
c
c  changes
c

c
      implicit none                       ! force delcaration of all variables
      include 'seidim.inc'                ! dimensions for rea block
      include 'seisan.inc'                ! dimensions for rea block
      include 'rea.inc'                   ! parameter common bliock

      character*80 data(10000)            ! s-file with data in text array

      character*200 text

      logical all                         ! true: read all data, false: headers
      integer seiclen                     ! function
      integer code                        ! error return code
      integer nevent                      ! number of events in file
      character*10 keys                   ! next choice key for routine findevin
      character*80 infile                 ! input file
      character*80 eventfile              ! single event file name
      logical ph,phase_ok                 ! true if a real phase
      real x                              ! help variable
      integer i                           ! counter

      call get_seisan_def
c
c   open output file
c
      open(2,file='norgse.out',status='unknown')
    
c
c   get input file name, or data base with time interval
c

      write(*,*) ' Event data input file'
      write(*,*)
     
      read(*,'(a)') infile

      open(1,file=infile,status='old',err=5)
      goto 6
 5    continue
      write(6,*)' Input file not found'
      stop
 6    continue


      all=.true.                  ! read all parameters
      nevent=0                    ! initialize counter

c
c-----------------------------------------------------------------
c  Loop to read events start here
c-----------------------------------------------------------------
c

  50  continue


c
c   read all parameters for one event from file unit 1
c
      call rea_event_in(1,all,data,code)

c
c   check if end of file (code=1), if so jump out of loop
c
      if(code.eq.1) goto 1000
c
      nevent=nevent+1               ! count events
c
c   write Event line
c
      text=' '
      text='Event           '//rea_locality
      write(2,'(a)') text(1:seiclen(text))
      write(2,*)' '
c
c   write the whole first header line
c
      write(6,'(a)') data(1)(1:79)

      write(2,'(a,a)')'   Date       Time        Err   RMS Latitude '//
     *'Longitude  Smaj  Smin  Az  Depth  Err Ndef Nsta Gap  mdist  '//
     *'Mdist Qual   Author      OrigID'
c
c   write all hypocenter lines
c
      do i=1,rea_nhyp

c
c   write origin
c
     

         text=' '
         write(text(1:4), '(i4)') hyp_year(i)
         write(text(6:7), '(i2)') hyp_month(i)
         write(text(9:10),'(i2)') hyp_day(i)
         text(5:5)='/'
         text(8:8)='/'
         if(hyp_hour(i).gt.-900) then
            write(text(12:13),'(i2)') hyp_hour(i)
            text(14:14)=':'
         endif

         if(hyp_min(i).gt.-900) then
            write(text(15:16),'(i2)') hyp_min(i)
            text(17:17)=':'
         endif         
         if(hyp_sec(i).gt.-900) 
     *   write(text(18:22),'(f5.2)') hyp_sec(i)
c
c rms
c
         if(hyp_rms(i).ge.0.0) write(text(30:35),'(f6.2)') hyp_rms(i)
c
c   lat lon depth
c
         if(hyp_lat(i).gt.-900) write(text(37:44),'(f8.4)') hyp_lat(i)
         if(hyp_lon(i).gt.-900) write(text(46:54),'(f9.4)') hyp_lon(i)
         if(hyp_depth(i).gt.-900) write(text(71:77),'(f7.1)')
     *   hyp_depth(i)
c
c  stations
c
         if(hyp_nstat(i).gt.-900) write(text(89:92),'(i4)') 
     *   hyp_nstat(i)
c
c  gap
c
         if(hyp_gap(i).gt.-900) write(text(94:96),'(i3)') 
     *   int(hyp_gap(i))
c
c   location agency
c
         text(119:122)=hyp_agency(i)
c
c   explosion etc
c
         if(i.eq.1) then
            if(hyp_type(1).eq.'E') text( 116:117)='kh'
            if(hyp_type(1).eq.'P') text( 116:117)='ke'
            if(hyp_type(1).eq.' ') text( 116:117)='se'
            if(hyp_type(1).eq.'Q') text( 116:117)='ke'
         endif
         
         write(2,'(a)') text(1:124)
      enddo

c
c   fault plane solutions
c
         if(rea_nfault.gt.0) then
           write(2,'(a)')' (#FAULT_PLANE Typ Strike'//
     *    '   Dip    Rake  NP  NS Plane Author   )'
           do i=1,rea_nfault
              text=' '
              text(2:3)='(#'
              read(rea_fault(i)(1:10),'(f10.1)') x
              write(text(19:25),'(f7.1)')x
              read(rea_fault(i)(11:20),'(f10.1)') x
              write(text(26:31),'(f6.1)') x
              read(rea_fault(i)(21:30),'(f10.1)') x
              write(text(32:39),'(f8.1)') x

              text(55:57)=rea_fault(i)(67:69)
              text(64:64)=')'
              write(2,'(a)') text(1:64)
           enddo
         endif



      write(2,*)' '
c
c   magnitudes
c
      write(2,'(a)') 'Magnitude  Err Nsta Author      OrigID'

      do i=1,rea_nmag
         text=' '
         text(1:1)='M'
         text(2:2)=hyp_mag_type_all(i)
         write(text(7:10),'(f4.1)') hyp_mag_all(i)
         text(21:23)=hyp_mag_agency_all(i)
         write(2,'(a)') text(1:26)
      enddo

      write(2,*)' '
c
c   phases
c
      write(2,'(a)') 'Sta     Dist  EvAz Phase        Time      Tres'//
     *'  Azim AzRes   Slow   SRes Def   SNR       Amp   Per Qual'//
     *' Magnitude    ArrID' 
c
      do i=1,rea_nphase
c
c   do not write SPEC phases
c
         if(rea_phase(i)(1:4).eq.'SPEC') goto 10  ! end of loop
         text=' '
c
c   station
c
         text(1:5)=rea_stat(i) 
c
c   distance
c
         if(rea_dist(i).gt.-900) 
     *   write(text(6:12),'(f7.2)') rea_dist(i)/111.2
c
c   azimuth
c
         if(rea_az(i).gt.-900) write(text(14:18),'(f5.1)') rea_az(i)
c
c   phase
c        
         text(20:27)=rea_phase(i)
c
c   time
c
         if(rea_hour(i).gt.-900.and.rea_min(i).gt.-900.
     *   and.rea_sec(i).gt.-900)
     *   write(text(29:40),'(i2,a,i2,a,f6.3)') rea_hour(i),':',
     *   rea_min(i),':',rea_sec(i)
c
c   residuals, rea_res can be residual in arrival time, baz and magnitude
c   so it must be checked
c
         if(rea_baz_res(i).gt.-900.0) 
     *   write(text(53:58),'(f6.1)') rea_baz_res(i)
c
c   check if a real phase, END and BAZ and amplitude phases not accepted
c
         ph=phase_ok(rea_phase(i))
         if(rea_res(i).gt.-900.0.and.ph) 
     *   write(text(41:46),'(f6.1)') rea_res(i)
c
c   amplitude and period
c
         if(rea_amp(i).gt.-900) write(text(83:92),'(f10.1)')rea_amp(i)    
         if(rea_per(i).gt.-900) write(text(93:98),'(f6.1)')rea_per(i) 
c
c   baz and app. vel. 
c
         if(rea_baz_obs(i).gt.-900) 
     *   write(text(47:52),'(f6.1)')rea_baz_obs(i)    
         if(rea_vel(i).gt.-900) write(text(59:65),'(f7.1)')rea_vel(i) 
         if(rea_baz_res(i).gt.-900) write(text(53:58),'(f6.1)')
     *   rea_baz_res(i)  
c
c   polarity and I or E
c 
         text(101:102)='__'
         if(rea_polarity(i).eq.'C') text(101:101)='c'
         if(rea_polarity(i).eq.'D') text(101:101)='d'
         if(rea_onset(i).eq.'I') text(102:102)='i'
         if(rea_onset(i).eq.'E') text(102:102)='e'
      
 
         write(2,'(a)') text(1:102)


 10      continue   ! phase was skipped

      enddo

      write(2,*)' '
      
c
c   get next event
c
      goto 50
c
c     end of file or data base
c
 1000 continue
c
      write(6,*)            ! blank line
      close(2)              ! close output file
      write(6,*) 'Number of events in input file', nevent
      write(6,*) 'Output file name is norgse.out'

      stop
      end
