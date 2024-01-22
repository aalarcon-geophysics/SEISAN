c  program to convert data from Geophysical Survey of Russian Academy of Science 
c  to SEISAN Nordic format.
c 
c  reading three files: output from hypo program, russion version
c                       output from .arr files from russion WSG system
c                       filenr.lis with wav files which must have
c                          seisan names since time is read from name
c                          and used to associate with s-file. for now
c                          600s is  used to associate. potentially
c                          waveform file name could be put into several
c                          s-files.
c
c  since not all info is in any of the files, the program combines the
c  two to make an almost complete Nordic file.
c
c  the amplitude AML is supposed to be read on WA trace and in nm
c  amplitudes and P and S uses filter xx and unit is in xxx
c
c  since the hyp files do not have component, it is taken from .arr file
c  assuming P is on Z and S on N
c
c  amplitudes on P and S creates a new phase which is put 2 s after the
c  relevant phase so as no to be plotted on top. it is not known exactly
c  where it is picked

c--------------------------------------------------------------------------
c
c  header line is duplicated, 2. line with agency ORG
c
c  All events are assumed local
c  Check of phases on next day realtive to main header, then add 24 h
c
c  Time of amplitude is taken from .arr file 
c
c  For detail on parameters and variables names, see rea.inc
c
c
c  changes
c    
c  dec 12 2023 jh: remove factor 10 for amplitude, make sure all component
c                  codes are upper case, add agency and operator to phases

      implicit none                       ! force delcaration of all variables
      include 'seidim.inc'                ! dimensions for rea block
      include 'rea.inc'                   ! parameter common block
      include 'rea2.inc'
      include 'seisan.inc'

      character*80 data(5000)             ! s-file with data in text array
      integer nwav                        ! number of wavform files
      character*80 wavfile(10000)         ! waveform file names
      integer wavfile_used(10000)         ! indicate if file has been used(1)
      real*8 abstime_wavfile(10000)       ! abs start time of wav file
      real*8 abstime_sfile                ! abs time of origin time
      character*80 err_text               ! error text
      character*250 text
      character*80 infile,infile1         ! input file
      character*3  agency                 ! agency for mag and hypo
      integer nml                         ! numbwer of ml in arr file
      real ml                             ! ml ---------------------
      real wav_dif                        ! time in s for wav file to be included
      integer nmiss                       ! number of events without wav files
      character*20 wav_miss(2000)         ! dates of --------------------------
      double precision  abstime
      integer doy,doy1                    ! day of year
      integer year,month,day,hr,minute
      real sec
      real*8 time_before_1970,time_after_1970,time_after_0000
c-- arguments
      character*80 args(10)    
c---number of arguments 
      integer nars       
C-- SYSTEM TIME FOR USED FOR TIME OF OPERATION
      CHARACTER*12 p_time
      character*14 proc_time 
      character*3 operator
      integer seiclen                     ! function

      logical all                         ! true: read all data, false: headers
      logical dup                         ! true if duplicate header
      character*1 answer
      integer code                        ! error return code
      integer model                       ! model number
      real min                            ! minute of degree
      real x,xx                           ! help variable
      integer nevent                      ! number of events in file
      integer ev_number                   ! original event number
      logical arc                         ! true if arc line
      integer i,k,k2,j,l                  ! counters
      character*1 ucase                   ! function


      call get_seisan_def
      nmiss=0
c
c   check s-file format
c
      if(new_nordic_format) then
         rea_new_out=.true.              ! write new format
      endif

c
c   open output file
c
      open(2,file='gsrnor.out',status='unknown')
        
      infile=' '

c
c   get input file name, check if exist
c

 9    continue
      write(6,*) 'Give input hypo file'
      read(5,'(a)') infile
c
c   could be just making response files
c
      if(infile(1:4).eq.'resp') then
         write(6,*)'Converting SAC response files'
         write(6,*)
         call make_seisan_sac_resp
         stop
      endif

      open(1,file=infile,status='old',err=10)
      goto 11
 10   continue
      write(6,*)'No such input file'
      goto 9
 11   continue

 19   continue
      write(6,*) 'Give input arr file'
      read(5,'(a)') infile1
      open(4,file=infile1,status='old',err=22)
      goto 21
 22   continue
      write(6,*)'No such input file'
      goto 19
 21   continue

 23   continue
      write(6,*)'Give operator, max 3 chars'
      read(5,'(a)') operator
      if(operator.eq.' ') goto 23

      write(6,*)'Time difference in secs for wav file'//
     *' to be include in s-file, def 600s (enter)'
      read(5,'(a)') text
      if(text.eq.' ') then
         wav_dif=600.0
      else
         read(text,*) wav_dif
      endif

      all=.true.                  ! write all parameters 
      nevent=0                    ! initialize counter

C
C   GET SYSTEM TIME
C
      CALL SYSTIME(p_time,PROC_TIME)

c
c   get filenr.lis with wav names
c
      open(8,file='filenr.lis',status='old',err=40)
      goto 41
 40   continue
      write(6,*)'No filenr.lis with waveform file names, continue(y/n)'
      read(5,'(a)') answer
      if(answer.ne.'y') stop
 41   continue
c
c   read file
c
      nwav=1
 42   continue
      read(8,'(7x,a)',end=43) wavfile(nwav)
      wavfile_used(nwav)=0
      if(wavfile(nwav).eq.' ') goto 43
      nwav=nwav+1
      goto 42
 43   continue
      nwav=nwav-1
      if(nwav.gt.0) write(6,*)'Number of waveform files',nwav
      write(6,*)
c
c  calculate abstime of wav file
c
      do i=1,nwav
         read(wavfile(i),'(i4,1x,i2,1x,i2,1x,2i2,1x,i2)') 
     *   year,month,day,hr,minute,k
         sec=k
         call timsec(year,month,day,hr,minute,sec,abstime_wavfile(i))
      enddo
      close(8)
c
c-----------------------------------------------------------------
c  Loop to read events starts here
c-----------------------------------------------------------------
c

c
c   new event
c
  50  continue

c
c   clear variables
c
      rea_nphase=0      ! count number of phase lines
      rea_nhyp=0        ! count number of hypocenters
      rea_nwav=0        ! --------------- waveform files
      rea_ncomment=0    ! --------------- comment lines
      rea_nmacro=0      ! --------------- macroseismic data
      rea_nfault=0      ! --------------- fault plane solutions
      rea_nspec=0       ! --------------- spectral solutions
      rea_nmag=0        ! --------------- magnitudes
      rea_id_line=' '   ! no id line
      k=0               ! phase counter

      call rea_hyp_clear(1)
      call rea_hyp_clear(2)
      do i=1,500
         call rea_phase_clear(i)
      enddo
     
      rea_nwav=0  
      rea_locality=' '   
c
c  read one line
c

 20   continue
      read(1,'(a)',end=300,err=300) text
      do i=1,150
         if(ichar(text(i:i)).eq.9)then
             text(i:i)=' '  ! remove tab
             write(6,*)'tab removed'
         endif
      enddo

      if(text(1:100).eq.' ') goto 20          ! read to non blank line

c      write(6,*) text(1:80)
c
c   find model
c
      i=index(text,'CRUSTAL MODEL')
      if(i.gt.0) then
         read(text(i+13:i+16),*) model
         write(6,*)
         write(6,*)'Model number', model
         if(model.gt.0) write(hyp_model(1),'(I1)') model
      endif    
c
c   check if next event
c
      if(text(14:14).eq.'.'.and.text(22:22).eq.'.'.
     *and.text(30:30).eq.'.') then
         continue
      else
         goto 20
      endif
c 
c   id line
c
      rea_id_line(61:64)=text(2:5)
      rea_id_line(2:11)='ACTION:H71' 
      rea_id_line(58:60)='ID:'
      rea_id_line(80:80)='I' 
      WRITE(rea_id_line(31:34),'(A)')OPERATOR
      WRITE(rea_id_line(13:26),'(A)')PROC_TIME     
      rea_id_line(28:30)='OP:'
      rea_id_line(36:42)='STATUS:'        
      read(text(1:2),'(i2)') hyp_year(1)
      hyp_year(1)=hyp_year(1)+2000
      read(text(3:4),'(i2)') hyp_month(1)
      read(text(5:6),'(i2)') hyp_day(1)
      read(text(7:8),'(i2)') hyp_hour(1)
      read(text(9:10),'(i2)') hyp_min(1)
      read(text(11:15),'(f5.2)') hyp_sec(1)

      hyp_agency(1)='GS '
      i=hyp_sec(1)
      write(rea_id_line(61:74),'(i4,6i2)')
     *hyp_year(1),hyp_month(1),hyp_day(1),
     *hyp_hour(1),hyp_min(1),i
      do i=61,74
         if(rea_id_line(i:i).eq.' ') rea_id_line(i:i)='0'
      enddo
c
c location
c
c  lat

      read(text(18:19),'(i2)') i
      read(text(20:23),'(f4.1)') min
      hyp_lat(1)=i+min/60.0
cxx   if(text(28:3).eq.'S') hyp_lat(1)=-hyp_lat(1)
c  lon
      read(text(25:27),'(i3)') i
      read(text(28:31),'(f4.1)') min
      hyp_lon(1)=i+min/60.0
c     if(text(39:39).eq.'W') hyp_lon(1)=-hyp_lon(1)
      read(text(32:36),*) hyp_depth(1)
c
c   if zero indicate explosion
c
      if(hyp_depth(1).eq.0.0) hyp_type(1)='E'
c
c   magnitude mpv
c
      
      if(text(38:41).ne.' ') then
         read(text(38:41),*)x
         rea_ncomment=rea_ncomment+1
         rea_comment(rea_ncomment)=' '
         rea_comment(rea_ncomment)(2:14)='Magnitude Mp:'
         write(rea_comment(rea_ncomment)(15:22),'(f7.1)') x
         rea_comment(rea_ncomment)(80:80)='3'
         hyp_mag(2,1)=x
         hyp_mag_agency(2,1)='GS '
         rea_nmag=rea_nmag+1
      endif
c
c   gap
c
      if(text(52:54).ne.' ') then
         read(text(52:54),*)hyp_gap(1)
      endif

c
c  energy class
c
      if(text(43:46).ne.' ') then
         read(text(43:46),*) x
         rea_ncomment=rea_ncomment+1
         rea_comment(rea_ncomment)=' '
         rea_comment(rea_ncomment)(1:16)=' Energy class: '
         write(rea_comment(rea_ncomment)(17:22),'(f5.1)') x
         rea_comment(rea_ncomment)(80:80)='3'
      endif
      

      read(text(62:66),*) hyp_rms(1)

      if(text(68:81).ne.' ') then
         read(text(68:73),*)hyp_lat_err(1)
         hyp_lon_err(1)=hyp_lat_err(1)
         read(text(75:81),*) hyp_depth_err(1)
         hyp_error(1)=.true.
      endif

cccccccccccccccccccccccccccc    phases   cccccccccccccccccccccccccc  


 66   continue
      read(1,'(a)',end=300,err=300) text
c      write(6,*) 'Phase:', text(1:108)

      if(text.eq.' '.or.text(1:4).eq.' SSS') goto 30    ! write out     
c
c   assume one more phase
c
c
c     write(6,*) text(1:117)
      k=k+1

      call rea_phase_clear(k)     
      rea_stat(k)=text(1:4)//' '

      read(text(8:13),*,err=222) x ! skip distance if big indicated by ****
      if(x.gt.0.0) then
          rea_dist(k)=x
c         read(text(14:18),*) rea_ain(k)
          read(text(14:18),*) rea_az(k)
      endif
 222  continue
   
c
c   read times if a p phase
c
      if(text(32:32).ne.' ') then
         read(text(42:47),*) rea_sec(k)
         read(text(36:40),'(i2,i3)') rea_hour(k),rea_min(k)
         if(text(51:54).ne.' ') 
     *   read(text(50:54),*) rea_res(k) 
         rea_phase(k)(1:1)=text(32:32)
         rea_onset(k)=text(31:31)
         rea_weight_in(k)=text(34:34)
      endif

      rea_year(k)=hyp_year(1)
      rea_month(k)=hyp_month(1)
      rea_day(k)=hyp_day(1)
c
c   polarity
c
            
      if(text(33:33).eq.'-') rea_polarity(k)='D'
      if(text(33:33).eq.'+') rea_polarity(k)='C'

c
c   read times if a s phase
c
      if(text(59:59).ne.' ') then
         k=k+1
         read(text(63:68),*) rea_sec(k)
         if(text(71:75).ne.' ') 
     *   read(text(71:75),*) rea_res(k) 
         rea_phase(k)(1:1)=text(59:59)
         rea_weight_in(k)=text(61:61)
         rea_onset(k)=text(58:58)
         rea_year(k)=hyp_year(1)
         rea_month(k)=hyp_month(1)
         rea_day(k)=hyp_day(1)
         rea_stat(k)=rea_stat(k-1)
         rea_hour(k)=rea_hour(k-1)
         rea_min(k)=rea_min(k-1)
         rea_dist(k)=rea_dist(k-1)
         rea_ain(k)=rea_ain(k-1)
         rea_az(k)=rea_az(k-1)
      endif


c
c   go for next phase
c
      goto 66

c--------------------------------------------------------------
c  end of one event for first file, read next file, the arr file
c--------------------------------------------------------------

 30   continue

      write(6,*) 'Number of phases found in hypo file ', k

      
      do j=1,k
c
c   since all times in output are referred to same minute
c   a new minute must be calculated not to get overflow in seconds
c
         call date_doy(doy1,rea_day(j),rea_month(j),rea_year(j))

         call timsec(rea_year(j),rea_month(j),rea_day(j),
     *   rea_hour(j),rea_min(j),rea_sec(j),abstime)

         call sectim(abstime,rea_year(j),doy,rea_month(j),rea_day(j),
     *   rea_hour(j),rea_min(j),rea_sec(j))
c
c   if data has been converted to next day, correct hr  so
c   it refers to date of header
c
         if(doy.gt.doy1) then
             rea_hour(j)=rea_hour(j)+24
         endif       
      enddo


      k2=0   ! count phases in arr file
      nml=0  ! count ml magnitudes

 200  continue
 
      read(4,'(a)',end=250) text
      if(text.eq.' '.or.text(1:3).eq.'SSS') goto 250
      k2=k2+1

      call rea2_phase_clear(k2)

      rea2_stat(k2)=text(1:5)
      rea2_com(k2)=text(62:64)
      if(rea2_com(k2)(3:3).eq.' ') then
         rea2_com(k2)(3:3)=rea2_com(k2)(2:2)
         rea2_com(k2)(2:2)=' '
      endif


      read(text(8:25),*) time_after_1970   ! s since 1970
      call timsec(1970,1,1,0,0,0.0,time_before_1970)
      time_after_0000=time_before_1970+time_after_1970
      call sectim(time_after_0000,year,doy,month,day,hr,minute,sec)

c      write(6,*) year,doy,month,day,hr,minute,sec

      rea2_hour(k2)=hr
      rea2_min(k2)=minute
      rea2_sec(k2)=sec

      read(text(35:38),*) rea2_year(k2)
      read(text(39:41),*) i
      call dte(i,rea2_day(k2),rea2_month(k2),rea2_year(k2))
      rea2_phase(k2)=text(71:78)
c
c   read amplitude 
c
       read(text(137:145),*) x
c      x=x*10.0     ! seems that amplitude is in um*100

      if(rea2_phase(k2)(1:3).eq.'AML'.and.x.gt.0.0) then
c          rea_phase(k2)='IAML'
          read(text(149:154),*) rea2_per(k2)
          rea2_amp(k2)=x
c
c  read magnitude for average ml
c
          read(text(158:162),*) xx
          if(xx.gt.0.0) then
             nml=nml+1
             ml=ml+xx
          endif
      endif
c
c   if amplitude and phase P or S, create a new phase
c
      if(rea2_phase(k2)(1:3).ne.'AML'.and.x.gt.0.0) then
         k2=k2+1
         call rea2_phase_clear(k2)
         rea2_year(k2)=rea2_year(k2-1)
         rea2_month(k2)=rea2_month(k2-1)
         rea2_day(k2)=rea2_day(k2-1)
         rea2_stat(k2)=rea2_stat(k2-1)
         rea2_com(k2)=rea2_com(k2-1)
         rea2_hour(k2)=rea2_hour(k2-1)
         rea2_min(k2)=rea2_min(k2-1)
c
c   add 2 sec so not plotted at same point as s
c
         rea2_sec(k2)=rea2_sec(k2-1)+2.0   
         rea2_amp(k2)=x
         read(text(149:154),*) rea2_per(k2)
         rea2_phase(k2)='AMP'//'-'//rea2_phase(k2-1)(1:1)
      endif
      x=0.0

      goto 200

 250  continue     

c      do i=1,k2
c         write(6,*) rea2_stat(i),rea2_com(i),rea2_year(i),
c     *   rea2_month(i),rea2_day(i),rea2_hour(i),rea2_min(i),
c     *   rea2_sec(i),rea2_phase(i),rea2_amp(i),rea2_per(i)
c      enddo
c
c   calculate average ml
c
      if(nml.gt.0) then
         ml=ml/nml
         hyp_mag(1,1)=ml
         hyp_mag(3,1)=ml
         hyp_mag_type(1,1)='L'
         hyp_mag_agency(1,1)='GS '
         hyp_mag_agency(3,1)='GS '
         hyp_mag_type(3,1)='L'
         rea_nmag=rea_nmag+1

      endif
 

c
c   now both files read for one event, check and combine
c

c
c   times must be similar in two files, P-phases are compared
c   if more then 1 s different stop
c
      do i=1,k
        do j=1,k2
            if(rea_stat(i).eq.rea2_stat(j).and.
     *         rea_phase(i)(1:1).eq.'P'.and.rea2_phase(j)(1:1).eq.'P') 
     *         then
                  if(abs(rea_sec(i)-rea2_sec(j)).gt.1) then
                     write(6,*)
     *              'hyp and arr files do not have same P-arrival time',
     *              ' for station ',rea_stat(i)
                    write(6,'(a,1x,i4,1x,2i2,1x,2i2,f6.2)')
     *              'Hypofile: ',rea_year(i),
     *              rea_month(i),rea_day(i),
     *              rea_hour(i),rea_min(i),rea_sec(i)
                    write(6,'(a,1x,i4,1x,2i2,1x,2i2,f6.2)')
     *              'Arr file: ',rea2_year(j),
     *              rea2_month(j),rea2_day(j),
     *              rea2_hour(j),rea2_min(j),rea2_sec(j)
                    write(6,'(a,f15.2)') 
     *              'Time after 1970 in arr-file for last station: ',
     *               time_after_1970
                    stop
                endif
             endif
          enddo
      enddo
     
c
c   componenets
c
      do i=1,k
        do j=1,k2
            if(rea_stat(i).eq.rea2_stat(j)) then
               if(rea_phase(i)(1:1).eq.'P') 
     *         rea_com(i)=rea2_com(j)(1:2)//'Z'   ! always Z on P 
               if(rea_phase(i)(1:1).eq.'S') 
     *         rea_com(i)=rea2_com(j)(1:2)//'N'   ! always N on S
c
c   make sure upper case
c
               do l=1,3
                  rea_com(i)(l:l)= ucase(rea_com(i)(l:l))
                  rea2_com(j)(l:l)= ucase(rea2_com(j)(l:l))
               enddo
             endif
          enddo
      enddo
c
c   get az and dist from hypo data 
c

      do i=1,k
        do j=1,k2
           if(rea_stat(i).eq.rea2_stat(j)) then
               if(rea2_phase(j)(1:2).eq.'AM') then
                  rea2_dist(j)=rea_dist(i)
                  rea2_az(j)=rea_az(i)
               endif 
           endif
          enddo
      enddo
c
c   amplitudes from arr file
c
      do j=1,k2
         if(rea2_phase(j)(1:2).eq.'AM') then
            k=k+1
            call rea_phase_clear(k)
            call rea_copy_phase_rea2_to_rea(j,k)
         endif
      enddo                 
c      
      rea_nphase=k  

      do k=1,rea_nphase
c
c   since all times in output are referred to same minute
c   a new minute must be calculated not to get overflow in seconds
c
         call date_doy(doy1,rea_day(k),rea_month(k),rea_year(k))

         call timsec(rea_year(k),rea_month(k),rea_day(k),
     *   rea_hour(k),rea_min(k),rea_sec(k),abstime)

         call sectim(abstime,rea_year(k),doy,rea_month(k),rea_day(k),
     *   rea_hour(k),rea_min(k),rea_sec(k))
c
c   if data has been converted to next day, correct hr  so
c   it refers to date of header
c
         if(doy.gt.doy1) then
             rea_hour(k)=rea_hour(k)+24
         endif
c
c   change AML to IAML
c
         if(rea_phase(k)(1:3).eq.'AML') rea_phase(k)(1:4)='IAML'
c
c   operator and agency
c
         rea_operator(k)=operator(1:3)
         rea_agency(k)='GS '        
      enddo

      nevent=nevent+1               ! count events

       hyp_dist_id(1)='L'
       rea_nhyp=1
       dup=.true.     ! duplicate header line
       if(dup) then
          hyp_year(2)=hyp_year(1)
          hyp_month(2)=hyp_month(1)
          hyp_day(2)=hyp_day(1)
          hyp_hour(2)=hyp_hour(1)
          hyp_min(2)=hyp_min(1)
          hyp_sec(2)=hyp_sec(1)
          hyp_dist_id(2)=hyp_dist_id(1)
          hyp_type(2)=hyp_type(1)
          hyp_lat(2)=hyp_lat(1)
          hyp_lon(2)=hyp_lon(1)
          hyp_depth(2)=hyp_depth(1)
          hyp_agency(2)='ORG'
          hyp_rms(2)=hyp_rms(1)
          hyp_mag(1,2)=hyp_mag(1,1)
          hyp_mag_agency(1,2)='ORG'
          hyp_mag_type(1,2)=hyp_mag_type(1,1)
          hyp_mag(2,2)=hyp_mag(2,1)
          hyp_mag_agency(2,2)='ORG'
          hyp_mag_type(2,2)=hyp_mag_type(2,1)
          rea_nhyp=2
        endif
c
c   add wav file name if given
c
c
c   abs time of origin time
c
      call timsec(hyp_year(1),hyp_month(1),hyp_day(1),hyp_hour(1),
     *hyp_min(1),hyp_sec(1),abstime_sfile)
  
      if(nwav.gt.0) then
         do i=1,nwav
            if(dabs(abstime_sfile-abstime_wavfile(i)).lt.wav_dif) then
c
c   add to s-file
c
               rea_nwav=1
               rea_wav(1)=' '
               rea_wav(1)(2:seiclen(wavfile(i))+1)=wavfile(i)
     *         (1:seiclen(wavfile(i)))
               rea_wav(1)(80:80)='6'
               write(6,*)'Waveform file added ', wavfile(i)
               wavfile_used(i)=1  ! indicate used
               goto 76
           endif
         enddo

         nmiss=nmiss+1
         write(6,*)'No waveform file for this event'
         write(wav_miss(nmiss),'(i4,1x,2i2,1x,2i2,f5.1)')
     *   hyp_year(1),hyp_month(1),hyp_day(1),hyp_hour(1),
     *   hyp_min(1),hyp_sec(1)
 76      continue
      endif
 
c
c   count stations used for location
c
       call rea_count_stations
       hyp_nstat(1)=rea_nstat
       hyp_nstat(2)=hyp_nstat(1)

c
c   make sure upper case
c
        do k=1,rea_nphase

           do l=1,3
              rea_com(k)(l:l)= ucase(rea_com(k)(l:l))
           enddo
        enddo
 
c
c   write out event
c

       call rea_event_out(2,all,data,code)

c
c   write the whole first header line to screen
c
      write(6,'(a)') data(1)(1:79)
c
c   get next event
c

       goto 50

 300   continue
c
c     end of file
c
c
      write(6,*)            ! blank line
      close(2)              ! close output file
      
      k=0
      do i=1,nwav
         if(wavfile_used(i).eq.0) k=k+1
      enddo

      write(6,*)'Number of waveform files not used ',k

      if(k.gt.0) then
         do i=1,nwav
            if(wavfile_used(i).eq.0) write(6,'(a,a)')
     *     ' File not used: ',wavfile(i)(1:seiclen(wavfile(i)))
         enddo
      endif
      write(6,*)
      write(6,*)'Number of events without waveform file', nmiss

      if(nmiss.gt.0) then
         write(6,*)'Events which did not get a waveform file name'
         do i=1,nmiss
            write(6,'(1x,a)') wav_miss(i)
         enddo
      endif
        
      write(6,*)
      write(6,*)'Output file name is gsrnor.out'


      stop
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine hypo1
c
c
c
c    part of program that can be used to read hypo out file type 1
c    *****************  not used *********************************
c

      implicit none                       ! force delcaration of all variables
      include 'seidim.inc'                ! dimensions for rea block
      include 'rea.inc'                   ! parameter common bliock
      include 'seisan.inc'

      character*80 data(5000)             ! s-file with data in text array
      character*80 err_text               ! error text
      character*150 text
      character*80 infile                 ! input file
      character*3  agency                 ! agency for mag and hypo
      double precision  abstime
      integer doy,doy1                    ! day of year
c-- arguments
      character*80 args(10)    
c---number of arguments 
      integer nars       
      character*1 format                  ! some variation in newer hypoinverse
      integer version                     ! different input versions
      logical all                         ! true: read all data, false: headers
      logical eof                         ! true: end of file
      logical head                        ! true if header info
      logical read_head                   ! true if header should be read
      logical dup                         ! true if duplicate header
      character*1 answer
      integer code                        ! error return code
      real min                            ! minute of degree
      real x                              ! help variable
      integer nevent                      ! number of events in file
      integer ev_number                   ! original event number
      logical arc                         ! true if arc line
      integer i,k,l                       ! counters
      character*1 ucase                   ! fubction

c
c   new event
c
  50  continue

c
c   clear variables
c
      rea_nphase=0      ! count number of phase lines
      rea_nhyp=0        ! count number of hypocenters
      rea_nwav=0        ! --------------- waveform files
      rea_ncomment=0    ! --------------- comment lines
      rea_nmacro=0      ! --------------- macroseismic data
      rea_nfault=0      ! --------------- fault plane solutions
      rea_nspec=0       ! --------------- spectral solutions
      rea_nmag=0        ! --------------- magnitudes
      rea_id_line=' '   ! no id line
      k=0               ! phase counter

      call rea_hyp_clear(1)
      call rea_hyp_clear(2)
      do i=1,500
         call rea_phase_clear(i)
      enddo
     
      rea_nwav=0  
      rea_locality=' '   
c
c  read one line
c
 20   continue
      read(1,'(a)',end=300,err=300) text
         do i=1,150
            if(ichar(text(i:i)).eq.9)then
                 text(i:i)=' '  ! remove tab
c                 write(6,*)'tab removed'
            endif
         enddo


      if(text(1:100).eq.' ') goto 20          ! read to non blank line

c      write(6,*) text(1:80)
c
c   check if next event
c
      if(text(3:6).eq.'DATE') then
         read(1,'(a)',end=300,err=300) text  ! next line with header
          do i=1,150
            if(ichar(text(i:i)).eq.9)then
                 text(i:i)=' '  ! remove tab
c                 write(6,*)'tab removed'
            endif
          enddo
c 
c   id line
c
c
               rea_id_line(61:64)=text(2:5)
               rea_id_line(2:11)='ACTION:H71' 
               rea_id_line(58:60)='ID:'
               rea_id_line(80:80)='I'             
               head=.true.
               read(text(2:3),'(i4)') hyp_year(1)
               hyp_year(1)=hyp_year(1)+2000
               read(text(4:5),'(i2)') hyp_month(1)
               read(text(6:7),'(i2)') hyp_day(1)
               read(text(9:10),'(i2)') hyp_hour(1)
               read(text(11:12),'(i2)') hyp_min(1)
               read(text(14:18),'(f5.2)') hyp_sec(1)
c
c               write(6,*) text(1:80)
c               write(6,*) hyp_year(1),hyp_month(1)

               hyp_agency(1)='GS '
               i=hyp_sec(1)
               write(rea_id_line(61:74),'(i4,6i2)')
     *         hyp_year(1),hyp_month(1),hyp_day(1),
     *         hyp_hour(1),hyp_min(1),i
               do i=61,74
                 if(rea_id_line(i:i).eq.' ') rea_id_line(i:i)='0'
               enddo
               rea_id_line(76:76)='L'
c location

c  lat
               if(text(20:27).ne.' ') then
               read(text(20:21),'(i2)') i
               read(text(23:27),'(f5.2)') min
               hyp_lat(1)=i+min/60.0
c               if(text(28:3).eq.'S') hyp_lat(1)=-hyp_lat(1)
c  lon
               read(text(28:30),'(i3)') i
               read(text(32:36),'(f5.2)') min
               hyp_lon(1)=i+min/60.0
c               if(text(27:27).eq.' ') hyp_lon(1)=-hyp_lon(1)
               if(text(39:39).eq.'W') hyp_lon(1)=-hyp_lon(1)

               read(text(37:42),*) hyp_depth(1)
c
c   magnitude from p and s?
c
               if(text(43:46).ne.' ') then
                  read(text(43:46),'(f4.2)') x
                  rea_ncomment=rea_ncomment+1
                  rea_comment(rea_ncomment)=' '
                  rea_comment(rea_ncomment)(2:16)='Magnitude Mpv: '
                  write(rea_comment(rea_ncomment)(18:25),'(f5.1)')x
                  rea_comment(rea_ncomment)(80:80)='3'
               endif
c
c   coda mag
c               if(text(78:81).ne.' ') then
c                  rea_nmag=rea_nmag+1
c                  read(text(77:81),'(f5.2)') hyp_mag(rea_nmag,1)
c                  hyp_mag_type(rea_nmag,1)='C'
c                  hyp_mag_agency(rea_nmag,1)='TIR'
c               endif

                read(text(59:63),*) hyp_rms(1)
               endif


                if(text(54:56).ne.' ') then
                  read(text(53:56),*) hyp_gap(1)
                  read(text(64:68),*)hyp_lat_err(1)
                  hyp_lon_err(1)=hyp_lat_err(1)
                  read(text(69:73),*) hyp_depth_err(1)
                  hyp_error(1)=.true.
               endif

             goto 20     ! read next line            
           endif

cccccccccccccccccccccccccccc    phases   cccccccccccccccccccccccccc  



      if(text(3:11).eq.'STN  DIST') then


 66      continue
         read(1,'(a)',end=300,err=300) text
         write(6,*) 'Phase:', text(1:108)


         if(text.eq.' ') goto 30    ! write out     


c
c   assume one more phase
c
cx
c           write(6,*) text(1:117)
            k=k+1

            call rea_phase_clear(k)     
            rea_stat(k)=text(2:5)//' '
c
c   normal location for componet
c
c            rea_co(k)(1:1)=text(10:10)
c            rea_co(k)(2:2)=text(12:12)

c
c   stat comp might be blank, assume same as previous
c
c            if(rea_stat(k).eq.' ') then
c               rea_stat(k)=rea_stat(k-1)
c               rea_co(k)=rea_co(k-1)
c            endif


            read(text(7:11),*,err=222) x ! skip distance if big indicated by ****
            if(x.gt.0.0) then
               rea_dist(k)=x
               read(text(17:19),*) rea_ain(k)
               read(text(13:15),*) rea_az(k)
            endif
 222        continue
   
c
c   read times if a p phase
c
            if(text(31:35).ne.' ') then
               read(text(31:35),*) rea_sec(k)
               read(text(26:29),'(2i2)') rea_hour(k),rea_min(k)
               if(text(54:59).ne.' ') 
     *         read(text(121:125),*) rea_res(k) 
               rea_phase(k)(1:1)=text(22:22)
               rea_onset(k)=text(21:21)
               rea_weight_in(k)=text(24:24)
            endif

         rea_year(k)=hyp_year(1)
         rea_month(k)=hyp_month(1)
         rea_day(k)=hyp_day(1)

c
c   read times if a s phase
c
            if(text(105:105).ne.' ') then
               k=k+1
               read(text(108:113),*) rea_sec(k)
               if(text(121:125).ne.' ') 
     *         read(text(121:125),*) rea_res(k) 
               rea_phase(k)(1:1)=text(105:105)
               rea_weight_in(k)=text(24:24)
                rea_year(k)=hyp_year(1)
                rea_month(k)=hyp_month(1)
                rea_day(k)=hyp_day(1)
                rea_stat(k)=rea_stat(k-1)
                rea_hour(k)=rea_hour(k-1)
                rea_min(k)=rea_min(k-1)
                rea_dist(k)=rea_dist(k-1)
                rea_ain(k)=rea_ain(k-1)
                rea_az(k)=rea_az(k-1)
            endif
c
c   polarity
c
            
            if(text(23:23).eq.'-') rea_polarity(k)='D'
            if(text(23:23).eq.'+') rea_polarity(k)='C'

c
c   read amplitude
c
            if(text(66:69).ne.' ') then  
               k=k+1         
               read(text(66:69),*) rea_amp(k)
               rea_phase(k)(1:4)='IAML'
            
c
c   read period
c
               if(text(89:94).ne.' ') then
                  read(text(89:94),*) rea_per(k)
               endif
c
c   convert amplitudes to nm
c
c               rea_amp(k)=rea_amp(k)*100


                rea_year(k)=hyp_year(1)
                rea_month(k)=hyp_month(1)
                rea_day(k)=hyp_day(1)
                rea_stat(k)=rea_stat(k-1)
                rea_hour(k)=rea_hour(k-1)
                rea_min(k)=rea_min(k-1)
                rea_sec(k)=rea_sec(k-1)
                rea_dist(k)=rea_dist(k-1)
                rea_az(k)=rea_az(k-1)
            endif
c         endif

 

c         rea_hour(k)=hyp_hour(1)
c         rea_min(k)=hyp_min(1)                 
c      endif

c
c   go for next phase
c
         goto 66
      endif

      goto 20

c----------------------------------------------
c  end of one event, write out
c----------------------------------------------

 30   continue

      write(6,*) 'Number of phases found ', k
      rea_nphase=k  

c
c   since all times in output are referred to same minute
c   a new minute must be calculated not to get overflow in seconds
c
c         goto 666
         call date_doy(doy1,rea_day(k),rea_month(k),rea_year(k))

         call timsec(rea_year(k),rea_month(k),rea_day(k),
     *   rea_hour(k),rea_min(k),rea_sec(k),abstime)

         call sectim(abstime,rea_year(k),doy,rea_month(k),rea_day(k),
     *   rea_hour(k),rea_min(k),rea_sec(k))
c
c   if data has been converted to next day, correct hr  so
c   it refers to date of header
c
         if(doy.gt.doy1) then
             rea_hour(k)=rea_hour(k)+24
         endif
c 666  continue        
cxx      enddo

     
      nevent=nevent+1               ! count events

c
c   write out event
c

       hyp_dist_id(1)='L'
       rea_nhyp=1
       dup=.true.     ! duplicate header line
       if(dup) then
          hyp_year(2)=hyp_year(1)
          hyp_month(2)=hyp_month(1)
          hyp_day(2)=hyp_day(1)
          hyp_hour(2)=hyp_hour(1)
          hyp_min(2)=hyp_min(1)
          hyp_sec(2)=hyp_sec(1)
          hyp_dist_id(2)=hyp_dist_id(1)
          hyp_lat(2)=hyp_lat(1)
          hyp_lon(2)=hyp_lon(1)
          hyp_depth(2)=hyp_depth(1)
          hyp_agency(2)='ORG'
          hyp_rms(2)=hyp_rms(1)
          hyp_mag(1,2)=hyp_mag(1,1)
          hyp_mag_agency(1,2)=hyp_mag_agency(1,1)
          hyp_mag_type(1,2)=hyp_mag_type(1,1)

          hyp_mag(2,2)=hyp_mag(2,1)
          hyp_mag_agency(2,2)='ORG'
          hyp_mag_type(2,2)=hyp_mag_type(2,1)
          rea_nhyp=2
        endif
c
c   make sure upper case
c
        do k=1,rea_nphase

           do l=1,3
              rea_com(k)(l:l)= ucase(rea_com(k)(l:l))
           enddo
        enddo

       call rea_event_out(2,all,data,code)

c
c   write the whole first header line to screen
c
      write(6,'(a)') data(1)(1:79)
c
c   get next event
c
c      read_head=.true.
c      if(.not.eof) goto 50
       goto 50

 300   continue
       return
       end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine make_seisan_sac_resp
c
c   convert GSR SAC files to SEISAN SAC response files
c
      implicit none

      character*80 text
      character*80 filein,fileout    ! resp files
      character*80 out(20)  ! lines in resp file to write
      character*5 stat      ! station code
      character*3 com       ! componenet code
      character*3 xmonth(12)! months in text
      integer year,month,day,hour,min
      real sec
      integer i, seiclen,k
      data xmonth /'jan','feb','mar','apr','may',
     *'jun','jul','aug','sep','oct','nov','dec'/

      open(1,file='filenr.lis',err=10)
      goto 20
 10   continue
      write(6,*) 'No filenr.lis file'
      return
 20   continue
c
c   file loop of filenr.lis
c
 30   continue
      read(1,'(7x,a)',end=100) filein 
      if(filein.eq.' ') goto 100
      write(6,'(a,a30)')'Input file:  ',filein(1:30)
c
c   open and read file
c
      open(2,file=filein,status='old')
c
c   read file loop
c

c
c   if no time, set a time
c
      year=2000
      month=1
      day=1
      hour=0
      min=0
      stat=' '
      com=' '

 40   continue

      read(2,'(a)',end=50) text
      if(text(1:9).eq.'# Station') read(text(11:15),'(a)') stat
      if(text(1:9).eq.'# Channel') read(text(11:14),'(a)') com
c
c   check com for wrong position of orientation and lower case
c
      if(com(2:2).eq.'Z'.or.com(2:2).eq.'N'.or.com(2:2).eq.'E') then
         com(3:3)=com(2:2)
         com(2:2)=' '
      endif

      if(com(1:1).eq.'e') com(1:1)='E'
      if(com(1:1).eq.'b') com(1:1)='B'
      if(com(1:1).eq.'s') com(1:1)='S'
      if(com(2:2).eq.'h') com(2:2)='H'
      if(com(3:3).eq.'z') com(3:3)='Z'
      if(com(3:3).eq.'n') com(3:3)='N'
      if(com(3:3).eq.'e') com(3:3)='E'     

 
      if(text(1:6).eq.'# Time') then
         do i=1,seiclen(text)
            if(text(i:i).eq.'.') text(i:i)=' '
            if(text(i:i).eq.':') text(i:i)=' '
         enddo
      
         read(text(8:30),*)year,month,day,hour,min,sec
      endif 
  
      if(text(1:5).eq.'ZEROS') then
c
c   if year is 2000,  no date inside, try to get date from filename
c
         if(year.eq.2000) then
             i=seiclen(filein)

             if(filein(i-3:i-3).eq.'.'.and.i.gt.15) then
                read(filein(i-12:i-9),'(i4)') year
                read(filein(i-5:i-4),'(i2)') day
                do k=1,12
                   if(filein(i-8:i-6).eq.xmonth(k)) goto 45
                enddo
                write(6,*)'No month found'
                return
 45             continue
                month=k
             endif
         endif
c
c   if stat is blank, not found inside, try to get
c   from file name
c
         if(stat.eq.' ') then     
            i=index(filein,'_')
            stat(1:i-1)=filein(1:i-1)
            filein(i:i)='*'
            if(seiclen(filein).lt.15) then  ! no date in name
               k=index(filein,'.')
            else
               k=index(filein,'_')
            endif
            com(1:k-i-1)=filein(i+1:k-1)
c
c   check com for wrong position of orientation and lower case
c
            if(com(2:2).eq.'Z'.or.com(2:2).eq.'N'.or.
     *         com(2:2).eq.'E') then
               com(3:3)=com(2:2)
               com(2:2)=' '
            endif
c
c  next seems not to be neded if from file name
c
            if(com(1:1).eq.'e') com(1:1)='E'
            if(com(1:1).eq.'b') com(1:1)='B'
            if(com(1:1).eq.'s') com(1:1)='S'
            if(com(2:2).eq.'h') com(2:2)='H'
            if(com(3:3).eq.'z') com(3:3)='Z'
            if(com(3:3).eq.'n') com(3:3)='N'
            if(com(3:3).eq.'e') com(3:3)='E'     

         endif  

 
c
c   make outfile name
c
c
         fileout=' '
         fileout(1:5)=stat
         fileout(6:7)=com(1:2)
         fileout(9:9)=com(3:3)
         fileout(10:10)='.'
         write(fileout(11:25),'(i4,1x,i2,1x,i2,1x,2i2)') 
     *   year,month,day,hour,min
         fileout(15:15)='_'
         fileout(18:18)='_'
         fileout(21:21)='_'
         fileout(26:29)='_SAC'
         do i=1,25
            if(fileout(i:i).eq.' ') fileout(i:i)='_'
         enddo
         if(fileout(16:16).eq.'_') fileout(16:16)='0'
         if(fileout(19:19).eq.'_') fileout(19:19)='0'
         if(fileout(22:22).eq.'_') fileout(22:22)='0'
         if(fileout(24:24).eq.'_') fileout(24:24)='0'
         write(6,'(a,a30)')'Output file: ',fileout(1:30)
         open(3,file=fileout,status='unknown')
      endif
      if(text.ne.' ') write(3,'(a)') text
      goto 40  ! next line    

 50   continue
      close(2)
      close(3)

      goto 30  ! next file

 100  continue
      close(1)
      return
      end     