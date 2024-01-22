 
c  reading albania bulletin data, jh 2019-2020

c--------------------------------------------------------------------------
c  Program for reading hypoinverse output data as used in albania and 
c  converting to nordic format. plus a few old formats.
c
c  argument: none: hypoinverse
c            1: kind of nordic format
c            2: old format, quite different. Input have 2 files like
c               APR97 and APR97.DAT with phases and epicentrs
c               respectively
c            3: nearly nordic format, 2008-2009, some different 2008
c            4: --------------------, 2007
c            5: june to aug, 2008
c  
c  only option 0 can write new nordic format
c
c  agency is hardwired to TIR
c
c  header line is duplicated, 2. line with agency ORG
c
c  All events are assumed local
c  Check of phases on next day realtive to main header, the add 24 h
c
c  Time of amplitude is arbitrary since no time is given for amplitude
c  so it is set to 100 s after header hr min
c--------------------------------------------------------------------------c
c
c  For detail on parameters and variables names, see rea.inc
c
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
      integer i,k                         ! counters


      call get_seisan_def

c
c   check s-file format
c
      if(new_nordic_format) then
         rea_new_out=.true.              ! write new format
      endif


c
c   open output file
c
       open(2,file='albnor.out',status='unknown')
        
       infile=' '
c
c   argument is type of input
c   no argument: hypoinverse
c   1: old nordic
c   2: old old format before 200
c   3: 2008-2009
c   4: 2007
c   5: jun-aug 2008
c
        write(6,*) ' Formats'
        write(6,*) '0: hypoinverse'
        write(6,*) '1: 2012-2014'
        write(6,*) '2: 1995-2005'
        write(6,*) '3: 2008-2009'
        write(6,*) '4: 2007'
        write(6,*) '5: jun-aug-2008'

        version=0     ! hypoinverse

        call get_arguments(nars,args)
        if(nars.eq.0) then
           write(6,*)'Give format version'
           read(5,*) version
        endif 
        if(nars.gt.0) then
           read(args(1),*) version
           if(version.ne.1.and.version.ne.2.and.version.ne.3.
     *     and.version.ne.4.and.version.ne.5.and.version.ne.0) then
              write(6,*)'Wrong version, use blank, 1,2,3 or 4'
              stop
           endif
        endif   
        write(6,*)

        if(version.eq.0) write(6,*)'Hypoinverse format'
        if(version.eq.1) write(6,*)'Old Nordic like format'
        if(version.eq.2) write(6,*)'1995-2005 format'
        if(version.eq.3) write(6,*)'2008-2009 format'
        if(version.eq.4) write(6,*)'2007 format'
        if(version.eq.5) write(6,*)'2008 may-aug format'


c
c   get input file name, check if exist
c

 9    continue
      if(infile.eq.' ') then
         write(6,*) 'Give input file'
         read(5,'(a)') infile
      endif
      open(1,file=infile,status='old',err=10)
      goto 11
 10   continue
      write(6,*)'No such input file'
      infile=' '
      goto 9
 11   continue

c
c   old nordic
c
      if(version.eq.1) then
         close(1)
         call old_albanian(infile,nevent)
         goto 300
      endif

c
c   old old
c
      if(version.eq.2) then
         close(1)
         call old_old_albanian(infile,nevent)
         goto 300
      endif


      if(version.eq.3) then
         call albanian_2008(infile,nevent)
         goto 300
      endif

      if(version.eq.4) then
         call albanian_2007(infile,nevent)
         goto 300
      endif

      if(version.eq.5) then
         call albanian_2008_may_aug(infile,nevent)
         goto 300
      endif
c
c   hypoinverse
c

      all=.true.                  ! read all parameters
 
      nevent=0                    ! initialize counter
      read_head=.true.            ! header should be read first time

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
c                 write(6,*)'tab removed'
            endif
         enddo


      if(text(1:100).eq.' ') goto 20          ! read to non blank line
cxx
      write(6,*) text(1:80)
c
c   check if next event
c
      if(text(2:5).eq.'YEAR') then
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
               rea_id_line(2:11)='ACTION:HIN' 
               rea_id_line(58:60)='ID:'
               rea_id_line(80:80)='I'             
               head=.true.
               read(text(2:5),'(i4)') hyp_year(1)
               read(text(7:8),'(i2)') hyp_month(1)
               read(text(10:11),'(i2)') hyp_day(1)
               read(text(14:15),'(i2)') hyp_hour(1)
               read(text(16:17),'(i2)') hyp_min(1)
               read(text(19:23),'(f5.2)') hyp_sec(1)
cxx
c               write(6,*) text(1:80)
c               write(6,*) hyp_year(1),hyp_month(1)

               hyp_agency(1)='TIR'
               i=hyp_sec(1)
               write(rea_id_line(61:74),'(i4,6i2)')
     *         hyp_year(1),hyp_month(1),hyp_day(1),
     *         hyp_hour(1),hyp_min(1),i
               do i=61,74
                 if(rea_id_line(i:i).eq.' ') rea_id_line(i:i)='0'
               enddo
               rea_id_line(76:76)='L'
c location
               if(text(26:27).ne.' ') then
               read(text(26:27),'(i2)') i
               read(text(29:33),'(f5.2)') min
               hyp_lat(1)=i+min/60.0


               if(text(28:28).eq.'S') hyp_lat(1)=-hyp_lat(1)
c  lon
               read(text(36:38),'(i3)') i
               read(text(40:44),'(f5.2)') min
               hyp_lon(1)=i+min/60.0
cxx               if(text(27:27).eq.' ') hyp_lon(1)=-hyp_lon(1)
               if(text(39:39).eq.'W') hyp_lon(1)=-hyp_lon(1)
               read(text(45:51),'(f6.2)') hyp_depth(1)
c
c   magnitude
c
               rea_nmag=0
               if(text(72:75).ne.' ') then
                  read(text(71:75),'(f5.2)')hyp_mag(1,1)

c
c   since this is amplitude based magnitude, we assume it is L
c
                     hyp_mag_type(1,1)='L'
                     rea_nmag=1
                     hyp_mag_agency(1,1)='TIR'
               endif
c   coda mag
               if(text(78:81).ne.' ') then
                  rea_nmag=rea_nmag+1
                  read(text(77:81),'(f5.2)') hyp_mag(rea_nmag,1)
                  hyp_mag_type(rea_nmag,1)='C'
                  hyp_mag_agency(rea_nmag,1)='TIR'
                endif
                read(text(53:57),'(f5.2)') hyp_rms(1)
               endif


c               if(text(43:45).ne.' ') then
c                  read(text(43:45),'(f3.0)') hyp_gap(1)
c                  read(text(86:89),'(f4.2)')hyp_lat_err(1)
c                  hyp_lon_err(1)=hyp_lat_err(1)
c                  read(text(90:93),'(f4.2)') hyp_depth_err(1)
c                  hyp_error(1)=.true.
c               endif

             goto 20     ! read next line            
           endif

cccccccccccccccccccccccccccc    phases   cccccccccccccccccccccccccc  



      if(text(2:8).eq.'STA NET') then
         format=' '
         format=text(14:14)

 66      continue
         read(1,'(a)',end=300,err=300) text
         write(6,*) 'Phase:', text(1:108)


         if(text.eq.' ') goto 30    ! write out     
c
c   in case no blank lines between events, more cases might be needed
c
         if(text(2:14).eq.'ERROR ELLIPSE') goto 30   
         if(text(7:16).eq.'UNWEIGHTED') goto 30 
         if(text(2:9).eq.'*** SKIP') goto 30
         if(text(2:13).eq.'USE SUPPLIED') goto 30

c
c   remove format change in newer data, shift columns
c
         if(format.eq.'L') then
            text(13:124)=text(15:126)
         endif  

c
c   find if a phase or amplitude
c
c      if((text(30:31).ne.' '.or.text(34:34).eq.'6').and.
c     *    text(2:3).ne.' ') then

         if((text(34:34).eq.'6'.and.text(30:31).eq.' ').or.
     *   (text(31:31).eq.'S'.or.text(31:31).eq.'P'.
     *   or.text(30:30).eq.'S'.or.text(30:30).eq.'P')) then
c
c   assume one more phase
c
cxx
c           write(6,*) text(1:117)
            k=k+1

            call rea_phase_clear(k)     
            rea_stat(k)=text(2:6)
c
c   normal location for componet
c
            rea_co(k)(1:1)=text(10:10)
            rea_co(k)(2:2)=text(12:12)

c
c   stat comp might be blank, assume same as previous
c
            if(rea_stat(k).eq.' ') then
               rea_stat(k)=rea_stat(k-1)
               rea_co(k)=rea_co(k-1)
            endif


            read(text(15:20),'(f6.1)',err=222) x ! skip distance if big indicated by ****
            if(x.gt.0.0) then
               rea_dist(k)=x
               read(text(26:28),'(f3.0)') rea_ain(k)
               read(text(22:24),'(f3.0)') rea_az(k)
            endif
 222        continue
   
c
c   read times if a phase
c
            if(text(30:31).ne.' ') then
               read(text(36:41),'(f6.2)') rea_sec(k)
               if(text(60:64).ne.' ') 
     *         read(text(59:64),'(f6.2)') rea_res(k) 
c
c  phase can be 2 letters differently positioned
c   
               if(text(30:30).ne.' ') then
                  rea_phase(k)(1:2)=text(30:31)
               else
                  rea_phase(k)(1:1)=text(31:31)
               endif
c
c   coda if places on same line as phase
c
               if(text(87:90).ne.' ') read(text(86:89),'(f4.0)',err=77) 
     *         rea_coda(k)
               goto 78
 77            continue
               write(6,*) 'error reading coda'
 78            continue
            endif
c
c   read amplitude
c
            if(text(101:105).ne.' ') then
               if(format.eq.'L') then               
                  read(text(101:105),'(f5.0)',err=88)rea_amp(k)
               else
                  read(text(99:102),'(f5.0)',err=88)rea_amp(k)
               endif
c
c   convert amplitudes to nm
c
               rea_amp(k)=(rea_amp(k)/2800.0)*1000000.0

               if(format.eq.'L') then
                  read(text(107:110),'(f4.0)') rea_per(k)
               else
                  read(text(104:106),'(f3.1)') rea_per(k)
               endif
               rea_phase(k)(1:4)='IAML'
               rea_sec(k)=100.0          ! arbitrary
               goto 89
 88            continue
               write(6,*)'error readng amp'
 89            continue              
            endif
         endif

 
c
c   date and time same for p and s except sec and same as origin time
c
         rea_year(k)=hyp_year(1)
         rea_month(k)=hyp_month(1)
         rea_day(k)=hyp_day(1)
         rea_hour(k)=hyp_hour(1)
         rea_min(k)=hyp_min(1)                 
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
      enddo

     
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
          hyp_agency(2)=hyp_agency(1)
          hyp_rms(2)=hyp_rms(1)
          hyp_mag(1,2)=hyp_mag(1,1)
          hyp_mag_agency(1,2)=hyp_mag_agency(1,1)
          hyp_mag_type(1,2)=hyp_mag_type(1,1)

          hyp_mag(2,2)=hyp_mag(2,1)
          hyp_mag_agency(2,2)=hyp_mag_agency(2,1)
          hyp_mag_type(2,2)=hyp_mag_type(2,1)
          rea_nhyp=2
        endif

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
c
c     end of file
c
c
      write(6,*)            ! blank line
      close(2)              ! close output file

      if(version.lt.2.or.version.eq.3) then
         write(6,*) 'Number of events in input file', nevent
         write(6,*) 'Output file name is albnor.out'
      endif

      stop
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine old_albanian(infile,n)

c   for albania bulletins 2012 to 2014
c   nasty format looking like nordic and with tabs, handwritten
c   so some errors
c   this was a small program before


      implicit none
      character*90 t1
      character*80 t2
      integer i,j,k,n
      character*80 infile
      real x
   
c      write(6,*)'file'
c      read(5,'(a)')infile
      open(1,file=infile,status='old')
c      open(2,file='t.out',status='unknown')

      n=0
10    continue
      read(1,'(a)',end=99) t1
      write(6,*) t1
c
c   header, only 2012 to 2014
c
      if(t1(2:5).eq.'2014'.or.t1(2:5).eq.'2013'.or.t1(2:5).eq.'2012') 
     *then
         n=n+1
         do i=1,90
            if(ichar(t1(i:i)).eq.9)then
                 t1(i:i)=' '  ! remove tab
c                 write(6,*)'tab removed'
            endif
         enddo
c         read(t1(10:11),*) x   ! fix day position some files 2012
c         i=x
c         write(t1(10:11),'(i2)')i



         t2=' '

         if(t1(2:5).eq.'2012'.and.(t1(7:7).eq.'4'.or.t1(7:7).eq.'5'.or.
     *   t1(7:7).eq.'6'.or.t1(7:7).eq.'7'.or.t1(7:7).eq.'8'.or.
     *   t1(7:7).eq.'9'))
     *   then   ! april-sep
         read(t1(9:10),*) x   ! fix day position some files 2012
         i=x
         write(t1(9:10),'(i2)')i
         read(t1(11:15),*) x   ! hrmin error
         i=x
         write(t1(11:14),'(i4)') i

         t2(1:5)=t1(1:5)
         t2(7:8)=t1(6:7)
         t2(9:10)=t1(9:10)
         t2(12:15)=t1(11:14)
c         t2(17:20)=t1(17:20)  ! some like this
         t2(17:20)=t1(17:20)
         t2(22:22)='L'

         if(t1(26:36).ne.' ') then
            t2(24:38)=t1(23:37)
         endif

c         if(t1(25:38).eq.' ') then
             t2(46:48)='TIR'
c             goto 20
c         endif
         if(t1(40:41).eq.' ') goto 115
         read(t1(39:41),*,err=155,end=155) x
         write(t2(39:43),'(f5.1)') x
 155      continue
         
         t2(46:48)='TIR'
         t2(52:55)=t1(51:54)
 115      continue
         if(t1(57:60).ne.' ') then
            read(t1(56:59),*,err=170,end=170) x
            write(t2(57:59),'(f3.1)') x
            t2(60:60)='C'
            t2(61:63)='TIR'
 170         continue
         endif

 120       continue
         t2(80:80)='1'
         write(2,'(a)') t2
         write(2,'(a)') t2
        
         else   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         if(t1(8:8).eq.'1') then   ! jan 2012
            t2=' '
            t2(1:56)=t1(1:56)
            t2(57:63)=t1(58:64)
            t2(80:80)='1'
         else

         t2(1:8)=t1(1:8)
         t2(9:15)=t1(10:16)
         t2(17:20)=t1(19:22)
         t2(22:22)='L'
         if(t1(26:34).ne.' ') then
            t2(25:38)=t1(26:39)
         endif
         if(t1(25:38).eq.' ') then
             t2(46:48)='TIR'
             goto 20
         endif
         if(t1(41:43).eq.' ') goto 15
         read(t1(41:43),*,err=55,end=55) x
         write(t2(39:43),'(f5.1)') x
 55      continue
         
         t2(46:48)='TIR'
         t2(52:55)=t1(53:56)
 15      continue
         if(t1(57:60).ne.' ') then
            read(t1(57:60),*,err=70,end=70) x
            write(t2(57:59),'(f3.1)') x
            t2(60:60)='C'
            t2(61:63)='TIR'
 70         continue
         endif
20       continue

         endif
         t2(80:80)='1'
         write(2,'(a)') t2
         write(2,'(a)') t2 
         endif

     
      endif
c
c  phases
c
      if(t1(2:5).eq.'STAT') then
      write(2,243)

 243  FORMAT(
     *' STAT SP IPHASW D HRMM SECON CODA AMPLIT PERI AZIMU VELO ',             
     *'AIN AR TRES W  DIS CAZ7')      
         
 12      continue
         read(1,'(a)',end=99) t1
         t2=' '
         if(t1.ne.' ') then
            x=0
            t2(1:28)=t1(1:28)
            if(t1(30:33).ne.' ')read(t1(61:63),*,err=60,end=60) x   ! coda
            if(t1(81:83).ne.' ')read(t1(81:83),*,err=60,end=60) x   ! coda
            i=x
            if(i.gt.0) write(t2(31:33),'(i3)') i
 60         continue
            write(2,'(a)') t2
            goto 12
         else
            write(2,*)
         endif
      endif
      goto 10
99    continue
c      write(6,*) n
      return  
      end
            
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine old_old_albanian(infile,nevent)
c
c  for albnaina bulletins 1995 to 20??
c
c
c
      implicit none                       ! force delcaration of all variables
      include 'seidim.inc'                ! dimensions for rea block
      include 'rea.inc'                   ! parameter common bliock

      character*80 data(5000)             ! s-file with data in text array
      character*80 err_text               ! error text
      character*150 text
      character*80 infile                 ! input file
      character*3  agency                 ! agency for mag and hypo
      character*1 ucase
      double precision  abstime
      integer doy                         ! day of year
c-- arguments
      character*80 args(10)    
c---number of arguments 
      integer nars       
      character*1 format
      logical all                         ! true: read all data, false: headers
      logical eof                         ! true: end of file
      logical head                        ! true if header info
      logical read_head                   ! true if header should be read
      logical dup                         ! true if duplicate header
      character*1 answer
      integer code                        ! error return code
      real x,mag                          ! help variable
      integer nmag
      integer nevent                      ! number of events in file
      integer ev_number                   ! original event number
      logical arc                         ! true if arc line
      integer i,k,nfound                  ! counters
      integer seiclen
      real diff 
      character*1 dist_id          

       integer day(5000), hour(5000),min(5000)
       integer nepi,nstat(5000)
       real sec(5000),lat(5000),lon(5000),depth(5000),rms(5000),
     * xmag(5000)
       nfound=0


      text=infile(1:seiclen(infile))//'.dat'

      call read_epi
     *(text,nepi,day,hour,min,sec,lat,lon,depth,nstat,
     *xmag,rms)
c


      open(1,file=infile,status='old',err=10)
      goto 11
 10   continue
      write(6,*)' No such input file'
      infile=' '
      stop

 11   continue

      all=.true.                  ! read all parameters
 
      nevent=0                    ! initialize counter

      nmag=0

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
      dist_id='L'
      call rea_hyp_clear(1)
      call rea_hyp_clear(2)
      do i=1,500
         call rea_phase_clear(i)
      enddo
     
      rea_nwav=0  
      rea_locality=' ' 
      nmag=0
      mag=0.0  
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


      write(6,*) text(1:80)
c
c   check if next event, then read header
c
      if(text(1:2).ne.' ') then

c 
c   id line
c
               rea_id_line(2:11)='ACTION:ALN' 
               rea_id_line(58:60)='ID:'
               rea_id_line(80:80)='I'  

           
               read(text(14:15),'(i2)') hyp_year(1)
               if(hyp_year(1).lt.100.and.hyp_year(1).gt.90) 
     *             hyp_year(1)=hyp_year(1)+1900
               if(hyp_year(1).lt.20) hyp_year(1)=hyp_year(1)+2000
               read(text(12:13),'(i2)') hyp_month(1)
               read(text(10:11),'(i2)') hyp_day(1)
               hyp_agency(1)='TIR'
               if(text(17:17).eq.'l') dist_id='D'
           endif

cccccccccccccccccccccccccccc    phases follows   cccccccccccccccccccccccccc  




 66      continue
         read(1,'(a)',end=30,err=300) text
         write(6,*)  text(1:108)

         if(text.eq.' ') goto 30    ! write out     
c
c   assume one more phase
c

            k=k+1

            call rea_phase_clear(k)     
            rea_stat(k)(1:4)=text(5:9)
            do i=1,4
               rea_stat(k)(i:i)=ucase(rea_stat(k)(i:i))
            enddo
c
c   assume componet
c
            rea_co(k)='SZ'

c
c   stat comp might be blank, assume same as previous
c
            if(rea_stat(k).eq.' ') then
               rea_stat(k)=rea_stat(k-1)
               rea_co(k)=rea_co(k-1)
            endif

c
c   station mags
c
            if(text(32:34).ne.' ') then
              nmag=nmag+1
              read(text(32:34),'(f3.1)')x
              mag=mag+x
            endif
  
c
c   read times if a phase
c
            if(text(11:13).ne.' ') then
               if(text(17:18).ne.' ') then
                   read(text(17:18),'(i2)')  rea_hour(k)
               else
                   rea_hour(k)=rea_hour(k-1)
               endif
               if(text(20:21).ne.' ') then
                  read(text(20:21),'(i2)')  rea_min(k)
               else
                  rea_min(k)=rea_min(k-1)
               endif
               read(text(23:26),'(f4.1)') rea_sec(k)
               rea_phase(k)(1:3)=text(11:13)
               
               do i=1,3
                 rea_phase(k)(i:i)=ucase(rea_phase(k)(i:i))
               enddo
               rea_onset(k)=rea_phase(k)(1:1)
               rea_phase(k)(1:2)=rea_phase(k)(2:3)
               rea_phase(k)(3:3)=' '

               if(text(10:10).eq.'+') rea_polarity(k)='C'
               if(text(10:10).eq.'-') rea_polarity(k)='D'


               if(k.eq.1) then
                  hyp_hour(1)=rea_hour(1)
                  hyp_min(1)=rea_min(1)
                  hyp_sec(1)=rea_sec(1)

                  i=hyp_sec(1)
                  write(rea_id_line(61:74),'(i4,6i2)')
     *            hyp_year(1),hyp_month(1),hyp_day(1),
     *            hyp_hour(1),hyp_min(1),i
                  do i=61,74
                     if(rea_id_line(i:i).eq.' ') rea_id_line(i:i)='0'
                  enddo
                  rea_id_line(76:76)=dist_id
               endif
         endif

c
c   go for next phase
c
         goto 66

c----------------------------------------------
c  end of one event, write out
c----------------------------------------------

 30   continue

      write(6,*) 'Number of phases found ', k
      rea_nphase=k
c
c     calculate mag  
c
       if(nmag.gt.0) then
          mag=mag/nmag        
          hyp_mag_type(1,1)='C'
          hyp_mag_type(3,1)='C'
          rea_nmag=2
          hyp_mag_agency(1,1)='TIR'
          hyp_mag_agency(3,1)='TIR'
          hyp_mag(1,1)=mag
          hyp_mag(3,1)=mag
       endif   

      nevent=nevent+1               ! count events

c
c    check if there is an epicenter, assume no more than 30 s difference
c

        do i=1,nepi
          if(hour(i).ne.-999) then
           diff=hyp_sec(1)-sec(i)+(hyp_min(1)-min(i))*60.0+
     *     (hyp_hour(1)-hour(i))*3600.0 + (hyp_day(1)-day(i))*24*3600
           if(abs(diff).lt.70.0) then
              hyp_hour(1)=hour(i)
              hyp_min(1)=min(i)
              hyp_sec(1)=sec(i)
              hyp_lat(1)=lat(i)
              hyp_lon(1)=lon(i)
              hyp_depth(1)=depth(i)
              hyp_rms(1)=rms(i)
              hyp_nstat(1)=nstat(i)
c
c  if no mag already and mag in epi file, put it in
c
              if(hyp_mag(1,1).lt.0.0.and.xmag(i).gt.0.0) then 
                  hyp_mag(1,1)=xmag(i)
                  hyp_mag_type(1,1)='C'
                  hyp_mag_type(3,1)='C'
                  rea_nmag=2
                  hyp_mag_agency(1,1)='TIR'
                  hyp_mag_agency(3,1)='TIR'
                  hyp_mag(3,1)=xmag(i)
               endif
c
c update id line
c
              k=hyp_sec(1)
              write(rea_id_line(61:74),'(i4,6i2)')
     *        hyp_year(1),hyp_month(1),hyp_day(1),
     *        hyp_hour(1),hyp_min(1),k
              do k=61,74
                 if(rea_id_line(k:k).eq.' ') rea_id_line(k:k)='0'
              enddo

              nfound=nfound+1
              hour(i)=-999   ! indicate found
           endif
         endif
       enddo
 
c
c   write out event
c

       hyp_dist_id(1)=dist_id
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
          hyp_mag_agency(1,2)='ORG'
          hyp_mag_type(1,2)=hyp_mag_type(1,1)
          hyp_mag(2,2)=hyp_mag(2,1)
          hyp_mag_agency(2,2)=hyp_mag_agency(2,1)
          hyp_mag_type(2,2)=hyp_mag_type(2,1)
          rea_nhyp=2
        endif

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
c
c     end of file
c
c
      write(6,*)            ! blank line
      close(2)              ! close output file
      write(6,*) 'Number of events in input file', nevent
      write(6,*) 'Number of events in epifile and events matched', 
     *            nepi,nfound

      if(nfound.lt.nepi) then
          write(6,*) 'events not matched'
          do i=1,nepi
             if(hour(i).ne.-999) then
                write(6,'(i2,1x,i2,1x,i2,1x,f5.1)') 
     *          day(i),hour(i),min(i),sec(i)
             endif
          enddo
      endif

      write(6,*) 'Output file name is albnor.out'

      return
      end

ccccccccccccccccccccccccccc

      subroutine read_epi
     *(filename,nepi,day,hour,min,sec,lat,lon,depth,nstat,mag,rms)
c
c   read old albanian bulletins before 2008, epicenters only to be added to phases
c
       implicit none

       integer day(5000), hour(5000),min(5000)
       integer nepi,i,nstat(5000)
       real sec(5000),lat(5000),lon(5000),depth(5000),rms(5000),
     * mag(5000)
       character*80 text,filename

       open(1,file=filename,status='old')

       nepi=0

 1     continue
       read(1,'(a)',end=100) text
       write(6,*) text
       if(text.eq.' ') goto 100
       nepi=nepi+1
       read(text(9:10),'(i2)')day(nepi)
       read(text(13:22),'(i2,1x,i2,1x,f4.1)') 
     * hour(nepi),min(nepi),sec(nepi)
       read(text(24:40),'(f6.2,f7.2f4.0)') 
     * lat(nepi),lon(nepi),depth(nepi)
       read(text(54:55),'(i2)') nstat(nepi)
       read(text(46:51),'(f6.1)') rms(nepi)
       read(text(42:45),'(f4.1)') mag(nepi)
       goto 1

 100   continue
       close(1)
       write(6,*)   
       write(6,*)'number of events in epi file', nepi
       write(6,*)   
       do i=1,nepi
          write(6,'(i2,1x,i2,1x,i2,1x,f5.1,5f6.2,i3)') 
     *    day(i),hour(i),min(i),sec(i), 
     *    lat(i),lon(i),depth(i),rms(i),mag(i),nstat(i)
       enddo
       return
       end


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine albanian_2008(infile,nevent)
c
c   read and write a deformed nordic file, ca 2008
c
      implicit none
      character*80 t,t1,infile
      integer i,nevent

      nevent=0

 10   continue
      read(1,'(a)',end=20)t
      do i=1,80   
         if(ichar(t(i:i)).eq.9)then
             t(i:i)=' '  ! remove tab
             write(6,*)'tab removed'
         endif
         if(ichar(t(i:i)).eq.0)then
             t(i:i)=' '  ! remove null
             write(6,*)'null removed'
         endif
      enddo
      write(6,*) t
      if(t(2:3).eq.'20') then
          nevent=nevent+1
          if(t(22:22).ne.'L'.and.t(22:22).ne.'D') then
              write(6,'(a,a,a)')'*',t(21:23),'*'
          endif
          t(80:80)='1'
          if(t(61:62).ne.' ') t(61:63)='TIR'
          t(46:48)='TIR'
          if(t(57:63).ne.' ') then
              t(73:79)=t(57:63)
          endif
          if(t(12:21).eq.' ') then  ! no time
              read(1,'(a)') t1
              read(1,'(a)') t1
              t(12:20)=t1(19:27)
              backspace(1)
              backspace(1)
          endif
          write(2,'(a)') t
          t(46:48)='ORG'
          if(t(61:63).ne.' ') t(61:63)='ORG'
          write(2,'(a)') t          
          goto 10
      endif         
      if(t(2:5).eq.'STAT') then
         t(69:80)=' W  DIS CAZ7'
         t(58:60)='AIN'
          write(2,'(a)') t
         goto 10
      endif
      if(t(80:80).eq.'E') then
          write(2,'(a)') t
         goto 10
      endif
      if(t(2:3).ne.'20') then
         t(71:80)=' '
      endif
      write(2,'(a)') t
      goto 10
20    continue
      return
      end


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine albanian_2007(infile,nevent)
c
c   read and write a deformed nordic file, ca 2007
c
      implicit none
      character*80 t,t1,infile,tt
      integer i,nevent, hrmin,k,day,year,month
      real lat,lon,depth,mag,sec

      nevent=0

      write(6,*)' Give month for data in 2007'
      read(5,*) month

 10   continue

      read(1,'(a)',end=20)tt
      if(tt.eq.' ') goto 10
      do i=1,80   
         if(ichar(tt(i:i)).eq.9)then
             tt(i:i)=' '  ! remove tab
             write(6,*)'tab removed'
         endif
         if(ichar(tt(i:i)).eq.0)then
             tt(i:i)=' '  ! remove null
             write(6,*)'null removed'
         endif
      enddo


      if(tt(3:3).eq.'H'.or.tt(4:4).eq.'H'.or.tt(5:5).eq.'H') then
c
c   assume header, remove chars  not numbers
c
c
          do i=1,80
             k=ichar(tt(i:i))
             if(k.eq.43.or.k.eq.45.or.k.eq.46.or.k.eq.32.or.
     *       (k.ge.48.and.k.le.57)) then
                 continue
             else
                 tt(i:i)=' '
             endif
          enddo
          write(6,'(a)') tt                      
          nevent=nevent+1
          
          t=' '
          t(80:80)='1'
          t(46:48)='TIR'

          t(22:22)='L'
          year=2007
          write(t(2:5),'(i4)') year
          write(t(7:8),'(i2)') month
c
c   read header info from first line
c
          read(tt,*)day,hrmin,sec,lat,lon
          write(t(9:10),'(i2)') day
          write(t(12:15),'(i4)') hrmin
          write(t(17:20),'(f4.1)') sec
          write(t(24:38),'(f7.3,f8.3)') lat,lon  
c     
c   2. header line
c
      read(1,'(a)',end=20)tt
      do i=1,80   
         if(ichar(tt(i:i)).eq.9)then
             tt(i:i)=' '  ! remove tab
             write(6,*)'tab removed'
         endif
         if(ichar(tt(i:i)).eq.0)then
             tt(i:i)=' '  ! remove null
             write(6,*)'null removed'
         endif
      enddo
      write(6,'(a)') tt

          do i=1,80
             k=ichar(tt(i:i))
             if(k.eq.43.or.k.eq.45.or.k.eq.46.or.k.eq.32.or.
     *       (k.ge.48.and.k.le.57)) then
                 continue
             else
                 tt(i:i)=' '
             endif
          enddo
          write(6,'(a)') tt
          mag=0.0  
          read(tt,*,err=44,end=44) depth,mag
          goto 47
 44       continue
          read(tt,*) depth
 47       continue
          write(t(39:43),'(f5.1)') depth
          if(mag.gt.0.0)then 
             write(t(57:59),'(f3.1)') mag
             t(60:63)='CTIR'
             t(73:79)=t(57:63)
          endif  


   
          write(2,'(a)') t
          write(6,'(a)') t
          t(46:48)='ORG'
          if(t(61:63).ne.' ') t(61:63)='ORG'
          write(2,'(a)') t
          goto 10
      endif
c
c 
       
      if(tt(2:5).eq.'STAT') then
         t=' '
         t=tt
         t(30:80)='CODA AMPLIT PERI AZIMU VELO AIN AR TRES W  DIS CAZ7'
         write(2,'(a)') t
c
c   phases
c
  15     continue
         read(1,'(a)',end=20) t
         if(t.eq.' ') then
           write(2,'(a)')' '
           goto 10
         else
           write(2,'(a)') t(1:28)
           goto 15
         endif
      endif
      goto 10

20    continue
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine albanian_2008_may_aug(infile,nevent)
c
c   read and write  2008 may to august
c
      implicit none
      character*80 t,t1,infile,tt,ttt(100)
      integer i,nevent, hrmin,k,day,year,month,hour,min,kk
      real lat,lon,depth,mag,sec

      nevent=0

      kk=0

 10   continue

      read(1,'(a)',end=20)tt
      write(6,*) tt
      if(tt.eq.' ') goto 10


      if(tt(2:5).eq.'2008') then
          t=' '

          t(80:80)='1'
          t(46:48)='TIR'

          t(22:22)='L'
          t(1:10)=tt(1:10)

          ttt(1)=t
          ttt(2)=t  

c
c   next event
c  
          read(tt,'(1x,i4,1x,2i2)') year,month,day                 
          nevent=nevent+1
          kk=2


c   read phases

 22       continue
          read(1,'(a)',end=33) t
          write(6,*) t
          if(t.eq.' ')  then
              kk=kk+1
              ttt(kk)=' '
              goto 33  ! end of event, goto write out
          endif
c
c   save phase
c
          if(t(2:4).ne.'Loc'.and.t(2:4).ne.'LOC') then
             kk=kk+1
             read(t(18:35),*) hrmin,sec
             t(18:35)=' '
             write(t(19:27),'(i4,1x,f4.1)') hrmin,sec
             ttt(kk)=t
             goto 22
          endif


ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c location which comes after phases
c
          if(t(2:4).eq.'Loc'.or.t(2:4).eq.'LOC') then
c
c   remove non numeric
c
          tt=t
          do i=1,80
             k=ichar(tt(i:i))
             if(k.eq.43.or.k.eq.45.or.k.eq.46.or.k.eq.32.or.
     *       (k.ge.48.and.k.le.57)) then
                 continue
             else
                 tt(i:i)=' '
             endif
          enddo
  
c
c   read header info from first line
c
          mag=0.0
          read(tt,*,end=99)lat,lon,mag
          goto 98
 99       continue
          read(tt,*)lat,lon
 98       continue


c     
c   2. header line
c
      
      read(1,'(a)') tt
      write(6,*) tt
c
c   remove non numeric
c
          do i=1,80
             k=ichar(tt(i:i))
             if(k.eq.43.or.k.eq.45.or.k.eq.46.or.k.eq.32.or.
     *       (k.ge.48.and.k.le.57)) then
                 continue
             else
                 tt(i:i)=' '
             endif
          enddo
          tt(1:3)=' '
          read(tt,*) hour,min,sec
          write(6,*) tt
          write(6,*) 'hms',hour,min,sec
          depth=10.0   ! no depth
          t=' '
          write(t,'(1x,i4,1x,2i2)') year,month,day
          write(t(39:43),'(f5.1)') depth
          if(mag.gt.0.0) write(t(57:59),'(f3.1)') mag
          write(t(24:38),'(f7.3,f8.3)') lat,lon  
          write(t(12:15),'(2i2)') hour,min
          write(t(17:20),'(f4.1)') sec

          if(mag.gt.0.0) then    
           t(60:63)='CTIR'
           t(73:79)=t(57:63)
          endif



          t(80:80)='1'
          t(46:48)='TIR'

          t(22:22)='L'

          ttt(1)=t
          ttt(2)=t  

          endif   ! header

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c
c   write out
c
 33       continue
 
c          write(6,'(a)') t
          ttt(2)(46:48)='ORG'
          if(ttt(2)(61:63).ne.' ') ttt(2)(61:63)='ORG'

          if(ttt(1)(12:20).eq.' ')then
cxx
             write(6,'(a)') ttt(1)
             write(6,'(a)') ttt(3)
             write(6,'(a)') ttt(3)(19:27)
             ttt(1)(12:20)=ttt(3)(19:27)
             ttt(2)(12:20)=ttt(3)(19:27)
          endif
          do i=1,kk
            if(ttt(i).ne.' ')write(2,'(a)') ttt(i)
          enddo
          write(2,*) ' '
          goto 10
      endif
c
c 
       
c      if(tt(2:5).eq.'STAT') then
c         t=' '
c         t=tt
c         t(30:80)='CODA AMPLIT PERI AZIMU VELO AIN AR TRES W  DIS CAZ7'
c         write(2,'(a)') t
c
c   phases
c
c  15     continue
c         read(1,'(a)',end=20) t
c         if(t.eq.' ') then
c           write(2,'(a)')' '
c           goto 10
c         else
c           write(2,'(a)') t(1:28)
c           goto 15
c         endif
c      endif
c      goto 10

20    continue
      return
      end
         
         
         