c--------------------------------------------------------------------------
c  Read a cat file (Noridc2 format) and calculate average station magnitude 
c  residual for all stations in file. Calcualation is by componenet.
c  Amplitude  must use IASP naming like IAML
c
c  jh march 2021
c--------------------------------------------------------------------------c
c
c 
c
c  changes
c
c  mar 10 2023 jh: 2. output of largest residuals
c  jun 21 2023 jh: output of residual distribution, alt lon elev, options
c
      implicit none                       ! force delcaration of all variables
      include 'seidim.inc'                ! dimensions for rea block
      include 'seisan.inc'                ! dimensions for rea block
      include 'rea.inc'                   ! parameter common bliock

      character*80 data(10000)            ! s-file with data in text array

      logical all                         ! true: read all data, false: headers
      integer seiclen                     ! function
      integer code                        ! error return code
      integer nevent                      ! number of events in file
      character*10 keys                   ! next choice key for routine findevin
      character*14 starttime,endtime      ! time interval to read
      character*80 basename               ! data base or file name
      character*80 infile                 ! input file or base name
      character*80 eventfile              ! single event file name
      real res(10000,10000)               ! residuals
      integer nres(10000)                 ! number of residuals for station
      real mz(10000),mn(10000),me(10000)  ! avarage mags for each component
      integer nstat                       ! number of stations
      integer nchan                       ! number of channels
      real slat,slon,elev                 ! syation coordinates and elevation
      real mh(10000)                      ! average magnitude on horizontals
      character*8 stat_com(10000)         ! stations and components
      character*8 stat_com_old            ! previous at write out
      character*5 stat(10000)             ! station codes
      character*1 mag_type                ! type of magnitude
      character*1 comp_use                ! which component to use, Z...
      character*1 accept_blank            ! if blank, accpt blank component
      character*8 amp_type                ! amplitude type for magnitude
      real x(5000)                        ! help variable
      integer mag_dis(21)                 ! mag distriubution
      real av,sd                          ! average mag and sd
      integer status,eventno,newmonth,fstart ! for routine findevin
      real min_av,max_av                  ! residual range to use 
      integer min_nres                    ! min number in av for 2.output
      logical one_stat_only               ! if true, only first station written out
      integer base                        ! type of input 0: seisan data base, 
      character *80 text
                                          !     1: index file, 2: single file
      integer i,k,m                       ! counter
c
c   initialize
c
      do i=1,10000
         do k=1,10000
            res(k,i)=0.0
         enddo
         nres(i)=0
         stat_com(i)=' '
         stat(i)=' '
         mz(i)=-99
         mn(i)=-99
         me(i)=-99
      enddo
      do i=1,21 
         mag_dis(i)=0
      enddo
      nstat=0
      nchan=0
      min_av=-10.0
      max_av=999.0
      min_nres=0
      stat_com_old='######'
      accept_blank=' '
      comp_use=' '
      one_stat_only=.false.
 1    continue

      write(6,*)'Magnitude type: L, C, b, B, s, S or W'
      read(5,'(a)') mag_type   
      write(6,*)'Other selection criteria for 2. output: y/n=enter' 

      read(5,'(a)') text
      if(text.eq.' ') goto 11

      write(6,*) 'Average absolute residual range, enter for all' 
      read(5,'(a)') text
      if(text.eq.' ') then
          min_av=-10.0
          max_av=999.0
      else
          read(text,*) min_av,max_av
      endif

      write(6,*)'Minimum number of residuals for average, enter for all'
      read(5,'(a)') text
      if(text.eq.' ') then
         min_nres=0
      else
         read(text,*) min_nres
      endif
      write(6,*)'Component to use, Z, N or E, if blank use all'
      read(5,'(a1)') comp_use
      write(6,*)'Accept blank component (y=enter/n)'
      read(5,'(a1)') accept_blank 
      write(6,*)'Only list first entry from a station(y/n=enter)'
      read(5,'(a)') text
      if(text(1:1).eq.'y') one_stat_only=.true.


 11   continue
   
      amp_type=' '
      if(mag_type.eq.'L') then
         amp_type='IAML    '
      elseif(mag_type.eq.'C') then
         amp_type='END     '
      elseif(mag_type.eq.'b') then
         amp_type='IAmb    '
      elseif(mag_type.eq.'B') then
         amp_type='IVmB_BB '
      elseif(mag_type.eq.'s') then
         amp_type='IAMs_20 '
      elseif(mag_type.eq.'S') then
         amp_type='IVMs_BB '
      elseif(mag_type.eq.'W') then
         amp_type='SPEC    '
      endif
      if(amp_type.eq.' ') goto 1  ! enter again
   

      call get_seisan_def
c
c   open output file
c
      open(2,file='magres_all.out',status='unknown')
      open(8,file='magres_selected.out',status='unknown')
      open(9,file='magres_histogram.out',status='unknown')
      open(10,file='magres_distance.out',status='unknown')
      open(11,file='magres_largest_z.out',status='unknown')

    
c
c   get input file name, or data base with time interval
c

      write(*,*) ' Event data input - select one:'
      write(*,*)
      WRITE(*,*) 
     *'    SEISAN default data base or                     :',
     *'Enter '
      write(*,*) 
     *'    Alternative data base, give 1-5 letter code or  :'  
      WRITE(*,*) 
     *'    Local index file, name must start with index or :'
      WRITE(*,*) 
     *'    Local data base, write ,, or                    :'
      WRITE(*,*) 
     *'    Filename for one file, min. 6 chars or with a . : '
      
      write(*,*)
      read(*,'(a)') infile
      basename=infile(1:80)
                       
      write(*,*)
C
c   check if this is a single multiple event file (base=2),
c   general data base (base=0) or  a local data base or 
c   index file (base=1)
c

      starttime=' '
      endtime=' '

      keys(1:4)='NEXT'    ! always use next event

c
c   initially assume a file
c
      base=2
c
c   a SEISAN 5 letter data base, blank is default data base, ',,'
c   is a local data base. the name cannot have a '.' and must
c   be less than 6 chars long
c
      if(seiclen(basename).lt.6.and.index(basename,'.').eq.0) then 
         base=0
      endif
c
c   case of index file or local data base
c
 
      if(basename(1:5).eq.'INDEX'.or.basename(1:5).eq.'index'.
     *     or.basename(1:2).eq.',,') then
        base=1
      endif
                              
c
c  get time interval for a seisan data base, no time interval used for
c  index file or local data base
c
      if(base.eq.0) then
         write(*,'('' Start Time           (YYYYMMDDHHMMSS): '',$)')
         read(*,'(a14)') starttime
         write(*,'('' End Time, enter is to end of month:    '',$)')
         read(*,'(a14)') endtime
         write(*,*)
      endif
c
c   open access for the the relevant input option
c


c
c  a file
c
      if(base.eq.2) then
         open(1,file=infile,status='old',err=5)
         goto 6
 5       continue
         write(6,*)' Input file not found'
         stop
 6       continue
      endif


      all=.true.                  ! read all parameters
      nevent=0                    ! initialize counter

c
c-----------------------------------------------------------------
c  Loop to read events start here
c-----------------------------------------------------------------
c

  50  continue

c
c   read from relevant input type
c
      if(base.lt.2) then     ! data base  or index file event
         call findevin
     *   (basename,starttime,endtime,keys,0,eventno,
     *   eventfile,fstart,newmonth,status)
c
c   check if end of time interval or errors, then
c   stop
C     
         if(status.gt.0) then
            write(6,*)' STOP WITH STATUS=',status
            goto 1000  ! stop
         endif
C
C   open data base input single event file
c

         open(1,file=eventfile,status='old',err=7)
         goto 8
 7       continue
         write(6,*)' Input file not found: ',eventfile
         stop
 8       continue
      endif

c
c   read all parameters for one event from file unit 1
c
      call rea_event_in(1,all,data,code)

c
c   close file if from data base since only one event
c
      if(base.lt.2) close(1)
c
c   check if end of file (code=1), if so jump out of loop
c
      if(code.eq.1) goto 1000
c
      nevent=nevent+1               ! count events
c
c   write the whole first header line
c
      write(6,'(a)') data(1)(1:79)

c
c   look for specific mag residuals
c
       do i=1,rea_nphase

          if(rea_phase(i).eq.amp_type.and.rea_mag_res(i).gt.-900) 
     *    then
c
c   convert res to a number between 1 and 21
c
             k=rea_mag_res(i)*10+11
             if(k.ge.1.and.k.lt.22)then
                mag_dis(k)=mag_dis(k)+1
             endif
c
c   residual vs distance
c
             write(10,*) rea_dist(i),rea_mag_res(i)

              do k=1,nchan
                 
                  if(stat_com(k).eq.rea_stat(i)//rea_com(i)) then
c
c   one more for this station and channel
c
                     nres(k)=nres(k)+1
                     res(k,nres(k))=rea_mag_res(i)
                     goto 20
                  endif
              enddo

c  if here, add station comp
c
              nchan=nchan+1
              stat_com(nchan)=rea_stat(i)//rea_com(i)
              nres(k)=nres(k)+1
              res(k,nres(k))=rea_mag_res(i)
           endif
 20        continue
       enddo 

c
c   find number of different stations
c

       do i=1,rea_nphase

          if(rea_phase(i).eq.amp_type.and.rea_mag_res(i).gt.-900) 
     *    then
              do k=1,nstat                
                if(stat(k).eq.rea_stat(i)) goto 25
              enddo
c
c  if here, add station 
c
              nstat=nstat+1
              stat(nstat)=rea_stat(i)
           endif
 25        continue
       enddo                 
                 

c
c   get next event
c
      goto 50
c
c     end of file or data base
c
 1000 continue

       if(nchan.eq.0) then
          write(6,*)' No resdidual data in input file'
          stop
       endif
       write(6,*)

       write(6,'(a,a)') 'Residuals for magnitude type ',mag_type
       write(6,*)
       write(6,'(a)')' STAT COM      N        AV      SD'

       write(2,'(a,a)') 'Residuals for magnitude type ',mag_type
       write(2,*)
       write(2,'(a)')
     *' STAT COM      N        AV      SD     LAT     LON   ELEVA'
       write(8,'(a,a)') 'Residuals for magnitude type ',mag_type
       write(8,*)
       write(8,'(a)')
     *' STAT COM      N        AV      SD     LAT     LON   ELEVA'
                     
       do i=1,nchan
         do k=1,nres(i)
            x(k)=res(i,k)
         enddo

         call sdv(nres(i),x,av,sd)

         if(stat_com(i)(8:8).eq.'Z') then
c
c   find station index that corresponds
c
            do m=1,nstat
              if(stat(m).eq.stat_com(i)(1:5)) then
                 mz(m)=av
              endif
            enddo
         endif


         if(stat_com(i)(8:8).eq.'N') then
c
c   find station index that corresponds
c
            do m=1,nstat
              if(stat(m).eq.stat_com(i)(1:5)) then
                 mn(m)=av
              endif
            enddo
         endif

         if(stat_com(i)(8:8).eq.'E') then
c
c   find station index that corresponds
c
            do m=1,nstat
              if(stat(m).eq.stat_com(i)(1:5)) then
                 me(m)=av
              endif
            enddo
         endif

         call stat_loc(stat_com(i),' ',slat,slon,elev)
c
c  limting parmeters
c
         if(nres(i).lt.min_nres) goto 40    ! number of residuals

         if(abs(av).lt.min_av.or.abs(av).gt.max_av) goto 40  ! residual range

         if(stat_com(i)(8:8).eq.' '.and.accept_blank.ne.' ') go to 40 ! not accept blank

         if(stat_com(i)(8:8).ne.comp_use.and.comp_use.ne.' '.
     *   and.accept_blank.ne.' ') goto 40 ! componenet


         if(stat_com(i)(1:5).eq.stat_com_old(1:5).and.one_stat_only)  
     *   goto 40  ! only one stat, use first entry


c
c   write selected
c

         write(8,'(1x,a8,2x,i5,2x,2f8.2,3f8.3)') stat_com(i),
     *   nres(i),av,sd,slat,slon,elev

 40      continue

c
c   all residuals
c
           
         write(6,'(1x,a8,2x,i5,2x,2f8.2)') stat_com(i),nres(i),av,sd
c
c   all residuals
c

         write(2,'(1x,a8,2x,i5,2x,2f8.2,3f8.3)')stat_com(i),nres(i),av,
     *   sd,slat,slon,elev
         stat_com_old=stat_com(i)
         
       enddo 
       write(6,*) 
c
c  calculate average horizontal vs vertical per station
c
        k=0
        do i=1,nstat
           if(mz(i).gt.-90.0.and.me(i).gt.-90.0.and.me(i).gt.-90.0)
     *     then
              k=k+1
              mh(k)= (mn(i)+me(i))/2.0-mz(i)
              write(6,'(a,a,f7.2)') 
     *       'Average horizontal mag res minus mag res on '//
     *       'Z for station ',
     *       stat(i), mh(k)
          endif
        enddo

c
c  calculate average horizontal vs for all stations
c
       if(k.gt.1) then
          call sdv(k,mh,av,sd)
          write(6,*)
          write(6,'(a,2f6.2)')
     *   'Average and sd of all mag res on horizontal - mag res on Z'
     *   ,av,sd
       endif               
c
      do i=1,21
         write(9,*) (i-11)/10.0, mag_dis(i)
      enddo

      write(6,*)            ! blank line
      close(2)              ! close output file
      write(6,*) 'Number of events in input file or data base', nevent
      write(6,*) 'Number of channels with mag', nchan
      write(6,*) 'Number of stations with mag',nstat
      write(6,*) 'Number of stations with magnitudes on Z,N and E',k
      write(6,*) 'Output file name is magres_all.out for all stations'
      write(6,*) 
     *'Output file name is magres_selected.out for selected stations'
      write(6,*) 
     *'Output of histogram, all residuals, is magres_histogram.out'
      write(6,*) 
     *'Output of residuals vs distance, all data is magres_distance.out'

      stop
      end
