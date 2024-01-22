c--------------------------------------------------------------------------------
c program to use with EEV. It merges an event into current eev eventd from a given 
c data base. header or whole event can be merged and duplicate phases
c can be removed.
c the arguments are in order 
c
c base name for event to merged in
c start time for interval to look for events in basename
c end time ---------------------------------------------
c fileneme of current event in eev
c
c the duplicate phases are optionally removed from event to be merged in if
c same station, component, phase and time within 0.1 s. there is no check
c if same location and network, simple to put in.
c
c if inserved as heade rline, the event type is changed to same type as old
c header line so id and file name remains the same 
c
c jh feb 2021
c
c---------------------------------------------------------------------------------

c
c  changes
c

c
      implicit none                       ! force delcaration of all variables
      include 'seidim.inc'                ! dimensions for rea block
      include 'seisan.inc'                ! dimensions for rea block
      include 'rea.inc'                   ! parameter common block
      include 'rea2.inc'                  ! secon rea block

      character*80 data(max_data)         ! s-file with data in text array
      character*80 datah(1000)            ! all s-files headers in interval
      character*80 data_eev               ! header from eev event
      character*80 sfiles(1000)           ! corresponding s-files
      character*80 eev_sfile              ! sfile from eev
      character*80 text

      logical all                         ! true: read all data, false: headers
      integer code                        ! error return code
      character*10 keys                   ! next choice key for routine findevin
      character*14 starttime,endtime      ! time interval to read
      character*80 basename               ! data base 
      character*80 infile                 ! input file or base name
      character*80 eventfile              ! single event file name
      integer status,eventno,newmonth,fstart ! for routine findevin
      integer year,doy,month,day,hour,min,isec  ! any event
      integer year_e,doy_e,month_e,day_e,hour_e,min_e! eev event
      real lat_e,lon_e
      real sec,sec_e
      integer nars                        ! number of arguments
      character*80 args(10)               ! arguments
      real*8 eev_time                     ! abs time of event from eev
      real*8 base_time(1000)              ! ---------------- database
      real dist(1000)                     ! distance eev event to base event
      real baz,azi                        ! not used
      real window                         ! seconds of window to search in data base
      real*8 t1,t2                        ! start and end time ---------------------
      integer action                      ! action to do with file to insert
      integer nevent                      ! number of events in window
      integer i,k,j                       ! counters

      call get_seisan_def

      window=2400   ! default +- 20 min
      text=' '
c
c   get arguments
c
      call get_arguments(nars,args)

      if(nars.ne.2) then
         write(6,*)' Wrong number of arguments for xxx'
      endif

      basename=args(1)
      eev_sfile=args(2)

      all=.true.                  ! read all parameters

c
c   get time of s-file from eev
c
      open(1,file=eev_sfile,status='old',err=5)
      goto 6
 5    continue
      write(6,*)'Error opening file: ',eventfile
      stop
 6    continue
      call rea_event_in(1,.true.,data,code)
      close(1)
c
c   save data for reference event for later comparison
c
      year_e=hyp_year(1)
      month_e=hyp_month(1)
      day_e=hyp_day(1)
      hour_e=hyp_hour(1)
      min_e=hyp_min(1)
      sec_e=hyp_sec(1)
      lat_e=hyp_lat(1)
      lon_e=hyp_lon(1)
      data_eev=data(1)
     
c
c   back here to increase or decrease window
c

 7    continue
      write(6,'(a,i4,a)')' Window is', int(window/60.0),' minutes'
      if(window.lt.100.0) then
        write(6,*)'Window cannot be smaller, increased to 10 min'
        window=10*60.0
      else
        if(text(1:1).eq.'+') window=window*2.0
        if(text(1:1).eq.'-') window=window/2.0
      endif

c
c   calculate time range 
c
      call timsec(year_e,month_e,
     *day_e,hour_e,min_e,sec_e,eev_time) 
      t1=eev_time-window/2.0
      t2=t1+window
      call sectim(t1,year,doy,month,day,hour,min,sec)
      isec=sec
      write(starttime,'(i4,5i2)')year,month,day,hour,min,isec
      call sectim(t2,year,doy,month,day,hour,min,sec)
      isec=sec
      write(endtime,'(i4,5i2)')year,month,day,hour,min,isec
      do i=1,14
        if(starttime(i:i).eq.' ')starttime(i:i)='0'
        if(endtime(i:i).eq.' ')endtime(i:i)='0'
      enddo

c      write(6,*)'Start and end time ',starttime,' ',endtime

      keys='NEXT'
      nevent=0
      do i=1,1000
         call findevin
     *   (basename,starttime,endtime,keys,0,eventno,
     *   eventfile,fstart,newmonth,status)
c
c   end of time interval
c
         if(status.eq.3) goto 200
         sfiles(i)=eventfile
         nevent=nevent+1
         open(1,file=eventfile,status='old',err=10)
         goto 11
 10      continue
         write(6,*)'Error opening file: ',eventfile
         stop
 11      continue

         call rea_event_in(1,.true.,data,code)
c
c   save header
c
         datah(i)=data(1)
c
c  abs time of base event
c
         call timsec(hyp_year(1),hyp_month(1),
     *   hyp_day(1),hyp_hour(1),hyp_min(1),hyp_sec(1),base_time(i))
c
c   distance to reference eev event
c        
         if(hyp_lat(1).gt.-900.0.and.hyp_lon(1).gt.-900.0.
     *   and.lat_e.gt.-900.0.and.lon_e.gt.-900.0) then
            call distaz(hyp_lat(1),hyp_lon(1),lat_e,
     *      lon_e,dist(i),azi,baz)
            dist(i)=dist(i)*111.2
         else
            dist(i)=-1.0
         endif
         close(1)
      enddo

 200  continue

      if(nevent.eq.0) then
         write(6,*)
     *  'No events in window, increase window(+), enter to stop'
c         call readonekey(text(1:1))
         read(5,'(a)') text
         if(text(1:1).eq.'+') then
           goto 7
         else
           stop
         endif
      endif
c
c   display events, eev event at correct place in list
c
      write(6,'(a)')'------DT--DDIS-----------------------------'//
     *'--------------------------------------------------'
      do i=1,nevent
         k=(base_time(i)-eev_time)/60.0  ! difference in min
         if(base_time(i).lt.eev_time) then
           if(dist(i).ge.0.0) then
              write(6,'(i3,i5,i6,a)')i,k,int(dist(i)),datah(i)(1:79)
           else
             write(6,'(i3,i5,6x,a)')i,k,datah(i)(1:79) 
           endif            
         else
           continue
         endif
      enddo
      write(6,'(a,9x,a)')' EEV ',data_eev(1:79)
      do i=1,nevent
         k=(base_time(i)-eev_time)/60.0
         if(base_time(i).gt.eev_time) then
           k=(base_time(i)-eev_time)/60.0
           if(dist(i).ge.0.0) then
              write(6,'(i3,i5,i6,a)') i,k,int(dist(i)),datah(i)(1:79)
           else
             write(6,'(i3,i5,6x,a)')i,k,datah(i)(1:79) 
           endif
         else
           continue
         endif
      enddo 
      write(6,'(a)')'------DT--DDIS-----------------------------'//
     *'--------------------------------------------------'      
      
 50   continue
      write(6,*) 
     *'Chose one event number to include or'//
     *' increase(+) or decrease(-) window, enter to stop'
      text=' '
      read(5,'(a)') text
c      call readonekey(text(1:1))
      if(text(1:1).eq.'+'.or.text(1:1).eq.'-') goto 7
      if(text(1:1).eq.' ') stop
c      call readonekey(text(2:2))
c      if(ichar(text(2:2)).eq.13) then   ! CR, 1 digit number
         read(text,*,err=50 ) k
c         goto 190
c      else
c         call readonekey(text(3:3))        ! next digit
c         if(ichar(text(3:3)).eq.13) then   ! 2 digit numberc
c            read(text,*,err=50) k ! 2 digit number
c         endif
c      endif 
c 190  continue        
      if(k.lt.1.or.k.gt.nevent) then
         write(6,*)'Wrong number'
         goto 50
      endif

 201  continue
      write(6,*)
      write(6,*)'Following event is selected:'
      write(6,*) '   ',datah(k)(1:79)
      write(6,*)
      write(6,*)'Options: '
      write(6,*)'Do nothing, return to EEV,                       Enter'
      write(6,*)'Put in header line as #1 line:                       1'
      write(6,*)'Put in header line as #2 line:                       2'
      write(6,*)'Put in whole file:                                   3'
      write(6,*)'Put in whole file, delete duplicate inserted phases: 4'
      write(6,*)'Put in whole file, weight out inserted phases:       5'
      write(6,*)
      read(5,'(a)') text
      if(text.eq.' ') stop
      read(text,*,err=201) action
         
      if(action.lt.1.or.action.gt.5) then
         write(6,*)'Wrong choice'
         goto 201
      endif

      if(action.eq.1.or.action.eq.2) then
         open(1,file=eev_sfile,status='old',err=20)
         goto 21
 20      continue
         write(6,*)'Error opening file: ',eev_sfile
         stop
 21      continue
         call rea_event_in(1,.true.,data,code)
         rewind 1
c
c  put as first header line
c
         if(action.eq.1) then
c
c   change event type to what it was before so id and file do no have
c   to be changed
c
            datah(k)(22:22)=data(1)(22:22) 
c
            write(1,'(a)') datah(k)
            do i=1,rea_nrecord
               write(1,'(a)')data(i)
            enddo
         endif
c
c  put as 2. header line
c
         if(action.eq.2) then
            write(1,'(a)') data(1)
            write(1,'(a)') datah(k)
            do i=2,rea_nrecord
               write(1,'(a)')data(i)
            enddo
         endif
      endif
         
c
c   put in whole file, first read file to put in
c
      if(action.eq.3.or.action.eq.4.or.action.eq.5) then
         open(1,file=sfiles(k),status='old',err=30)
         goto 31
 30      continue
         write(6,*)'Error opening file: ',sfiles(k)
         stop
 31      continue
         call rea_event_in(1,.true.,data,code)
         close(1)
c
c   copy data to rea2
c
         call rea_copy_rea_to_rea2
c
c   read the file from eev
c
         open(1,file=eev_sfile,status='old',err=40)
         goto 41
 40      continue
         write(6,*)'Error opening file: ',eev_sfile
         stop
 41      continue
         call rea_event_in(1,.true.,data,code)
         rewind 1
c
c   check if any duplicate phases based on time, phase, station and componenet
c
         if(action.eq.4) then
            do i=1,rea2_nphase
               do j=1,rea_nphase
                 if(dabs(rea_abs_time(j)-rea2_abs_time(i)).lt.0.1.and.
     *           rea_stat(j).eq.rea2_stat(i).and.
     *           rea_com(j).eq.rea2_com(i).and.rea_phase(j).
     *           eq.rea2_phase(i)) then
                    rea2_phase(i)(1:6)='DELETE'   ! delete in rea2
                    write(6,'(a,1x,a,1x,a,1x,a)') 'Duplicate phase: ',
     *              rea_stat(i),rea_com(i),rea_phase(i)
                 endif
              enddo
            enddo
         endif 
c
c   check if weight out inserted phases
c
         if(action.eq.5) then
            do i=1,rea2_nphase
               rea2_weight_in(i)='4'
            enddo
         endif       
c
c   merge rea2 with rea, also if not belonging together, rea
c   data has the main header
c   
         call rea_merge_rea2_to_rea 
c
c   write out modified event, the phases indicted by DELETE are not 
c   written
c
         write(6,*)'Save modified event(y/n=enter)'
         read(5,'(a)')text
         if(text(1:1).ne.'y') then
             write(6,*)'Event not saved'
             stop
         endif
         call rea_event_out(1,all,data,code)
      endif
c         

      stop
      end
