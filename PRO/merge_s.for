c--------------------------------------------------------------------------
c  Merge two  files or a data base and a file
c--------------------------------------------------------------------------
c
c  The main data base or file is all copied out again.
c  Events from the merge file is compared to events in main file
c  and if time difference is less than given, the event is appened. 
c  Duplicate phases are deleted. Duplicate is defined as same phase
c  from same station with a time difference of less than 0.1 s. Component
c  is not checked since some data might not have component. 
c  Both input sources must be in chronological order: Input from a
c  data base  (or using a collect from a data base) might require
c  an update with new ID lines.
c  Input can be old or new format but output is the one set up by the 
c  seisan.def.
c  There is no check for distance id.
c  Event from merge file that does not match in time can be put in main
c  output file or in a separate file.
c
c
c  changes:
c
c   2023 02 20: add output files of events used
c

      implicit none                       ! force delcaration of all variables
      include 'seidim.inc'                ! dimensions for rea block
      include 'seisan.inc'                ! dimensions for rea block
      include 'rea.inc'                   ! main parameter common block
      include 'rea2.inc'                  ! secodary ------------------

      character*80 data(max_data)         ! s-file with data in text array
      character*80 data2(max_data)        ! s-file with data in text array

      logical all                         ! true: read all data, false: headers
      integer seiclen                     ! function
      integer code                        ! error return code
      integer duplicate                   ! how to duplicate
      logical copy_all                    ! if true, copy all to output file
      integer copy_unit                   ! unit for output of events o ffile to merge 
      integer nevent_main                 ! number of events in main file
      integer nevent_merge                ! number of events in merged file
      integer nevent_append               ! number of events appended
      character*100 text
      character*10 keys                   !next choice key for routine findevin
      character*14 starttime,endtime      ! time interval to read
      character*80 basename               ! data base or file name
      character*80 infile                 ! input main file or base name
      character*80 infile_merge           ! file to be merged into main
      character*80 eventfile              ! single event file name
      real time_dif                       ! max time differencd for event to be appended
      real*8 time, time2                  ! abs times of hyp
      real*8 time_old,time2_old           ! ---
      logical end_of_merge_events         ! end of file
      logical not_appended                ! true if event not appened
      logical full_phase_compare          ! if true use whole phase name
      integer status,eventno,newmonth,fstart ! for routine findevin
      integer nstat2,nphas2,nhead2,nrecord2,id2
      integer nstat,nphas,nhead,nrecord,id
      character*1 type,type2,exp,exp2
      integer n_delete                    ! count deleted phases
      integer base                        ! type of input 0: seisan data base, 
                                          !     1: index file, 2: single file
      integer i,j                         ! counters

      call get_seisan_def

      end_of_merge_events=.false.         ! initially not at end
      not_appended=.false.
c
c   open output file
c
      open(3,file='merge_s.out',status='unknown')
      open(4,file='merge_s_not.out',status='unknown')
      open(7,file='merge_s_main_used.out',status='unknown')
      open(8,file='merge_s_merge_used.out',status='unknown')

    
c
c   get input file name, or data base with time interval
c

      write(*,*) ' Main data base or file to merge into - select one:'
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
      
      read(*,'(a)') infile
      basename=infile(1:80)
                       
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
c   open file if a file for main
c
      if(base.eq.2) then
         open(1,file=infile,status='old',err=5)
         goto 6
 5       continue
         write(6,*)' Input file not found'
         stop
 6       continue
      endif

c
c   file name of file to be merged into main
c
      write(6,*)' File to be merged into main'
      read(5,'(a)') infile_merge
c
c   open merge file
c
      open(2,file=infile_merge,status='old',err=9)
      goto 10
 9    continue
      write(6,*)' Input file not found'
      stop
 10   continue

      write(6,*)
     *' Max time difference for event to be appended, def(enter)=50s'
      read(5,'(a)') text
      if(text.eq.' ') then
         time_dif=50.0
      else
         read(text,*) time_dif
      endif

 20   continue
      write(6,*)     ' Delete duplicate phases:'//
     *' 0=no, 1: from merge file (default enter), 2:from main file '
      read(5,'(a)') text
      if(text.eq.' ') then
         duplicate=1
      else
         read(text,*) duplicate
      endif
      if(duplicate.ne.0.and.duplicate.ne.1.and.duplicate.ne.2) goto 20

      write(6,*)' Put all events from file'//
     *' to merge in output file y,n=default (enter)'
      copy_all=.false.
      read(5,'(a)') text
      copy_unit=4
      if(text(1:1).eq.'y')then
         copy_all=.true.
         copy_unit=3
      endif

      write(6,*)
     *' Compare whole phase name (enter) or only first letter (f)'
      read(5,'(a)') text
      if(text.eq.' ') then
         full_phase_compare=.true.
      else
         full_phase_compare=.false.
      endif


      all=.true.                  ! read all parameters
      nevent_main=0               ! initialize counter
      nevent_merge=0
      nevent_append=0
      n_delete=0
      time2=0.0                   ! force reading of first event
                                  ! in compare loop below
      time_old=0.0
      time2_old=0.0


c---------------------------------------------------
c  loop for reading main events
c---------------------------------------------------

  50  continue

c
c   read main event from relevant input type
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
c   read  one event from main file unit 1
c
      rea_nhead=-1    ! only fill data
      call indata(1,nstat,nphas,nhead,nrecord,type,exp,data,id)

c
c   close file if from data base since only one event
c
      if(base.lt.2) close(1)
c
c   check if end of file (nrecord=0), if so jump out of loop
c
      if(nrecord.eq.0) goto 1000
c
      nevent_main=nevent_main+1               ! count main events
c
c   check if in chrono order, 5s slack accepted
c     
      call abs_time_from_hyp(data(1),time)
      if(dabs(time-time_old).gt.5.0.and.time.lt.time_old) then
         write(6,'(a,a)')'Main event not in chronolocal order ',
     *   data(1)(1:22)
         stop
      endif
      time_old=time
c
c   compare times
c


 60   continue
      write(text,'(a,a,a,a,a,f14.1)')'Main: ',data(1)(1:22),
     * '  Merge: ',data2(1)(1:22),' dif= ', (time-time2)
      if(dabs(time-time2).le.time_dif) text(81:90)='*Appended*'
      if(time2.gt.0.0) write(6,'(a)') text(1:90)    ! first time no comparison done
      if(time2-time.gt.time_dif.or.end_of_merge_events) then
c
c   main event too early, write out main and advance to next event
c   in main
c
         rea_nrecord=nrecord
         rea_nhead=nhead
         code=-1
         call rea_event_in(0,all,data,code)
         call rea_event_out(3,all,data,code)
         goto 50   ! next event in main
      endif
c
c   check if merge event file has to be advanced
c
      if(time-time2.gt.time_dif) then
c
c   if previous event was not appended, write it out
c
         if(not_appended) then
            rea_nrecord=nrecord2
            rea_nhead=nhead2
            code=-1            ! determine format
            call rea_event_in(0,all,data2,code)
            call rea_event_out(copy_unit,all,data2,code)
         endif

         nhead2=-1               ! only fill data
         call indata(2,nstat2,nphas2,nhead2,nrecord2,
     *   type2,exp2,data2,id2)

         if(nrecord2.eq.0)  then
             end_of_merge_events=.true.
             goto 60
         endif
         not_appended=.true.
c
c   check chrono order, 5s slack accepted
c
         call abs_time_from_hyp(data2(1),time2)
         if(time2.lt.time2_old.and.dabs(time-time_old).gt.5.0) then
            write(6,'(a,a)')'Merge event not in chronological order ',
     *      data2(1)(1:22)
            stop
         endif
         time2_old=time2
         nevent_merge=nevent_merge+1
         goto 60    ! compare again
      endif
c
c   check if events are close in time, then append
c
      if(dabs(time-time2).le.time_dif) then
         nevent_append=nevent_append+1
c
c   read both to memory, first event to append
c
         rea_nrecord=nrecord2
         rea_nhead=nhead2
         code=-1
         call rea_event_in(0,all,data2,code) 
c
c   write to sync file
c
          call rea_event_out(8,all,data2,code)        
c
c   copy to rea2
c
         call rea_copy_rea_to_rea2
c
c   put main event in memory
c
         rea_nrecord=nrecord
         rea_nhead=nhead
         code=-1  
         call rea_event_in(0,all,data,code)

c
c   write to sync file
c
          call rea_event_out(7,all,data,code)    
c
c   delete duplicate phases in rea2
c
         if(duplicate.gt.0) then
            do i=1,rea2_nphase
               do j=1,rea_nphase
                 if(dabs(rea_abs_time(j)-rea2_abs_time(i)).lt.0.1.and.
     *           rea_stat(j).eq.rea2_stat(i)) then
                    if(rea_phase(j)(1:4).eq.'SPEC'.
     *              or.rea2_phase(i)(1:4).eq.'SPEC') then
c
c   spectral phase, use full phase anme and component
c
                       if(rea_com(j).eq.rea2_com(i).and.rea_phase(j).
     *                 eq.rea2_phase(i)) then
                          if(duplicate.eq.1) then
                             rea2_phase(i)(1:6)='DELETE'
                          else
                             rea_phase(j)(1:6)='DELETE'
                          endif
                       endif
                       n_delete=n_delete+1   
           
                    else
c
c   normal phase
c

                       if((full_phase_compare.and.rea_phase(j).
     *                 eq.rea2_phase(i)).or.
     *                 (.not.full_phase_compare.and.
     *                 rea_phase(j)(1:1).eq.rea2_phase(i)(1:1)) ) then
                          if(duplicate.eq.1) then
                             rea2_phase(i)(1:6)='DELETE'
                          else
                             rea_phase(j)(1:6)='DELETE'
                          endif
                      endif
                      n_delete=n_delete+1
                    endif
                  endif
               enddo
            enddo
         endif

c
c   merge
c   
         call rea_merge_rea2_to_rea

c
c   write out merged event
c
         call rea_event_out(3,all,data,code)  
         not_appended=.false.
         goto 50  ! get next event from  main
      endif


c
c     end of file or data base
c
 1000 continue

c
c   write out rest of merge file in any left
c
      if(.not.end_of_merge_events) then
         call rea_event_in(2,all,data2,code)
         if(code.eq.1) goto 1001  
         call rea_event_out(copy_unit,all,data2,code)
         nevent_merge=nevent_merge+1
         goto 1000
      endif
 1001 continue
 
c
      write(6,*)            ! blank line
      close(2)              ! close output file
      write(6,*) 'Number of events in main input file or data base',
     * nevent_main
      write(6,*) 'Number of events in merge input file            ',
     * nevent_merge
      write(6,*) 'Number of events appended                       ',
     * nevent_append
      write(6,*) 'Number of deleted phases                        ',
     * n_delete

      write(6,*) 'Output file name is merge_s.out'
      if(copy_all) then
         close(4,status='delete')  ! file not use so delete
      else
         write(6,*)  
     *   'Output file of not appended events is merge_s_not.out'
      endif
      write(6,*)
     *'Output file of events used from main is merge_s_main_used.out'
      write(6,*)
     *'Output file of events used from merge is merge_s_merge_used.out'
      stop
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine abs_hyp(time)
c
c   abs time of hyp for rea
c
      implicit none
      include 'seidim.inc'
      include 'rea.inc'
      real*8 time
      call timsec(hyp_year(1),hyp_month(1),hyp_day(1),
     *hyp_hour(1),hyp_min(1),hyp_sec(1),time)
      return
      end

      subroutine abs_hyp2(time)
c
c   abs time of hyp for rea2
c
      implicit none
      include 'seidim.inc'
      include 'rea2.inc'
      real*8 time
      call timsec(hyp2_year(1),hyp2_month(1),hyp2_day(1),hyp2_hour(1),
     *hyp2_min(1),hyp2_sec(1),time)
      return
      end

      subroutine abs_time_from_hyp(text, time)
c
c   get abs time time from a type one line in text
c
      implicit none
      character*80 text
      integer year,month,day,hour,min
      real sec
      real*8 time
      read(text,'(1x,i4,1x,2i2,1x,2i2,f5.1)') 
     *year,month,day,hour,min,sec
      call timsec(year,month,day,hour,min,sec,time)
      return
      end
