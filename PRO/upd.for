C
C   program to update data base with existing solutions from s-files
c   without relocation, just copy. the program can optinally also update
c   ID and s-file names to events are in chronological ordwer
c
c   updates:
c
c   april 14, 1999 jh : -------- version 7.0 check ---------------------
c                       many changes
c   aug   24, 1999 lo : init logfile and epifile
c   nov 2 1999   jh   : recompiled to include new findevin and indata
c   nov 25 2001  jh   : def base (agecy_dir) was still 3 chars, changed to 5
c   jan 4 2010   jh   : bug with initializing first_event
c   mar 14 2019  jh   : new format, operator to 3 chars
c   jun 8  2020  jh   : fix id and file name for chrono order
c
      implicit none
      include 'seidim.inc'
      include 'seisan.inc'
      include 'libsei.inc'
      include 'rea.inc'
c---name of top directory
      character*60 top_directory
c---one event in nordic format, one line of data  
c--- event type, indcator of explosion, indicator of fixing depth  
      character*1 typ,exp
c---directory separator character
      character*1 dchar
      character*1 answer
      character*80 sfiles(max_index$)       ! save s-files names
c---number of header lines, records, stations in nordic file, help variables
      integer nhead,nrecord,nstat,nphase,i,itp
c---id line indicator
      integer id
c---logfile name when updating
      character*80 logfile
c---flag for indicatin if first month  when updating
      integer first_month
c---number of events in one month in data base
      integer nout
c---line in log file
      character*80 logrecord
      character*80 data(max_data)   ! s-file
c---time of making update
      character*12 p_time
      character*1 id_sync       ! if y syncronize id and o time
      character*14 proc_time
c---operator id
      character*4 operator
c---data base code
      CHARACTER*5 TBASE
c---first and last event used when updating
      character*12 first_event,last_event
C
C  next for data base operation
C
      character*10 keys                      !next choice key
      character*14 starttime,endtime         ! time interval for location
      character*40 basename                  ! data base or file name
      character*80 eventfile                 ! single event file name
      character*5  agency_dir                ! agency used for def. base
      integer status,eventno,newmonth,fstart ! 
c---name of epi file in data base
      CHARACTER*80 EPIFILE
c logical for end of file
       logical          b_eof
c logical for existing file or not
       logical          b_old
c returned code
       integer          code
c read unit #1 
       integer          read02
c sei clen
      integer seiclen
      integer k

c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c                                                           
c   get computer specifics
c
      call dir_char(dchar)         ! dirctory delimiter character
      call get_def_base(agency_dir)
      call get_seisan_def
      epifile = ' '

c 
c   get directory structure
c
      call topdir(top_directory)
      itp=index(top_directory,' ')-1
c
c   input data base name and time interval
c
      write(6,*)'Give 1-5 letter data base name, return for default'
      read(*,'(a40)') basename                       
      tbase=basename(1:5)
      IF(TBASE.EQ.'     ') TBASE(1:5)= agency_dir
      WRITE(6,*)'OPERATOR ID (max 3 char):'
      READ(5,'(A4)') OPERATOR
      operator(4:4)=' '
      IF(OPERATOR.EQ.'    ') STOP

      write(6,*)'Keep sfile name and ID (default=enter) or'
      write(6,*)'synchronize sfile name and ID with origin time (s)'
      read(5,'(a)') answer
      id_sync='n'
      if(answer.eq.'s')then
         write(6,*)
     *  'ID and S-file names will now be updated with the origin time',
     *  ', confirm=y'
         read(5,'(a)') answer
         if(answer.ne.'y') stop
         id_sync='y'
      endif

      WRITE(6,'(a,$)')
     *' START TIME(Year-Month)                          : '
      READ(5,'(A14)') STARTTIME
      IF(STARTTIME(7:7).NE.' ') THEN
         WRITE(*,*) ' YOU MUST START WITH BEGINNING OF MONTH'
         WRITE(*,*) ' THIS HAS NOW BEEN CORRECTED'
         STARTTIME(7:14)='        '
      ENDIF
      WRITE(6,'(a,$)')
     *' END TIME(Year-Month), RETURN IS TO END OF MONTH : '
      READ(5,'(A14)') ENDTIME
      IF(ENDTIME(7:7).NE.' ') THEN
         WRITE(*,*)' YOU MUST END WITH END OF MONTH'
         WRITE(*,*)' THIS HAS NOW BEEN CORRECTED'
         ENDTIME(7:14)='        '
      ENDIF


c
c   make shure base is 5 chars
c
      if(tbase(1:1).ne.',') then
         do i=2,5
           if(tbase(i:i).eq.' ') tbase(i:i)='_'
        enddo
      endif

      keys(1:4)='NEXT'    ! start with next event
      nout=0             

c
c---------------------------------------------------------------------
c  event loop starts here, always come back here after processing one event
c---------------------------------------------------------------------
c
 50   continue
c
C----------------------------------------------------------------------
C    next event 
C----------------------------------------------------------------------
C
C
C  GET FILENAME OF NEXT EVENT, save previous if any
c  only save if at least one event has been read
C
      if(nout.gt.0) last_event=eventfile(fstart:fstart+11) ! for log file

      call findevin
     *(basename,starttime,endtime,keys,0,eventno,
     *eventfile,fstart,newmonth,status)

C----------------------------------------------------------------------
C  CHECK FOR OUTPUT IN CAT DATA BASE IF NEW MONTH
c  or end of time period=end of month
c  also make first cat file name here
C----------------------------------------------------------------------

      IF((NEWMONTH.EQ.1.or.status.eq.3)) THEN
c
c  only rewrite if more then one event, will be zero first time

         if(nout.gt.0) then     
c
c   sort file names, might be different from before
c
            call sortfi(sfiles,nout)
c
c   read all s-files and make cat file in chrono order 
c
            write(6,*)
            write(6,*)' Put CAT file in chrono order'
            write(6,*)


            do k=1,nout
               if(new_nordic_format) rea_old_format_required=.false.  ! since no processing  
               call sei open(old$+warn$,            ! Open 
     &                  ' ',                   ! Prompt file name (n/a).
     &                  sfiles(k),             ! File name
     &                  read02,                ! unit #
     &                  b_old,                 ! Already exists? (n/a).
     &                  code)                  ! Returned condition.

               call indata(read02,nstat,nphase,nhead,
     *         nrecord,typ,exp,data,id)
               write(31,'(a80)')(data(i),i=1,nrecord)
               call sei code(fort$,code,read02,b_eof)
               call sei close (close$,read02,code) ! close
            enddo

            if(new_nordic_format) rea_old_format_required=.true.  ! since now processing if new month
C
C  CLOSE CAT BASE FILE 
C
            CLOSE(31)  
          endif   ! nout=0
c
c   if not at end of time period, open first/next epi/cat file
c
          if(status.ne.3) then
C
C  CLOSE PREVIOUS CAT BASE FILE IF ANY
C
            CLOSE(31,ERR=3333)
 3333       CONTINUE
C
C  YEAR-MONTH
C
            IF(DATA(1)(2:3).EQ.'  ') DATA(1)(2:3)='19'
            EPIFILE(1:4)=eventfile(fstart-8:fstart-5)
            EPIFILE(5:6)=EVENTFILE(FSTART-3:FSTART-2)
            EPIFILE(7:10)= '.CAT'
            WRITE(6,'(1x,a,a10)') 'CATFILE IS ',EPIFILE(1:10)
C
C  OPEN NEW  cat file
C
            OPEN(UNIT=31,FILE=top_directory(1:itp)//
     &      dchar//'REA'//dchar//tbase//dchar//
     &      'CAT'//dchar//EPIFILE(1:10),STATUS='unknown')
            nout=0         
         endif
      endif         ! end of action for new month
c
c----------------------------------------------------------
c  section for reading and writng files first time
c-----------------------------------------------------------  
c
c   check if  errors or end of time interval, then stop
C     
         if(status.gt.0) then
            write(6,*)' STOP WITH STATUS=',status
            stop   ! stop
         endif
C
C   open data base input single event file and read it
c   set flag for single event nrecord= -1
c
         nrecord=-1
         open(21,file=eventfile,status='old')
         call indata(21,nstat,nphase,nhead,nrecord,typ,exp,data,id)
         nout=nout+1
         sfiles(nout)=eventfile

C
c   output header line on screen
C
         write(6,2838) eventno,data(1)(1:71)
 2838    format(1x,'#',i5,1x,a71)

         IF(ID.NE.0) THEN
c
c   check that id line has not changed since first read
c   could happen when adding a new error line
c
            do i=1,nhead+1   
              if(data(i)(80:80).eq.'I') id=i
            enddo
          endif
c
c   If requested, rewrite s-file with name according to origin time, at same time
c   check header line and correct id line, else continue
c
c
         if(id_sync.eq.'y') then 
c
c   get system time
c
            call systime(p_time,proc_time)
            call rewrite_sfile
     *      (21,sfiles(nout),tbase,proc_time,operator,data,'US ')
         else 
            sfiles(nout)=eventfile    ! it was not changed
         endif
         close(21)
c
c   get next event
c
         keys='NEXT'
         go to 50                                        
C
c   this was the end
C
      stop
      end 
                                                              
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
