
C########################################################################### 
   
      SUBROUTINE INDATA
     *(FILE,NSTAT,NPHAS,NHEAD,NRECORD,TYPE,EXP,DATA,ID)
C
C   ROUTINE READS ONE EVENT IN NORDIC FORMAT INTO TEXT
C   ARRAY DATA. FILE IS OPENED OUTSIDE ROUTINE  AND IT IS
C   ASSUMED THAT NEXT RECORD TO READ IS A HEADER.
C
c   FIRST VERSION FROM AROUND 1984
c
c   this verison also reads the new format
c
c   rea.inc variables affecting the reading:
c
c   new_nordic_format true:    New format always used for output. If 
c                              new_nordic_format_only is false, input can be
c                              old or new, if true, only new format can be 
c                              used as input.
c
c   new_nordic_format false:   Only old format accepted as inut and output.
c
c   new_nordic_format_only:    If true, only new nordic can be used as input 
c   
c   rea_old_format_required:   If true and new_nordic_format true, old format
c                              is returned in data. used for programs like hyp.
c
c   variable format:           new, old or ukn.The actual format of the phase lines.  
c                              This is compared to the required format and if not
c                              the same, reading stops.
c-----------------------------------------------------------------------------------
c
c   check of file:
c
c   if a blank line in phases and flag set (nrecord=-1 on input 
c                 setting read_to_end_of_file=.true.), blank lines 
c                 will be removed and all
c                 phases read. Several blank lines can be handled, also if
c                 following each other. DO NOT USE IN MULTI EVENT FILE.
c
c   if a blank line in headers or a line shorter than 80 with no line type
c                 it will be terminated by line type 3. Blank lines in headers
c                 will not be converted if read_to_end_of_file=.false. since then
c                 an event without phases will not be recognized.
c
c   if a header line is longer then 80 chars (can caused by 2 char characters),
c                 characters above 127 are replaced by '?' and the line type
c                 be set to the last character of the original line.
c
c   null chars are replaced by blanks in rea routines.
c
c   a line with ctl z is skipped.
C
c   if a line type is wrong in the headers, the line will be removed 
c                 when locating. no check in indata except for the first line. 
c   
c    the corrections will be done internally when reading so an s-file is only
c    corrected when rewritten like with updata in hyp or picking phases in mulplt
C
c   updates:
c   1990-01-30, R.N.ARVIDSSON TO FIT PC-DOS.
c   apr 4 91 by j.h. : GIVE OUT NUMBER OF PHASES AND STATIONS
C   NOV 23 91        : CHANGE TO NEW NORDIC FORMAT, SOME CLEAN UP
c   jan 16 92        : do not count phasses and stations weighted out
C   AUG 28 92        : MAKE SURE LAST LINE IS 80 CHARS BLANK
c   jul    93 by jh  : version 3.0
c   nov 12 93        : count number of stations taking into account that
c                      stations might not follow each other in file
C!JAB(BGS)Nov94      : install error handling.
c   feb 7     jh     : fix up blank line handling a bit, no message,
c                      check for ctl z in file on PC
c   dec 14, 95       : check for array dimension in data
c   dec 7   97 jh    : count phases and stations correctly in case of weight
c                      4 and 9
c   sep 98, jh       : ------ version 7.0 check: year 2000, 5 char station
c   october 29,99 jh : nhead not defined if only header lines and no blank 
c                      line at end
c   january 17 11 jh ; also stop if reading errror on first header line,
c                      with gfortran on pc, if already at end, a new read
c                      would create an error, not eof
c                      if eof while reading headers only, number of headers
c                      were wrongly counted
c   nov 16 2018   jh : implement new format
c   nov 22, 2019  jh : put back in chcek for blank lines and a lot more checking
c   may 22, 2020  jh : if nhead=-1, do not put data into rea, fix bug in 
c                      check_if_phase_line_format
c   sep 14, 2020  jh : number of stations was not counted correctly in some rare cases
c   oct 16, 2020  jh : mostly changes in comments
c   nov 10, 2020  jh : bug using type 7 line for old format, delete blank type 1 lines
c                      move save in data5 for output in old format,
c                      problem with type 4 line,
c   mar 16  2021  jh : change some warning output
c   apr  1  2022  fh : allow "A" as valid phase hint for new Nordic format
c   jan 09  2023  jh : update variabel id after input_rea_all
c|
c
C   INPUT :   FILE: FILE NUMBER TO READ FROM
c             nrecord: if -1, always read to end of file, do no tuse for cat files
C
C   OUTPUT:   NSTAT:   NUMBER OF STATIONS
C             NPHAS:   NUMBER OF PHASES FOR EVENT
C             NHEAD:   NUMBER OF HEADER RECORDS
c                      if nhead on input is -1, only read to data, do not
c                      put in rea
C             NRECORD: TOTAL NUMBER OF RECORDS FOR EVENT INCLUDING LAST
C                      BLANK, IF ZERO OUT, END OF FILE IS HIT, IF
C                      -1 in, read to end of file
C             TYPE:    EVENT TYPE (D,R OR L)
C             EXP:     E FOR EXPLOSION
C             DATA:    EVENT RECORDS
C             ID:      RECORD NUMBER FOR ID LINE, 0: NO ID LINE
C
C    NOTE*** nphas and nstat are not counted for weighted out phases
c
      IMPLICIT NONE
      include 'seidim.inc'
      include 'seisan.inc'
C---TYPE OF LINE
      CHARACTER*1 LINE_TYPE  
C---SEE ABOVE  
      CHARACTER*1 TYPE,EXP
C---STATION CODES 
      CHARACTER*5 OLDSTA,NEWSTA
C---DATA AND A BLANK LINE 
      CHARACTER*80 DATA(*),BLANK
c--- a long line
      character*160 long_line
C---LOGICAL FILE NUMBER FOR READ 
      INTEGER FILE
C---SEE ABOVE  
      INTEGER NPHAS,NRECORD,NHEAD,NSTAT,ID
c-- endof line read or not
      logical read_to_end_of_file,test
      logical all              ! true if all parmerts read and written
      logical read_data_only   ! true if only filling data
C---HELP VARIABLE
      INTEGER IERR,i,i1,i2,i3,k,seiclen
      real x,x1                 ! help variabels      
C
C    Seisan library inserts and routines...
C    ======================================
C
       include  'libsei.inc'               ! Library definitions & data defns.
       include  'rea.inc'
       external  sei code                  ! Error encoder.
       integer   code                      ! local condition
       logical   b_eof                     ! Flag end of file.
C
C    ============= end of list ==========

      logical      rea_new_out_org            ! if true, new format out, org. value
      logical      format_found               ! format determined if true
      logical      phase_ok,ph_ok             ! true if iaspei phase, not amp
      logical      blank_line_message         ! if true, give message
      logical      blank_line                 ! blank line found in phases      
      logical      phase                      ! true if a phase line
      character*3  format                     ! new, old or ukn, from phase line
      character*3  format7                    ! format from type 7 line

c
c   set format and check if input format should be determined
c
      if(new_nordic_format) then
         if(new_nordic_format_only) then
            format_found=.true. ! do not look for format, input must be new
            rea_new_in=.true.   ! old gives error
         else
            format_found=.false.  ! input format has to be found, can be old or new
         endif
         rea_new_out=.true.    ! output must be new
      else
         rea_new_in=.false.    ! format must be old, new gives error
         rea_new_out=.false.   ! output must be old
         format_found=.true.   ! do not look for format
      endif

c
c
c   check if read should be done to end of file, this is
c   an option used if it is known that only one event is available
c   and can be used to avoid problems if an extra blank line is present
c   in phase data. can be set with nrecord for each call or globally
c   with rea_read_to_sfile_end.
c

      blank_line_message=.false.
      blank_line=.false.
      read_to_end_of_file=.false.
      if(nrecord.eq.-1.or.rea_read_to_sfile_end.eq.-1)
     *read_to_end_of_file=.true.
      read_data_only=.false.
      if(nhead.eq.-1) read_data_only=.true.
c
      do i=1,80
        blank(i:i)=' '
      enddo
c
      NRECORD=1
      IERR=0
      ID=0
      NSTAT=0
      nphas=0
      OLDSTA='****'
      format7='ukn'
      format='ukn'
C
C   READ FIRST HEADER, ALSO BACK HERE IF FIRST LINE IS BLANK
C
 150  CONTINUE

c
c  read, now (jan 2011) also stop if reading error
c
      read(file,'(a80)',end=19,err=19) data(1)
      goto 20
 19   continue
      goto 9999
 20   continue

C
      do i=1,80
        if(ichar(data(1)(i:i)).eq.26) then
          write(6,*) 'Found ctl z in INDATA, skip line'
          goto 150  ! skip ctl z
        endif
      enddo

      IF(DATA(1).EQ.BLANK) THEN
         WRITE(6,*)'**********  BLANK HEADER LINE ***************'
         IERR=1
         GO TO 150   ! read next line
      ENDIF
c
c   now assume first header line, later checked in rea
c
      READ(DATA(1),'(21X,2A1,56X,A1)') TYPE,EXP,LINE_TYPE
      IF(IERR.EQ.1) THEN
C
C     WRITE DATE ETC BEFORE BLANK HEADER LINE
C
         WRITE(6,'(A80)') DATA(1)
         IERR=0
      ENDIF
c
C---------------------------------------------------------------------
C   LOOP FOR READING HEADERS, ASSUME FIRST HEADER LINE READ
C---------------------------------------------------------------------
c
 1    CONTINUE
         NRECORD=NRECORD+1 
         if(nrecord.gt.max_data) then
            write(6,*) ' Data dimension max_data exeded, is now ',
     *      max_data
            write(6,*) ' Can be changed in seidim.inc'
            stop
         endif

 111     continue                                   ! from just below
c         write(*,*) ' debug lo using indata '
cc         READ(FILE,'(A80)',IOSTAT=CODE) DATA(NRECORD) ! jh nov 19
           READ(FILE,'(A160)',IOSTAT=CODE) long_line
           data(nrecord)=long_line(1:80)

         call sei code( fort$,                      ! Stop on error.
     &                  code,                       ! Condition.
     &                  file,                       ! File with condition.
     &                  b_eof )                     ! End of file?.
c         if (data(nrecord)(1:5).eq.'-----') then
c           data(nrecord)='                                        ' ! lo 15/11/2019 to read sfile from log, where blank may be missing
c         endif

c
c   check for end of file
c
         if( b_eof ) then
             nhead=nrecord-1                        ! will add blank line            
             goto 3                         
         endif
c
c   check for ctl z, maybe not needed
c
         do i=1,80
           if(ichar(data(nrecord)(i:i)).eq.26) then ! skip ctl z
             write(6,*)'Found ctl z in INDATA, skip line' 
             goto 111 
           endif
         enddo

         READ(DATA(NRECORD),'(79X,A1)') LINE_TYPE
c
c   if line_type is blank, it can be start of phases or 2 byte characters 
c   in line so check for non standard characters and replace with ?. It can
c   also be an extra character on line, a blank line or missing line type.
c
c   first check if a phase line, if so no more checks. if previous line was 
c   type 7, then it must be a phase line so also no check. this check for type 7
c   allows for first phase line being blank and also allows for no type 7 line.
c   So if no type 7 line and first phase line is blank, this line will become
c   a blank type 3 line and then be part of headers. Reading of headers will 
c   continue until a valid phase line has been found.
c
c   if not reading to end of file, like for multiple event files, a blank
c   line in headers will be taken as end of event and reading will stop
c   for this event.  
c

          if(line_type.eq.' '.or.line_type.eq.'4') then
             call      ! check if phase line and the format of phase line
     *       check_if_phase_line_format(data(nrecord),phase,format)
c
          if(data(nrecord-1)(80:80).ne.'7') then
             if(.not.phase) then   
c
c   assume a wrong header line
c
               do i=1,160
                  if(ichar(long_line(i:i)).gt.127) long_line(i:i)='?'
               enddo
               k=seiclen(long_line)
c
c   if k>80, a longer line might
c   be caused by a nonstandard character so truncate line, assunme last 
c   char is line type, no check if a valid type. if invalid type, a warning 
c   is given in rea.for
c  
c              write(6,'(a)')'Something wrong with a header line:'
c              write(6,'(a)') long_line(1:k)
c              write(6,'(a)')'for event'
c              write(6,'(a)') data(1)         
               if(k.gt.80) then
                  line_type=long_line(k:k)
                  data(nrecord)(1:79)=long_line(1:79)
                  data(nrecord)(80:80)=line_type
                  write(6,'(a)')'Line had more then 80 chars'
                  write(6,'(a)')'It has been truncted to'
                  write(6,'(a)') data(nrecord)
                  write(6,'(a)')'for event'
                  write(6,'(a)') data(1)         
               else  
c
c   if a header line and too short, terminate with 3. if a cat file
c   and not reading to end of file, a blank line will be taken as
c   end of event.
c
                  if(.not.read_to_end_of_file.and.data(nrecord).eq.' ')
     *            then
                    goto 666
                  endif
                  data(nrecord)(1:79)=long_line(1:79)
                  data(nrecord)(80:80)='3'
                  line_type='3'
                  write(6,'(a)')'Line had less than 80 chars'
                  write(6,'(a)')'It has been terminated with type 3'
                  write(6,'(a)') data(nrecord)
               endif        
            endif
          endif
          endif

 666   continue    

c
c   check for blank type 1 line, remove
c
       if(line_type.eq.'1'.and.data(nrecord)(1:79).eq.' ') then
          nrecord=nrecord-1
          write(6,*)'Blank type 1 line removed'
          goto 1
       endif        
C
C   CHECK FOR ID LINE
C
         IF(DATA(NRECORD)(80:80).EQ.'I') ID=NRECORD
c
c  Use header line 7 to determine format
c
         if(line_type.eq.'7') then
            format7='ukn'
            if(data(nrecord)(2:79).eq.
     *     'STAT COM NTLO IPHASE   W HHMM SS.SSS   PAR1  PAR2 AGA'//
     *    ' OPE  AIN  RES W  DIS CAZ') format7='new'  
            if(data(nrecord)(2:79).eq.
     *     'STAT SP IPHASW D HRMM SECON CODA AMPLIT PERI AZIMU'//
     *     ' VELO AIN AR TRES W  DIS CAZ') format7='old'
          endif

C
C   CHECK IF MORE HEADERS
C
         IF(LINE_TYPE.NE.' '.AND.LINE_TYPE.NE.'4') THEN
            GO TO 1       ! more headers
         ENDIF
c
c-----------------------------------------------------------------------
c   no more headers
c
         NHEAD=NRECORD-1 
c
c    ready to read phases if any, could be only headers
c


c------------------------------------------------------------------------------
c
c   Phase lines
c
C-----------------------------------------------------------------------------
c
C   READ PHASE RECORDS UNTIL BLANK LINE OR END OF FILE
c   always to eof if flag set, used with single events to trap
c   blank lines
C-----------------------------------------------------------------------------
c

 2    CONTINUE

      IF(DATA(NRECORD)(1:40).EQ.BLANK(1:40)) then 
c
c  check if end of file or blank line in phase lines
c
         if(read_to_end_of_file) then
c
c   read next line, check if last line, then ok
c
            READ(FILE,'(A80)',IOSTAT=CODE) DATA(NRECORD)
            call sei code( fort$,                   ! Stop on error.
     &                     code,                    ! Condition.
     &                     file,                    ! File with condition.
     &                     b_eof )                  ! End of file?.
            if( b_eof ) goto 3                      ! & skip
c
c   blank line message only given once pr file
c
c           if(blank_line_message) 
c    *      write(6,*) ' Blank line in phases, will be removed !!'
            blank_line=.true.
            goto 2    ! there could be more blank lines 
         else
            goto 3
         endif
      endif
c
c   if here and blank_line is true,  there must 
c   have been a blank line in phases and not
c   only one or several blank lines at end. only blank lines in 
c   phases will result in a message
c
      if(blank_line) blank_line_message=.true.
c
c--------------------------------------------------------------------------------
c   getting to here, this must be a real phase line, time to check formats
c   possibely also determine format of phase line
c   only use first phase line to check format
c-----------------------------------------------------------------------------------
c

c
c   if phase line format has not been determined before, do it here
c
      if(format.eq.'ukn')then
         call     
     *   check_if_phase_line_format(data(nrecord),phase,format)  
      endif
c
c   check if line 7 format corresponds to phase line format
c
      if(format.ne.'ukn'.and.format7.ne.'ukn') then
         if(format.ne.format7) then
           if (data(nrecord)(80:80).ne.'4') then !lo
c     write(*,*) ' lo ',data(nrecord),format
            write(6,*)
     *     ' Format given by 7-line and phase data do not match, stop'
              stop
           endif
         endif
       endif
c
c   if a fixed input format is required, check if it corresponds to
c   format of file
c

      if(format_found.and.nphas.le.1) then
         if(.not.rea_new_in.and.(format.eq.'new'.or.format7.eq.'new'))
     *   then
             write(6,*)' Input format is not Nordic (old), stop'
             stop
         endif

         if(rea_new_in.and.(format.eq.'old'.or.format7.eq.'old')) then
             write(6,*)' Input format is not Nordic2, stop'
             stop
         endif
      endif   
c
c   set input format if not set before
c
      if(.not.format_found) then
c
c   at least one of indcators must give correct format
c
          if(format.eq.'new') rea_new_in=.true.
          if(format7.eq.'new') rea_new_in=.true.
          if(format.eq.'old') rea_new_in=.false.
          if(format7.eq.'old') rea_new_in=.false.
          if(format.ne.'ukn'.or.format7.ne.'ukn') format_found=.true.
      endif          
     
      if (data(nrecord)(1:5).eq.'-----') then    ! lo 15/11/2019
        data(nrecord)=' '
        goto 3
      endif

 10   continue

c
c   if phase is weighted out, phase and station is not counted
c

c--------------------------------------------------------------
c   new format
c--------------------------------------------------------------

      if(rea_new_in) then
      if(data(nrecord)(25:25).ne.'4'.and.data(nrecord)(25:25).ne.'9') 
     *then  
C
C   COUNT AZIMUTH AS A PHASE
C
         if(data(nrecord)(17:18).eq.'BA') nphas=nphas+1  
c
c   find if an iaspei phase, T is, BAZ is not
c
         ph_ok=phase_ok(data(nrecord)(17:24))
         if(data(nrecord)(17:19).eq.'BAZ') ph_ok=.true.
C
C  COUNT PHASES
C
         if(ph_ok) nphas=nphas+1    
c
c   count stations, check all previous stations
c

         if (ph_ok) then  ! only count stations which have a phase, not amplitude and coda
            newsta=data(nrecord)(2:6)
            test=.false.
            do i=nhead+1,nrecord-1
c
c   do not compare to stations with invalid phase
c
              ph_ok=phase_ok(data(i)(17:24))
              if(data(i)(17:19).eq.'BAZ') ph_ok=.true.
              if(newsta.eq.data(i)(2:6)
     *        .and.data(i)(25:25).ne.'4'
     *        .and.data(i)(25:25).ne.'9'.and.ph_ok) then
                 test=.true.
              endif
            enddo
            if(.not.test) then
              nstat=nstat+1
            endif
         endif
      endif
c
c----------------------------------------------------------------------------
      else           ! old format
c----------------------------------------------------------------------------
c
      if(data(nrecord)(15:15).ne.'4'.and.data(nrecord)(15:15).ne.'9') 
     *then  

c
C   COUNT AZIMUTH AS A PHASE
C 
         if(data(nrecord)(47:51).ne.'     ') nphas=nphas+1    ! baz with phase
c
c   find if an iaspei phase, BAZ is not, T is
c
         ph_ok=phase_ok(data(nrecord)(11:18))
         if(data(nrecord)(11:13).eq.'BAZ') ph_ok=.true.   ! could be a phase
                                                          ! in old format if it comes from new
C
C  COUNT PHASES
C
         if(ph_ok) nphas=nphas+1    
c
c   count stations, check all previous stations
c

         if (ph_ok) then  ! only count stations which have a phase, not amplitude and coda
            newsta=data(nrecord)(2:6)
            test=.false.
            do i=nhead+1,nrecord-1
c
c   do not compare to stations with invalid phase

              ph_ok=phase_ok(data(i)(11:18))
              if(data(i)(11:13).eq.'BAZ') ph_ok=.true.
              if(newsta.eq.data(i)(2:6)
     *        .and.data(i)(15:15).ne.'4'
     *        .and.data(i)(15:15).ne.'9'.and.ph_ok) then
                 test=.true.
              endif
            enddo
            if(.not.test) then
              nstat=nstat+1
            endif
         endif
      endif
      endif


c
c   position at next record
c
      nrecord=nrecord+1 

         if(nrecord.gt.max_data) then
            write(6,*) ' Data dimension max_data exeded, is now ',
     *      max_data
            write(6,*) ' Can be changed in seidim.inc'
            stop
         endif

 777  continue                                ! from just below
      READ(FILE,'(A80)',IOSTAT=CODE) DATA(NRECORD)
      call sei code( fort$,                   ! Stop on error.
     &               code,                    ! Condition.
     &               file,                    ! File with condition.
     &               b_eof )                  ! End of file?.
      if( b_eof ) then                        ! & skip.
         goto 3                                  ! 
      else                                    ! Otherwise.
         if(ichar(data(nrecord)(1:1)).eq.26) goto 777  ! skip ctl z
         GO TO 2                              ! Next to check.
      end if                                  !
c      if (data(nrecord)(1:5).eq.'-----') data(nrecord)=' ' ! lo 15/11/2019 to read sfile from log, where blank may be missing

C---------------------------------------------------------
C   END OF DATA FOR ONE EVENT
C---------------------------------------------------------


 3    CONTINUE

      if(blank_line_message)     
     *write(6,*) ' Blank line in phases, will be removed !!'

C
C   MAKE SURE LAST RECORD REALLY IS BLANK, COULD BE  ANYTHING IF
C   LAST LINE IF FILE WAS NOT 80 CHARS OR NOT BLANK
C
      DATA(NRECORD)=BLANK


      if(read_data_only) return

c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   Always put data in memory meaning filling out the REA variables. 
c   So every time indata is called, data in memory is overwritten.
c   This is true for both old and new format. 
c   If new format, also rewrite array data so it is in new format if
c   old format was input.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c


         rea_nrecord=nrecord
         rea_nhead=nhead
c
c   clear data5, might be more then nrecord if old data converted to new
c   and new lines created
c
         do i=1,2*nrecord
            data5(i)=' '
         enddo

         do i=1,nrecord
c
c   save info in column 1, sometimes used
c
            data5(i)(120:120)=data(i)(1:1)

         enddo

        call rea_input_all(data,.true.,code)  ! only read internally

        nrecord=rea_nrecord
        nhead=rea_nhead
        id=rea_id_line_number   ! could be moved around, jh jan 23

c
c---------------------------------------------------------------
c   check if data has to be transformed from new to old format
c   in arrry data for programs using only old format internally 
c   like hyp. 
c---------------------------------------------------------------
c

      if(rea_new_in.and.rea_old_format_required) then  

c
c   write out event in old format in array data, nothing is written here in data5
c   used by programs hyp, update, bul, norhin,
c
          rea_new_out_org=rea_new_out   ! save old value at set it back later
          rea_new_out=.false.
          call rea_event_out(0,.true.,data,code)
          rea_new_out=rea_new_out_org
c
c   new phases might have been added like END  ! fix new not sure
c
          nrecord=rea_nrecord
          nhead=rea_nhead 
          id=rea_id_line_number   ! could be moved around

c
c   put in data5  parameters to be carried over in hyp and other programs,    
c   used when internal data is old format and input format new.
c   Used to carry over data from new format that is not in old format.
c   now used in hyp, norhin, update.
c
c   this mus tbe done after converting to old format since otherwise
c   there is no correspondence between data and data5
c
        k=rea_nspec    ! spectra are first in rea phase list

        do i=nhead+1,nrecord
           k=k+1
           data5(i)(75:119)=' '
           data5(i)(75:77)=rea_com(k)
c
           data5(i)(81:84)=rea_network(k)//rea_location(k)
           data5(i)(85:90)=
     *     rea_agency(k)(1:3)//rea_operator(k)(1:3)

        enddo

       endif
c
c------------------------------------------------------------------
c  check if format has to be transferred to new format in data if
c  old format input but new format is used as output
c  if there a requirment that old format is returned,
c  skip this step. also skip if only new format should be used
c------------------------------------------------------------------

       if(.not.rea_new_in.and.rea_new_out.and..not.
     *    rea_old_format_required.and..not.new_nordic_format_only) then

          rea_nrecord=nrecord
          rea_nhead=nhead
          all=.true.                    !  write all
c
c   write out event in new format in array data but with separate phases for e.g. coda
c

          call rea_event_out(0,all,data,code) 
          nrecord=rea_nrecord
          nhead=rea_nhead
          id=rea_id_line_number   ! is moved around in rea

        endif   



      RETURN

C
C   END OF FILE
C
9999  NRECORD=0     ! indicates no data

      RETURN
      END
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine to_next_event(unit)
c
c   read to next event
c
      implicit none
      character*80 text
      integer unit,i
      
      do i=1,10000
         read(unit,'(a80)') text
         if(text.eq.' ') return
      enddo
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine check_if_phase_line_format(text,phase,format)
c
c   check if a phase line, if so, also the format
c   all kinds of of line type 4 is checked
c
      implicit none
      character*80 text
      logical phase         ! true if a phase line
      logical phase_ok      ! true if a phase
      character*3 format    ! new, old or blank
      integer hr,min
      real sec

      phase=.false.
      format='ukn'
c
c   if whole line is blank, it cannot be a phase line
c
      if(text.eq.' ') return

c
c   new format
c
       read(text(24:28),'(i5)',err=10)   ! old format sec field
       read(text(27:37),'(2i2,f7.1)',err=10) hr,min,sec

       if(text(10:10).eq.' '.and.
     *    text(15:15).eq.' '.and.
     *    (text(27:27).eq.' '.or.
     *    text(27:27).eq.'1'.or.text(27:27).eq.'2'.or.
     *    text(27:27).eq.'0').and.
     *    hr.ge.0.and.hr.le.26.and.
     *    min.ge.0.and.min.le.60.and.
     *    sec.ge.0.0.and.sec.le.200.0.and.
     *    text(29:37).ne.' ') then
c
c   check if valid phase, all types of phase lines
c
              if(phase_ok(text(17:25)).or.
     *                    text(17:19).eq.'END'.or.
     *                    text(17:19).eq.'BAZ'.or.
     *                    text(17:18).eq.'IA'.or.
     *                    text(17:18).eq.'IV'.or.
     *                    text(17:18).eq.'AM'.or.
     *                    text(17:18).eq.'AP'.or.
     *                    text(17:18).eq.'A '.or.
     *                    (text(17:17).eq.' '.and.
     *                    (text(16:16).eq.'I'.or.text(16:16).eq.'E')))
     *        then
                 phase=.true.
                 format='new'
                 return
              endif
       endif

 10    continue

c
c  maybe old format
c
       read(text(19:28),'(2i2,f6.1)',err=20) hr,min,sec

       if((text(19:19).eq.' '.or.text(19:19).eq.'1'.or.
     *    text(19:19).eq.'2'.or.text(19:19).eq.'0').and.
     *    hr.ge.0.and.hr.le.26.and.
     *    min.ge.0.and.min.le.60.and.
     *    sec.ge.0.0.and.sec.le.200.0) then
c
c   check if valid phase line, all kinds
c

              if(phase_ok(text(11:18)).or.
     *                    text(11:13).eq.'END'.or.
     *                    text(11:13).eq.'BAZ'.or.
     *                    text(11:12).eq.'IA'.or.
     *                    text(11:12).eq.'IV'.or.
     *                    text(11:12).eq.'AM'.or.
     *                    text(11:12).eq.'AP'.or.
     *                    (text(11:11).eq.' '.and.
     *                    (text(10:10).eq.'I'.or.text(10:10).eq.'E')))
     *                    then
                 phase=.true.
                 format='old'
                 return
              endif
       endif
c
c   not a phase line
c
 20   continue
      return
      end
