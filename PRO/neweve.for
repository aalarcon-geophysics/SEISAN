CJAB(BGS)Mar28/1995 : changes to assist in data entry
CSTART**************************************************************************
c              PROGRAM TO CREATE SEISMIC DATA FILE
C              ===================================
C
C    WRITTEN BY EZRA AND REUBEN, MAY 1990
c
c
C    CHANGES AND UPDATES:
C    1991-04-04; CL : INITILIZATION OF TEXT STRINGS ADDED
C    1992  2  7  JH : NEW NORDIC, EXTRA HEADER LINE, do not ask for hr min
c    sep 10 92 ny jh: fix problems
c    sep 8  93      : check if output file there, fix bug
c    sep 18         : stop any where
CJAB(BGS)Mar95      : Make header easier to enter.
c---------------------------------------------------------------------
c    oct 98          --------------   version 7.0 check ---------------
c                    5 station chars, remove explanation line for header
c
c                     and sec in header
C
C                VARIABLE IDENTIFICATION
C                ------------------------
C           STAT: station name
C           SP: S=instrument type: S=short period
c                                  L=long period, I=intermediate
c               P=component(Z,N,E)
C           I=Quality indicator(I,E,( ) )
C           PHAS=phase identification(P,S,Pg e.t.c)
c           W= weighting indicator
c           D=first motion(U: up; D:down; or C: compression; D:dilatation)
c           CODA=duration(to noise) in seconds
c           AMPLIT=amplitude(zero-peak) in nanometers
c           PERI=period in seconds
c           HR= hours,MM=minutes,SECON=seconds
c      
                  integer      start              !JAB(BGS)Mar95. Text position.
                  CHARACTER*80 TEXT/' '/,OLDTEX/' '/
                  character    chr_text *(80)     !JAB(BGS)Mar95.
                  character answer
                  INTEGER ERROR
c
      integer i,ii,jj                           ! number of arguments
      integer narg                            ! number of arguments
      character*80 arg(40)                    ! arguments
      character event_type                    ! Event type
      character*24 event_time                 ! Event time ISO 8601
c 2020-02-12T14:22:30.123Z
c 123456789012345678901234
c 1234 56 78 90 12 34 567
      integer year                            ! number of arguments
      integer mdr                             ! number of arguments
      integer day                             ! number of arguments
      integer hour                            ! number of arguments
      integer min                             ! number of arguments
      real sec                                ! number of arguments
c     integer time_array                             ! number of arguments
      INTEGER, DIMENSION(17) :: time_array           ! number of arguments
      character*9 event_lat                  ! Event time ISO 8601
      real lat                                ! number of arguments
      character*10 event_lon                  ! Event time ISO 8601
      real lon                                ! number of arguments
      character*8 event_dep                  ! Event time ISO 8601
      real dep                                ! number of arguments
      character*3 event_op                  ! Event time ISO 8601
      character*3 event_agency              ! Event time ISO 8601
      character*5 event_db                  ! Event time ISO 8601


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

c          
      call get_arguments(narg,arg)
      if(narg.ge.1) then
        call PROCESS_FLAG
c       call WRITE_EVENT
c        read(event_time(1:4),'(i4)') year
c        if(year.lt.-999.OR.year.gt.2070) then
c	  WRITE(6,*) ' Not a valid event year: ',year
c          call TIME_FORMAT_ERROR(event_time)
cc         stop
c        endif
c        read(event_time(6:7),'(i2)') mdr
c        if(mdr.lt.1.OR.mdr.gt.12) then
c	  WRITE(6,*) ' Not a valid event mdr : ',mdr
c          call TIME_FORMAT_ERROR(event_time)
cc         stop
c        endif
c        read(event_time(9:10),'(i2)') day
c        if(day.lt.1.OR.day.gt.31) then
c	  WRITE(6,*) ' Not a valid event day : ',day
c          call TIME_FORMAT_ERROR(event_time)
cc         stop
c        endif
c        read(event_time(12:13),'(i2)') hour
c        if(hour.lt.0.OR.hour.gt.23) then
c	  WRITE(6,*) ' Not a valid event hour: ',hour
c          call TIME_FORMAT_ERROR(event_time)
cc         stop
c        endif
c        read(event_time(15:16),'(i2)') min
c        if(min.lt.0.OR.min.gt.59) then
c	  WRITE(6,*) ' Not a valid event min : ',min
c          call TIME_FORMAT_ERROR(event_time)
cc         stop
c        endif
c        read(event_time(18:23),'(f6.3)') sec
c        if(sec.lt.0.0.OR.sec.ge.60.0) then
c	  WRITE(6,*) ' Not a valid event sec : ',sec
c          call TIME_FORMAT_ERROR(event_time)
c         stop
c        endif
c      endif
cc end event time
        WRITE(6,*) 'done flags'
      stop
      endif
c end call get_argnuemts
c          
c          
      WRITE(6,*) '  *************************************************'
	  WRITE(6,*) ' This program creates a file with new events'
	  WRITE(6,*) ' Output file: neweve.out'
	  WRITE(6,*) ' neweve.out is used as input to program: SPLIT'
      WRITE(6,*) '  *************************************************'
	  WRITE(6,*) '   '
	  WRITE(6,*) '        Note the following.....!!!!!'
	  WRITE(6,*) '   '
	  WRITE(6,*) ' 1. Text will be converted to UPPERCASE'   !JAB(BGS)Mar95.
	  WRITE(6,*) ' 2. Enter data only below MARKED fields'
	  WRITE(6,*) ' 3. RETURN (blank line) is new event.'
	  WRITE(6,*) ' 4. STOP  stops the program'
          write(6,*) ' 5. To correct a line, use _ for blank'
          write(6,*) ' 6. Auto repeat of STAT SP HHMM'
	  WRITE(6,*)
C
C   OPEN FILE
C
      OPEN(2,FILE='neweve.out',STATUS='new',err=888)
      goto 889
 888  continue
      write(6,*)' The NEWEVE.OUT file exists, do you want to',
     *          '  overwrite(y/n)'
      read(5,'(a)') answer
      if(answer.eq.'y'.or.answer.eq.'Y') then
          open(2,file='neweve.out',status='unknown')
      else
          stop
      endif
 889  continue           
C
C  ENTER HERE FOR NEW EVENT
C
   10 CONTINUE
C
      WRITE(6,*) 'New event: First enter header information....'
      WRITE(6,*)'YEAR MMDD LE' 
      READ(5,'(A)') chr_text                     !JAB(BGS)Mar95.
      call sei upc ( chr_text )                  !JAB(BGS)Mar95. Uppercase.
      if( chr_text(:4) .eq. 'STOP' ) stop        !JAB(BGS)Mar95.
c                                                !JAB(BGS)Mar95.
c    Find start position for text, so that       !JAB(BGS)Mar95.
c    different hardware situations can be        !JAB(BGS)Mar95.
c    accounted for                               !JAB(BGS)Mar95.
c                                                !JAB(BGS)Mar95.
      start = 0                                  !JAB(BGS)Mar95. Initialise.
1111  start = start + 1                          !JAB(BGS)Mar95. Increment.
      if( chr_text .eq. ' ' )then                !JAB(BGS)Mar95. Invalid.
      write(*,*)'Blank entry...try again...'     !JAB(BGS)Mar95.
      goto 10                                    !JAB(BGS)Mar95. Try again.
c                                                !JAB(BGS)Mar95.
      else if(chr_text(start:start) .eq. ' ')then!JAB(BGS)Mar95. Not there yet!.
      goto 1111                                  !JAB(BGS)Mar95. & try again.
c                                                !JAB(BGS)Mar95.
      else                                       !JAB(BGS)Mar95. Found.
      text(2:) = chr_text(start:)                !JAB(BGS)Mar95. & setup.
      end if                                     !JAB(BGS)Mar95.  
c
      TEXT(22:23)=TEXT(12:13)
      TEXT(12:13)='  '
      TEXT(1:1)=' '
C
C   CHECK HEADER
C
      IF(TEXT(22:22).NE.'L'.AND.TEXT(22:22).NE.'R'.AND.TEXT(22:22).
     *NE.'D') THEN
         WRITE(6,*)' YOU  MUST GIVE EVENT TYPE L, R OR D'
         GOTO 10
      ENDIF
      READ(TEXT(2:10),'(I4,1X,2I2)',ERR=27) IYEAR,IMONTH,IDAY
      goto 28
 27   CONTINUE
         WRITE(6,*)' SOMETHING WRONG WITH YEAR AND DATE INFO'
         goto 10
 28   continue
      IF(IYEAR.GT.2100.OR.IYEAR.LT.0.OR.IMONTH.GT.12.OR.IMONTH.
     *LT.1.OR.IDAY.GT.31.OR.IDAY.LT.1) THEN
         WRITE(6,*)' SOMETHING WRONG WITH YEAR AND DATE INFO'
         GOTO 10
      ENDIF
      WRITE(2,'(A)') TEXT
C
C Print headers in file and screen
C
c      WRITE(2,201)
c 201  FORMAT(' YYYY MMDD HHMM  SEC LE LT.XXX LON.XXX DP.XFF     N RMS',
c     *'                        3')	  
      WRITE(2,200)
  200 FORMAT(' STAT SP IPHASW D HRMM SECON CODA AMPLIT PERI',
     *' AZIMU VELO AIN AR TRES W  DIS CAZ7'  )
c fixed 2020-03-18 pv:
c    *' AZIMU VELO AIN AR TRES  W DIS CAZ7'  )
c     *' AZIMU VELO SNR AR TRES  W DIS CAZ7'  )
C
C   ***** NOTE: SOME DIFFERENCE BETWEEN COMPILERS MIGHT OCCUR HERE......
C
      WRITE(6,*)'STAT SP IPHASW D HRMM SECON CODA AMPLIT PERI'
C
C Start of input data
      DO 20 I=1,2000
      read(5,'(A)') chr_text                    !JAB(BGS)Mar95. Get text.
      call sei upc( chr_text )                  !JAB(BGS)Mar95. Make uppercase.
      text(1:75) = ' ' // chr_text(start:)      !JAB(BGS)Mar95. positioned data.
C
C Check for end of data entry
C
	    IF(TEXT(2:5).EQ.'STOP') GOTO 99
C
C  Check for end of event
C
        IF(TEXT(1:30).EQ.'                             ') GOTO 30
C
C  Check if information from previous line should be used
C
C  STATION
C
        IF(TEXT(2:6).EQ.'     ')TEXT(2:6)=OLDTEX(2:6)  
        OLDTEX(2:6)=TEXT(2:6)
C Type of instrument
        IF(TEXT(7:7).EQ.' ') TEXT(7:7)=OLDTEX(7:7)
        OLDTEX(7:7)=TEXT(7:7)
c Component type
	   IF(TEXT(8:8).EQ.' ') TEXT(8:8)=OLDTEX(8:8)
	   OLDTEX(8:8)=TEXT(8:8)
c Quality indicator
C	  IF(TEXT(10:10).EQ.' ') TEXT(10:10)=OLDTEX(10:10)
C	  OLDTEX(10:10)=TEXT(10:10)
c Phase type
C	 IF(TEXT(11:11).EQ.' ') TEXT(11:11)=OLDTEX(11:11)
C	 OLDTEX(11:11)=TEXT(11:11)
c Hours 
        IF(TEXT(19:20).EQ.'  ')TEXT(19:20)=OLDTEX(19:20)
        OLDTEX(19:20)=TEXT(19:20)
c Minutes 
	  IF(TEXT(21:22).EQ.'  ') TEXT(21:22)=OLDTEX(21:22)
	  OLDTEX(21:22)=TEXT(21:22)
C Check input data
        CALL CHECK(start,TEXT,ERROR)      !JAB(BGS)Mar95.
        WRITE(2,'(A)')TEXT(1:75)
        oldtex=text
   20 CONTINUE   
C
   30 CONTINUE
c Leave blank between events
      WRITE(2,330)
  330 FORMAT(' ')
      GOTO 10
 99   CONTINUE
      WRITE(2,340)
 340  FORMAT(' ')
      STOP
      END

C            SUBROUTINE FOR VALIDATING INPUT DATA
C            ------------------------------------   
             SUBROUTINE CHECK(start,TEXT,ERROR)    !JAB(BGS)Mar95.
C
             CHARACTER*80 TEXT,NEWTEX
             INTEGER ERROR
             integer start                         !JAB(BGS)Mar95.
             character    chr_text *(80)           !JAB(BGS)Mar95.
C
C  Input:   text: input line to chech,if something is wrong
c                 a new line is asked
c  Output:  text:the corrected line
c                 ERRORS: NO ERROR=0
C                         ERRORS=1
C
          ERROR=0
C
C  Start of checking loop
   25 CONTINUE
c   
c Check onset phase
C
       IF(TEXT(10:10).NE.'I'.AND.TEXT(10:10).NE.'E'.AND.TEXT(10:10).
     *   NE.' ') THEN
         WRITE(6,120) 
  120    FORMAT(' ***ONSET NOT VALID ***')
         ERROR=1
       ENDIF
C Check phase
c change lot 03/2006
       IF((TEXT(11:11).NE.'P'.AND.TEXT(11:11).NE.'S'.AND.TEXT(11:11)
     *  .NE.' '.and.text(11:11).ne.'L'.and.text(11:11).ne.'R'.and.
     *  text(11:11).ne.'A')
     *  .OR.TEXT(10:11).EQ.'  ') THEN
        WRITE(6,150) TEXT(11:11)
  150   FORMAT(' ***PHASE INPUT  ' ,A1,' NOT LEGAL,CORRECT LINE')
        ERROR=1
      ENDIF
C
C Check whether time is   within the range
c
      READ(TEXT(19:20),'(I2)',ERR=123) IHOUR
      goto 1124
 123  CONTINUE
	  WRITE(6,260) TEXT(19:20)
      errror=1
 1124 continue

      IF(IHOUR.GT.24.OR.IHOUR.LT.0) THEN
	    WRITE(6,260) TEXT(19:20)
  260   FORMAT(' ***hours  ',A2,' out of range,correct time **')
        ERROR=1
      ENDIF
C
c Check for minutes
C
      READ(TEXT(21:22),'(I2)',ERR=124) MIN
      goto 1224
 124  continue
      write(6,270)
      error=1
 1224 continue
      IF(MIN.LT.0.OR.MIN.GT.60) THEN
	    WRITE(6,270)
  270   FORMAT(' ***time in minutes is out of range ***')
        ERROR=1
	  ENDIF	 			  		
C Check if any errors
      IF(ERROR.EQ.0) GOTO 40
C If errors are present
      WRITE(6,*)'Correct data---------'
      WRITE(6,*)'STAT SP IPHASW D HRMM SECON CODA AMPLIT PERI'
      write(*,*)text(2:)                                  !JAB(BGS)Mar95.
C
C  Read corrected text line, compare with old text and replace
c  chars in old text with chars in new text if different,or blank
c  if underscore
c
      read(5,'(A)') chr_text                        !JAB(BGS)Mar95.
      call sei upc( chr_text )                      !JAB(BGS)Mar95.
      if( chr_text(:4) .eq. 'STOP' ) stop           !JAB(BGS)Mar95.
      newtex = ' ' // chr_text(start:)              !JAB(BGS)Mar95.
C                                                   !JAB(BGS)Mar95.
      DO 30 I=2,80
        IF(NEWTEX(I:I).EQ.' ') GOTO 30
        if(newtex(i:i).eq.'_') then
           text(i:i)=' '
           goto 30
        endif
        IF(NEWTEX(I:I).NE.TEXT(I:I)) TEXT(I:I)=NEWTEX(I:I)
   30 CONTINUE
      ERROR=0
      GOTO 25
C End of loop
  40  CONTINUE
      RETURN
      END                       


C     SUBROUTINE FOR reporting time format error
C     ------------------------------------   
      SUBROUTINE TIME_FORMAT_ERROR(time)    ! pv
C
      character*24 time
c
c       read(time(1:4),'(i4)') year
c  WRITE(6,*) ' Not a valid event year: ',year
        WRITE(6,*) ' Not a valid time:  ',time
        WRITE(6,*) ' Event time format: 2020-02-12T14:22:30.123Z'
        WRITE(6,*) ' Only from year 1900 to year 2070'
          stop
      RETURN
      END                       
c
c        SUBROUTINE FOR listing -help
      subroutine FLAG_HELP
c
      character*3 agency
      character*5 database
      call get_agency(agency)
      call get_def_base(database)
c
       WRITE(6,*) 'NEWEVE - x'
       WRITE(6,*) ''
       WRITE(6,*) 'Usage: neweve [options]'
       WRITE(6,*) ' or:   neweve'
       WRITE(6,*) ''
       WRITE(6,*) 
     +' Without options multiple events can be added to neweve.out'
       WRITE(6,*) ''
       WRITE(6,*) ' With options one event can be added to database'
       WRITE(6,*) ''
       WRITE(6,*) ' ## Options ##'
       WRITE(6,*) ' -help       Show this usage message'
       WRITE(6,*) ' -h          Show this usage message'
       WRITE(6,*) 
     +' -type type  Specify the event type options L/R/D. Default is L'
       WRITE(6,*) ' -time time  Specify time of event'
       WRITE(6,*) '               time format: YYYY-MM-DDTHH:MM:SS.SSSZ'
       WRITE(6,*) '               E.g.:        2020-02-12T14:22:30.123Z'
       WRITE(6,*) '                 Only from year 1900 to year 2070'
       WRITE(6,*) ' -model model    Default is blanck'
c      WRITE(6,*) ' -lat latitude   Default is blanck'
c      WRITE(6,*) ' -lon longitude  Default is blanck'
c      WRITE(6,*) ' -dep depth      Default is blanck'
       WRITE(6,*) ' -op operator    Default is blanck'
       WRITE(6,*) ' -db database    Default is: ',database
       WRITE(6,*) ' -ag agency      Detault is: ',agency
       WRITE(6,*) ''
       WRITE(6,*) ' Examples:'
       WRITE(6,*) '            ',
     +'neweve -time 2020-02-12T14:22:30.123Z -op pv'
       WRITE(6,*) '            ',
     +'neweve -time 2020-02-12T14:22:30.123Z ',
     +'-type R model X -op pv -ag BER -db TEST'
c      WRITE(6,*) '            ',
c    +'neweve -time 2020-02-12T14:22:30.123Z -lat 55.3 -lon 12.4 -op pv'
       WRITE(6,*) ''
      stop
      end
c
      subroutine PROCESS_FLAG
c
      implicit none                       ! force delcaration of all variables
      include 'seidim.inc'                ! dimensions for rea block
      include 'seisan.inc'
      include 'rea.inc'                   ! parameter common bliock
c
      character*80 data(5000)             ! s-file with data in text array
      character*80 text                   ! text array
c
      integer i,ii,jj                           ! number of arguments
      integer narg                            ! number of arguments
      character*80 arg(40)                    ! arguments
      character event_model                   ! Event model
      character event_type                    ! Event type
      character*24 event_time                 ! Event time ISO 8601
c 2020-02-12T14:22:30.123Z
c 123456789012345678901234
c 1234 56 78 90 12 34 567
      integer year                            ! number of arguments
      integer mdr                             ! number of arguments
      integer day                             ! number of arguments
      integer hour                            ! number of arguments
      integer min                             ! number of arguments
      real sec                                ! number of arguments
c     integer time_array                             ! number of arguments
      INTEGER, DIMENSION(17) :: time_array           ! number of arguments
      character*8 event_lat                  ! Event time ISO 8601
      real lat                                ! number of arguments
      character*9 event_lon                  ! Event time ISO 8601
      real lon                                ! number of arguments
      character*8 event_dep                  ! Event time ISO 8601
      real dep                                ! number of arguments
      character*3 event_op                  ! Event time ISO 8601
      character*3 event_agency              ! Event time ISO 8601
      character*5 event_db                  ! Event time ISO 8601
c
C-- SYSTEM TIME FOR USED FOR TIME OF OPERATION
      CHARACTER*12 p_time
      character*14 proc_time 
c
      logical all                         ! true: read all data, false: headers
      integer code                        ! error return code
c
      call get_seisan_def
c
      call rea_hyp_clear(1)
      call rea_hyp_clear(2)
c   clear variables
      rea_nphase=0      ! count number of phase lines
      rea_nhyp=0        ! count number of hypocenters
      rea_nwav=0        ! --------------- waveform files
      rea_ncomment=0    ! --------------- comment lines
      rea_nmacro=0      ! --------------- macroseismic data
      rea_nfault=0      ! --------------- fault plane solutions
      rea_nspec=0       ! --------------- spectral solutions
      rea_nmag=0        ! --------------- magnitudes
      rea_id_line=' '   ! no id line
      rea_locality=' '
      rea_nwav=0  
      rea_locality=' '   
      call rea_phase_clear(1)
c
      event_model=" "
      event_type="L"
      hyp_lat(1)=-999.
      hyp_lon(1)=-999.
      hyp_depth(1)=-999.
      event_op="   "
      event_agency="   "
c     hyp_dist_id(1)='L'
      hyp_fix_org(1)=' '
      hyp_model(1)=" "
      hyp_depth_flag(1)=" "
      hyp_epi_flag(1)=" "
c
      call get_arguments(narg,arg)
      if(narg.ge.1) then
      do i=1,narg
c begin event time
      if( arg(i)(1:5).eq.'-help' .OR. arg(i)(1:5).eq.'-h' ) then
        call FLAG_HELP
      endif
c begin event time
      if( arg(i)(1:5) .eq. '-time' ) then
        event_time=arg(i+1)
c check for correct chars:
        time_array = (/ 1,2,3,4,6,7,9,10,12,13,15,16,18,19,21,22,23 /)
        do jj=1,17
          if(ichar(event_time(time_array(jj):time_array(jj))).LT.48.OR.
     +      ichar(event_time(time_array(jj):time_array(jj))).gt.57) then
            call TIME_FORMAT_ERROR(event_time)
c           stop
          endif
        enddo
        if(event_time(5:5).NE."-") then
          call TIME_FORMAT_ERROR(event_time)
        endif
        if(event_time(8:8).NE."-") then
          call TIME_FORMAT_ERROR(event_time)
        endif
        if(event_time(11:11).NE."T") then
          call TIME_FORMAT_ERROR(event_time)
        endif
        if(event_time(14:14).NE.":") then
          call TIME_FORMAT_ERROR(event_time)
        endif
        if(event_time(17:17).NE.":") then
          call TIME_FORMAT_ERROR(event_time)
        endif
        if(event_time(20:20).NE.".") then
          call TIME_FORMAT_ERROR(event_time)
        endif
        if(event_time(24:24).NE."Z") then
          call TIME_FORMAT_ERROR(event_time)
        endif
        
        read(event_time(1:4),'(i4)') year
        if(year.lt.1900.OR.year.gt.2070) then
	  WRITE(6,*) ' Not a valid event year: ',year
          call TIME_FORMAT_ERROR(event_time)
c         stop
        endif
        read(event_time(6:7),'(i2)') mdr
        if(mdr.lt.1.OR.mdr.gt.12) then
	  WRITE(6,*) ' Not a valid event mdr : ',mdr
          call TIME_FORMAT_ERROR(event_time)
c         stop
        endif
        read(event_time(9:10),'(i2)') day
        if(day.lt.1.OR.day.gt.31) then
	  WRITE(6,*) ' Not a valid event day : ',day
          call TIME_FORMAT_ERROR(event_time)
c         stop
        endif
        read(event_time(12:13),'(i2)') hour
        if(hour.lt.0.OR.hour.gt.23) then
	  WRITE(6,*) ' Not a valid event hour: ',hour
          call TIME_FORMAT_ERROR(event_time)
        endif
        read(event_time(15:16),'(i2)') min
        if(min.lt.0.OR.min.gt.59) then
	  WRITE(6,*) ' Not a valid event min : ',min
          call TIME_FORMAT_ERROR(event_time)
        endif
        read(event_time(18:23),'(f6.3)') sec
        if(sec.lt.0.0.OR.sec.ge.60.0) then
	  WRITE(6,*) ' Not a valid event sec : ',sec
          call TIME_FORMAT_ERROR(event_time)
        endif
      endif
c end event time
c begin event model
      if( arg(i)(1:6) .eq. '-model' ) then
        event_model=arg(i+1)
        read(event_model,'(a1)') hyp_model(1)
      endif
c end event model
c begin event type
      if( arg(i)(1:5) .eq. '-type' ) then
        event_type=arg(i+1)
        if(event_type.EQ."L".OR.event_type.EQ."R".OR.
     +event_type.EQ."D") then
	  WRITE(6,*) ' Event type: ',event_type
        else
	  WRITE(6,*) ' Not a valid event type: ',event_type
          stop
        endif
	  WRITE(6,*) ' TYPE: neweve.out'
      endif
c end event type
cc begin event lat
c      if( arg(i)(1:4) .eq. '-lat' ) then
c        event_lat=arg(i+1)
cc       read(event_lat,'(f7.3)') lat
c        read(event_lat,'(f8.3)') hyp_lat(1)
c      endif
cc end event lat
cc begin event lon
c      if( arg(i)(1:4) .eq. '-lon' ) then
c        event_lon=arg(i+1)
cc       read(event_lon,'(f7.3)') lon
c        read(event_lon,'(f9.3)') hyp_lon(1)
c      endif
cc end event lon
cc begin event dep
c      if( arg(i)(1:4) .eq. '-dep' ) then
c        event_dep=arg(i+1)
cc       read(event_dep,'(f7.3)') dep
c        read(event_dep,'(f6.2)') hyp_depth(1)
c      endif
cc end event dep
c begin operator
      if( arg(i)(1:3) .eq. '-op' ) then
        event_op(1:3)=arg(i+1)
      endif
c end operator
c begin agency
      if( arg(i)(1:3) .eq. '-ag' ) then
        event_agency(1:3)=arg(i+1)
      endif
c end agency
c begin database
      if( arg(i)(1:3) .eq. '-db' ) then
        event_db=arg(i+1)
      endif
c end database
c begin station
      if( arg(i)(1:5) .eq. '-stat' ) then
        rea_stat(1)=arg(i+1)
        rea_nphase=1
	WRITE(6,*) 'stat:',rea_stat(1)
      endif
      if( arg(i)(1:4) .eq. '-net' ) then
        rea_network(1)=arg(i+1)
      endif
      if( arg(i)(1:5) .eq. '-chan' ) then
        rea_com(1)=arg(i+1)                  ! only SEED channel names are valid
      endif
      if( arg(i)(1:4) .eq. '-loc' ) then
        rea_location(1)=arg(i+1)
      endif
      if( arg(i)(1:6) .eq. '-phase' ) then
        rea_phase(1)=arg(i+1)
      endif
      if( arg(i)(1:6) .eq. '-onset' ) then
        rea_onset(1)=arg(i+1)
      endif
c end station
c begin archive
      if( arg(i)(1:4) .eq. '-arc' ) then
       rea_nwav=1
       text=' '
       text(80:80)='6'
       text(2:4)='ARC'
       if( arg(i+1)(1:1) .eq. '_' ) then
        text(6:6)='_'
       else 
        text(6:6)=''
       endif
       rea_wav(1)=text
c        nrecord1=nrecord1+1
c        nhead1=nhead1+1
      endif
c end archive
      enddo
c       call gmapstat
	  WRITE(6,*) ''
	  WRITE(6,*) ' TYPE: ',event_time
	  WRITE(6,*) ' TIME: ',year,mdr,day,hour,min,sec
c	  WRITE(6,*) ' LAT : ',lat
c	  WRITE(6,*) ' LON : ',lon
c	  WRITE(6,*) ' DEP : ',dep
	  WRITE(6,*) ' OP  : ',event_op
	  WRITE(6,*) ' DB  : ',event_db
	  WRITE(6,*) ' onset:',rea_onset(1)
	  WRITE(6,*) ''
c       stop
      endif
c end call get_argnuemts
c          
c
c 2020-02-12T14:22:30.123Z
c 123456789012345678901234
         read(event_time(1:4),'(i4)') hyp_year(1)
         read(event_time(6:7),'(i2)') hyp_month(1)
         read(event_time(9:10),'(i2)') hyp_day(1)
         read(event_time(12:13),'(i2)') hyp_hour(1)
         read(event_time(15:16),'(i2)') hyp_min(1)
         read(event_time(18:21),'(f4.1)') hyp_sec(1)
         read(event_agency,'(a3)') hyp_agency(1)
         read(event_type,'(a1)') hyp_dist_id(1)
c
c   id
c
      CALL SYSTIME(p_time,PROC_TIME)
         rea_id_line=' '
         rea_id_line(80:80)='I'    
         rea_id_line(2:11)='ACTION:NEW' 
         rea_id_line(28:32)='OP:XX'
         read(event_op(1:2),'(a2)') rea_id_line(31:32)
         rea_id_line(36:42)='STATUS:'
         WRITE(rea_id_line(13:26),'(A)')PROC_TIME
         i=hyp_sec(1)
         write(rea_id_line(61:74),'(i4,6i2)')
     *   hyp_year(1),hyp_month(1),hyp_day(1),
     *   hyp_hour(1),hyp_min(1),i
         do i=61,74
            if(rea_id_line(i:i).eq.' ') rea_id_line(i:i)='0'
         enddo
         rea_id_line(76:76)='L'      
c 
c    hypocenter
c
      if(rea_nphase.EQ.1) then
         rea_year(1)=hyp_year(1)
         rea_month(1)=hyp_month(1)
         rea_day(1)=hyp_day(1)
         rea_hour(1)=hyp_hour(1)
         rea_min(1)=hyp_min(1)
         rea_sec(1)=hyp_sec(1)
	WRITE(6,*) 'stat,',rea_stat(1),rea_min(1),hyp_min(1)
      endif
c
c   open output file
c
       open(12,file='gsenor.out',status='unknown')
c         rea_nphase=k
c         nevent=nevent+1               ! count events
          rea_nhyp=1
          all=.true.
          call rea_event_out(12,all,data,code)
       close(12)
c
      return 
      end
c          
c          
cEOF
