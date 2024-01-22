      subroutine rewrite_sfile
     *(read01,sfil,base,proc_time,operator,data,update_code)
c
c  routine runs the split function on file on read01 in seisan data base base.
c  only one event is written out. The output file name is sfil.
c  proc_time and operator as in program update.
c  special for this version is that the file name and id is always taken
c  from the origin time since it is used in connection with update.
c  modified from split dec 96, see also program split. Routine assumes that
c  the input file is there and that the id line is present.
c  update_code is for id line
c
c  variable data just put in window to share memory space with main program
c
c
       implicit none

C
C    Seisan library inserts and routines...
C    ======================================
C
       include 'seidim.inc'
       include 'libsei.inc'                ! Open file definitions
       include 'seisan.inc'
       include 'rea.inc'
C
       external sei open,                  ! Open file routine.
     &          sei close,                 ! Close file routine.
     &          sei code                   ! Error encoder.
C
C    ============= end of list ==========
C
c-- OUTPUT FILE NAME                                   
      CHARACTER*80 OUTFL
      integer nf            ! length of file name
c-- EVENT TYPE, L,R OR D                              
      CHARACTER*1  EVTYPE	
      character*3 update_code ! code for id_line		
c-- idline
      character*80 idline,old_id_line
c-- TOP DIR FOR REA               
      CHARACTER*40 TOP_DIRECTORY                
c-- dir separator
      character*1 dchar
C-- SYSTEM TIME FOR USED FOR TIME OF OPERATION
      character*14 proc_time 
C-- OPERATOR ID
      CHARACTER*4 OPERATOR
c-- HELP FORMAT LINE 0: NO, 1: YES               
      INTEGER FORMAT_INDICATOR			
c-- RECORDS IN FILE                                
      CHARACTER*80 DATA(*)			
c-- DATES AND TIMES                             
      INTEGER century,YEAR,MONTH		        
     *,DAY,HR,MIN,ISEC                                                          
c-- SECONDS                                                    
      REAL SEC					
c-- EXPLOSION INDICATOR                                
      CHARACTER*1 EXP	  			
c-- INDICATOR FOR USE OF DATA BASE                      
      CHARACTER*5 BASE				
c-- NUMBER OF STATIONS                                     
      INTEGER NSTAT				
c-- NUMBER OF RECORDS FOR EVENT                          
      INTEGER NRECORD				
c-- NUMBER OF HEADERS FOR EVENT                            
      INTEGER NHEAD				
c-- NUMBER OF PHASES FOR EVENT	  
      INTEGER NPHASE
C-- ID LINE NUMBER
      INTEGER ID
c-- TIME IN SEC SINCE 1900 EVENT                    
      DOUBLE PRECISION TIME			
c-- MINIMUM TIME --------------   
      DOUBLE PRECISION MIN_TIME                 
c-- DAY OF YEAR, NOT USED                                    
      INTEGER DOY				
c-- COUNTER                                                  
      INTEGER I,K	
c   length of top_directory
      integer topdirlen
c  agency
      character*5 agency
      character*80 sfil    ! name of sfile returned
c logical for end of file
       logical          b_eof
c logical for existing file or not
       logical          b_old
c returned code
       integer          code
c read unit #1 
       integer          read01
c write unit #1
       integer          write01
c file name
       character*80     chr_file
C                                                                               
C    SET DEFAULTS AND INITIAL PARAMETERS                                        
C
      call topdir(top_directory)
      call get_def_base(agency)
      topdirlen=index(top_directory,' ')-1
      call dir_char(dchar)
      b_f_debug$=.false.                  ! file debugging

c
C  rewind FILE                                                                   
C                         
      rewind read01              
C
C   START OF LOOP-------------------------------------------------
C                                       
                                                                                
 1    CONTINUE                                                                  
C                                                                               
C   READ  EVENT                                                                 
C   
         CALL INDATA(read01,NSTAT,NPHASE,NHEAD,NRECORD,
     *   EVTYPE,EXP,DATA,ID)


C                                                                               
C   CHECK FOR EOF                                                               
C                                                                           
      IF(NRECORD.EQ.0) GO TO 2
C
C                                                                               
C   CHECK IF FORMAT EXPLANATORY LINE IN FILE                        
C                                                                               
      FORMAT_INDICATOR=0                                                        
      DO K=1,NHEAD                                                              
         IF(DATA(K)(80:80).EQ.'7') FORMAT_INDICATOR=1                           
      ENDDO                                                                     
C                                                                               
C   GET ABS TIME FOR  EVENT, READ YEAR,MONTH AND DAY FROM HEADER,               
C   HR, MIN AND SEC FROM EARLIEST STATION IF NOT GIVEN IN HEADER
C                                                                               
      READ(DATA(1),'(1X,2I2,1X,2I2,1X,2I2,1X,F4.1)')                             
     *CENTURY,YEAR,MONTH,DAY,HR,MIN,SEC
C
C   ASSUME 1900 IF NO CENTURY GIVEN
C
      IF(CENTURY.EQ.0) CENTURY=19
      year=century*100+year                                                               
C                                                                               
C   CHECK IF HR AND MIN IN HEADER                                               
C                                                                               
c-- FIND EARLIEST TIME if phase records available, used if no hr min in header
c                
      IF(DATA(1)(12:15).EQ.'    '.and.(nrecord-1).gt.nhead) THEN		
         MIN_TIME=10E20                                                         
         DO I=NHEAD+1,NRECORD-1  
c            if(new_nordic_format) then
            if(rea_new_in) then
               read(data(i)(27:37),'(2i2,f7.3)') hr,min,sec
            else                                               
               READ(DATA(I),'(18X,2I2,1X,F5.1)') HR,MIN,SEC
            endif                        
c-- ABS TIME              
            CALL TIMSEC(YEAR,MONTH,DAY,HR,MIN,SEC,TIME)	
            IF(TIME.LT.MIN_TIME) MIN_TIME=TIME                                  
         ENDDO                                                                  
         CALL SECTIM(MIN_TIME,YEAR,DOY,MONTH,DAY,HR,MIN,SEC)                    
c--  EARLIEST TIME  
         WRITE(DATA(1)(1:20),'(1X,I4,1X,2I2,1X,2I2,1X,F4.1)') 
     *   YEAR,MONTH,DAY,HR,MIN,SEC                   
         DATA(1)(80:80)='1'
      ENDIF                                                                 
c
C                                                                               
C  GENERATE OUTPUT FILE NAME FROM ORIGIN TIME or earliest time of phases                                                  
C     
      ISEC=SEC      
C
      do i=2,5
        if(base(i:i).eq.' ') base(i:i)='_'
      enddo
      IF(BASE.NE.'     '.and.base(1:2).ne.',,') THEN
         WRITE                                                                  
     *   (OUTFL,307)dchar,BASE,dchar,YEAR,dchar,MONTH,dchar,DAY,HR,
     *   MIN,ISEC,EVTYPE,YEAR,MONTH           
 307     FORMAT(
     *   a1,A5,a1,I4,a1,I2,a1,I2,'-',2i2,'-',i2,a1,'.S',i4,I2)               
C
         DO I=1,34                                                              
            IF(OUTFL(I:I).EQ.' ') OUTFL(I:I)='0'                                
         ENDDO   
         nf=34                                                               
      ELSE                                                                      
         WRITE(OUTFL,303)DAY,HR,MIN,ISEC,EVTYPE,'S',YEAR,MONTH                      
 303     FORMAT(I2,'-',2i2,'-',i2,a1,'.',a1,i4,I2)                                  
         DO I=1,19                                                                 
            IF(OUTFL(I:I).EQ.' ') OUTFL(I:I)='0'                                   
         ENDDO
         nf=19          
      ENDIF
c
c   make id line if not there, else update current id line
c   also save old id line
c
      old_id_line=' '
      if(id.ne.0) then
         idline=data(id)
         old_id_line=idline
         old_id_line(2:7)='OLDACT'
         old_id_line(80:80)='3'
      endif

      if(id.eq.0) THEN
         idline(1:40)= ' ACTION:                   OP:     STATU'
         idline(41:80)='S:               ID:                   I'
      endif
c
c  update id line
c

      WRITE(IDLINE(31:33),'(A)')OPERATOR(1:3)
      WRITE(IDLINE(13:26),'(A)')PROC_TIME
      WRITE(IDLINE(9:11),'(A)') update_code
      WRITE(IDLINE(61:75),'(i4,5I2)')
     *YEAR,MONTH,DAY,HR,MIN,ISEC
      DO I=61,74
         IF(IDLINE(I:I).EQ.' ') IDLINE(I:I)='0'
      ENDDO

c
c   close and delete input file
c
      close (read01,status='delete') ! close
C                                                                               
C  OPEN AND WRITE OUTPUT FILE                                                   
C
 3    continue                 ! get here from below when file did exist
      IF(BASE(1:2).NE.',,') THEN                                                    
         chr_file = top_directory(1:topdirlen)//dchar//'REA'//outfl
            call sei open(unknown$+warn$,        ! Open a unknown status file.
     &                    ' ',                   ! Prompt file name (n/a).
     &                    chr_file,              ! File name
     &                    write01,               ! Write unit #1
     &                    b_old,                 ! Already exists? (n/a).
     &                    code)                  ! Returned condition.
            sfil=chr_file
      ELSE
            call sei open(unknown$+warn$,        ! Open a unknown status file.
     &                    ' ',                   ! Prompt file name (n/a).
     &                    outfl,                 ! File name
     &                    write01,               ! Write unit #1
     &                    b_old,                 ! Already exists? (n/a).
     &                    code)                  ! Returned condition.
            sfil=outfl
      ENDIF
c
c   make new index
c
      if(b_old) then
          write(6,*)' Creating a new id for event'
          call inc_id(idline,outfl,nf)
          call sei close (close$,write01,code) ! close
          goto 3                            ! go and try to open again
      endif
c
  10  continue 
C
C   WRITE FILE
C                                  

C                                                                     
c-- ORIGINAL HEADER LINES FIRST
C   IF NO ID LINE, PUT IN AS NUMBER 2
C
          write(write01,'(a80)',iostat=code) data(1)
          call sei code(fort$,code,write01,b_eof)
c
c   put in id update indicator
c
      idline(76:76)='S'
c
c  if new id line, write as #2, else where it was before
c
      if(id.eq.0) then
          write(write01,'(a80)',iostat=code) idline
          call sei code(fort$,code,write01,b_eof)
      else
         data(id)=idline
      endif
c
c   write rest of header lines, skip format help lines
c
      if(nhead.ge.2) then
	 do i=2,nhead 
            if(data(i)(80:80).ne.'7') then
               write(write01,'(a80)',iostat=code) data(i)
            endif
	 enddo
         if(old_id_line.ne.' ') write(write01,'(a)') old_id_line
      endif
      call sei code(fort$,code,write01,b_eof)
C
c-- write help line 
C
      if(new_nordic_format) then
         write(write01,244,iostat=code)
      else
         write(write01,243,iostat=code)
      endif

      call sei code(fort$,code,write01,b_eof)
 243  FORMAT(
     *' STAT SP IPHASW D HRMM SECON CODA AMPLIT PERI AZIMU VELO ',             
     *'AIN AR TRES W  DIS CAZ7')  
 244  Format(
     *' STAT COM NTLO IPHASE   W HHMM SS.SSS   PAR1  PAR2 AGA',
     *  ' OPE  AIN  RES W  DIS CAZ7')             
c-- WRITE REST OF EVENT         
       write(write01,'(a80)',iostat=code)
     * (data(i),i=nhead+1,nrecord)
      call sei code(fort$,code,write01,b_eof)
      call sei close (close$,write01,code)
c
 2    continue

      return                                                                      
      END                                                                       
                                                                                
