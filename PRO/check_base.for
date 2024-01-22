c   program to check S-file data base
c 
c   jens havskov, oct 1999
C   uppdates:
c   oct  29 99 jh : able to do several data bases
c                   automatically find all bases, 
c                   posibility of only finding number of events
c   may 23 19  jh : new format, just include files
c   dec 16 19  jh : check for missing type 1 header line, increase 
c                   dimension of data base array.
c   mar 12 20  jh : also read from a file and other small changes
c   mar 15 21  jh : also check for not using directories when looking for REA dirs
c   may 4  21  jh : check for duplicate phases, must be P or S types, first 2 letters
c                   are checked, all is put to upper case particualrly so Pg and PG
c                   will be the same
c   nov 29 23  jh : improve duplicate phases. more wwite out, compare whole phase
c                   name. no change of case since it gives problems with other 
c                   phases.

        implicit none	
C
C    Seisan library inserts and routines...
C    ======================================
C
       include 'libsei.inc'                ! File operations.
       include 'seidim.inc'                ! Dimentions
       include 'seisan.inc'
       include 'rea.inc'
C
       external sei open,                  ! Open file routine.
     &          sei close,                 ! Close file routine.
     &          sei code                   ! Error encoder.
C
C    ============= end of list ==========
C
c event data
       character*80	data(max_data),data1	
       character*80     filename
c exp indicator
       character*1	exp		
c event type
       character*1      type
c number of recors for event
       integer		nrecord		
c  number of header lines
       integer		nhead		 
c number of stations
       integer		nstat, nphas
c start and end time of select
       character*14     start_time,end_time 
       character*40	base_name(1000)	
       character*5      bas(1000)      ! real base names
c event file name
       character*80	evfile	
c select key
       character*10     key		
       integer		status,new_month,fstart,event_no,error 
       integer bad_id,no_id,bad_s, no_type_one   ! error counters 		
       integer 		id		
c logical for existing file or not
       logical          b_old
c returned code      
       integer          code
c units        
       integer          readu,printu, no_id_u,bad_id_u,bad_s_u,
     *                  no_type_one_u,dup_phase_u,dup_phase_n
       integer dup_phase             ! number of events with duplicate phases
       integer nerr                  ! error indicator
       character*1      check        ! kind of check
       character*1      ucase        ! function
       logical          from_file    ! true if indput from a file 
       integer ibase, nbase,nb,i,k
c


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

      call get_seisan_def

c
c   open output files
c
      call sei open(unknown$,              ! Open a unknown status file. 
     &              ' ',                   ! Prompt file name (n/a).
     &              'check_base.out',      ! File name
     &              printu,                ! Print unit
     &              b_old,                 ! Already exists? (n/a).
     &              code)                  ! Returned condition.
      call sei open(unknown$,              ! Open a unknown status file. 
     &              ' ',                   ! Prompt file name (n/a).
     &              'index_no_id.out',     ! File name
     &              no_id_u,                ! Print unit
     &              b_old,                 ! Already exists? (n/a).
     &              code)                  ! Returned condition.
      call sei open(unknown$,              ! Open a unknown status file. 
     &              ' ',                   ! Prompt file name (n/a).
     &              'index_no_type_one.out',     ! File name
     &              no_type_one_u,                ! Print unit
     &              b_old,                 ! Already exists? (n/a).
     &              code)                  ! Returned condition.
      call sei open(unknown$,              ! Open a unknown status file. 
     &              ' ',                   ! Prompt file name (n/a).
     &              'index_bad_id.out',     ! File name
     &              bad_id_u,              ! Print unit
     &              b_old,                 ! Already exists? (n/a).
     &              code)                  ! Returned condition.
      call sei open(unknown$,              ! Open a unknown status file. 
     &              ' ',                   ! Prompt file name (n/a).
     &              'index_bad_s.out',     ! File name
     &              bad_s_u,               ! Print unit
     &              b_old,                 ! Already exists? (n/a).
     &              code)                  ! Returned condition.
      call sei open(unknown$,              ! Open a unknown status file. 
     &              ' ',                   ! Prompt file name (n/a).
     &              'index_dup_phase.out',     ! File name
     &              dup_phase_u,               ! Print unit
     &              b_old,                 ! Already exists? (n/a).
     &              code)                  ! Returned condition.

      call sei open(unknown$,              ! Open a unknown status file. 
     &              ' ',                   ! Prompt file name (n/a).
     &              'dup_phase.out',       ! File name
     &              dup_phase_n,           ! Print unit
     &              b_old,                 ! Already exists? (n/a).
     &              code)                  ! Returned condition.
c   
c   file or data base
c
       from_file=.false.
 1     continue
       write(6,'(a)') 
     *'Give file name or data base name, file name must have a .'
       write(6,'(a)') 'Enter to search for all data bases'
       read(5,'(a)') filename
       if(filename.ne.' ') then
         i=index(filename,'.')
         if(i.eq.0) then  ! a data base
            base_name(1)=filename(1:5)
            nbase=1
            goto 60
         else    ! a file
            open(1,file=filename,status='old',err=2)
            from_file=.true.
            write(6,'(a)')
     *     ' Enter data base name for creating output index files,'//
     *     ' enter is local data base'
            read(5,'(a)') base_name(1)(1:5)
c
c   fill out blanks in base name 
c
            if(base_name(1)(1:5).ne.'     ') then
               do i=2,5
                  if(base_name(1)(i:i).eq.' ') base_name(1)(i:i)='_'
                enddo
            endif     
            goto 80
 2          continue
            write(6,*)'No such file'
            goto 1  
         endif
       endif
c
c   get possible data bases
c
       call get_base_names(nb,bas)
       write(6,*)' Number of data bases',nb
       write(6,'(13(1x,a5))')(bas(i),i=1,nb)
c
c   input base name and time interval
c
       nbase=1
       write(6,*)
       write(6,*)
     *' Base name(s), * is all, ,, is local,'//
     *' else, one pr line, return for no more'
  50   continue
       read(5,'(a40)') base_name(nbase)
       if(base_name(nbase)(1:1).eq.'*') then
         nbase=nb
         do i=1,nb
           base_name(i)=bas(i)
         enddo
         goto 60
       endif
       if(base_name(nbase).eq.' ') then
          nbase=nbase-1
       else
          nbase=nbase+1
          goto 50
       endif
 60    continue
       write(6,*)' Start time'
       read(5,'(a14)') start_time
       write(6,*)' End time, return for end of month'
       read(5,'(a14)') end_time
       write(6,*)
     *' Check data base (c) or',
     *' only update statistics for program base(u)'
       read(5,'(a)') check
       check=ucase(check)
c
 3     continue
       key='          '		 
       
c
c  reset counters for errors
c
       no_id=0
       bad_id=0
       bad_s=0
       no_type_one=0
       dup_phase=0
c
c  read and check loop
c
       do ibase=1,nbase
 5     continue
       CALL findevin	
     * (base_name(ibase),start_time,end_time,key,0,
     * event_no,evfile,fstart,new_month,status)
c
c  check for end
c
       if(status.ne.0) go to 99	 
       if(check.eq.'U') goto 5
c
       call sei open(old$,                          ! Open a existing file.
     &                       ' ',                   ! Prompt file name (n/a).
     &                       evfile,                ! File name
     &                       readu,                 ! Read unit #1
     &                       b_old,                 ! Already exists? (n/a).
     &                       code)                  ! Returned condition.
   
c
c   read file
c
c
c   only single files
c
       nrecord=-1

       call indata
     * (readu,nstat,nphas,nhead,nrecord,type,exp,data,id)

       write(6,*) data(1)
       error=0
c
c   check if type 1 line, read again header line since indata repairs missing 
c   type 1
c
       rewind(readu)
       read(readu,'(a)') data(1)
       if(data(1)(80:80).ne.'1') then
          write(printu,'(a,a)') ' Error found in file: ',evfile
          no_type_one=no_type_one+1
          write(no_type_one_u,'(7x,a)') evfile
          write(printu,*)' Header line not type 1'
          write(6,*)' Header line not type 1'
          error=error+1
       endif

c
c   check if id line
c
       if(id.eq.0) then
          write(printu,'(a,a)') ' Error found in file: ',evfile
          write(printu,*)' No id line'
          write(6,*)' No id line'
          write(no_id_u,'(7x,a)') evfile
          no_id=no_id+1
          error=error+1
       endif
c
c  check if id line and file name are the same
c
       if(id.gt.0) then
       call check_id(evfile,type,data(id),nerr)
       if(nerr.ne.0) then
          write(printu,'(a,a)') ' Error found in file: ',evfile
          write(6,*)' File name and id different'
          write(printu,*)' File name and id different'
          write(bad_id_u,'(7x,a)') evfile
          bad_id=bad_id+1
          error=error+1
       endif
       endif
c
c  check s-file
c
       call check_s(data,nrecord,evfile,nerr,2,printu)
       if(nerr.gt.0) then
          write(printu,'(a)') data(1)(1:60)
          write(bad_s_u,'(7x,a)') evfile
          bad_s=bad_s+1
          error=error+1
       endif

       if(error.gt.0) then
          write(6,*)' ************** errros in file ****************'
       endif
c
c   check if duplicate phases
c

c
c   check if duplicate phases
c
       do i=1,rea_nphase-1
          do k=i+1,rea_nphase
c
c   make sure upper case, problem with e.g Pg and PG, but then
c   that gives a problems with e.g. PcP, so commented out
c
c              call sei upc(rea_phase(i)(2:2))

c
c   only use P and S types so a few others are not checked
c


            if((rea_phase(i)(1:1).eq.'P'.or.rea_phase(i)(1:1).eq.'S'.or.
     *          rea_phase(i)(1:1).eq.'p'.or.rea_phase(i)(1:1).eq.'s')
     *    .and.(rea_phase(k)(1:1).eq.'P'.or.rea_phase(k)(1:1).eq.'S'.or.
     *          rea_phase(k)(1:1).eq.'p'.or.rea_phase(k)(1:1).eq.'s')
     *     .and.rea_phase(k)(1:4).ne.'SPEC'
     *     .and.rea_phase(i)(1:4).ne.'SPEC')
     *     then
c
c  taken out checking only first 2 chars, not use why
c
c              if(rea_phase(i)(1:.eq.rea_phase(k)(1:2).and.
              if(rea_phase(i).eq.rea_phase(k).and.
     *        rea_stat(i).eq.rea_stat(k)) then
                 dup_phase=dup_phase+1
                 write(dup_phase_u,'(7x,a)') evfile
                 write(dup_phase_n,'(a)') evfile               
                 write(dup_phase_n,'(a,1x,a,3x,a,5x,a,1x,a,3x,a)')
     *           rea_stat(k),rea_com(k),rea_phase(k),
     *           rea_stat(i),rea_com(i),rea_phase(i)
                 write(dup_phase_n,*)
                 goto 55
             endif
             endif
         enddo
      enddo
 55   continue
c
c   close file
c
       call sei close (close$,readu,code)
c
c   back to next event
c  
       goto 5	

99    continue
      enddo              ! end of base check loop
      goto 199
c
c-----------------------------------------------------------
c   check a CAT file
c-----------------------------------------------------------
c
 80   continue	
      error=0
c
c   read header line to check for type 1, since indata repairs missing 1
c
       read(1,'(a)',end=199) data1
       backspace(1)

       call indata
     * (1,nstat,nphas,nhead,nrecord,type,exp,data,id)
       if(nrecord.eq.0) goto 199
       write(6,*) data(1)
c
c   make sfile name
c

       call make_sfile_name
     *(data,nrecord,nhead,id,type,base_name(1)(1:5),evfile)
c
c   check if 1 in type 1 line
c
       if(data1(80:80).ne.'1') then
          write(printu,'(a,a)') ' Error found in file: ',evfile
          no_type_one=no_type_one+1
          write(no_type_one_u,'(7x,a)') evfile
          write(printu,*)' Header line not type 1'
          write(6,*)' Header line not type 1'
          error=error+1
       endif	
c
c   check if id line
c
       if(id.eq.0) then
          write(printu,'(a,a)') ' Error found in file: ',evfile
          write(printu,*)' No id line'
          write(6,*)' No id line'
          write(no_id_u,'(7x,a)') evfile
          no_id=no_id+1
          error=error+1
       endif

c
c  check s-file
c
       call check_s(data,nrecord,evfile,nerr,2,printu)
       if(nerr.gt.0) then
          write(bad_s_u,'(7x,a)') evfile
          bad_s=bad_s+1
          error=error+1
       endif

       if(error.gt.0) then
          write(6,*)' ************** errros in file ****************'
       endif 

c
c   check if duplicate phases
c
       do i=1,rea_nphase-1
          do k=i+1,rea_nphase
c
c   make sure upper case, problem with e.g Pg and PG, but then
c   that gives a problems with e.g. PcP, so commented out
c
c              call sei upc(rea_phase(i)(2:2))

c
c   only use P and S types so a few others are not checked
c


            if((rea_phase(i)(1:1).eq.'P'.or.rea_phase(i)(1:1).eq.'S'.or.
     *          rea_phase(i)(1:1).eq.'p'.or.rea_phase(i)(1:1).eq.'s')
     *    .and.(rea_phase(k)(1:1).eq.'P'.or.rea_phase(k)(1:1).eq.'S'.or.
     *          rea_phase(k)(1:1).eq.'p'.or.rea_phase(k)(1:1).eq.'s')
     *     .and.rea_phase(k)(1:4).ne.'SPEC'
     *     .and.rea_phase(i)(1:4).ne.'SPEC')
     *     then
c
c  taken out checking only first 2 chars, not use why
c
c              if(rea_phase(i)(1:.eq.rea_phase(k)(1:2).and.
              if(rea_phase(i).eq.rea_phase(k).and.
     *        rea_stat(i).eq.rea_stat(k)) then
                 dup_phase=dup_phase+1
                 write(dup_phase_u,'(7x,a)') evfile
                 
                 write(dup_phase_n,'(a)') evfile
                 write(dup_phase_n,'(a,1x,a,3x,a,5x,a,1x,a,3x,a)')
     *           rea_stat(k),rea_com(k),rea_phase(k),
     *           rea_stat(i),rea_com(i),rea_phase(i)
                 write(dup_phase_n,*)
                 goto 155
             endif
             endif
         enddo
      enddo
155   continue
       
       goto 80   ! next event

 199   continue
 
c
c  print out statistics
c
       write(6,*)
       write(6,*)
     *' Number of events without id-lines:          ', no_id
        if(.not.from_file) write(6,*)
     *' Number of events with wrong id   :          ', bad_id
       write(6,*)
     *' Number of events with missing type 1:       ', no_type_one
       write(6,*)
     *' Number of events with duplicate phases:     ', dup_phase
       write(6,*)
     *' Number of events with other errors in file: ', bad_s
       

      write(6,*)
      write(6,*)
      write(6,*)' Output file name is:                  '// 
     *'              check_base.out'
      write(6,*)
     *' Index file with events with no id lines:            '//
     *'index_no_id.out' 
      if(.not.from_file) write(6,*)
     *' Index file with events with wrong id line:    '//
     *'      index_bad_id.out' 
       write(6,*)
     *' Index file with events with wrong type 1:      '//
     *'     index_no_type_one.out' 
       write(6,*)
     *' Index file with events with duplicate phase:   '//
     *'     index_dup_phase.out' 
       write(6,*)
     *' File with events and duplicate phases          '//
     *'     dup_phase.out' 
         write(6,*)
     *' Index file with events with other errors in S-file:'//
     *' index_bad_s.out'
      write(6,*)

      stop
      end
     

      subroutine get_base_names(nbase,base)
c
c     read all names from REA directory and sort out names 5 characters long
c    
c     
      implicit none
      character*60 top_dir     ! seisan path
      character*1 dchar        ! dir separation char
      logical sun,linux,pc     ! computer type
      character*5 base(*)      ! bases
      integer nbase            ! number of bases
      integer n,i,k
      character*80 all_files(500) ! all names in REA
      character*80 text
      integer seiclen          ! function


      call topdir(top_dir)
      call dir_char(dchar)
      call computer_type(sun,pc,linux)
c
      text=top_dir(1:seiclen(top_dir))//dchar//'REA' 
c
c   make a list of files
c
       call getfiles(text,seiclen(text),all_files,500,n)
c
c   find all with 5 chars and no .
c
       nbase=0
       do i=1,n
         if(seiclen(all_files(i)).ne.5) goto 100 
         do k=1,5
            if(all_files(i)(k:k).eq.'.'.or.all_files(i)(k:k).eq.dchar)
     *      goto 100
         enddo
         
         nbase=nbase+1
         base(nbase)=all_files(i)(1:5)
 100     continue
       enddo
c
c     
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine make_sfile_name
     *(data,nrecord,nhead,id,evtype,base,sfile_name)
c
c   make sfile name from s-file content. Routine is mostly taken from
c   program split.
c   the sfile name might not be the same as in the data base from which
c   data was collected, but it is the same as would be used if split up
c   again in current system. that again migh not be the same as used in data base
c   if s-files has no id line.if taken from a different system, the location
c   of REA might be different so index fields would not be correct.
c   
c
      implicit none
      include 'libsei.inc'                
      include 'seidim.inc'                
      include 'seisan.inc'
c-- INPUT FILE                                         
      CHARACTER*80 FILE				
c-- OUTPUT FILE NAME                                   
      CHARACTER*80 OUTFL,sfile_name			
cx      integer nf            ! length of file name
c-- EVENT TYPE, L,R OR D                              
      CHARACTER*1  EVTYPE			
c-- TOP DIR FOR REA               
      CHARACTER*40 TOP_DIRECTORY                
C-- SYSTEM TIME FOR USED FOR TIME OF OPERATION
C-- OPERATOR ID
c-- dir separator
      character*1 dchar
c-- RECORDS IN FILE                                
      CHARACTER*80 DATA(max_data)
c-- TIME IN SEC SINCE 0000                     
      DOUBLE PRECISION TIME			
c-- OLD -------------             
      DOUBLE PRECISION TIME_OLD                 
c-- MINIMUM TIME --------------   
      DOUBLE PRECISION MIN_TIME                 
c-- DAY OF YEAR, NOT USED                                    
      INTEGER DOY				
c-- DATES AND TIMES                             
      INTEGER YEAR,MONTH		        
     *,DAY,HR,MIN,ISEC                                                          
c-- SECONDS                                                    
      REAL SEC							
c-- INDICATOR FOR USE OF DATA BASE                      
        CHARACTER*5 BASE						
c-- NUMBER OF RECORDS FOR EVENT                          
      INTEGER NRECORD				
c-- NUMBER OF HEADERS FOR EVENT                            
      INTEGER NHEAD				
C-- ID LINE NUMBER
      INTEGER ID			
c-- COUNTER                                                  
      INTEGER I,K				
c   length of top_directory
      integer topdirlen

      call topdir(top_directory)
      topdirlen=index(top_directory,' ') -1
      call dir_char(dchar)

C
C   CHECK IF ID LINE PRESENT, IF NOT GET TIME FROM EVENT DATA
C
      IF(ID.EQ.0) THEN
C                                                                               
C   GET ABS TIME FOR  EVENT, READ YEAR,MONTH AND DAY FROM HEADER,               
C   HR, MIN AND SEC FROM EARLIEST STATION IF NOT GIVEN IN HEADER
C                                                                               
         READ(DATA(1),'(1X,i4,1X,2I2,1X,2I2,1X,F4.1)')                             
     *   YEAR,MONTH,DAY,HR,MIN,SEC
C                                                                               
C   CHECK IF HR AND MIN IN HEADER                                               
C                                                                               
c-- FIND EARLIEST TIME                   
         IF(DATA(1)(12:15).EQ.'    ') THEN		
            MIN_TIME=10E20                                                         
            DO I=NHEAD+1,NRECORD-1                                                 
               READ(DATA(I),'(18X,2I2,F6.1)') HR,MIN,SEC                        
c-- ABS TIME              
               CALL TIMSEC(YEAR,MONTH,DAY,HR,MIN,SEC,TIME)	
               IF(TIME.LT.MIN_TIME) MIN_TIME=TIME                                  
            ENDDO                                                                  
            CALL SECTIM(MIN_TIME,YEAR,DOY,MONTH,DAY,HR,MIN,SEC)                    
c--  EARLIEST TIME  
            WRITE(DATA(1)(1:20),'(1X,I4,1X,2I2,1X,2I2,1X,F4.1)') 
     *      YEAR,MONTH,DAY,HR,MIN,SEC                   
            DATA(1)(80:80)='1'
         ENDIF                                                                     
      ELSE
         READ(DATA(ID)(61:74),'(i4,5I2)')YEAR,MONTH,DAY,HR,MIN,ISEC
         SEC=ISEC
      ENDIF

C                                                                               
C  GENERATE OUTPUT FILE NAME                                                    
C                                                                               
      ISEC=SEC                                                                  
C                                                                               
C   CHECK IF DATA BASE OR NOT                                                   
C
      IF(BASE.NE.'     ') THEN
         WRITE                                                                  
     *   (OUTFL,307) dchar,BASE,dchar,YEAR,dchar,MONTH,dchar,
     *   DAY,HR,MIN,ISEC,EVTYPE,YEAR,MONTH
 307     FORMAT(
     *   a1,A5,a1,I4,a1,I2,a1,I2,'-',2i2,'-',i2,a1,'.S',i4,I2)               
C
         DO I=1,34                                                              
            IF(OUTFL(I:I).EQ.' ') OUTFL(I:I)='0'                                
         ENDDO  
         sfile_name = top_directory(1:topdirlen)//dchar//'REA'//outfl                                                          
      ELSE                                                                      
         WRITE(OUTFL,303)DAY,HR,MIN,ISEC,EVTYPE,'S',YEAR,MONTH                      
 303     FORMAT(I2,'-',2i2,'-',i2,a1,'.',a1,i4,I2)                                  
         DO I=1,19                                                                 
            IF(OUTFL(I:I).EQ.' ') OUTFL(I:I)='0'                                   
         ENDDO  
         sfile_name = outfl      
      ENDIF


      return
      end