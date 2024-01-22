c                                                                      
c   modified version of collect to make automatic backup of a
c   data base
c
c   jens havskov, jun 2023                                           
c     
c
c   changes:
c
c   dec 10 2023 jh:  make backup for x number days back from real time
c
c   The program be operated manually. However, it is intended to be used
c   with a cron job to automatically backup a S-file data base. 
c   The program collects all S-files in a given time
c   interval for a given data base and optionally compresses the output
c   file. The input is only from  the prompt:
c
c   options: -start_time: time to start yyymmddhhmmss, at least yyyy
c                        if not given, use 1900, if less than 1000
c                        it is number fo days back from current time
c           -end_time  : time to end, if not given, use current time
c           -base_name : data base, if not given use default data base
c           -out       : directory ending with / where files are written
c                        if not given, working directory is used.
c                        file name contans base name and date of creation
c           -out_full  : complete name and path, no date and base in name
c           -compress  : name of compress program, if blank no compression
c
c   if using all defults, the program will collect all data in the
c   default data base from 1900 to the current date and write a file
c   with a name like 
c   
c   backup-sfiles.TEST_.20230630090515
c
c   in COM, there is an example script to be used for a cron job 

c   uppdates                                                      
C
      implicit none
      include 'seidim.inc'
      include 'rea.inc'
      include 'seisan.inc'
C
C    Seisan library inserts and routines...
C    ======================================
C
       include 'libsei.inc'                ! Open file definitions
C
       external sei open,                  ! Open file routine.
     &          sei close,                 ! Close file routine.
     &          sei code                   ! Error encoder.
C
C    ============= end of list ==========
C
c-- event data
       character*80	data(max_data)	
       character*80     text
c-- exp indicator                                         
       character*1	exp		
c-- event type                                      
       character*1      type		
c-- command to compress file	  
       character*80      compress            
c-- number of recors for event                           
       integer		nrecord		
c-- number of header lines                                 
       integer		nhead		
c-- number of stations                                     
       integer		nstat		
c-- number of phases
       integer nphase
c-- start and end time of select      
       character*14    start_time,end_time 
c-- data base name if not agency (blank)               
       character*40	base_name	
c-- event file name                                   
       character*80	evfile		
c-- select key                                       
       character*10     key		
c-- see subroutine find...       
       integer		status,new_month,fstart,event_no 
c-- counters for events, records etc.      
       integer nr,nd,nl,records,nevent	
c-- counter                                                   
       integer 		i		
c-- true if file exists (CLK)             
       logical          exist
c-- current timec
      character*14 proc_time       
      character*80 outfile
      character*180 out_location     ! directory for output file
c-- Arguments passed
       character*80     arg(10)
c-- id line number
       integer id
c---number of arguments and function
      integer nars
c---time and date
      integer year,month,day,hour,min,sec
      integer year1,month1,day1,hour1,min1,sec1
      real    xsec,xsec1
      real*8  difsec
      integer days_back        ! days back from real time to start
c logical for end of file
       logical          b_eof
c logical for existing file or not
       logical          b_old
c returned code
       integer          code
c read unit #1
       integer          read01
c write unit  #1
       integer          write01
       integer seiclen
       include 'version.inc'
c
c print version
c
      call print_ver
c        
c   get seisan defaults
c
      call get_seisan_def
c
c   check s-file format
c
      if(new_nordic_format) then
         rea_new_out=.true.              ! write new format
      endif
c
c   get current time
c
       call systime_full(proc_time)

       start_time = ' '
       end_time=' '
       compress=' '
       nars = 0
       base_name = ' '
       outfile=' '

       call get_arguments(nars,arg)    ! get arguments 

       if(nars .ne. 0) then
 
         do i = 1,nars
            if(arg(i)(1:11) .eq. '-start_time')then
                read(arg(i+1),'(a14)') start_time
            endif
            if(arg(i)(1:9) .eq. '-end_time')then
              read(arg(i+1),'(a14)') end_time
            endif
            if(arg(i)(1:10).eq. '-base_name')then
              read(arg(i+1),'(a40)') base_name
            endif
            if(arg(i)(1:9).eq.'-out_full') then 
              read(arg(i+1),'(a)') outfile
            else
               if(arg(i)(1:4) .eq. '-out')then
                 read(arg(i+1),'(a)') out_location 
               endif
            endif
            if(arg(i)(1:9) .eq. '-compress')then
              read(arg(i+1),'(a)') compress
            endif

            if(arg(i)(1:2).eq.'-h')then
              write(*,*)'    Command line input:'
              write(*,*)'-start_time yymmdd...'
              write(*,*)'-end_time yyyymmdd...(blank=end of month)'
              write(*,*)'          number less than 1000 is number of'//
     *                  ' days back'
              write(*,*)'-base_name XXX (blank=default)'
              write(*,*)'-compress name of compress program if used'
              write(*,*)'-out: directory to write standard file'
              write(*,*)'-out_full: full file name of output'
              stop
            endif
         enddo
       endif
       
       if(end_time.eq.' ') end_time=proc_time
       if(base_name.eq.' ') call get_def_base(base_name)
c
c   make output file name if not given
c
       if(outfile.eq.' ') then
          outfile=
     *    out_location(1:seiclen(out_location))//'backup-sfiles.'
     *    //base_name(1:seiclen(base_name))//'.'//proc_time
          write(6,*)outfile
       endif
c
       if(start_time .eq. ' ') start_time='1900' 
c
c   check if start time is days back indicated by start time is a number
c   less than 1000
c
       read(start_time,*) days_back
       if(days_back.lt.1000) then
c
c   calculate start time
c
           xsec=sec
           read(proc_time,'(i4,5i2)') year,month,day,hour,min,sec
           difsec=-days_back*86400.0
           call timadd(year,month,day,hour,min,xsec,difsec,
     *     year1,month1,day1,hour1,min1,xsec1)
           sec1=xsec1
           write(start_time,'(i4,5i2)')year1,month1,day1,hour1,min1,sec1
       endif

	                                                                    
c                                                                               
c  reset counters                                                               
c
                                                                               
       nl=0                                                                     
       nr=0                                                                     
       nd=0                                                                     
       nevent=0                                                                 
       records=0                                                                
c                                                                               
c   open output file                                                            
c                                                                               
            call sei open(unknown$+warn$,        ! Open a unknown status file.
     &                    ' ',                   ! Prompt file name (n/a).
     &                    outfile,               ! File name
     &                    write01,               ! Write unit #1
     &                    b_old,                 ! Already exists? (n/a).
     &                    code)                  ! Returned condition.

c                                                                               
c  start read and write loop                                                    
c                                                                               
 5     continue                                                                 
c-- always use next event                                
       key='          '		
       CALL findevin                                                       
     * (base_name,start_time,end_time,key,0,
     * event_no,evfile,fstart,new_month,status)                                 
       if(status.eq.0) then                                                     
c--   (CLK)     
         inquire(file=evfile,exist=exist)                         
c--   (CLK)     
         if (exist) then                                          
            call sei open(old$+warn$,            ! Open an old file.
     &                    ' ',                   ! Prompt file name (n/a).
     &                    evfile,                ! File name
     &                    read01,                ! Read unit #1
     &                    b_old,                 ! Already exists? (n/a).
     &                    code)                  ! Returned condition.
         nrecord=-1    ! read to end of file
         call indata(read01,nstat,nphase,nhead,nrecord,type,exp,data,id)
c
c                                                                               
c   count events etc                                                            
c                                                                               
           if(type.eq.'L') nl=nl+1                                              
           if(type.eq.'R') nr=nr+1                                              
           if(type.eq.'D') nd=nd+1                                              
           records=records+nrecord                                              
           nevent=nevent+1                                                      
c                                                                               
c  write event                              
c                                                                               
          write(write01,'(a80)',iostat=code)
     *          (data(i),i=1,nrecord)
          call sei code(fort$,code,write01,b_eof)
c                                                                               
c  print event name                                                             
c                                                                               
           write(6,'(1x,a79)') evfile(1:79)
           call sei close (close$,read01,code)

c-- (CLK)
         else                                                      
c-- (CLK)      
           write(6,'(1x,a60,a)') evfile(1:60),' doesn''t exist.'         
c-- (CLK)      
         endif                                                     
c-- back for next event                                         
         goto 5			
       else                                                                     
c-- 3 is end of time period                       
          if(status.eq.2) write(6,*)' End of index file'
          if(status.gt.3)       
     *    write(6,*)' ****** Something wrong, status= ', status                 
       endif                                                                    
c
c  optionally compress using command compresss
c
       if(compress.ne.' ') then
          text=compress(1:seiclen(compress))//' '
     *    //outfile(1:seiclen(outfile))
          write(6,*) text
          call systemc(text(1:seiclen(text)),seiclen(text))
       endif

c                                                                               
c  print out statistics                                                         
c                                                                               
      write(6,*)                                                                
      write(6,200)' Output file: ', outfile                                                       
      if(compress.ne.' ') write(6,'(a,a)')
     *' The file is compressed with ',compress(1:seiclen(compress))
 200  format(a,a)                                        
      write(6,*)                                                                
      write(6,201) nevent,nl,nr,nd,records                                      
 201  format(' Total number of events          ',i7,
     *     /,' Total number local events       ',i7                             
     *     /,' Total number of regional events ',i7                             
     *     /,' Total number of distant events  ',i7                             
     *     /,' Total number of records         ',i7)
      call sei close (close$,write01,code)
      stop                                                                      
      end                                                                       
                                                                                
                                                                                
