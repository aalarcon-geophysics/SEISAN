c
c   program to display the monitor status of netdet as
c   written by default in file netdet.now in netdet working directory
c   as specified in parameter file and start script.
c   if the default parameter file netdet.par is not used,  the  name of
c   the parameter must be given on the prompt. like if file is net1.par, then use net1
c   as argument
c
c   jh dec 2021
c
      implicit none

      character*130 text
      real*8 msec
      integer year,month,day,hour,min,isec
      real sec
      real lat,lon,depth,ml,mw           ! location and magnitude
      character*80 arg(5)                ! arguments to  program
      character*80 parfile               ! parameter file like netdet, without par
      character*40 netdet_dir            ! directory of netdet output
      integer nargs                      ! number of arguments
      integer old_minute, minute
      character*60 top_directory         ! seisan top dir
      character*60 last_trig             ! string with last trigger
      character*1 dchar                  ! dir separator
      integer i,k,seiclen

      write(6,*) char(7)
c                                                                               
c   get path to seismo                                                          
c                                                                               
      call topdir(top_directory)
c
      call dir_char(dchar)   ! directory separation character

      parfile=' '
      parfile(1:6)='netdet'   ! the ending of file is supposed to be .par

      last_trig=' '
c
c   get arguments
c
      call get_arguments(nargs,arg)

      if(nargs.eq.1.and.arg(1).ne.' ') parfile=arg(1)  ! this is netdet by default
c
c   open file and get netdet dir
c
      open(1,file=

     *top_directory(1:seiclen(top_directory))//dchar//'DAT'//dchar//
     *parfile(1:seiclen(parfile))//'.par',
     *status='old',err=5)
      goto 6

 5    continue
      write(6,'(a,a)')'Something wrong with opening parameter file ',
     *top_directory(1:seiclen(top_directory))//dchar//'DAT'//dchar//
     *parfile(1:seiclen(parfile))//'.par'
      stop

 6    continue

      do i=1,1000
         read(1,'(a)',err=5) text
         if(text(1:10).eq.'NETDET DIR') then
            netdet_dir=text(41:80)
            goto 7
         endif
      enddo
      close(1)
 7    continue

      if(netdet_dir.eq.' ') then
         write(6,*) 'netdet directory is not given in parameter file'
         stop
      endif

      old_minute=0

 1    continue

c
c   clear screen so message come in beginning
c      
      call systemc('clear',5)
      open(1,file=

     *netdet_dir(1:seiclen(netdet_dir))//dchar//
     *parfile(1:seiclen(parfile))//'.now',status='old',err=2)

      goto 3
 2    continue
      write(6,*)'Error opening file ',
     *parfile(1:seiclen(parfile)-4)//'.now in '
     *,netdet_dir(1:seiclen(netdet_dir))
      close(1)
      goto 4
 3    continue 


      read(1,'(a)') text    ! par file name

      read(1,'(a)') text    ! start time

      read(text(71:80),'(i10)') minute ! number of minutes since start
c
c   check if netdet is running
c
      if(minute.gt.old_minute) then
         write(6,'(a)')'        N E T D E T  I S  R U N N I N G'
      else
         write(6,'(a)')'        N E T D E T  I S  N O T  R U N N I N G'
      endif

      write(6,'(a,a)') 
     *'Parameter file used: ',parfile(1:seiclen(parfile))//'.par'

      old_minute=minute

      write(6,'(a)') text(1:60)   
      read(1,'(a)') text          ! run time
      write(6,'(a)') text(1:60)
      read(1,'(a)') text          ! channels 
      write(6,'(a)') text(1:60)
      read(1,'(a)') text          ! last trig and locaiton
c
c   beep if a new trigger
c
      if(text(1:60).ne.last_trig.and.seiclen(text(1:60)).gt.0.
     *and.seiclen(last_trig).gt.0) then
        do i=1,10
          write(6,*) char(7)
        enddo
        last_trig=text(1:60)
      endif      

      write(6,'(a)') text(1:60)   ! last trig
c
c   last location write out
c

      if(text(61:62).ne.'No') then
         read(text(61:80),'(i4,1x,2i2,1x,2i2,1x,f4.1)') 
     *   year,month,day,hour,min,sec
         isec=sec
         if(year.ne.0) then
            write(6,'
     *      (a,i4,1x,i2.2,a,i2.2,1x,i2.2,a,i2.2,a,i2.2)')
     *      'Time of last location       :  ', 
     *       year,month,'-',day,hour,':',min,':',isec
             write(6,'(a)') text(82:130)  ! location
         else
            write(6,'(a)')
     *      'Time of last location :        '
         endif
      else
         write(6,'(a)') text(61:91)
      endif 


      close(1)

 4    continue

c
c  wait 60 s
c
      do i=1,60
        call sleep(1)
c
c  get and print gmt time overwriting the same line
c
        call gmt_time(year,month,day,hour,min,sec,msec)
        isec=sec
        write(text,'(a,i4,1x,i2.2,a1,i2.2,1x,i2.2,a1,i2.2,a1,i2.2)') 
     *  'Current GMT time            :  ',
     *  year,month,'-',day,hour,':',min,':',isec
        write(6,10,advance='NO') text(1:59)//char(13)
 10     format(a)
      enddo
      goto 1

      stop
      end      


       subroutine gmt_time(year,month,day,hour,min,sec,abstime)
c
c  get gmt time, jh dec 2021
c
       implicit none
       integer date_time(8)
       integer year,month,day,hour,min,doy
       real sec
       real*8 abstime

       character*10 b(3)

       call date_and_time(b(1), b(2), b(3), date_time) ! system routine

c       print *,'date_time    array values:'
c       print *,'year=',date_time(1)
c       print *,'month_of_year=',date_time(2)
c       print *,'day_of_month=',date_time(3)
c       print *,'time difference in minutes=',date_time(4)
c       print *,'hour of day=',date_time(5)
c       print *,'minutes of hour=',date_time(6)
c       print *,'seconds of minute=',date_time(7)
c       print *,'milliseconds of second=',date_time(8)
c       print *, 'DATE=',b(1)
c       print *, 'TIME=',b(2)
c       print *, 'ZONE=',b(3)

       
       sec=date_time(7)+float(date_time(8))/1000.0

       call timsec(date_time(1),date_time(2),date_time(3),
     * date_time(5),date_time(6),sec,abstime)
c
c   add ofset to gmt
c
       abstime=abstime-date_time(4)*60.0
c
       call sectim(abstime,year,doy,month,day,hour,
     &       min,sec)  

       return
       end  
