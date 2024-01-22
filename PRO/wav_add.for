c--------------------------------------------------------------------------
c  
c  program to add waveform file names to s-file based on time
c
c  jh april 2022
c
c--------------------------------------------------------------------------c
c
c
c
c  changes
c

c
      implicit none                       ! force delcaration of all variables
      include 'seidim.inc'                ! dimensions for rea block
      include 'seisan.inc'                ! dimensions for rea block
      include 'rea.inc'                   ! parameter common block
      include 'libsei.inc'                ! for all seisan subroutines
      include 'waveform.inc'              ! waveform data

      character*80 data(5000)             ! s-file with data in text array
      character*80 text

      logical all                         ! true: read all data, false: headers
      integer seiclen                     ! function
      integer code                        ! error return code
      integer nevent                      ! number of events in file
      integer nwav                        ! number of waveform file names
      integer nerr                        ! number of errros reading wav files
      character*80 wav_name(10000)        ! file names in filenr.lis
      real*8 wav_time(10000)              ! abs time of wav file
      real*8 abs_first                    ! time of first channel                       
      integer wav_add_not(10000)          ! flag to indicate file not used
      real diff                           ! time dif. between s-file and wav file
      real max_diff                       ! max ------------------------------

      integer nadd                        ! number of wav files added
      character*10 keys                   ! next choice key for routine findevin
      character*14 starttime,endtime      ! time interval to read
      character*80 basename               ! data base or file name
      character*80 infile                 ! input file or base name
      character*80 eventfile              ! single event file name
      real*8 hyp_abs                      ! hyp abs time
      integer status,eventno,newmonth,fstart ! for routine findevin
      integer base                        ! type of input 0: seisan data base, 
                                          !     1: index file, 2: single file
      integer i,k                         ! counter

      call get_seisan_def
c
c  initialize 
c
      call wav_init
      call wav_mem_init
      max_diff=300.0
      nadd=0
      do i=1,10000
        wav_add_not(i)=0
      enddo
c
c   open output files
c
      open(2,file='wav_add.out',status='unknown')
      open(3,file='wav_add_not.out',status='unknown')
      open(4,file='wav_add_err.out',status='unknown')
    
      write(6,*)
     *'This program will read waveform file names from filenr.lis'
      write(6,*)
     *'A SEISAN data base or file will be opened'
      write(6,*)
     *'The origin time for the events will be compared with'
      write(6,*)'time of earlist channel in each wav file'
      write(6,*)
     *'If the time difference is smaller than a given value,'
      write(6,*)'the wav file name is added to the event s-file'
      write(6,*)
     *'All events, possibely modified, will be written out in a file'
      write(6,*)'The input source is not modified'
      write(6,*)
      write(6,*)
     *'Give max time difference between event and wav file,'
      write(6,*) '300s is default=enter'
      read(5,'(a)') text
      if(text.ne.' ') read(text,*) max_diff
      write(6,*)
    

c
c   read filenr.lis
c
      open(1,file='filenr.lis',status='old',err=1)
      goto 2
 1    continue
      write(6,*)' No filenr.lis file'
      stop

 2    continue

      nwav=1
      
 3    continue
      read(1,'(7x,a)',end=4) wav_name(nwav)
      if(wav_name(nwav).eq.' ') goto 4
      write(6,'(a,a)') 'File name      : ',wav_name(nwav)
c

c
c   get start time
c
c
c   read all headers of file nwaw
c
      call wav_init
      wav_filename(1)=wav_name(nwav)
      call read_wav_header(1)

c
c   output possible errors
c
      if(wav_error_message.ne.' ') then
        write(6,'(a)') wav_error_message
 
        nerr=nerr+1
        write(4,'(i5,1x,a)') nwav,wav_name(nwav)
        nwav=nwav-1
      else

        
        abs_first=10.0**20
        do i=1,wav_nchan
           if(wav_abs_time(i).lt.abs_first) then
              k=i
              abs_first=wav_abs_time(i)
           endif
        enddo
        write(6,'(a,i4,1x,i2,1x,i2,1x,2i2,f5.1)')'File start time: ', 
     *  wav_year(k),wav_month(k),
     *  wav_day(k),wav_hour(k),wav_min(k),wav_sec(k)

c
c   write out the format
c
         write(6,'(a,a)') 'Format ',wav_file_format(1)
c
c   use abs time from earliest channel for comparison
c
         wav_time(nwav)=wav_abs_time(k)
      endif  

      nwav=nwav+1
      goto 3

 4    continue
      close(1)
      nwav=nwav-1
      if(nwav.eq.0) then
         write(6,*) 'No file names in filenr.lis'
         stop
      endif
      write(6,*)

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

      write(6,*)
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
      call timsec(hyp_year(1),hyp_month(1),hyp_day(1),
     *hyp_hour(1),hyp_min(1),hyp_sec(1),hyp_abs)

c
c   check if a corresponding wav file, if so add to event
c
      do i=1,nwav
         diff=dabs(hyp_abs-wav_time(i))
         if(diff.le.max_diff) then
            rea_nwav=rea_nwav+1
            rea_wav(rea_nwav)=' '
            rea_wav(rea_nwav)(2:seiclen(wav_name(i))+1)=
     *      wav_name(i)(1:seiclen(wav_name(i)))
            rea_wav(rea_nwav)(80:80)='6'
            wav_add_not(i)=1   ! indicate added
            nadd=nadd+1
         endif
      enddo

c
c
c   write out modified event, the array data has also been modified
c
       call rea_event_out(2,all,data,code)
c
c   get next event
c
      goto 50
c
c     end of file or data base
c
 1000 continue
c
c     write files not added
c
      k=0
      do i=1,nwav
         if(wav_add_not(i).eq.0) then
            k=k+1
            write(3,'(i5,2x,a)') k,wav_name(i)
         endif
      enddo
c
      write(6,*)            ! blank line

      close(2)              ! close output files
      close(3)
      close(3)

      write(6,'(a,i4)')
     *'Number of waveform files in filenr.lis     :', nwav
      write(6,'(a,i4)') 
     *'Number of errors with waveform files       :', nerr
      write(6,'(a,i4)') 
     *'Number of events in input file or data base:', nevent
      write(6,'(a,i4)') 
     *'Number of wav files added                  :', nadd
      write(6,'(a,i4)') 
     *'Number of wav files not added              :', k
      write(6,'(a)') 
     *'Output file name is wav_add.out'
      write(6,'(a)') 
     *'Output file name of files not added is wav_add_not.out'
      write(6,'(a)') 
     *'Output file name of wav files with error is wav_add_err.out'

      stop
      end
