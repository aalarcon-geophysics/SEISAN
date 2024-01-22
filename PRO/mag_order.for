c--------------------------------------------------------------------------
c  mag_order, Reordring of magntudes for prime hypocenter
c
c  The magnitudes are reordered with largest one first. All possible
c  6 magnitudes are written from left to right so there are no magnitude gaps
c  on header line(s). So if e.g. there is no magnitude in pos 2 and one in 3,
c  the one in 3 is moved. 
c
c  The input can come from a file, a seisan data base or an index file
c
c  Some parameters are then modified and the modified event is written
c  to the output file.
c--------------------------------------------------------------------------c
c
c  For detail on parameters and variables names, see rea.inc
c
c
c  changes
c

c
      implicit none                       ! force delcaration of all variables
      include 'seidim.inc'                ! dimensions for rea block
      include 'seisan.inc'                ! dimensions for rea block
      include 'rea.inc'                   ! parameter common bliock

      character*80 data(5000)             ! s-file with data in text array

      logical all                         ! true: read all data, false: headers
      integer seiclen                     ! function
      integer code                        ! error return code
      integer nevent                      ! number of events in file
      character*10 keys                   !next choice key for routine findevin
      character*14 starttime,endtime      ! time interval to read
      character*80 basename               ! data base or file name
      character*80 infile                 ! input file or base name
      character*80 eventfile              ! single event file name
      integer status,eventno,newmonth,fstart ! for routine findevin
      integer base                        ! type of input 0: seisan data base, 
                                          !     1: index file, 2: single file
      real mag(6)                         ! magntudes for main hyp
      integer isort(6)                    ! for sorting magnitudes
      integer i,k                         ! counter

      call get_seisan_def
c
c   open output file
c
      open(2,file='mag_order.out',status='unknown')
    
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
c
c  put magnitudes in order
c

c
c   remove blank magnitudes
c
      k=0
      do i=1,6
         if(hyp_mag(i,1).ne.-999.0) then
            k=k+1
            hyp_mag(k,1)=hyp_mag(i,1)
            hyp_mag_type(k,1)=hyp_mag_type(i,1)
            hyp_mag_agency(k,1)=hyp_mag_agency(i,1)
         endif
      enddo
c
c   shift left
c
      if(k.lt.6) then
         do i=k+1,6
           hyp_mag(i,1)=-999.0
         enddo
      endif
c
c   sort according to size
c
      do i=1,k
        mag(i)=hyp_mag(i,1)
      enddo
      call r4sort(k,mag,isort)
c
c   reorder
c
      do i=1,k
        hyp_mag(k-i+1,1)=mag(isort(i))
      enddo


c
c   write out modified event
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
      write(6,*)            ! blank line
      close(2)              ! close output file
      write(6,*) 'Number of events in input file or data base', nevent
      write(6,*) 'Output file name is mag_order.out'

      stop
      end
