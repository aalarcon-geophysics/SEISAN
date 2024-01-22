c--------------------------------------------------------------------------
c  Program for converting between old and new nordic format, nordic2
c  
c  read  events, determines format, write out in new or old
c
c  jh may 2019     
c
c  arguments    -format 1: nordic, 1 nordic2
c               -file  input file
c
c  apr 15 2020 jh: set dim of data to max_data
c  jan 22 2021 pv: added arguments for aga and opr, to be added when converting to new format
c  dec 06 2023 jh: if converting without phases, the type 7 line, is not rewritten if forcing to
c                  old format.
c                 
c
      implicit none                       ! force delcaration of all variables
      include 'seidim.inc'                ! dimensions for rea block
      include 'seisan.inc'                ! dimensions for rea block
      include 'rea.inc'                   ! parameter common bliock

      character*80 data(max_data)         ! s-file with data in text array

      logical all                         ! true: read all data, false: headers
      integer seiclen                     ! function
      integer code                        ! error return code
      integer nevent                      ! number of events in file
      character*10 keys                   ! next choice key for routine findevin
      character*80 infile                 ! input file or base name
      character*1 answer                  ! answer
      integer nars                        ! number of arguments
      character*80 arg(10)                ! arguments

c     integer base                        ! type of input 0: seisan data base, 
                                          !     1: index file, 2: single file
      integer i                           ! counter
      character*3 fixaga                  ! Agency code
      character*3 fixope                  ! operator id

c
c   in this program do not read seisan.def since the program should
c   be able to convert from any format to any format independently
c   of what was set in SEISAN.DEF
c

      new_nordic_format_only=.false.   ! do not force new only
      new_nordic_format=.true.         ! can read old or new

      fixaga='   '
      fixope='   '

      call get_arguments(nars,arg)

      if(nars.gt.0) then
         do i=1,nars
            if(arg(i).eq.'-file') infile=arg(i+1)
            if(arg(i).eq.'-format') answer=arg(i+1)
            if(arg(i).eq.'-ope') fixope=arg(i+1)
            if(arg(i).eq.'-aga') fixaga=arg(i+1)
         enddo
         goto 1
      endif


      write(6,*)' Give output format, nordic (1) or nordic2 (2) = enter'

      read(5,'(a)') answer

 1    continue
c
c   write out event in required format, new or old 
c
      if(answer.eq.'1') then
         rea_old_format_required=.true.
      endif
c
c   open output file
c
      open(2,file='nor2nor2.out',status='unknown')
    
c
c   get input file name
c

      if(nars.eq.0) then
         write(*,*) 'Input file'

         write(*,*)
         read(*,'(a)') infile
      endif



      open(1,file=infile,status='old',err=5)
      goto 6
 5    continue
      write(6,*)' Input file not found'
      stop
 6    continue
 
      all=.true.                  ! read all parameters
      nevent=0                    ! initialize counter

c
c-----------------------------------------------------------------
c  Loop to read events start here
c-----------------------------------------------------------------
c

  50  continue

c
c   read all parameters for one event from file unit 1
c
      call rea_event_in(1,all,data,code)
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
c   write on screen a bit info on event, number of headers and
c   total number of records
c
      write(6,*)' Number of headers and number of records',
     *rea_nhead,rea_nrecord


       do i=1,rea_nrecord
          if(i.GT.rea_nhead.AND.answer.EQ.'2'.AND.i.LT.rea_nrecord) then
            if(fixaga.NE.'   ')data(i)(52:54)=fixaga(1:3)
            if(fixope.NE.'   ')data(i)(56:58)=fixope(1:3)
          endif
c
c  make sure 7 header is ok if forcing to converting to old format
c
          if(answer.eq.'1'.and.data(i)(80:80).eq.'7')
     *    data(i)=                                                        
     *  ' STAT SP IPHASW D HRMM SECON CODA AMPLIT PERI AZIMU VELO '//             
     *  'AIN AR TRES W  DIS CAZ7'  
          write(2,'(a80)') data(i)
       enddo
c
c   get next event
c
      goto 50
c
c     end of file 
c
 1000 continue
c
      write(6,*)            ! blank line
      close(2)              ! close output file
      write(6,*) 'Number of events in input file', nevent
      write(6,*) 'Output file name is nor2nor2.out'

      stop
      end
