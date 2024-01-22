c--------------------------------------------------------------------------
c  Sample program for reading events from 2 files and using rea and rea2
c  structure.
c  The program opens 2 files and read one event from each. The events are 
c  compared for duplicatre phases. The two events are then merged and duplicate
c  phases are deleted when writing out the merged data.
c 
c--------------------------------------------------------------------------c
c
c  For detail on parameters and variables names, see rea.inc and rea2.inc
c  See also append_s.for for another program example
c
c
c  changes:
c

      implicit none                       ! force delcaration of all variables
      include 'seidim.inc'                ! dimensions for rea block
      include 'seisan.inc'                ! dimensions for rea block
      include 'rea.inc'                   ! parameter common block
      include 'rea2.inc'
      character*80 data(max_data/2)       ! s-file with data in text array
      logical all                         ! true: read all data, false: headers
      integer code                        ! error return code
      integer nevent                      ! number of events in file
      character*80 text  
      character*80 infile,infile2         ! input file
      integer i,j                         ! counters

      call get_seisan_def
c
c   open output file
c
      open(3,file='sample_rea_to_rea2.out',status='unknown')
c
      write(6,*)' Data from 2 different s-files are read, '
      write(6,*)' one event at a time in each.'
      write(6,*)' The two files are merged together, file 1 is'
      write(6,*)' inserted into file 2, so file 2 main header remins.'
      write(6,*)' Id line from file 1 is converted to a comment line.'
      write(6,*)' Locality line is only kept from file2, from file 1,'
      write(6,*)' it is converted to a comment'
      write(6,*)' If duplicate phases are found, they are deleted'
      write(6,*)' from file 1.'
      write(6,*)
    
c
c   get input file name 1
c

      write(*,*) ' Input file 1'

      read(*,'(a)') infile
                      
         open(1,file=infile,status='old',err=5)
         goto 6
 5       continue
         write(6,*)' Input file not found'
         stop
 6       continue

      write(*,*) ' Input file 2'

      read(*,'(a)') infile2
                      
         open(2,file=infile2,status='old',err=7)
         goto 8
 7       continue
         write(6,*)' Input file not found'
         stop
 8       continue

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
c   write part of first header line
c
      write(6,*)
      write(6,'(a,a)') 'File1: ',data(1)(1:64)
c
c   copy data to rea2
c
      call rea_copy_rea_to_rea2
c
c   read all parameters for one event from file unit 2
c   overwriting what was read from unit 1
c
      call rea_event_in(2,all,data,code)
c
c   check if end of file (code=1), if so jump out of loop
c
      if(code.eq.1) goto 1000
c
      nevent=nevent+1               ! count events
c
c   write part of first header line
c
      write(6,'(a,a)') 'File2: ',data(1)(1:64)
c
c   show first 5  phases or as many as there are 
c   of the two files, could be a spectral phase.
c   udefined values of numbers are set to  -999 so check
c   before writing out.
c
      j=5
      if(rea2_nphase.le.5) j=rea2_nphase
      write(6,*) 
     *'First 5 phases or as many as there are of the two events'
      write(6,*) 'File1--------'
      do i=1,j
         text=' '
         write(text,'(a,1x,a,1x,a,1x)') 
     *   rea2_stat(i),rea2_com(i),rea2_phase(i)
         if(rea2_hour(i).gt.-1) write(text(20:21),'(i2)')   rea2_hour(i)
         if(rea2_min(i). gt.-1) write(text(22:23),'(i2)')   rea2_min(i)
         if(rea2_sec(i). gt.-1) write(text(24:29),'(f6.2)') rea2_sec(i)
         write(6,'(a)') text(1:30)
      enddo
      write(6,*) 'File2--------'
      j=5
      if(rea_nphase.le.5) j=rea_nphase
      do i=1,j
         text=' '
         write(text,'(a,1x,a,1x,a,1x)') 
     *   rea_stat(i),rea_com(i),rea_phase(i)
         if(rea_hour(i).gt.-1) write(text(20:21),'(i2)')   rea_hour(i)
         if(rea_min(i). gt.-1) write(text(22:23),'(i2)')   rea_min(i)
         if(rea_sec(i). gt.-1) write(text(24:29),'(f6.2)') rea_sec(i)
         write(6,'(a)') text(1:30)
      enddo
c
c   check if any duplicate phases based on time, phase, station and componenet
c
      do i=1,rea2_nphase
         do j=1,rea_nphase
           if(dabs(rea_abs_time(j)-rea2_abs_time(i)).lt.0.1.and.
     *     rea_stat(j).eq.rea2_stat(i).and.
     *     rea_com(j).eq.rea2_com(i).and.rea_phase(j).
     *     eq.rea2_phase(i)) then
              rea2_phase(i)(1:6)='DELETE'   ! delete in rea2
              write(6,'(a,1x,a,1x,a,1x,a)') 'Duplicate phase: ',
     *        rea_stat(i),rea_com(i),rea_phase(i)
           endif
        enddo
      enddo        
c
c   merge rea2 with rea, also if not belonging together, rea1
c   data has the main header
c   
      call rea_merge_rea2_to_rea 
c
c   write out modified event, the phases indicted by DELETE are not 
c   written
c
       call rea_event_out(3,all,data,code)
c
c   get next 2 events
c
       write(6,*)
       write(6,*)' Enter to go to next events, 0 to stop'
       read(5,'(a)') text
       if(text.eq.' ') goto 50
       read(text,*) i
       if(i.eq.0) goto 1000
       
c
c     end of file
c
 1000 continue
c
      write(6,*)           
      write(6,*) 'Number of events read', nevent
      write(6,*) 'Output file is sample_rea_to_rea2.out'

      stop
      end
