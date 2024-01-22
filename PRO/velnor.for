c--------------------------------------------------------------------------
c   converts velest cnv files to nordic
c--------------------------------------------------------------------------
c
c   jh july 2020
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
      character*80 text

      logical all                         ! true: read all data, false: headers
      integer seiclen                     ! function
      integer code                        ! error return code
      integer nevent                      ! number of events in file
      real*8  abs_org_time                ! absolute origin time
      integer doy                         ! day of year
     
      character*80 infile                 ! input file or base name
  
      integer i                           ! counter

      call get_seisan_def
c
c   open output file
c
      open(2,file='test.out',status='unknown')
    
c
c   get input file name, or data base with time interval
c

      write(*,*) ' Event data input'    
      read(*,'(a)') infile
  

c
c  open input file
c

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

      call rea_hyp_clear(1)     
      call rea_main_clear                                                                                                                                                  
c
c   read header 
c
      read(1,'(a)',end=1000) text
      read(text(1:6),'(3i2)') hyp_year(1),hyp_month(1),hyp_day(1)
      hyp_year(1)=hyp_year(1)+2000  ! assume after year 2000
      read(text(8:11),'(2i2)') hyp_hour(1),hyp_min(1)
      read(text(13:17),*) hyp_sec(1)
      read(text(19:25),*) hyp_lat(1)
      if(text(26:26).eq.'S') hyp_lat(1)=-hyp_lat(1)
      read(text(28:35),*) hyp_lon(1)    
      if(text(36:36).eq.'W') hyp_lon(1)=-hyp_lon(1)
      read(text(38:43),*) hyp_depth(1)
      read(text(63:67),*) hyp_rms(1)
      
      hyp_dist_id(1)='L'

      rea_nhyp=1

c
c   read phases
c
 60   continue
      read(1,'(a)',end=1000) text
      if(text.ne.' ') then 
         rea_nphase=rea_nphase+1
         call rea_phase_clear(rea_nphase)
         rea_stat(rea_nphase)=text(1:4)
         rea_weight_in(rea_nphase)=text(6:6)
         rea_phase(rea_nphase)=text(5:5)
         read(text(7:12),*)rea_sec(rea_nphase)

         if(text(13:16).ne.' ') then       
            rea_nphase=rea_nphase+1
            call rea_phase_clear(rea_nphase)
            rea_stat(rea_nphase)=text(13:16)
            rea_weight_in(rea_nphase)=text(18:18)
            rea_phase(rea_nphase)=text(17:17)
            read(text(19:24),*)rea_sec(rea_nphase)
         endif   

         if(text(25:28).ne.' ') then
            rea_nphase=rea_nphase+1
            call rea_phase_clear(rea_nphase)
            rea_stat(rea_nphase)=text(25:28)
            rea_weight_in(rea_nphase)=text(30:30)
            rea_phase(rea_nphase)=text(29:29)
            read(text(31:36),*)rea_sec(rea_nphase)
         endif

         if(text(37:40).ne.' ') then
            rea_nphase=rea_nphase+1
            call rea_phase_clear(rea_nphase)
            rea_stat(rea_nphase)=text(37:40)
            rea_weight_in(rea_nphase)=text(30:30)
            rea_phase(rea_nphase)=text(41:41)
            read(text(31:36),*)rea_sec(rea_nphase)
         endif    

         if(text(49:52).ne.' ') then
            rea_nphase=rea_nphase+1
            call rea_phase_clear(rea_nphase)
            rea_stat(rea_nphase)=text(49:52)
            rea_weight_in(rea_nphase)=text(42:42)
            rea_phase(rea_nphase)=text(41:41)
            read(text(43:48),*)rea_sec(rea_nphase)
         endif 

         if(text(61:64).ne.' ') then
            rea_nphase=rea_nphase+1
            call rea_phase_clear(rea_nphase)
            rea_stat(rea_nphase)=text(61:64)
            rea_weight_in(rea_nphase)=text(66:66)
            rea_phase(rea_nphase)=text(65:65)
            read(text(67:72),*)rea_sec(rea_nphase)
         endif          
      goto 60
      endif
     
      nevent=nevent+1               ! count events
c
c   fix time since it is relative to origin time
c
      do i=1,rea_nphase
         call timsec(hyp_year,hyp_month(1),hyp_day(1),hyp_hour(1),
     *   hyp_min(1),hyp_sec(1),abs_org_time)
         abs_org_time=abs_org_time+rea_sec(i)
         call sectim(abs_org_time,doy,rea_year(i),rea_month(i),
     *   rea_day(i),rea_hour(i),rea_min(i),rea_sec(i))
      enddo
c
c   write out
c
      call rea_event_out(2,all,data,code)
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

c
c   get next event
c
      goto 50
c
c     end of file 
c
 1000 continue
c

      close(2)              ! close output file
      write(6,*)
      write(6,*) 'Number of events in input file ', nevent
      write(6,*) 'Output file name is test.out'

      stop
      end
