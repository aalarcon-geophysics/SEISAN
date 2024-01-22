c--------------------------------------------------------------------------
c  running epimap without questions
c  file name can be first argument or if no argument is asked for
c  both normal and compact files can be used
c  the border of the map is determined from the content of the input file
c  lat-lon divisions are 1/5 of the range, however not less than 1 deg
c  the parameters which can be set in SEISAN.DEF are:
c
c  projection number, default 3 for Mercator
c  map source, default WORLD.MAP
c  plot stations, defualt no
c  these parameters are also used for map option in EEV
c
c
c  j. havskov december 2017
c--------------------------------------------------------------------------
c
c
c
c  changes
c
c  may 23 2018 jh: restrict limits to 90 and 180
c  may 4  2020 jh: add plot of fps
c  sep 25 2020 jh: if only one event, use limits from seisan.def
c  feb 2  2021 jh: plot of localities with l, no plot with n, 
c                  fixed magintude symbol m
c  apr 29 2021 jh: bugs with combination of fps and other options
c  may 13 2021 jh: plot rays and stations

      implicit none                       ! force delcaration of all variables
      include 'seidim.inc'                ! dimensions for rea block
      include 'seisan.inc'                ! seisan general
      include 'rea.inc'                   ! parameter common bliock

      character*80 data(5000)             ! s-file with data in text array
      real lat(100000),lon(100000)        ! lat and lon
      real lamin,lamax,lomin,lomax        ! lat-lon grid with the data
      integer ladel,lodel                 ! grid size for lat lon
      integer code                        ! error code
      character*60 top_directory          ! seisan top directory
      character*1 dchar                   ! dir separation char

      real slat(10000),slon(10000)        ! coordinates of stations
      integer nstat                       ! number of stations
      character*5 stat(10000)             ! station codes

      integer nars                        ! number of arguments
      character*80 infile(8)              ! arguments

      logical all                         ! true: read all data, false: headers
      logical plot_fps                    ! true if plot fps
     
      logical plot_location_name          ! true if plot location name
      logical scale_magnitude             ! if true, scale magnitude
      logical no_plot                     ! if true, do not plot info on left
      logical ray_path                    ! if true, plot ray path
      logical plot_stations               ! if true plot stations
      logical plot_stations_labels        ! if true plot stations with labels
      integer nray                        ! number of ray paths
      integer iphase(50)                  ! number of chars to compare for ray
      integer nphase                      ! number of phases for ray paths
      character*8 phase(50)               ! phases for ray paths
      real weight                         ! minimum weight for phase to be used for ray path


      integer seiclen                     ! function
      integer nevent                      ! number of events in file
      character*120 text                  ! general text
      logical compact                     ! true if compact file
      integer i,k                         ! counter

c
c print version
c
      include 'version.inc'
      out_version_date=' '
      if (version_new) out_version_date=version_date
      call print_ver

      write(6,*)
      write(6,*)' Options, by interactive input or as arguments'
      write(6,*)
     *' The first argument is filenme so e.g. map select.out f s'
      write(6,*)
      write(6,'(a)')'  s: plot stations as triangles'
      write(6,'(a)')'  S: --------- with stations code above'
      write(6,'(a)')'  f: plot fault plane solutions'
     
      write(6,'(a)')'  m: plot epicenter symbol according to magnitude'
     *//' range '
      write(6,'(a)')'  l: plot localities names'
      write(6,'(a)')'  r: plot rays'
      write(6,*)
 



      call get_seisan_def
      call topdir(top_directory)
      call dir_char(dchar)
      plot_fps=.false.
      no_plot=.false.
      scale_magnitude=.true.
      plot_location_name=.false.
      plot_stations=.false.
      plot_stations_labels=.false.
c      plot_location_symbol=.false.
      plot_location_name=epimap_plot_locality
      nray=0
c
c   get arguments
c
      call get_arguments(nars,infile)

      if(infile(2).eq.'f') plot_fps=.true.
c
c   get input file name if no argument
c
      if(nars.eq.0) then   
          write(*,*) ' Give input file'  
          read(*,'(a)') infile(1)

         write(6,*)
     *   ' Plot stations(s), stations with code(S),'//
     *   ' fixed size for magnitude symbols(m),'
         write(6,*)
     *   ' fault plane solutions(f), localty(l) and rays(r)'//
     *   ' e.g. sf, enter for none'
         read(5,'(a)') text


         do i=1,seiclen(text)
             if(text(i:i).eq.'s') plot_stations=.true.
             if(text(i:i).eq.'S') then
                plot_stations=.true.
                plot_stations_labels=.true.
             endif 
             if(text(i:i).eq.'f') plot_fps=.true.
             if(text(i:i).eq.'m') scale_magnitude=.false.
             if(text(i:i).eq.'r') ray_path=.true.
         enddo    

      else
          do i=2,nars
             if(infile(i)(1:1).eq.'f') plot_fps=.true.
             if(infile(i).eq.'m') scale_magnitude=.false.
             if(infile(i).eq.'l') plot_location_name=.true.
             if(infile(i).eq.'n') no_plot=.true.
             if(infile(i).eq.'r') ray_path=.true.
             if(infile(i).eq.'s') plot_stations=.true.
             if(infile(i).eq.'S') plot_stations_labels=.true.
          enddo
      endif
           
            
                    
      open(1,file=infile(1),status='old',err=5)
      goto 6
 5    continue
      write(6,*)'Input file not found'
      stop
 6    continue

c
c   check if a compact file
c
      call nortype(1,compact)
      if(compact) then
         write(6,*)'Input file  compact'
      endif

c
c   if ray path, enter phases to use
c
      if(ray_path) then
         k=1
         write(6,*)
     *   'Enter phases to use for ray paths, one per line'//
     *   ', terminate with enter'
         write(6,*)'Only the chars given will be used so entring'//
     *   ' e.g. P, all P-type phases will be used'
 7       continue
         read(5,'(a)')phase(k)
         if(phase(k).eq.' ') goto 8
         iphase(k)=seiclen(phase(k))
         k=k+1
         goto 7
 8       continue
         nphase=k-1
         write(6,*) 'Minimum output weight for phase-stat to be used'//
     *              ', enter for non zero weight phases '
         read(5,'(a)') text
         if(text.eq.' ') then 
           weight=0.0001
         else
           read(text,*) weight
         endif
         open(4,file='epimap.ray',status='unknown')
      endif        

      all=.true.                  ! read all parameters
      nevent=0                    ! initialize counter of events
      k=0                         ! ---------------------------- with location
      rewind (1)
c
c-----------------------------------------------------------------
c  Loop to read events start here
c-----------------------------------------------------------------
c

  50  continue

c
c   read all parameters for one event from file unit 1
c
      if(compact) then
         read(1,'(a)',end=1000) text
         if(text(24:38).ne.' ') then   ! check if located
            k=k+1
            read(text,'(23x,f7.3,f8.3)') lat(k),lon(k)
          endif
      else
         call rea_event_in(1,all,data,code)
c
c   check if end of file (code=1), if so jump out of loop
c
         if(code.eq.1) goto 1000
c
c   save lat lon if there
c
         if (hyp_lat(1).ne.-999.0) then
            k=k+1
            lat(k)=hyp_lat(1)
            lon(k)=hyp_lon(1)
         endif

c
c    ray path
c
         if(ray_path) then
            if(compact) then
               write(6,*)'Cannot make ray paths with a compact file'
            else
               call stat_event(4,phase,nphase,iphase,weight,i,1,
     *         stat,slat,slon,nstat)
               nray=nray+i
            endif
         endif
      endif
c
      nevent=nevent+1               ! count events
c
c   write the whole first header line
c
c      write(6,'(a)') data(1)(1:79)

c
c   get next event
c
      goto 50
c
c     end of file 
c

 1000 continue

      close(1)
      close(4)
c
c     find area of data
c
      lamax=-100.0
      lamin=100.0
      lomax=-400.0
      lomin=400.0

      do i=1,k
        if(lat(i).gt.lamax) lamax=lat(i)
        if(lat(i).lt.lamin) lamin=lat(i)                
        if(lon(i).gt.lomax) lomax=lon(i)
        if(lon(i).lt.lomin) lomin=lon(i)
      enddo
                 
      write(6,*)            ! blank line
      close(2)              ! close output file

      write(6,*) 'Number of events in input file', nevent
      write(6,*) 'Number of events with location',k
      write(6,*) 'Lattitude range ',lamin,lamax
      write(6,*) 'Longitude range ', lomin,lomax
c
c   find outline to use, increase a bit
c
      if(k.gt.1) then
         lamax=lamax+0.5
         lamin=lamin-0.5
         lomax=lomax+0.5
         lomin=lomin-0.5
      else
c
c   if only one event, use limits from seisan.def
c
         lamax=lamax+map_lat
         lamin=lamin-map_lat
         lomax=lomax+map_lon
         lomin=lomin-map_lon
      endif
c
c  check limits
c
      if(lamax.gt.90.0) lamax=89.0
      if(lamin.lt.-90.0) lamin=-89.0
      if(lomax.gt.180.0) lomax=180.0
      if(lomin.lt.-180.0) lomin=-180.0

      if(ray_path) then
        do i=1,nstat
           if(slat(i).gt.lamax) lamax=slat(i)
           if(slat(i).lt.lamin) lamin=slat(i)                
           if(slon(i).gt.lomax) lomax=slon(i)
           if(slon(i).lt.lomin) lomin=slon(i)
        enddo
      endif
c
c   grid, assume 5 divisions, only integer, smallest is 1 deg
c
      ladel=(lamax-lamin)/5.0
      if(ladel.lt.1) ladel=1     
      lodel=(lomax-lomin)/5.0
      if(lodel.lt.1) lodel=1
      write(6,*)'lat-lon grid size ', ladel,lodel
c
c   make epimap input file
c
      if(k.gt.0) then
         open(2,file='map.inp',status='unknown')
         if(map_proj.eq.0) map_proj=3  ! user mercator by default
         write(2,'(i2)') map_proj
         write(2,'(f6.1,1x,f6.1)') lamin,lamax
         write(2,'(f7.1,1x,f7.1)') lomin,lomax
         write(2,'(a)')'        '
         write(2,'(2i3)') ladel,ladel
         write(2,'(2i3)') lodel,lodel
         write(2,'(a)')'        '
         write(6,*) 'Map file ',map_file
         if(map_file.eq.' ') map_file='WORLD'
         text=top_directory(:seiclen(top_directory))
     &           // dchar // "DAT" // dchar //
     &           map_file(1:seiclen(map_file)) // ".MAP"
         write(2,'(a)') text(1:seiclen(text))
         if(ray_path.and.nray.gt.0) write(2,'(a)')'epimap.ray'
         write(2,'(a31)') '$$$$$$end of contour list$$$$$$'
         write(2,'(a)')'        ' ! title
c         write(2,*)'File plotted: '//infile(1)(1:seiclen(infile(1)))  ?????
         if(plot_fps) then
            write(2,'(a1)')'f'
         else
            write(2,'(a)')'        ' ! no fps
         endif
         write(2,'(a)')'        '  ! contour levels files

         if(plot_location_name.and.epimap_locality_file.ne.' ') then
             write(2,'(a)')'P'
             write(2,'(a)')epimap_locality_file
             write(2,'(a)')'$$$$$$end of placename file list$$$$$$'
         endif
c
c   only plot stations used
c
         if(ray_path.and.plot_stations) then  
            write(2,'(a)')'s   '
            do i=1,nstat
               write(2,'(a5)') stat(i)
            enddo
            write(2,'(a)') '$$$$$$end of station list$$$$$$'
            goto 10
         endif           
c
c  plot stations
c
         if(plot_stations) then  ! from argument
            write(2,'(a)')'x        '
         elseif(plot_stations_labels)then
            write(2,'(a)')'a        '
         else             
            write(2,'(a1,a)') map_stations,'      '  ! from SEISAN.DEF
         endif

 10      continue
             
         write(2,'(a)') infile(1)
         write(2,'(a38)') '$$$$$$end of epicentre file list$$$$$$'
         if(.not.scale_magnitude) write(2,'(a)')'Y'
         write(2,'(a)')'        '
         write(2,'(a)')'        '
         write(*,*) 'Writing map.inp for running epimap'
         close(2)  
c
c   plot
c 
         if(no_plot) then
             call systemc("epimap map.inp noplot",21)
         else
             call systemc("epimap map.inp",14)
         endif

         write(6,*)
         if(ray_path) then
            write(6,'(a,i5)')' Number of ray paths ',nray
            write(6,*) 
     *      'File with ray paths, epimap MAP format, is epimap.ray'
         endif
         write (6,*)'You can edit parameters in map.inp and rerun plot'
     *   ,' with command epimap map.inp'
 
      endif

      stop
      end


