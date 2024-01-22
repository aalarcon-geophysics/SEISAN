      subroutine hypmain(sfilename,from_se,message,nmessage,locate)


c   sfilename         	! file to locate
c   from_se		! if true, call comes from se
c   message             ! messages 
c   nmessage            ! number of messages
c   locate              ! true if event was located
c
c
c
c
c   program flow
c
c
c   hypmain routine -file hypmain
c          reads s-file 
c          call hypocent to locate, -file hyposub1 
c          magnitude calculations, -file hyposub3
c          write out data arrays
c 
c   hypocent routine locates one event -file hyposub1
c          reads test values
c          reads stations
c          reads model
c          reads phases, getphasn -in file hyposub2
c          find stat cooordinates for phases
c          calculated weights from 0-4 input, special for BAZ
c          prepare start location
c          call hypoloc (in file hypoloc) in a loop for different start depths                            
c          errror calculation?
c          some sorting
c          output, sreeen and print.out 
c                 
c   hypoloc routine  locates one event -file hypoloc
c          count phases etc
c          checks if locatable (enough phases etc)
c          sort phases in travel time order and store order in isort
c          initilize rmsv routine - routime in file hypoloc1
c          count non zero arrivals
c          locate with slownes and azimuth, not used
c          start location, routine startloc in file hyposb4
c          iteration loop, do 98
c             calculate rms, function rmsv, in file hypoloc1
c             inversion routine minv, in file hyposub2
c             parameter correction
c             if small change, jump out, goto 97
c          98 continue  
c          97 continue        
c
c******************************************************************************
c
c   multi model:
c
c   fixed vp/vs, determined by vp/vs on control line or
c   variable vp/vs given in parameter file
c   both p and s-models cand be used, but not only s-model
c   cannot use s-model and variabel vp/vs at the same time
c   some check for impossible models like low velocity
c   does not work with weight 9
c   not tried with fixed depth, multiple models
c   best 10 models written out
c   
c
c   
c
C   LATEST UPDATE :  6/98 BRL: added include file, eventno to hypocent window
c                    removed unused variables, minor changes to 
c                    interactive section
c   may 11 94 by jh : do double sorting
c   may 27 94       : put include statement one down
c   may 30          : clear old magnitudes
c   jun 15          : mw magnitude
c   jul 22          : bug with sorting
c   jul 25          : delete print.out at start up
c   sep 28          : do not delete agency if hypocenter fixed
c   dec 7           : clear underflow on sun
c   jan 3   95      : use env variable for file name
c   jan 9           : new find_ev_index
CJAB(BGS)Jan95      : Install error and file handling...
CJAB(BGS)Jan95        Note that "tau.for" has not been changed,thus unit 22
CJAB(BGS)Jan95        must be supplied to variable "read3". This will not
CJAB(BGS)Jan95        effect "libsei" routines since units are opened 64,63..
CJAB(BGS)Jan95        and is unlikely ever to reach the value of 22. If "tau"
CJAB(BGS)Jan95        is changed at some future data, no action need be taken.
CJAB(BGS)Jan95        Note also that all "hyp*" routines linked to "hyp.for"
CJAB(BGS)Jan95        which just "write" or "read" MAY need to be modified
c   jan 25          : make sure close when using eev, look for STATION0.HYP
c                     for seisan detection
c apr 6 95 jh       : bug
c jun 9   jh        : do not write loc. agency when no locate
c aug 16  jh        : do not modify event if no locate flag * is set
c jan 23 96         : q option when listing event
c mar 14 96         : modify clear_old, read_stat..
c sep 19 96         : clear 3 magnitude if default agency
c oct 11, 96        : fix sorting to use time so P comes before S
c dec 19, 96        : enable local data base and index file
c mar 20  97        : if hypocenter fixed, keep old location agency
c mar 24            : clear 3 magnitude, wrong place
c may 16            : sort even if not located
c feb 12 98         : clear residuals etc before locating, not after
c jun  6 98      lo : check for message
c jun 8 98          : BRL added datsave
c sep 1998   jh     : ---------------- version 7.0 check ------------------
c                     year 2000, 5 char bases, 5 char station
C oct 28  1998  bmt : linux changed, save option include                     
c jul 26 1999   jh  : add variable use_eev to hypocent and read_stat_model call
c sep 24            : add get_seisan_def and include seisan.inc to get new 
c                     geometrical spreading function in spec
c oct 19            : new hyposub1 and hypoloc1, weitght 4 prohblem
c nov 2, 99         : if input from file, any name longer than 5 chars can be
c nov 26 99 jh      : make it possible to use an input file name of 80 chars
c                     used also if no ','. and shorter if a '.'
c dec 8             : fix overflow in year 2000 output in routines hypoloc
c                     and hyposub1
c jan 2000          : multi model search
c feb 16 ,2000      : add output to update_spec, so no mw out if output false
c march 10, 2000    : move call clear_old up before location
c april 27, 2000    : give input file as argument
C august 2000       : grid search
c oct 30            : compiled for high accuracy, no changes
c feb 2001  lo      : BGS changes
c mar 4, 2001, jh   : enable magnitude calculation without locaiton, test 106
c mar 19            : closing multi model operation properly
c jan 28, 2002      : write old locaiton
c apr 2002 jh       : input with file as argument changed so that output
c                     is the same as for eev
c apr 14 2002 bmt   : set maxline=1000 for jseisan
c may 26 2005 lot   : BGS extension, read model from sfile
c dec 20 2010 jh    : gfortran on pc, changes in hyp_print due to gfortran
c                     eof problem when readin line of text
c jan 07 2010 jh    : more changes in hyp_print, no room for ic on
c                     hypocenter line in screen print out
c oct 3 2011  jh:   : add option -seisanexp to be able to run one event
c                     from seisan explorer, added -update to overwrite 
c                     input event
c 2011-11-14 pv:    : add call gmap_auto that make a kml file after each 
c                     location is done
c 2011-12-16 jh:    : more on update for seisan exp
c jan 8 2013 jh     : comment out seidim.inc, now in hypparm.inc
c apr 25 2013 jh:   : output file with magnitudes, hypmag.out, use dwith plotml
C may 03 2013 jh:   : make it possible to use hyp for update from eev 
c mar 18 2015 fc:   : add support for automatically weigth out possible outliers 
c                     (only arrivals automatically read) and compute "better" solutions (minimum rms sense)
c dec 23 2015 jh    : add test to call of update_spec
c jan 05 2016 lo    : reset nmessage for each event
c feb 19 2016 jh    : fix a proper return if no station file, change
c                     name to hypmain
c mar 02 2016 jh    : force return if no location and from se
c mar 06 2016 jh    : ----------, was not to 999
c mar 12 2016 jh    : put message in other calls to get message from other routines
c mar 14 2016 jh    : fix return to se when event not to be located
c may 25 2016 jh    : fixed to run with old se, seisan version 10.4
c                     to run with now se with hyp, look for 'new se',
c                     line 1604
c aug 09 2016 jh    ; back to new se
c sep 16 2018 lo    : adjusted number of layers in hyposub6.for 200->150
c oct 04 2018 pv    : depth is fixed to 0.0F if event is G or C
c nov 14 2018 lo    : covariance initialized to 0 before event location,
c                     was problem of cov not being correct for multiple
c                     events and fixed depth
c jan    2019 jh    : new format, amny changes during 2019 until august
c nov 26 2019 jh    : program was allowed to update without id line, should have 
c                     stopped, could have couused funny errors, found with fbounds 
c                     check
c mar 29 2020 jh    : increase number fo messages from 50 to 2000
c jul 21 2020 jh    : station to 9chars for ntlo, look for !nt
c oct 5  2020 jh    : improve multi_model function, some changes in comments
c nov 16 2020 jh    : fix problem with recalculating hypocenter in file interactive
c                     mode
c dec 14 2020  jh   : changes in output of multi-model 
c feb 9  2021  jh   : -------------------------------
c feb 22 2021  jh   : output 30 models ------------- 
c sep 21 2021  jh   : add agency to hypocent call, needed for error line
c dec 29 2021  jh   : add -overwrite to prompt, fix probelms with sorting data5
c                     when using reject mode, disable deleting automatic phases
c                     with large residuals (problem with order for data5 and data,
c                     still not completely fixed)
c jan 27 2022  jh   : in reject mode also reject BAZ if res>30deg, change some
c                     parameters
c jul 21 2022  jh   : in reject routine, data5 had wrong size, fix output in print.out
c                     of file name.
c dec 30 2022  jh   , put in geo location

c

c added include file 3/31/94
      implicit none
      include 'hypparm.inc'
      include 'seisan.inc'
      include 'rea.inc'
      include 'mbn.inc'
C
C    SEISAN library/JAB inclusions...
c    --------------------------------
c<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
c   BRL 6/98: Note to modifiers of this program:
c    Please observe the C-style justification 
c    in statements you add, particularly in existing 
c    code, e.g.,
c     do i=1,n
c      do j=1,m
c       if (i.gt.1) then
c         if (j.gt.2) then
c          write (*,*)
c          ADDED STATEMENT
c     &     CONTINUATION OF ABOVE     
c         end if
c        end if
c       end do
c      end do
c<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<         
C
      include 'libsei.inc'                 ! Library definitions & data defns.
      external sei open,                   ! File open handler.
     &         sei close,                  ! & closure.
     &         sei clen,                   ! String length.
     &         sei code                    ! Error condition handler.
      integer  sei clen                    ! & function.
      integer  code,                       ! Condition.
     &         text_c,                     ! Text length.
     &         read1,                      ! Read unit1.
     &         read2,                      ! Ditto 2. opened elsewhere.
     &         read3,                      ! Ditto 3, opened in tau.for
     &         write1,                     ! Write unit1.
     &         write2,                     ! Ditto 2.
     &         write3                      ! & 3.
      parameter (text_c = 80)              ! & value.
      logical  b_flag                      ! Flag!!
      logical seisanexp                    ! true if from seisan explorer
      logical update                       ! true if overwrite
      logical reject                       ! true if reject bad phases
      logical overwrite                    ! true if overwirte input file
      character*1 answer
c-- indicator of computer used
      logical sun,pc,linux

      character*80 sfilename               ! from call
      logical from_se                      ! true if called from se
      character*80 message(2000)           ! messages on return
      integer nmessage                     ! number of messages

      character*120 data120(max_data)      ! for sorting

      character chr_text   *(text_c)       ! Text string.
C
C    ------- End of details -------
C
c---name of top directory
      character*60 top_directory
c---one event in nordic format, one line of data  
c---iaspei file name
      character*80 modnam
      character*80 id_old           ! old id line
c---original header buffer
      character*80 datsave,data_old ! could they be the same ??
c---location indicator, can be L, R or D
      character*1 loc_indicator(3)
      logical locate
c--- event type, indcator of explosion, indicator of fixing depth  
      character*1 typ,exp,fixdepth
c---indicator if output on screen  
      character*1 yter,yter1
      logical output
c---agency for magnitude  
      character*3 agency
      integer exit_code   ! exit code 0: normal, 1: update was made
c---directory separator character
      character*1 dchar
c---number of header lines, records, stations in nordic file, help variables
      integer nhead,nrecord,nstat,nfind,k,nphase,i
c---id line indicator
      integer id
c---model type or number
      character*1 model,model_org  
C-- hypocenter help variable
      integer init
c-- help varibale
      character*80 text
      character*300 txt
c---time of making update
      character*12 p_time
      character*14 proc_time
      logical keep_old_id_line    ! if tru keep old lines
c-- line counter and max number of line pr page
      integer nline,maxline
c-- save year, month ,day for old location
      integer year,month,day
c-- hour and difference in hours between solutions shifted one day
      integer hour,delhour
c-- min and sec of phase
      integer min
      real sec
c-- abs times for above
      double precision abstim1,abstim2
c-- covarriance matrix, origin time error
      real covar(3,3),oterr
c-- # phases      
      integer nphs
c
c
c   for multimodel
c

      integer nvelocity1,ivelocity1,nvelocity2,ivelocity2,
     *        nvelocity3,ivelocity3,nvelocity4,ivelocity4,
     *        nvelocity5,ivelocity5,nvelocity6,ivelocity6,
     *        nvelocity7,ivelocity7,nvelocity8,ivelocity8,
     *        nsvelocity1,nsvelocity2,nsvelocity3,nsvelocity4,
     *        nsvelocity5,nsvelocity6,nsvelocity7,nsvelocity8,
     *        isvelocity1,isvelocity2,isvelocity3,isvelocity4,
     *        isvelocity5,isvelocity6,isvelocity7,isvelocity8,
     *        nvpvs,ivpvs
      integer ndepth1,idepth1,ndepth2,idepth2,ndepth3,idepth3,
     *        ndepth4,idepth4,ndepth5,idepth5,ndepth6,idepth6,
     *        ndepth7,idepth7,ndepth8,idepth8
      real    v_start(10),vs_start(10),d_start(10),
     *        vs_min(30,10),vpvs_min(30),
     *        v_min(30,10),d_min(30,10),vpvs_start ! start and best
      real    d_delta(10),v_delta(10),vs_delta(10),vpvs_delta         ! step
      real    rmssum_min,rmssum_max                                   ! min  and max rms
      real    rms_min(30)        ! lowest rms, several models
      real    mdepth_min(30)     ! average depth for min solutions
      real    mdepth, avdepth    ! hypocenter depths for multi model
      integer nmdepth            ! number of depths
      integer imin               ! counter for imin
      integer nmin               ! number of best models stored
      integer kmax               ! counter for max rms
      real    rmax               ! largest rms of stored values
c
c   for grid search
c
      real minlat,maxlat,dellat         ! lat grid for search
      real minlon,maxlon,dellon         ! lon grid for search
      integer nlat,nlon                 ! number of grid points
      real  grid_rms,g_rms              ! minumum rms
      real grid_lat,grid_lon            ! corressponding location
      logical grid_search               ! true if grid search

      integer ilo,ila
      real bnglat,bnglon,bnge,bngn,bngerr ! variables to convert to BNG 
c
      integer j,m,l



c-- indicator if program is used in connection with EEV in seisan
      logical use_eev
c-- indicator if seisan data base present
      logical seisan
C
C  next for data base operation
C
      character*10 keys                      !next choice key
      character*14 starttime,endtime         ! time interval for location
      character*80 basename                  ! data base or file name
      character*80 infile                    ! input file or base name
      character*80 eventfile                 ! single event file name
      integer status,eventno,newmonth,fstart ! 
      integer base                           ! 0: data base, 1: single file
c
c---minimum number of stations to locate
      integer minstat
c---minimum number of phases to locate  
      integer minphase
c---code for current model  
      character*20 model_code  
c---magnitude Ml coefficients
c      real a,b,c,d  
c---number of arguments and function
       integer nars
       character*80 args(10)
c---operator
       character*4 operator

c---added 4/94: sort arrays for sorting output by distanace
      integer ksort(narriv)     
      integer lsort(narriv)
      character*5 old_dist
      character*5 old_stat
      integer isindex
      integer lunit,unit
      logical terminal,isatty
c      logical bgs_seisan
      character*10 extension
      character*240 mbnfile
      logical exists
c
c   for locality
c
      real locality_lat(10000),locality_lon(10000) ! localty locations
      character*56 locality(10000)                 ! locality names
      integer nlocality                            ! number of localities
c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver
      call computer_type(sun,pc,linux)

      call get_arguments(nars,args)

c      call get_bgs_seisan(bgs_seisan)
      call get_env_seisan_extension(extension)
      call get_seisan_def

c
c   force use of old format internally, only required if new format is used
c
       if(new_nordic_format) then
          rea_old_format_required=.true.
       endif
C
C    initialise...
C    =============
C
      grid_rms=99999999.0
c
c   check for grid search, then this is only posible argument
c
      grid_search=.false.
      if(nars.gt.0) then
        if(args(1)(1:11).eq.'-gridsearch') then
           grid_search=.true.
           nars=0
        endif
      endif
c
      seisanexp=.false.
      update=.false.
      reject=.false.
      overwrite=.false.
      rmssum_min=999999.0
      rmssum_max=-1.0
      code   = e_ok$                             ! Local condition.
      b_f_debug$ = .false.                       ! Debug to screen/file?
      read1  = 0                                 ! & file units.
      read2  = 0                                 ! Ditto.
      read3  = 22                                ! Ditto.
      write1 = 0                                 ! Ditto.
      write2 = 0                                 ! Ditto.
      write3 = 0                                 ! Ditto.

      exit_code=0

      nmessage=0
c                                                           
c   get computer specifics
c
      call dir_char(dchar)         ! dirctory delimiter character
c     call gradual_underflow()
c
c
c   get seisan defaults
c
      call get_seisan_def

c   set max number of lines pr page for screen print out
c      maxline=20
c modified to allow output to a pipe without expecting
c a reply from the terminal (bjb 2001/02/14)
c Function ISATTY may only be available on Solaris???
      lunit=6
      terminal=isatty(lunit)
      if(terminal) then
         maxline=20
      else
         maxline=1000
      endif
c
c   open output file with magnitudes
c
      open(98,file='hypmag.out',status='unknown')
c 
c   get directory structure
c
      call topdir(top_directory)
c
c   read locality file
c
      nlocality=0
      call sei get file( open$+ignore$,       ! Get file.
     &                      unit,             ! Opened on this unit.
     &                      code,             ! Returned condition.
     &                      'DAT',            ! Alternative directory to search.
     &                      'place_names.eev' )  ! For this file.

      if(code.ne.e_ok$) then
         write(6,*)' Place names file missing'
         goto 5658 
      endif

      k=0
      do i=1,10000
         k=k+1
         read(unit,'(a)',end=5657) text
         if(text.eq.' ') goto 5657
         read(text(1:24),*,err=5655) locality_lat(k),locality_lon(k)
         locality(k)=text(25:80)
         goto 5656
 5655    continue
         write(6,*) 'Error with locality file, line:'
         write(6,*) text
         k=k-1
 5656    continue
      enddo
 5657 continue
      nlocality=k-1
      call sei close (close$,unit,code) 
 5658 continue

c      write(6,*)'Number of localities',nlocality      


c read mbn parameters
      mbn_nstat=0
      mbn_nsource=0
      mbpnscale(1)=0.
      mbpnscale(2)=9999.
      mbnfile='mbn.par'
      inquire(file=mbnfile,exist=exists)
      if(.not.exists) then
         mbnfile=top_directory(1:seiclen(top_directory))//dchar//
     &           'DAT'//dchar//'mbn.par'
        inquire(file=mbnfile,exist=exists)
      endif
      if (exists) then
        call sei open( old$,                 ! Open file & stop if erro.
     &                   ' ',              ! Prompt (n/a).
     &                   mbnfile,          ! Filename.
     &                   read1,       ! Unit opened on.
     &                   b_flag,           ! Flag.
     &                   code )            ! Returned condition
        call read_mbn(read1) 
        call sei close( close$, read1, code ) ! Close file (stop on error).
      endif
c
c   check if SEISAN data base is present indicated by the presense of
c   the file station0.hyp in DAT directory
c
      chr_text = top_directory(:seiclen(top_directory)) //
     &           dchar // 'DAT' //dchar                 //
     &           'STATION0.HYP'
c
      call sei open( check$,            ! Check existance.
     &               ' ',               ! No prompt.
     &               chr_text, read1,   ! File & unnit.
     &               seisan,            ! File exists?.
     &               code )             ! Local condition (n/a).
c
      if( seisan ) then                 ! Multiple files.
      base = 0                          ! General database.
      else                              ! Otherwise only a single file.
      base = 1                          ! Only a single file.
      end if 

      if(from_se) then
        base=0 
        goto 20
      endif               
c
c   get file name from environment
c 

      call get_env_event(eventfile)

c
c   check if input of a file as argument, however if argument is
c   one starting with '-', continue. it must be the first argument.
c
 
      if(nars.gt.0.and.args(1)(1:1).ne.'-') then
        if(args(1)(1:5).ne.'-grid') then
           eventfile=args(1)(1:80)
           maxline=1000
        endif
      endif

c
c  check if non interactive, then no output
c
      call get_seisan_message(text)
c
c   find if program will run from within EEV indicated by the presense of file
c   name in an enviromental variable
c
  
      use_eev =  eventfile(1:1)        .ne. ' '    .and.
     &           ichar(eventfile(1:1)) .ne. 0  


c
c   check if run from seisan explorer or or other inputs. 
c
c
      if(nars.gt.0) then
        do i=1,nars
           if(args(i)(1:10).eq.'-seisanexp') then
             maxline=20
             seisanexp=.true.
           endif
           if(args(i)(1:7).eq.'-update') update=.true.
           if(args(i)(1:3).eq.'-op') operator=args(i+1)(1:4)
           if(args(i)(1:7).eq.'-reject') reject=.true.
           if(args(i)(1:10).eq.'-overwrite') overwrite=.true.
        enddo
      endif

      if(use_eev) then
C
C   open data base input single event file and read it
c   set flag for single event nrecord= -1
c
       write(6,*)eventfile
         nrecord = -1
         call sei open( old$,            ! Open old file (stop on error).
     &                  ' ',             ! No prompt.
     &                  eventfile, read1,! File & unit.
     &                  b_flag,          ! File exists? (n/a).
     &                  code )           ! Local condition (n/a).
         call indata(read1,nstat,nphase,nhead,nrecord,
     &               typ,exp,data,id)
         datsave=data(1)
c
c   stop if wrong header line
c

         if(nrecord.eq.0) stop

         call sei close( close$, read1, code ) ! Close file (stop on error).
c
         model = data(1)(21:21)
         yter = 'Y'                            ! output on screen
         output=.true.


      if (text.eq.'hyp non interactive') then
           yter='N' 
           output = .false.
      endif


         goto 20                               ! jump remaining questions
      endif

c
c   input data base name and time interval
c
      if(seisan) then
         write(*,*) ' Arrival time data input - select one:'
         write(*,*)
         WRITE(*,*) 
     *  '    SEISAN data base or                             :',
     &  'RETURN '
         write(*,*) 
     *  '    Alternative data base, give 1-5 letter code or  :'  
         WRITE(*,*) 
     *  '    Local index file, name must start with index or :'
         WRITE(*,*) 
     *  '    Local data base, write ,, or                    :'
         WRITE(*,*) 
     *  '    Filename for one file, min. 6 chars or with a . : '
      else
         WRITE(*,*) 'Filename for file in NORDIC format     : '
      endif
      write(*,*)
c     write(*,'(''    '',$)')

        read(*,'(a)') infile
      basename=infile(1:80)
                       
      write(*,*)
C
c   check if this is a single multiple event file (base=1),
c   general data base (base=0) or local data base, only check if seisan
c   data base is present
C
      if(seisan) then
         starttime=' '
         endtime=' '
         if(basename(1:5).eq.'INDEX'.or.basename(1:5).eq.'index'.
     *   or.basename(1:2).eq.',,') then
            base=0
            goto 2929
         endif
         if(seiclen(basename).lt.6.and.index(basename,'.').eq.0) 
     *   then
            base=0
            goto 2929
         endif
         base = 1                                  ! Single event file
 2929    continue
      endif
C
      if(base.eq.0.and.
     *basename(1:2).ne.',,'.and.basename(1:5).
     *ne.'INDEX'.and.basename(1:5).ne.'index') then 
                 ! only ask time interval for true base
         write(*,'('' Start Time           (YYYYMMDDHHMMSS): '',$)')
         read(*,'(a14)') starttime
         write(*,'('' End Time, RETURN is to end of month:   '',$)')
         read(*,'(a14)') endtime
         write(*,*)
      endif
C
C   check if interactive operation, 
C   changed to remove CR 3/94
c      if(nars.lt.1) then
      if (yter.ne.'N') then
        write(*,'('' Interactive operation (N/Y=default)'',$)')
        read(*,'(a1)')  yter
      endif
      yter1=' '       
      write(*,*)
      call sei upc( yter )
      if(yter.eq.' ') yter='Y'
      if(yter.eq.'Y') output=.true.
c
c   start with standard model
c
      model=' '     
c
c   enter here if used with eev
c
  20  continue

      if(from_se) then
         infile=sfilename   ! given in call for se
         model=' '          ! start with standard model
         base=1
         output=.false.     ! no output since through common block
         yter='N'           ! -----------------------------------
      endif
C
C   do not fix depth  
C
      fixdepth=' '
C
C   open  output files
C
      call sei open( unknown$,            ! Open print file.
     &               ' ',                 ! No prompt.
     &               'print.out', write1, ! File & unit.
     &               b_flag,              ! File exists?.
     &               code )               ! Condition (n/a).
c
c   on  sun, old events seem to remain in print.out file, so delete and reopen
c
      call sei close( delete$, write1, code ) ! Delete it (stop on error).
      call sei open( unknown$,                ! Open print file.
     &               ' ',                     ! No prompt.
     &               'print.out', write1,     ! File & unit.
     &               b_flag,                  ! File exists?.
     &               code )                   ! Condition (n/a).
c
      call sei open( unknown$,                ! Open hyp file.
     &               ' ',                     ! No prompt.
     &               'hyp.out', write2,       ! File & unit.
     &               b_flag,                  ! File exists?.
     &               code )                   ! Condition (n/a).
c
      call sei open( unknown$,                ! Open hyp file.
     &               ' ',                     ! No prompt.
     &               'hypsum.out', write3,    ! File & unit.
     &               b_flag,                  ! File exists?.
     &               code )                   ! Condition (n/a).

c
c   FC
c
c      call sei open( unknown$,                ! Open hyp file.   ! F.C.
c     &                 ' ',                     ! No prompt.
c     &                 'print1.out', write11,     ! File & unit.
c     &                 b_flag,                  ! File exists?.
c     &                 code )                   ! Condition (n/a).   ! F.C.
c
c      call sei open( unknown$,                ! Open hyp file.   ! F.C.
c     &                 ' ',                     ! No prompt.
c     &                 'print2.out', write12,     ! File & unit.
c     &                 b_flag,                  ! File exists?.
c     &                 code )                   ! Condition (n/a).   ! F.C.


c
c 4/94: write input filename to print output....
c
cxx
      if(seiclen(basename).lt.2) basename=eventfile
      write( write1,                               ! Details to print file.
     &       '('' Input File: '',a60,/)',          ! On format.
     &       iostat=code ) basename                ! On condition, the basename.
      call sei code( fort$, code, write1, b_flag ) ! Process the outcoe.
C
C   open hypocenter file with model and station and read
c   file is first searched for in your local directory, then in SEISAN
c   DAT directory
c
      model_org=model
      call read_stat_mod
     & (agency,model_code,model,minstat,minphase
     & ,modnam,loc_indicator,read2,write2,write1,
     & write3,read3,isort,
     &test,dlt,yter1,maxline,use_eev)
c
c   check if model file was there
c
      if(model.eq.'@') then
         nmessage=nmessage+1
         message(nmessage)=' No STATION'//model_org//'.HYP file'
         if(message(nmessage)(8:8).eq.' ') message(nmessage)(8:8)='0'
         write(6,*)message(nmessage)
         goto 999 
      endif

c
c   if used with eev, jump straight at location
c
      if(use_eev) goto 1000
C
C   open input event file if multiple event single file option has
C   has been selected 

      if(base.eq.1) then
C
      call sei open( old$,            ! Open old file (stop on error).
     &               ' ',             ! No prompt.
     &               infile, read1,   ! File & unit.
     &               b_flag,          ! File exists? (n/a).
     &               code )           ! Local condition (n/a).
      eventno = 0
 2222 continue
      endif
c
c   section on grid searching for a model
c
c   find if model seaching is done, open parameter file
c
      ndepth1=1
      ndepth2=1
      ndepth3=1
      ndepth4=1
      ndepth5=1
      ndepth6=1
      ndepth7=1
      ndepth8=1
      nvelocity1=1
      nvelocity2=1
      nvelocity3=1
      nvelocity4=1
      nvelocity5=1
      nvelocity6=1
      nvelocity7=1
      nvelocity8=1
      nsvelocity1=1
      nsvelocity2=1
      nsvelocity3=1
      nsvelocity4=1
      nsvelocity5=1
      nsvelocity6=1
      nsvelocity7=1
      nsvelocity8=1
      nvpvs=1
      imin=0
      nmin=30
      do i=1,nmin
        rms_min(i)=100.0 - i
        do k=1,10
           v_min(i,k)=0.0
           vs_min(i,k)=0.0
           d_min(i,k)=0.0
        enddo
      enddo
c
c--------------------------------------------------------------------------
c   check if multi model is allowed in this mode which is only input from
c   a multi event file and no interactive output
c--------------------------------------------------------------------------
c
      if(use_eev.or.yter.eq.'Y') goto 8877
c
      multi_model=.false.
      open(2,file='h_models.par',status='old', err=8877)
      read(2,'(a)') text    ! info header line
      i=0

      read(2,'(10x,4(2f10.3,i10))',end=8765)
     *v_start(1),v_delta(1),nvelocity1,d_start(1),d_delta(1),ndepth1,
     *vs_start(1),vs_delta(1),nsvelocity1,vpvs_start,vpvs_delta,nvpvs
      i=i+1
      if(v_start(i).eq.0.0) then
         i=i-1
         goto 8765  ! to stop on blank line
      endif

      read(2,'(10x,2f10.3,i10,2f10.3,i10,2f10.3,i10)',end=8765)
     *v_start(2),v_delta(2),nvelocity2,d_start(2),d_delta(2),ndepth2,
     *vs_start(2),vs_delta(2),nsvelocity2
      i=i+1
      if(v_start(i).eq.0.0) then
         i=i-1
         goto 8765  ! to stop on blank line
      endif 

      read(2,'(10x,2f10.3,i10,2f10.3,i10,2f10.3,i10)',end=8765)
     *v_start(3),v_delta(3),nvelocity3,d_start(3),d_delta(3),ndepth3,
     *vs_start(3),vs_delta(3),nsvelocity3
      i=i+1
      if(v_start(i).eq.0.0) then
         i=i-1
         goto 8765  ! to stop on blank line
      endif
      
      read(2,'(10x,2f10.3,i10,2f10.3,i10,2f10.3,i10)',end=8765)
     *v_start(4),v_delta(4),nvelocity4,d_start(4),d_delta(4),ndepth4,
     *vs_start(4),vs_delta(4),nsvelocity4
      i=i+1
      if(v_start(i).eq.0.0) then
         i=i-1
         goto 8765  ! to stop on blank line
      endif      

      read(2,'(10x,2f10.3,i10,2f10.3,i10,2f10.3,i10)',end=8765)
     *v_start(5),v_delta(5),nvelocity5,d_start(5),d_delta(5),ndepth5,
     *vs_start(5),vs_delta(5),nsvelocity5
      i=i+1
      if(v_start(i).eq.0.0) then
         i=i-1
         goto 8765  ! to stop on blank line
      endif
     
      read(2,'(10x,2f10.3,i10,2f10.3,i10,2f10.3,i10)',end=8765)
     *v_start(6),v_delta(6),nvelocity6,d_start(6),d_delta(6),ndepth6,
     *vs_start(6),vs_delta(6),nsvelocity6
      i=i+1
      if(v_start(i).eq.0.0) then
         i=i-1
         goto 8765  ! to stop on blank line
      endif
      
      read(2,'(10x,2f10.3,i10,2f10.3,i10,2f10.3,i10)',end=8765)
     *v_start(7),v_delta(7),nvelocity7,d_start(7),d_delta(7),ndepth7,
     *vs_start(7),vs_delta(7),nsvelocity7
      i=i+1
      if(v_start(i).eq.0.0) then
         i=i-1
         goto 8765  ! to stop on blank line
      endif
      
      read(2,'(10x,2f10.3,i10,2f10.3,i10,2f10.3,i10)',end=8765)
     *v_start(8),v_delta(8),nvelocity8,d_start(8),d_delta(8),ndepth8,
     *vs_start(8),vs_delta(8),nsvelocity8
      i=i+1
      if(v_start(i).eq.0.0) then
         i=i-1
      endif


 8765 continue
c
c  if no s-velocity model, reset numbers
c
      if(nsvelocity1.eq.0) nsvelocity1=1
      if(nsvelocity2.eq.0) nsvelocity2=1
      if(nsvelocity3.eq.0) nsvelocity3=1
      if(nsvelocity4.eq.0) nsvelocity4=1
      if(nsvelocity5.eq.0) nsvelocity5=1
      if(nsvelocity6.eq.0) nsvelocity6=1
      if(nsvelocity7.eq.0) nsvelocity7=1
      if(nsvelocity8.eq.0) nsvelocity8=1
c
c   make sure no zeros
c
      if(nvelocity1.eq.0) nvelocity1=1
      if(nvelocity2.eq.0) nvelocity2=1
      if(nvelocity3.eq.0) nvelocity3=1
      if(nvelocity4.eq.0) nvelocity4=1
      if(nvelocity5.eq.0) nvelocity5=1
      if(nvelocity6.eq.0) nvelocity6=1
      if(nvelocity7.eq.0) nvelocity7=1
      if(nvelocity8.eq.0) nvelocity8=1
       
      if(ndepth1.eq.0) ndepth1=1
      if(ndepth2.eq.0) ndepth2=1
      if(ndepth3.eq.0) ndepth3=1
      if(ndepth4.eq.0) ndepth4=1
      if(ndepth5.eq.0) ndepth5=1
      if(ndepth6.eq.0) ndepth6=1
      if(ndepth7.eq.0) ndepth7=1
      if(ndepth8.eq.0) ndepth8=1

      if(nvpvs.eq.0) nvpvs=1     ! no test for vpvs, not given in file h_models.par
c
c      write(6,*)nsvelocity1,nsvelocity2,nsvelocity3,nsvelocity4,
c     *nsvelocity5,nsvelocity6,nsvelocity7,nsvelocity8
c      write(6,*)nvelocity1,nvelocity2,nvelocity3,nvelocity4,
c     *nvelocity5,nvelocity6,nvelocity7,nvelocity8
c      write(6,*)ndepth1,ndepth2,ndepth3,ndepth4,
c     *ndepth5,ndepth6,ndepth7,ndepth8
c
c   average hypocenter depths and numbers
c
      avdepth=0.0
      nmdepth=0
      
c
c   make sure no variation of first interface, usually 0
c
   
      d(1)=d_start(1)
      d_delta(1)=0.0
      ndepth1=1

c      write(6,*)nsvelocity1,nsvelocity2,nsvelocity3,
c     *nsvelocity4,nsvelocity5,
c      *nsvelocity6,nsvelocity7,nsvelocity8,nvpvs

      if(nsvelocity1*nsvelocity2*nsvelocity3*nsvelocity4*nsvelocity5*
     *nsvelocity6*nsvelocity7*nsvelocity8.gt.1.and.nvpvs.gt.1) then
          write(6,*)'You cannot vary vp/vs and s-model at the same time'
          stop
      endif
c
c   check that no low velocity model is made
c
      if(nvelocity1.gt.1.and.
     *(v_start(1)+v_delta(1)*(nvelocity1-1)).ge.v_start(2).and.
     *v_start(2).gt.0.0) then
         write(6,*) 'Low P-velocity layer in layer 1'
         stop
      endif
      if(nvelocity2.gt.1.and.
     *(v_start(2)+v_delta(2)*(nvelocity2-1)).ge.v_start(3).and.
     *v_start(3).gt.0.0) then
         write(6,*) 'Low P-velocity layer in layer 2'
         stop
      endif
      if(nvelocity3.gt.1.and.
     *(v_start(3)+v_delta(3)*(nvelocity3-1)).ge.v_start(4).and.
     *v_start(4).gt.0.0) then
         write(6,*) 'Low P-velocity layer in layer 3'
         stop
      endif
      if(nvelocity4.gt.1.and.
     *(v_start(4)+v_delta(4)*(nvelocity4-1)).ge.v_start(5).and.
     *v_start(5).gt.0.0) then
         write(6,*) 'Low P-velocity layer in layer 4'
         stop
      endif
      if(nvelocity5.gt.1.and.
     *(v_start(5)+v_delta(5)*(nvelocity5-1)).ge.v_start(6).and.
     *v_start(6).gt.0.0) then
         write(6,*) 'Low P-velocity layer in layer 5'
         stop
      endif
      if(nvelocity6.gt.1.and.
     *(v_start(6)+v_delta(6)*(nvelocity6-1)).ge.v_start(7).and.
     *v_start(7).gt.0.0) then
         write(6,*) 'Low P-velocity layer in layer 6'
         stop
      endif
      if(nvelocity7.gt.1.and.
     *(v_start(7)+v_delta(7)*(nvelocity7-1)).ge.v_start(8).and.
     *v_start(8).gt.0.0) then
         write(6,*) 'Low P-velocity layer in layer 7'
         stop
      endif
         
       if(nsvelocity1.gt.1.and.
     *(vs_start(1)+vs_delta(1)*(nsvelocity1-1)).ge.vs_start(2).and.
     *vs_start(2).gt.0.0) then
         write(6,*) 'Low S-velocity layer in layer 1'
         stop
      endif
      if(nsvelocity2.gt.1.and.
     *(vs_start(2)+vs_delta(2)*(nsvelocity2-1)).ge.vs_start(3).and.
     *vs_start(3).gt.0.0) then
         write(6,*) 'Low S-velocity layer in layer 2'
         stop
      endif
      if(nsvelocity3.gt.1.and.
     *(vs_start(3)+vs_delta(3)*(nsvelocity3-1)).ge.vs_start(4).and.
     *vs_start(4).gt.0.0) then
         write(6,*) 'Low S-velocity layer in layer 3'
         stop
      endif
      if(nsvelocity4.gt.1.and.
     *(vs_start(4)+vs_delta(4)*(nsvelocity4-1)).ge.vs_start(5).and.
     *vs_start(5).gt.0.0) then
         write(6,*) 'Low S-velocity layer in layer 4'
         stop
      endif
      if(nsvelocity5.gt.1.and.
     *(vs_start(5)+vs_delta(5)*(nsvelocity5-1)).ge.vs_start(6).and.
     *vs_start(6).gt.0.0) then
         write(6,*) 'Low S-velocity layer in layer 5'
         stop
      endif
      if(nsvelocity6.gt.1.and.
     *(vs_start(6)+vs_delta(6)*(nsvelocity6-1)).ge.vs_start(7).and.
     *vs_start(7).gt.0.0) then
         write(6,*) 'Low S-velocity layer in layer 6'
         stop
      endif
      if(nsvelocity7.gt.1.and.
     *(vs_start(7)+vs_delta(7)*(nsvelocity7-1)).ge.vs_start(8).and.
     *vs_start(8).gt.0.0) then
         write(6,*) 'Low S-velocity layer in layer 7'
         stop
      endif
                 
c

      close(2)
      if(i.ge.2) then
         multi_model=.true.
c
c   calculate number of tests
c
      k=ndepth2*ndepth3*ndepth4*ndepth5*ndepth6*ndepth7*ndepth8
      k=k*nvelocity1*nvelocity2*nvelocity3*nvelocity4*nvelocity5
     **nvelocity6*nvelocity7*nvelocity8
      k=k*nsvelocity1*nsvelocity2*nsvelocity3*nsvelocity4*nsvelocity5
     **nsvelocity6*nsvelocity7*nsvelocity8*nvpvs

c      write(6,*)
c     *ndepth1,ndepth2,ndepth3,ndepth4,ndepth5,ndepth6,ndepth7,ndepth8
c      write(6,*)nvelocity1,nvelocity2,nvelocity3,nvelocity4,nvelocity5
c     *,nvelocity6,nvelocity7,nvelocity8
c      write(6,*)(v_start(k),v_delta(k),vs_start(k),vs_delta(k),k=1,10)
         nl=i

         write(6,*)' Multi model mode, number of layers:',i
         write(6,*)' Number of models: ',k
         write(6,*)' Continue(n/y=return) ?'
         read(5,'(a)') text
         if(text(1:1).ne.' '.and.text(1:1).ne.'y') stop
         open(2,file='h_models.out',status='unknown')
      endif

 8877 continue

c
c   vpvs is outer loop
c

      do ivpvs=1,nvpvs
      if(multi_model) then
          if(nvpvs.gt.0) then
             pos=vpvs_start+(ivpvs-1)*vpvs_delta
          endif
      endif

      do ivelocity1=1,nvelocity1
      if(multi_model) then
         v(1)=v_start(1)+(ivelocity1-1)*v_delta(1)
         vs(1)=v(1)/pos
      endif

      do isvelocity1=1,nsvelocity1
      if(multi_model.and.nsvelocity1.gt.1) then
         if(nvpvs.eq.1) vs(1)=vs_start(1)+(isvelocity1-1)*vs_delta(1)
      endif
   

      do ivelocity2=1,nvelocity2
      if(multi_model) then
         v(2)=v_start(2)+(ivelocity2-1)*v_delta(2)
         vs(2)=v(2)/pos
      endif

      do isvelocity2=1,nsvelocity2
      if(multi_model.and.nsvelocity2.gt.1) then
         if(nvpvs.eq.1) vs(2)=vs_start(2)+(isvelocity2-1)*vs_delta(2)
      endif
  
      do idepth2=1,ndepth2
      if(multi_model) then
         d(2)=d_start(2)+(idepth2-1)*d_delta(2)
      endif

      do ivelocity3=1,nvelocity3
      if(multi_model) then
         v(3)=v_start(3)+(ivelocity3-1)*v_delta(3)
         vs(3)=v(3)/pos
      endif

      do isvelocity3=1,nsvelocity3
      if(multi_model.and.nsvelocity3.gt.1) then
         if(nvpvs.eq.1) vs(3)=vs_start(3)+(isvelocity3-1)*vs_delta(3)
      endif


      do idepth3=1,ndepth3
      if(multi_model) then
         d(3)=d_start(3)+(idepth3-1)*d_delta(3)
      endif

      do ivelocity4=1,nvelocity4
      if(multi_model) then
         v(4)=v_start(4)+(ivelocity4-1)*v_delta(4)
         vs(4)=v(4)/pos
      endif

      do isvelocity4=1,nsvelocity4
      if(multi_model.and.nsvelocity4.gt.1) then
         if(nvpvs.eq.1) vs(4)=vs_start(4)+(isvelocity4-1)*vs_delta(4)
      endif


      do idepth4=1,ndepth4
      if(multi_model) then
         d(4)=d_start(4)+(idepth4-1)*d_delta(4)
      endif

      do ivelocity5=1,nvelocity5
      if(multi_model) then
         v(5)=v_start(5)+(ivelocity5-1)*v_delta(5)
         vs(5)=v(5)/pos
      endif

      do isvelocity5=1,nsvelocity5
      if(multi_model.and.nsvelocity5.gt.1) then
         if(nvpvs.eq.1) vs(5)=vs_start(5)+(isvelocity5-1)*vs_delta(5)
      endif
  
 

      do idepth5=1,ndepth5
      if(multi_model) then
         d(5)=d_start(5)+(idepth5-1)*d_delta(5)
      endif

      do ivelocity6=1,nvelocity6
      if(multi_model) then
         v(6)=v_start(6)+(ivelocity6-1)*v_delta(6)
         vs(6)=v(6)/pos
      endif

      do isvelocity6=1,nsvelocity6
      if(multi_model.and.nsvelocity6.gt.1) then
         if(nvpvs.eq.1) vs(6)=vs_start(6)+(isvelocity6-1)*vs_delta(6)
      endif


      do idepth6=1,ndepth6
      if(multi_model) then
         d(6)=d_start(6)+(idepth6-1)*d_delta(6)
      endif

      do ivelocity7=1,nvelocity7
      if(multi_model) then
         v(7)=v_start(7)+(ivelocity7-1)*v_delta(7)
         vs(7)=v(7)/pos
      endif

      do isvelocity7=1,nsvelocity7
      if(multi_model.and.nsvelocity7.gt.1) then
         if(nvpvs.eq.1) vs(7)=vs_start(7)+(isvelocity7-1)*vs_delta(7)
      endif


      do idepth7=1,ndepth7
      if(multi_model) then
         d(7)=d_start(7)+(idepth7-1)*d_delta(7)
      endif

      do ivelocity8=1,nvelocity8
      if(multi_model) then
         v(8)=v_start(8)+(ivelocity8-1)*v_delta(8)
         vs(8)=v(8)/pos
      endif

      do isvelocity8=1,nsvelocity8
      if(multi_model.and.nsvelocity8.gt.1) then
         if(nvpvs.eq.1) vs(8)=vs_start(8)+(isvelocity8-1)*vs_delta(8)
      endif


      do idepth8=1,ndepth8
      if(multi_model) then
         d(8)=d_start(8)+(idepth8-1)*d_delta(8)
      endif
 



      keys(1:4)='NEXT'    ! start with next event



c
c set covariance matrix to 0, lo 14/11/2018
c
      do l=1,3
        do m=1,3
          covar(l,m)=0.
        enddo
      enddo

c 3/94: added init call to zero residual summary

      call hypocent(4,read2,write2,write1,write3,read3,init,
     &'N',data,modnam,eventno,dlt,isort,test,yter1,maxline,nstat,
     &locate,covar,oterr,nhead,nrecord,use_eev,nmessage,message,agency)

C
c
c---------------------------------------------------------------------
c  event loop starts here, always come back here after locating one event
c  or to make a new choice in interactive mode
c---------------------------------------------------------------------
c
 50   continue
      nmessage=0

c
C----------------------------------------------------------------------
C   Point to same, next or another event 
C----------------------------------------------------------------------
C
      if(base.eq.0) then              ! data base event
         call findevin
     *   (basename,starttime,endtime,keys,0,eventno,
     *   eventfile,fstart,newmonth,status)
c
c   check if end of time interval or errors, then
c   close hyp.out and stop
C     
         if(status.gt.0) then
            write(6,*)' STOP WITH STATUS=',status
            GOTO 900   ! stop
         endif
C
C   open data base input single event file and read it
c   set flag for single event nrecord= -1
c
         nrecord=-1
         call sei open( old$,            ! Open old file (stop on error).
     &                  ' ',             ! No prompt.
     &                  eventfile, read1,! File & unit.
     &                  b_flag,          ! File exists? (n/a).
     &                  code )           ! Local condition (n/a).
         call indata(read1,nstat,nphase,nhead,nrecord,
     &               typ,exp,data,id)
         datsave=data(1)
         call sei close( close$, read1, code ) ! Close file (stop on error).
c
      else                             ! single file  multiple event mode
c
         if(keys(1:4).eq.'NEXT') then
            call indata(read1,nstat,nphase,nhead,nrecord,
     &               typ,exp,data,id)
            datsave=data(1)

            if(nrecord.eq.0) goto 900   ! check for end of file
            eventno=eventno+1

         endif
c
c  selection by number 
c
         if(keys(1:1).eq.'#') keys(1:4)=keys(2:5)
         if(keys(1:1).eq.'1'
     *   .or.keys(1:1).eq.'2'.or.keys(1:1).eq.'3'
     *   .or.keys(1:1).eq.'4'.or.keys(1:1).eq.'5'
     *   .or.keys(1:1).eq.'6'.or.keys(1:1).eq.'7'
     *   .or.keys(1:1).eq.'8'.or.keys(1:1).eq.'9') then
            read(keys(1:4),'(i4)') nfind
            if(keys(4:4).eq.' ') read(keys(1:3),'(i3)') nfind
            if(keys(3:3).eq.' ') read(keys(1:2),'(i2)') nfind
            if(keys(2:2).eq.' ') read(keys(1:1),'(i1)') nfind
            if(nfind.eq.0) goto 75    ! make choice
            if(nfind.eq.eventno) goto 75
c
            if(nfind.lt.eventno) then
            rewind( read1, iostat=code )                ! Rewind unit.
            call sei code( fort$, code, read1, b_flag ) ! Process outcome.
            eventno = 0
            k = nfind
c
            else
            k = nfind - eventno
            endif
c
            do i=1,k
               call indata(read1,nstat,nphase,nhead,nrecord
     *         ,typ,exp,data,id)
               datsave=data(1)
               eventno=eventno+1
               if(nrecord.eq.0) then
                  write(6,*)' no such number ',nfind
                  rewind( read1, iostat=code )                ! Rewind unit.
                  call sei code( fort$, code, read1, b_flag ) ! Process outcome.
                  eventno=0
                  keys(1:4)='NEXT'
                  goto 50
               endif
            enddo
        endif
      endif
c
c--------------------------------------------------------------
c  decide what to do next if in interactive mode
c--------------------------------------------------------------
c
 75   continue
      if(yter.eq.'Y') then

c 4/94: inserted section to calculate the real # phases, nphs
c I want ALL the phases and NOT including azimuths (BRL)
        isindex=2
        do while (data(isindex)(80:80).ne.'4'.and.data(isindex)(80:80)
     &  .ne.' ')
          isindex=isindex+1
        enddo
        isindex=isindex-1  
        nphs=nrecord-1-isindex                   
        
          write(6,289) eventno,data(1)(1:22),nphs
 289      format(1x,'#',i5,1x,a22,' NPHS=',i4,3x,'T Q L #XXX ', $)
          read(5,'(a)') keys
          if(keys(1:4).eq.'    ') keys(1:4)='NEXT'
          if ( keys(1:1) .eq. 'Q'.or. keys(1:1) .eq. 'q' ) goto 900
c
c   type event
c
         if (keys(1:1).eq.'t'.or.keys(1:1).eq.'T') then
              write(6,*)
              nline=0
              do i=1,nrecord
                 write(6,'(a)')data(i)
                 nline=nline+1
                 if(nline.gt.maxline) then
                    nline=0

c   changed to not give CR (BRL)
                    write(6,'('' Return to continue'',$)')
                    
                    read(5,'(a)') text
                 endif
               enddo
               keys(1:4)='SAME'      ! use same event
               goto 50               ! back for new choice
          endif
c
c   back one event
c
          if(keys(1:1).eq.'b'.or.keys(1:1).eq.'B') then
              keys(1:4)='BACK'
              goto 50
          endif
c
c  scroll in print.out file
c
          if(keys(1:1).eq.','.or.keys(1:1).eq.'.') then
              call hyp_print(write1,keys(1:1),maxline)
              keys(1:4)='SAME'
              goto 50 
          endif
C 
C   if location has been chosen go on, else restart in 50
C
          if(keys(1:1).ne.'l'.and.keys(1:1).ne.'L') goto 50
      endif
C
C------------------------------------------------------------------------
c   location start here
C--------------------------------------------------------------------------
C

 1000 continue
c
c   assume this is after year 1900 nad before 2000 and put in header
C
      if(data(1)(2:3).eq.'  ') data(1)(2:3)='19'
c
c   check if disable location has been set on for this event, in which case
c   event is passed on with no modification to hyp.out
c
      if(data(1)(45:45).eq.'*') then

        if(.not.from_se) write(6,*)'Event flagged not to be located'
        nmessage=nmessage+1
        message(nmessage)='Event flagged not to be located'
        keys(1:4)='NEXT'
        goto 31               ! go to outputs
      endif
c
c   check if a new model has to be read in
c
c      write(*,*)model,data(1)(21:21)
      if(model.ne.data(1)(21:21)) then
         model=data(1)(21:21)
         call read_stat_mod
     *   (agency,model_code,model,minstat,minphase,
     *   modnam,loc_indicator,read2,write2,
     &   write1,write3,read3,isort,test,dlt,
     *   yter1,maxline,use_eev)

         if(model.eq.'@') then
            nmessage=nmessage+1
            message(nmessage)=' No STATION'//data(1)(21:21)//'.HYP file'
            if(message(nmessage)(8:8).eq.' ') message(nmessage)(8:8)='0'

            if(.not.from_se)
     *      write(6,'(a)') message(nmessage)

            if(from_se) goto 999     ! return, fix jh mar 6, 2016
         else

            if(.not.from_se) write(*,*)' Model changed'
            nmessage=nmessage+1
            message(nmessage)=' Model changed'
         endif
      endif

c
c set xnear, xfar and starting depth from sfile
c
      xxfar=-1.
      xxnear=-1.
c
      if (test(107).eq.1.) then
        do i=1,nhead
          if (data(i)(2:6).eq.'XNEAR'.and.data(i)(80:80).eq.'3') then
cXNEAR  150.0 XFAR  300.0 SDEP 7.5
c format changed lot 20/12/2005
            read(data(i)(2:37),'(6x,f6.1,6x,f6.1,6x,f5.1)') 
     &           xxnear,xxfar,ztr
            if(write1.gt.0) then
              write(write1,*)
              write(write1,'(a)') 
     &          ' starting depth, xnear, xfar '//
     &          'from event file '
              write(write1,'(4(f6.2,1x))') ztr,xxnear,xxfar
            endif
          endif
        enddo
      endif

c
c set velocity model from event file, BGS, lot 11/5/2005
c
      if(extension(1:3).eq.'BGx') then
        xxfar=-1.
        xxnear=-1.
        k=0
        do i=1,nhead
          if (data(i)(79:80).eq.'93') then
c4.00  0.00/ 5.90  2.52/ 6.45  7.55/ 7.00 18.87/ 8.00 34.15/                  93
            read(data(i),'(6(f5.2,1x,f5.2,1x))')
     &        v(k+1),d(k+1),v(k+2),d(k+2),v(k+3),d(k+3),v(k+4),d(k+4),
     &        v(k+5),d(k+5),v(k+6),d(k+6)
            k=k+6
c            write(*,*) data(i)
          elseif (data(i)(79:80).eq.'A3') then
c  0. 100. 200. 1.73                                                          A3
            read(data(i),'(3f5.0,f5.2)') ztr,xxnear,xxfar,pos
c            write(*,*) data(i)
          endif
        enddo
        if (k.gt.0.and.pos.gt.0.) then
          nl=0 
          do i=1,k
            if (v(i).ne.0.) then
c              write(*,*) v(i),d(i)
              nl=nl+1
              vs(i)=v(i)/pos
            endif
          enddo
c
c output
c

          if(.not.from_se) write(*,*) 
     &    ' Using model and location parameters from event file '
          nmessage=nmessage+1
          message(nmessage)=
     *    ' Using model and location parameters from event file '
          if(write1.gt.0) write(write1,*)           
          if(write1.gt.0)
     &  write(write1,'('' Model taken from event file'',/)')
          if(write1.gt.0)
     &    write(write1,'('' Depth, km   Vp, km/s   Vs, km/s'')')
          do  i=1,nl
c         reff=' '
c         if(i.eq.nconrad)reff='B'
c         if(i.eq.nmoho)reff='N'
           if(write1.gt.0)write(write1,'(1x,f9.2,2f11.2,3x)')
     &      d(i),v(i),vs(i)
c,reff
          end do
          
          if(write1.gt.0) then
            write(write1,*)
            write(write1,'(a)') ' starting depth, xnear, xfar, vp/vs '//
     &        'from event file '
             write(write1,'(4(f6.2,1x))') ztr,xxnear,xxfar,pos
          endif

c convert depths to thicknesses and store parameters in parm(i)
          do i=1,nl-1
            parm(nl+i)=d(i+1)-d(i)
c            if(i.eq.1.and.test(40).eq.0.0)then
c              parm(nl+i)=d(i+1)-d(i)+float(maxelv)*.001
c            endif
          end do
          do i=1,nl
            parm(i)=v(i)
          enddo
        endif
      endif
C
C check if type of event should be located 
C
      locate=.false.
      do i=1,3 
         if(typ.eq.loc_indicator(i)) locate=.true.
      enddo
c
c   check if only magnitudes should be calculated
c
      if(test(106).eq.1.0) then
         locate=.false.
         goto 30
      endif
c
c  check if event has enough phases and stations and if it
c  should be located
c      
      if(nphase.ge.minphase.and.locate.
     *and.nstat.ge.minstat) then  
c
c   set fixdepth indicator
c
         if(fixdepth .ne. ' ')then
            data(1)(44:44)=fixdepth
         endif
c
c  if type is E or P, fix depth to 0.0 km
c  
         if(exp.eq.'P'.or.exp.eq.'E') then
            data(1)(39:44)='  0.0F'
         endif

c
c  if type is G or C (Glacial/Cryo), fix depth to 0.0 km
c  
         if(data(1)(23:23).eq.'G'.or.data(1)(23:23).eq.'C') then
            data(1)(39:44)='  0.0F'
         endif

      else
         locate=.false.
         write(6,291)typ,nphase
 291     format
     *   (1x,'type',1x,a1,2x,'NPHASE=',
     *   i2,'  NOT LOCATABLE')

         nmessage=nmessage+1
         write(message(nmessage),291) typ,nphase 
         if(nphase.lt.minphase) then
            write(6,*)' Too few phases'
            nmessage=nmessage+1
            message(nmessage)=' Too few phases'

            if(from_se) goto 999      ! return

         endif
         if(nstat.lt.minstat) then
            write(6,*) ' Too few stations'
            nmessage=nmessage+1
            message(nmessage)=' Too few stations'

            if(from_se) goto 999      ! return
         endif
c
c  check if location indicator is set or correctly set
c
         if(typ.ne.'L'.and.typ.ne.'R'.and.typ.ne.'D') then

            write(6,*)' Missing or wrong event type, must be L,R or D'
            nmessage=nmessage+1
            message(nmessage)=
     *      ' Missing or wrong event type, must be L,R or D'

            if(from_se) goto 999      ! return
         endif

         keys='NEXT'
         locate=.false.       ! was not located

C
C  No location, still try to calculate magnitudes  
C
         go to 30
      endif
C
c    locate one event
C
      init=2
      if(yter.eq.'Y') write(6,*)
c
c   if interactive output, position print file at end
c
      if(yter.eq.'Y') call hyp_print(write1,'e',maxline) 
c
c   clear old parameters
c
      call clear_old(data,nhead,nrecord,agency)

      do i=nhead+1,nrecord
         data(i)(61:70)='          '
c
c   keep distance if coda is there, so that magnitude can be calculated
c   in case not enough data for a location
c
         if(data(i)(30:33).eq.'    ') data(i)(71:75)='     '
         data(i)(76:79)='    '
      enddo

c
c   save year month and day before updating and calculate abs time
c
      read(data(1),'(1x,i4,1x,2i2)') year,month,day
      call  TIMSEC (year,month,day,0,0,0.0,abstim1)
c
c   save old header line
c
      data_old=data(1)
c     if(multi_model) then
c     write(6,*)' call hyp'
c     call hypocent(2,read2,write2,-1,write3,read3,
c    &init,'N',data,modnam,eventno,
c    &dlt,isort,test,yter1,maxline,nstat,locate,
c    &covar,oterr,nhead,nrecord,use_eev)
c     write(6,*) 'fin call hyp'
c     else


c -----------------------------------------------------------
c   check if grid search
c -----------------------------------------------------------
c
      if(grid_search) then
         multi_model=.true.   ! limit output
         open(27,file='gridsearch.out',status='unknown')
 4636    continue
         write(6,*) 'Latitude range and step'
         read(5,*) minlat,maxlat,dellat
         write(6,*) ' Longitude range and step'
         read(5,*)minlon,maxlon,dellon
         nlat=(maxlat-minlat+0.0001)/dellat+1
         nlon=(maxlon-minlon+0.0001)/dellon+1
         if(nlat.gt.71.or.nlon.gt.71) then
             write(6,*)
     *       ' Number of latitude and longitude points are: ',nlat,nlon
             write(6,*)' Max number is 71, choose again'
             goto 4636
         endif
c
c   write to contouring file
c
         write(27,'(a)')' Fields to use'
         write(27,'(a,2f10.3,i10)')
     *   ' Latitude range and number of values    ',
     *   minlat,maxlat,nlat
         write(27,'(a,2f10.3,i10)')
     *   ' Longitude range and number of values   ',
     *   minlon,maxlon,nlon
         write(27,'(a,a)')
     *   ' Contour level to plot and color          90.0'
         write(27,'(a,a)')
     *   ' Contour level to plot and color          50.0'
         write(27,'(a,a)')
     *   ' Contour level to plot and color          20.0'
         write(27,'(a,a)')
     *   ' Contour level to plot and color           5.0'
         write(27,'(a,a)')
     *   ' Contour level to plot and color           2.0'
         write(27,'(a,a)')
     *   ' Contour level to plot and color           1.8'
         write(27,'(a,a)')
     *   ' Contour level to plot and color           1.6'
         write(27,'(a,a)')
     *   ' Contour level to plot and color           1.4'
         write(27,'(a,a)')
     *   ' Contour level to plot and color           1.2'
         write(27,'(a,a)')
     *   ' Contour level to plot and color           1.0'
         write(27,'(a,a)')
     *   ' Contour level to plot and color           0.9'
         write(27,'(a,a)')
     *   ' Contour level to plot and color           0.8'
         write(27,'(a,a)')
     *   ' Contour level to plot and color           0.7'
         write(27,'(a,a)')
     *   ' Contour level to plot and color           0.6'
         write(27,'(a,a)')
     *   ' Contour level to plot and color           0.5'
         write(27,'(a,a)')
     *   ' Contour level to plot and color           0.4'
         write(27,'(a,a)')
     *   ' Contour level to plot and color           0.3'
         write(27,'(a,a)')
     *   ' Contour level to plot and color           0.2'
         write(27,'(a,a)')
     *   ' Contour level to plot and color           0.1'
         data(1)(44:45)='FF'        ! fix hypocenter
         do ila=1,nlat
            grid_lat=minlat+dellat*(ila-1)
            write(6,*) 'Lat=',grid_lat
            do ilo=1,nlon
               grid_lon=minlon+dellon*(ilo-1)
               write(data(1)(24:38),'(f7.3,f8.3)') grid_lat,grid_lon

               call hypocent(2,read2,write2,write1,write3,read3,
     &         init,'N',data,modnam,eventno,
     &         dlt,isort,test,yter1,maxline,nstat,locate,
     &         covar,oterr,nhead,nrecord,use_eev,nmessage,message,
     &         agency)
               g_rms=one_rms
               write(27,*) grid_lon,grid_lat,g_rms
               if(g_rms.lt.grid_rms) then
                  grid_rms=g_rms
                  maxlat=grid_lat
                  maxlon=grid_lon
               endif
            enddo
         enddo
c
c   get output at best location
c
         multi_model=.false.
         write(data(1)(24:38),'(f7.3,f8.3)') maxlat,maxlon
         call hypocent(2,read2,write2,write1,write3,read3,
     &   init,'N',data,modnam,eventno,
     &   dlt,isort,test,yter1,maxline,nstat,locate,
     &   covar,oterr,nhead,nrecord,use_eev,nmessage,message,agency)
         write(6,*)maxlat,maxlon,grid_rms
         close(27)
      else   

cccccccccccccc
cccccccccccccccccccccccccc end of grid search block cccccccccccccccccccccccccccc
cccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c reject phases
c

       
         if(reject) then 
            
            write(6,*) '***** Remove bad pics *****'
            nmessage=nmessage+1
            message(nmessage)='***** Remove bad pics *****'

            call reject_phases(read2,write2,write1,write3,read3,
     &      init,data,modnam,eventno,
     &      dlt,isort,test,yter1,maxline,nstat,locate,
     &      covar,oterr,nhead,nrecord,use_eev,data5)

c   normal solution
c
         else

c            write(*,*) ' debug lo hypmain nstat = ',nstat

            call hypocent(2,read2,write2,write1,write3,read3,
     &      init,'N',data,modnam,eventno,
     &      dlt,isort,test,yter1,maxline,nstat,locate,
     &      covar,oterr,nhead,nrecord,use_eev,nmessage,message,agency)

c            write(*,*) ' debug lo hypmain nstat = ',nstat
c
c  return if no location in se
c
            if(from_se.and..not.locate) goto 999 !return
         endif
      endif
c 
c   if multi model mode, go to next event
c
       if(multi_model) then
          write(6,2838) eventno,data(1)(1:71)

c
c  get depth for averge depth
c
          read(data(1)(39:43),*) mdepth
          data(1)=datsave
          avdepth=avdepth+mdepth
          nmdepth=nmdepth+1
          goto 50                !next event
       endif 
c
c   check if located
c
      if(.not.locate) goto 30
c
c   check if day has changed, if so correct all arrival times by + or -24 hours
c
      read(data(1),'(1x,i4,1x,2i2)') year,month,day
      call  TIMSEC (year,month,day,0,0,0.0,abstim2)
      if(dabs(abstim2-abstim1).gt.10.0) then  ! add 10 for posible round off err
          delhour=(dabs(abstim1-abstim2)+10.0)/3600
         if(abstim2.gt.abstim1) delhour=-delhour
         write(6,*)' Day has changed, hours added:', delhour

         nmessage=nmessage+1
         write(message(nmessage),*)' Day has changed, hours added:', 
     *   delhour
         write(write1,*,iostat=code)
     &         ' Day has changed, hours added:', delhour
         call sei code( fort$, code, write1, b_flag ) ! Process the outcoe.
c
         do i=nhead+1,nrecord-1
            read(data(i)(19:20),'(i2)') hour
            hour=hour+delhour
            write(data(i)(19:20),'(i2)') hour
         enddo
      endif
c
c   print out event on screen 
C
         if(yter.eq.'Y')
     *   call hyp_print(write1,'p',maxline) ! get output from print file
         if(yter.eq.'Y') write(6,*)
c
c-----------------------------------------------------------------------
c   enter here if event could not be located to only calculate magnitude
c   or no locate is desired by test 106 set
c-----------------------------------------------------------------------
c
 30   continue
c
c   clear old magnitudes, except 3.
c
      data(1)(57:72)='                '
      if(data(1)(77:79).eq.agency) data(1)(72:79)=' '
c
c   in case event not located, clear out old locations etc, however
c   keep 3. magnitude, unless same agency
c   if location is fixed, do not clean out header info
c   if test 106 set, do not clean location or location agency
C
      if(.not.locate.and.test(106).eq.0.0) then
         if(data(1)(44:44).ne.'F')data(1)(39:43)='     '
         if(data(1)(45:45).ne.'F')data(1)(24:38)='               '
         if(data(1)(44:45).ne.'FF') data(1)(46:48)='   ' ! keep old aga if fixe
         data(1)(49:72)='                        '
       endif

c
c   enter here if no location is made by using flag *
c
 31   continue
c
c  put in agency unless both epicenter and depth has been fixed
c  in which case it must the existing agency resposible for hypocenter
c  or if no location done
c
      if(data(1)(44:45).ne.'FF'.and.data(1)(45:45).ne.'*') 
     *data(1)(46:48)=agency
C
c   calculate  magnitudes, if no location flag is not set
C

         if(data(1)(45:45).ne.'*') then

            call update_mag(data,nhead,nrecord,agency,test,output)
            
c
c  update and average spectral information
c
            call update_spec(data,nrecord,nhead,agency,test,output)
         endif

c   output header line on screen, if not interactive, event number must be there
C
         if(yter.eq.'Y') then
            write(6,'(a)') data(1)(1:79)
c
c   write any second header line with additional magnitudes for prime solution
c
            do i=2,nhead
               if(data(1)(1:23).eq.data(i)(1:23).and.data(1)(46:48).eq.
     *            data(i)(46:48)) write(6,'(a)') data(i)(1:79)
            enddo
            write(6,'(a,a)') ' OLD:',data_old(6:79)

C
            if(extension(1:3).eq.'BGS') then
             if (data(1)(22:22).eq.'L') then
               read(data(1)(24:39),'(f7.3,f8.3)') bnglat,bnglon
               call latlon2ukgrid(bngerr,bnglat,bnglon,bnge,bngn)
               write(6,
     *      '('' Grid ref    : '',f9.3,'' East /'',f9.3,'' North'')')
     *            bnge,bngn
             endif
            endif
            write(6,*)
         else

            if(.not.from_se) write(6,2838) eventno,data(1)(1:71)
 2838       format(1x,'#',i9,1x,a71)
         endif
C
c  write normal output file, sort first if event has been located
C
c
c   nrecord might have changed if new input format since 
c   some new phases might have been created in rea routines like END
c


      if(rea_new_in) then
c         nhead=rea_nhead
c         nrecord=rea_nrecord
      endif

c 4/94: added sort by distance option
c
c     if(test(71).ne.0.0.and.locate)then

      if(test(71).ne.0.0)then
        do i=nhead+1,nrecord-1
          read(data(i)(71:75),'(f5.0)')dlt(i-nhead)
        end do

        call r4sort(nrecord-1-nhead,dlt,isort)
c
c   now sort so the distance sorted phase lines, for each group of
c   as in the original file
c
        old_dist=data(isort(1)+nhead)(71:75)
        old_stat=data(isort(1)+nhead)(2:6)
        k=0
        l=0
        do i=nhead+1,nrecord-1
           l=l+1
           read(data(isort(l)+nhead),'(18x,2i2,f6.2)') hour,min,sec
           if(data(isort(l)+nhead)(71:75).eq.old_dist.and.
     *     data(isort(l)+nhead)(2:6).eq.old_stat) then 
              k=k+1
              lsort(k)=isort(l)
              dlt(k)=sec+min*60+hour*3600
              if(i.ne.nrecord-1) goto 73   ! at the end, always sort what is left
              l=l+1                        ! since this is last value
           endif
c
c   if here, new distance or station or last group
c
           if(k.gt.1) then
               call r4sort(k,dlt,ksort)
               m=1
               do j=l-k,l-1
                  isort(j)=lsort(ksort(m))
                  m=m+1
               enddo 
           endif
           if(i.eq.nrecord-1) goto 73
           old_dist=data(isort(l)+nhead)(71:75)
           old_stat=data(isort(l)+nhead)(2:6)
           k=1    ! this is the first of the next group
           dlt(k)=sec+min*60+hour*3600
           lsort(k)=isort(l)
 73        continue
         enddo
c
c   put back in order in original array
c
        do i=1,nhead
           data2(i)=data(i)
        enddo 
        do i=nhead+1,nrecord-1
           data2(i)=data(isort(i-nhead)+nhead)
        enddo
        do i=1,nrecord-1
           data(i)=data2(i)
        enddo 
c        
c sort data5 -------------------------------------------------------------
c
        do i=nhead+1,nrecord-1
           data120(i)=data5(isort(i-nhead)+nhead)
        enddo
        do i=nhead+1,nrecord-1
           data5(i)=data120(i)
c           write(*,*) nhead,i,data(i)
c           write(*,*) nhead,i,data5(i)
        enddo
      endif
cccccccccccccccccccccccccccccccccc   end of sorting  ccccccccccccccccccccc

c
c   save in rea common block possibly for SE and/or for new format output
c   extra parameters com ntlo etc come from data5 since they are not
c   in array data
c
      rea_nhead=nhead
      rea_nrecord=nrecord  
      rea_new_in=.false.     ! since internal format is old format 

      call rea_event_in(0,.true.,data,code)

c
c   optionally put in geo location
c
      if(test(124).gt.0.0.and.hyp_lat(1).gt.-100.0.and.
     *   nlocality.gt.0) then
         call geo_location_many
     *   (nlocality,locality_lat,locality_lon,locality)
      endif   
c
c   if new format out, convert
c   if geo location, it must be corrected or added to array data
c
      if(rea_new_out.or.(test(124).gt.0.0.and.nlocality.gt.0)) then
         call rea_event_out(0,.true.,data,code)
         nrecord=rea_nrecord  ! will change if az, coda and amp readings on phase line
      endif
c
c   write out with or without sorting, this could be new format
c     
      write(write2,'(a80)',iostat=code)
     &(data(i),i=1,nrecord)
      call sei code(fort$,code,write2,b_flag) ! process outcome.

c
c  write solution to be used with gmap   ! fix new
c
cxx      call gmap_auto(data_old,data)   new fix - pv:did a fix, testing needed
c     write(6,*) 'debug: call gmap_auto(data_old,data)'
      call gmap_auto(data_old,data)
c
c   pause here if from seisan explorer, get info if update, 
c   previous se implementation
c

      keep_old_id_line=.true.
    
    
c
c   possibely update, this section not affected by new format since only header
c   change
c 
         if(update)then
            if(data(1)(45:45).eq.'*') then
               write(6,*)' Event marked for no location'
               write(6,*)' Event will not be updated'
               write(6,*)' Enter to continue'
               read(5,'(a)') text
               goto 2626
             endif
 
             if(.not.overwrite) then
                write(6,*)' You are now about to overwite the current ',
     *          'event in the data base.'
                write(6,*)' with the solution just shown'
                write(6,*) ' The catalog is not updated !!!!!'
                write(6,*)' Sure you want to update, (y/n) ?'
                read(5,'(a)') answer
             else             ! do not ask for confirmation
                answer='y' 
             endif
             if(answer.eq.'Y'.or.answer.eq.'y') then
c
c   get system time
c
                 call systime(p_time,proc_time)
c
c   find id line
c
                 id=0  ! was i
                 do i=1,nhead
                    if(data(i)(80:80).eq.'I') id=i
                 enddo
                 if(id.eq.0) then  ! was i hj nov 26 19
                    write(6,*)
     *              ' No id line, will stop, enter to continue'
                    read(5,'(a)') text
                    stop 0       ! do not update in explorer
                 endif
c
c   update id line
c
                 id_old=data(id) 
                 WRITE(DATA(ID)(31:34),'(A)')OPERATOR
                 WRITE(data(id)(13:26),'(A)')PROC_TIME
                 WRITE(data(id)(9:11),'(A)')'UP ' 
c 
c  open sfile to write       
c
                 call sei open( old$,            ! Open old file (stop on error).
     &           ' ',             ! No prompt.
     &           eventfile, read1,! File & unit.
     &           b_flag,          ! File exists? (n/a).
     &           code )           ! Local condition (n/a).
c
                 do i=1,nrecord
                    write(read1,'(a)',iostat=code) data(i)
c
c  duplicate id line
c
                     if(i.eq.id.and.keep_old_id_line)then
                        id_old(80:80)='3'
                        id_old(2:7)='OLDACT'
                        write(read1,'(a)') id_old
                     endif
                  enddo
                  call sei close (close$,read1,code)
                  exit_code=1  ! signal that seisan exp must be updated
               endif  ! yes to update
            endif     ! update
 2626    continue     ! Not to be located
c
c
c write out brief format output
c
      if(extension(1:3).eq.'BGS') then
         call make_hypoc_brief(data,nstat,nphase,nhead,nrecord)
      endif
c
c   return here if using eev
c
      if(use_eev) goto 999
c
c   return here if from se
c
      if(from_se) goto 999 
c
c   running in batch
c
      if(yter.ne.'Y') keys='NEXT'
c
c   interactive
c
      if(yter.eq.'Y') keys='SAME'

      data(1)=datsave
c
c   if interactive with a file and new format out, the interenal format
c   must be set back to old nordic
c
      if(base.eq.1.and.yter.eq.'Y'.and.rea_new_out) then
         rea_new_out=.false.
         call rea_event_out(0,.true.,data,code)
         rea_new_out=.true.


      endif
      go to 50                                        
C
c   this was the end
C
  900 continue
      if(yter.eq.'Y') call hyp_print(write1,'e',maxline)

      if(.not.from_se) then 
      if(yter.ne.'Y'.and.(.not.multi_model)) 
     *write(6,*)' Making residual summary'
      call hypocent(3,read2,write2,write1,write3,read3,
     &init,'N',data,modnam,eventno,
     &dlt,isort,test,yter1,maxline,nstat,locate,
     &covar,oterr,nhead,nrecord,use_eev,nmessage,message,agency)
      endif
      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc 
c  multi model inversion
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      if(multi_model) then
         if(rmssum.lt.rmssum_min.and.rmssum.gt.0.0001) then
            rmssum_min=rmssum
         endif
         if(rmssum.gt.rmssum_max.and.rmssum.gt.0.0001) then
            rmssum_max=rmssum
         endif

c
c  calculate average depth
c

         avdepth=avdepth/nmdepth
         nmdepth=0

c
c   check range of minimum models, if smaller than largest one stored, add
c   nmin is number of minimum models

         rmax=-1.0 
         do k=1,nmin
            if(rms_min(k).gt.rmax) then
                rmax=rms_min(k)
                kmax=k
            endif
         enddo
         if(rmssum.lt.rmax) then
            do i=1,nl
              v_min(kmax,i)=v(i)
              vs_min(kmax,i)=vs(i)
              d_min(kmax,i)=d(i)
              vpvs_min(kmax)=pos
              rms_min(kmax)=rmssum
              mdepth_min(kmax)=avdepth             
            enddo
         endif
c
c   do another test with data set
c
         rewind(read1)

         
c
c  write current model used with whole data set
c
         write(2,'(f7.3,2x,f6.3,31f6.2)') 
     *   rmssum,pos,(v(i),vs(i),d(i),i=1,nl),avdepth

         avdepth=0.0
      endif
c
c   enddo for multi model
c 
      enddo 
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo 
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo


      if(multi_model) then
c
c   maybe less than 10 nmin models
c
         do i=1,nmin
            if(vpvs_min(i).eq.0.0) goto 3344
         enddo
 3344    continue
         nmin=i-1

c
c   sort the models after rms
c
         call r4sort(nmin,rms_min,isort)

         write(2,*)
         write(2,*)' Minimum and maximum rms', rmssum_min,rmssum_max
         write(2,*)
         write(2,*)' The best models'
         write(2,*)
         write(6,*)' Minimum rms', rmssum_min
         txt='    rms   vp/vs'  

         k=20
         do i=1,nl
             txt(k:k+17)='vp    vs     d'
             k=k+18
         enddo
         txt(k-2:k+1)='av-h'

         write(2,'(a)') txt

         do k=1,nmin
            l=isort(k)
            write(2,'(f7.3,2x,f6.3,30f6.2)') 
     *      rms_min(l),vpvs_min(l),
     *      (v_min(l,i),vs_min(l,i),d_min(l,i),i=1,nl),mdepth_min(l)
         enddo
c
c   write models in hyp station0.hyp format
c
         do k=1,nmin
            l=isort(k)
            write(2,*)
            write(2,'(a,f6.3,3x,a,f6.3)') 
     *      'vp/vs= ',vpvs_min(l), 'rms=',rms_min(l)
            do i=1,nl
               write(2,'(3f7.2)')       
     *         v_min(l,i),d_min(k,i),vs_min(l,i)
            enddo
         enddo
      endif


c  close files (BRL 4/94)
 
      if(.not.from_se) then
         write(6,*)' print output in file print.out'
         write(6,*)' CAT-file in file hyp.out'
         write(6,*)' Summary file in hypsum.out'
      endif
  999 continue
      call sei close( close$+all$, 0, code ) ! Close all files.
      return 
      end                                                               

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine hyp_print(unit,action,maxline)
      implicit none
      include 'libsei.inc'
      external sei code
c
      integer   code          ! Local condition.
      logical   b_flag        ! Flag a situation?.
c
c   scroll forth and back in print.out file
c
      integer unit           ! unit for print.out file
      character*90 text      ! one line of text   ! to 90, gfortran pc
      integer maxline        ! number of lines to print
      character*1 action     ! what to do
      integer i,nline
c
c   print solution from station line
c
      if(action.eq.'p') then
         nline=0
 1       continue
         backspace( unit,iostat=code)
         call sei code(fort$,code,unit,b_flag)
c
         read(unit,'(a)',iostat=code) text 
         call sei code(fort$,code,unit,b_flag)
         if( b_flag ) goto 99
c
         if(text(4:7).eq.'date') goto 2
         backspace( unit,iostat=code)
         call sei code(fort$,code,unit,b_flag)
         goto 1
  2      continue
c
c  epi line found, write out
c
         write(6,'(a)') text(2:79)
         nline=nline+1
         read(unit,'(a)',iostat=code) text
         call sei code(fort$,code,unit,b_flag)
         if( b_flag ) goto 99
         write(6,'(a)') text(2:79)
         nline=nline+1
c
c   now go to station lines
c
 3       continue
         read(unit,'(a)',iostat=code) text 
         call sei code(fort$,code,unit,b_flag)
         if( b_flag ) goto 99
         if(text(2:4).eq.'stn') goto 4
         goto 3
c
 4       continue
         write(6,'(a)') text(1:84)     ! write only 79, gfortran pc  !nt
         nline=nline+1
 5       continue
         read(unit,'(a)',iostat=code)text 
         call sei code(fort$,code,unit,b_flag)
         if( b_flag ) goto 99
c
c   check for end of phases
c
         if(text(1:13).eq.'          ') go to 6
c
c   write station lines 
c
         write(6,'(a)') text(1:84)     ! write only 79, gfortran pc !nt
         nline=nline+1
         if(nline.gt.maxline) then
            nline=0
            write(6,'('' Return to continue, q to end listing '',$)')
            read(5,'(a)') text
            if(text(1:1).eq.'q'.or.text(1:1).eq.'Q') goto 6
         endif
         goto 5
 6       continue
         return
      endif
c
c   go back one screen
c
      if(action.eq.'.') then
         do i=1,maxline*2
           backspace (unit,iostat=code)
           call sei code(fort$,code,unit,b_flag)
         enddo
         do i=1,maxline
           read(unit,'(a)',iostat=code) text
           call sei code(fort$,code,unit,b_flag)
           if( b_flag ) goto 99
           write(6,'(a)') text
         enddo
         return
      endif
c
c  go foreward one screen
c
      if(action.eq.',') then
         do i=1,maxline
            read(unit,'(a)',iostat=code) text
            call sei code(fort$,code,unit,b_flag)
            if( b_flag ) goto 99
            write(6,'(a)') text
         enddo
         return
       endif
c
c   goto end of file
c
       if(action.eq.'e') then
 12        continue
           read(unit,'(a)',iostat=code) text
           call sei code(fort$,code,unit,b_flag)
           if( b_flag ) goto 99
           goto 12
       endif
c
 99    continue
       backspace( unit, iostat=code)
       call sei code(fort$,code,unit,b_flag)
 999   continue
       return
       end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine find_file_type(unit,type)
      save
      include 'libsei.inc'
      external sei code
c
      integer   code          ! Local condition.
      logical   b_flag        ! Flag a situation?.
c
c  find if an input file is Nordic (type=1) or index file (type 2) or
c  something else (type = 3)
c
      character*80 text
      integer i,unit,type 

      rewind( unit,iostat=code)
      call sei code(fort$,code,unit,b_flag)
      type=3
c
c   check if nordic file
c
      
c
c  one of the first 2 lines should be a header
c
      do i=1,2
        read(unit,'(a80)',iostat=code) text
        call sei code(fort$,code,unit,b_flag)
        if( b_flag ) goto 10                      ! End of file.
        if((text(22:22).ne.'L'.and.text(22:22).ne.'R'.
     *  and.text(22:22).ne.'D'.and.text(22:22).ne.' ').
     *  or.(text(11:11).ne.' '.or.text(16:16).eq.' '))
     *  type=3
      enddo
c
c
c   check for index file
c
       rewind( unit,iostat=code)
       call sei code(fort$,code,unit,b_flag)
c
       read(unit,'(a)',iostat=code) text
       call sei code(fort$,code,unit,b_flag)
       if( b_flag ) goto 10
c
       do i=1,80
        if((text(i:i+2).eq.'L.S'.or.text(i:i+3).eq.'D.S'.or.
     *     text(i:i+2).eq.'R.S').and.text(6:7).eq.'  ') then
           type=2
           return
        endif
       enddo 
c
 10    continue
       return
        end




cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine reject_phases(read2,write2,write1,write3,read3,
     &   init,data,modnam,eventno,
     &   dlt,isort,test,yter1,maxline,nstat,locate,
     &   covar,oterr,nhead,nrecord,use_eev,data5)




c
c

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      
c   this subroutine is mostly based changes made by fernando carrilo
c   it will find outliers 


      implicit none

      include 'hypparm.inc'
      include 'seisan.inc'
      include 'libsei.inc'



      integer nhead,nrecord,eventno
c-- indicator if program is used in connection with EEV in seisan
      logical use_eev
c      character*80 data(*)
      character*120 data5(max_data)
C-- hypocenter help variable
      integer init
c---iaspei file name
      character*80 modnam
      logical locate,b_flag
c--  max number of line pr page
      integer maxline
      integer nstat,read2,read3,write1,write2,write3
c---indicator if output on screen  
      character*1 yter,yter1
      integer seiclen


      integer i,j,k,l
      integer code         ! error code
c-- covarriance matrix, origin time error
      real covar(3,3),oterr      

c-- auxiliary: rms and residual values 
      real hyp_rms,fc_rms,fc_res,az_res,res1,res2
      integer iaz_res
      real hyp_rms_old  ! new jh
c-- auxliary variables for epicentral ordering in temporary s-files
      real depiK,depiJ
c-- 
      character*3 agency
      logical fc_autol,aml_off
      character*5 estac
      character*8 fc_str
      character*80 line
      integer nout
      integer h0,m0,nest,thS,tmS
      real s0,prof,tol1,tol2,tsS,distE
      integer idistE
      character*5 EST(500)
      real dist(500),tSmin(500),tSmax(500)
      character*1 smodel
      integer write1_tmp
      logical BRK
      integer nmessage
      character*80 message(2000)
      
c--   
      integer IT,NIT
c-- variables to control the two sets of data (P&S first arrivals; and P&Sg)
c
      integer nrecord1,nrecord2,nhead1,nhead2,write11,write12
      character*80 data1(max_data)
      character*120 data15(max_data)   !for data5
      character*120 data25(max_data)
      character*120 line5,line15,line25 ! for sorting
      integer ndata,ndata1,ndata2,isol



      

c -- temporary files to store two sets of print.out 
c --    one with results assuming P&S first arrivals; 
c --    other with results assuming of P&Sg arrivals
c
c      write(*,*) ' debug lo 11, ',write11
      if (write11.le.0) then
        call sei open( unknown$,                ! Open hyp file.   ! F.C.
     &                 ' ',                     ! No prompt.
     &                 'print1.out', write11,     ! File & unit.
     &                 b_flag,                  ! File exists?.
     &                 code )                   ! Condition (n/a).   ! F.C.
      endif
c
c      write(*,*) ' debug lo 12, ',write12
      if (write12.le.0) then
        call sei open( unknown$,                ! Open hyp file.   ! F.C.
     &                 ' ',                     ! No prompt.
     &                 'print2.out', write12,     ! File & unit.
     &                 b_flag,                  ! File exists?.
     &                 code )                   ! Condition (n/a).   ! F.C.
      endif
      
  
         NIT=1  
c
c   no sorting, makes trouble  with data5 in hypocent
c
         test(71)=0.0   
  
c
c   if possibly use p and sg, make an extra set of data for local events
c
        if(data(1)(22:22).eq.'L') then
           do i=1,nrecord
             data1(i)(1:80)=data(i)(1:80)   ! original, use s=first arriving s
             data15(i)=data5(i)    !nt
             data2(i)(1:80)=data(i)(1:80)   ! with sg
             data25(i)=data5(i)   ! nt
             if (i.gt.nhead) then
               if (data2(i)(11:14).eq.'S   ') data2(i)(12:12)='G'
             endif
             nrecord1=nrecord
             nrecord2=nrecord             
             nhead1=nhead
             nhead2=nhead             
           enddo  
c
c   here decide if testing for both s and sg
c         
           
           if (TEST(112).eq.0.0) then
             NIT=1     ! only p and s
           else
             NIT=2     ! also p and sg
           endif
       endif
c
c   loop for the 2 posibilities
c
           DO IT=1,NIT            
             if (IT.eq.2) then
               do i=1,nrecord2
                 data(i)(1:80)=data2(i)(1:80)
                 data5(i)=data25(i)  !nt
               enddo
               nhead=nhead2
               nrecord=nrecord2
               write1_tmp=write12
               write(6,*)' Also test for Sg-phases'
             else
               write1_tmp=write11
             endif 
c 
c  test 15 times, stops if rms less than a given value, should it be 
c  when no change. jh implemented it (see end of do loop)
c
           do i=1,15
             call hypocent(2,read2,write2,write1_tmp,write3,read3,
     &         init,'N',data,modnam,eventno,
     &         dlt,isort,test,yter1,maxline,nstat,locate,
     &         covar,oterr,nhead,nrecord,use_eev,nmessage,message,
     &         agency)

             goto 3333   ! sorting screws up data5 so skip for now
             do l=nhead+1,nrecord-2                ! order in epicentral distance 
               k=l
               read(data(k)(71:75),'(f5.0)') depiK
               do j=l+1,nrecord-1
                 read(data(j)(71:75),'(f5.0)') depiJ
                 if (depiK.gt.depiJ) then
                   k=j
                   depiK=depiJ
                 endif  
               enddo
               if (k.ne.l) then
                 line=data(k)(1:80)
                 line5=data5(k)
                 line15=data15(k)
                 line25=data25(k)
                 data(k)(1:80)=data(l)(1:80)
cnt
                 data5(k)=data5(l)
                 data15(k)=data15(l)
                 data25(k)=data25(l)

                 data(l)(1:80)=line(1:80)
                 data5(l)=line5
                 data15(l)=line15
                 data25(l)=line25
               endif
             enddo
 3333  continue
c
c    this should not happen
c

	     if (data(1)(52:55).ne.'****') then
               read(data(1)(52:55),'(f4.0)') hyp_rms
	     else
	       hyp_rms=49.0
	     endif
  
             fc_rms=hyp_rms*1.5   ! multiply by 1.5 (empirically defined)
c
c  check all  residuals
c
             do l=nhead+1,nrecord
               if (data(l)(15:15).ne.'5'.and.
     &             data(l)(15:15).ne.'9') then
                 if (data(l)(64:68).ne.'*****') then ! should no longer happen
                   read(data(l)(64:68),'(f5.0)') fc_res
                   read(data(l)(61:63),'(i3)') iaz_res
		 else
		   fc_res=99.9
                   iaz_res=99
		 endif    
                 fc_res=abs(fc_res)
                 az_res=abs(iaz_res)


c
c  weight out very large residuals
c
                 if ((data(l)(11:11).eq.'S'.or.data(l)(11:11).eq.'P')
     *                .and.fc_res.gt.99.0) data(l)(15:15)='4'

c
c   for s-phase, if residual larger than rms and larger than test(109), 
c   are weighted out,
c   keep if small. if in following iteration, they are smaller, they can be 
c   included again.
c
                 if (data(l)(11:11).eq.'S') then 
                   if (fc_res.ge.fc_rms.and.fc_res.gt.test(109)) then
                         data(l)(15:15)='4'
c                  elseif (fc_rms.lt.0.6.and.fc_res.lt.1.0) then
                   elseif (fc_rms.lt.1.5.and.fc_res.lt.test(109)) then
		         data(l)(15:15)='0'
                   endif
                   if(fc_res.gt.99.0) data(l)(15:15)='4'
                 endif
c
c   p only checked after 2. iteration
c      
                if (i.ge.3.and.data(l)(11:11).eq.'P') then
                   if (fc_res.ge.fc_rms.and.fc_res.gt.test(109)) then
                         data(l)(15:15)='4'
c before jan 2022  elseif (fc_rms.lt.0.6.and.fc_res.lt.1.0) then
                   elseif (fc_rms.lt.1.5.and.fc_res.lt.test(109)) then
                         data(l)(15:15)='0'
                   endif
                endif  
    
c
c   BAZ only checked after 2. iteration
c
                if (i.ge.3.and.data(l)(11:13).eq.'BAZ') then
                   if (az_res.gt.30.0) then
                         data(l)(9:9)='4'
                   elseif (fc_rms.lt.1.0.and.az_res.lt.30.0) then
                         data(l)(9:9)='0'
                   endif
                 endif   !BAZ

               endif    ! if w is not 5 or 9
             enddo        ! end of checking phase loop
c             write(*,*) i,hyp_rms
c
c   jump out if iteration is more than 5 and rms less than 0.3
c

cxx             if (i.gt.4.and.abs(hyp_rms_old-hyp_rms).lt.0.05) goto 977  ! added by jh
             if (i.gt.5.and.hyp_rms.lt.0.3) goto 977
             hyp_rms_old=hyp_rms   ! jh 
           enddo    ! location iteration loop  
                        
c
ccccccccccccccccc  above is end of iteration loop i cccccccccccccccccccccccccc
c
 977       continue
c
c   remove phases with largest residuals for automatic phases
c
c    DISABLE for NOW, jh jan 2022, screws up sorting in new format
c
           goto 2022

           nout=0
c           write(6,*)i,' iterations'                      
           do i=nhead+1,nrecord-nout
976          continue           
c             if (data(i)(11:12).ne.'AM'.and.   ! make sure only use phase arrivals  ! fc
c     &              data(i)(11:13).ne.'apg') then
             if (data(i)(11:11).eq.'P'.or.   ! make sure only use phase arrivals  ! jh
     &              data(i)(11:11).eq.'S') then
               if (data(i)(64:68).ne.'     ') then
                  read(data(i)(64:68),'(f5.0)') res1
                  res1=abs(res1)                       
               else 
                  res1=9999.9
               endif

c
c   if residual larger than TEST(110), remove phase line but only for automatic phases
c
cnt   
               if (data(i)(16:16).eq.'A') then ! only remove automatic                       
                 if (res1.gt.test(110)) then     
                   do j=i,nrecord-1
                     data(j)(1:80)=data(j+1)(1:80) ! shift records
cnt
                     data5(j)(1:80)=data5(j+1)(1:80) ! shift records
                     data15(j)=data15(j+1)
                     data25(j)=data25(j+1)
                   
                   enddo
                   nout=nout+1
c   i is now the next phase, jump back 
c
                       goto 976     
                 endif  
               endif         
             endif  
           enddo    ! end of removing phases 

           nrecord=nrecord-nout

 2022      continue   ! skip test above for automatic
c
c   if phase read in diferent channels, select "best" (lower residua) and 
c   flag the others with weight "5"
c   only if test(111) is set
c
           if(test(111).gt.0.0) then
          
 
           do i=nhead+1,nrecord-1         
c             if (data(i)(11:11).ne.'A'.and.data(i)(11:11).ne.'a') then  !fc
              if (data(i)(11:11).eq.'P'.or.data(i)(11:11).eq.'S') then   !jh
               if (data(i)(64:68).ne.'     ') then
                  read(data(i)(64:68),'(f5.0)') res1
                  res1=abs(res1)                       
               else 
                  res1=9999.9
               endif  
             endif  
             
             do j=i+1,nrecord        
               if (data(i)(2:6).eq.data(j)(2:6)) then
                 if (data(i)(11:14).eq.data(j)(11:14)) then
c                   if (data(i)(11:11).ne.'A'.and.             ! fc
c     &                 data(i)(11:11).ne.'a') then
                   if (data(i)(11:11).eq.'P'.or.               ! jh
     &                 data(i)(11:11).eq.'S') then
                      read(data(j)(64:68),'(f5.0)') res2
                      if (data(j)(64:68).ne.'     ') then
                        res2=abs(res2)
                      else 
                        res2=9999.9
                      endif  
                      if (res1.gt.res2) then 
                        data(i)(15:15)='5'     ! not used, mark as not reported
                      else 
                        data(j)(15:15)='5'
                      endif
                      goto 969
                   endif   
                 endif
               endif
             enddo  
969          continue             
           enddo              
           
c
c   remove automatic phases with weight 5 - flagged in previous block
c
           nout=0                  
           do i=nhead+1,nrecord-1-nout
974          continue           
             if (data(i)(11:12).ne.'AM') then
               if (data(i)(15:16).eq.'5A') then                        
                      do j=i,nrecord-1
                         data(j)(1:80)=data(j+1)(1:80)
cnt
                         data5(j)=data5(j+1)
                      enddo
                      nout=nout+1
                      goto 974
               endif         
             endif  
           enddo  
	              
	   nrecord=nrecord-nout

           endif ! of removing phases
c
c   next section maybe to be used later by fernando
c	   
	                               ! eliminar amplitudes "não conformes ...."
c	   call flag_out_amplitudesML                            
           
c           estac(1:5)='XXXXX'
c           do i=nhead+1,nrecord-1         
c             if (data(i)(2:6).eq.estac(1:5)) goto 972          
c             estac(1:5)=data(i)(2:6)
c             nout=i
c             do j=i+1,nrecord
c               if (data(i)(2:6).ne.data(j)(2:6)) goto 970
c               nout=j
c             enddo                 
c970          aml_off=.true.
c             do k=i,nout
c               if (data(k)(11:13).ne.'AML') aml_off=.false.
c             enddo
c             if (aml_off) then
c               do k=i,nout
c                 data(k)(15:15)='5'
c               enddo  
c             endif
c972          continue             
c
c	   enddo

                                       ! "balizar" zona de leitura de AML

c           nest=0
c           do i=nhead+1,nrecord-1  
c             do j=1,nest
c               if (data(i)(2:6).eq.EST(j)) goto 979
c          enddo
c	     nest=nest+1
c	     EST(nest)=data(i)(2:6)
c	     read(data(i)(71:75),'(f5.0)') dist(nest)
c979          continue	     
c	   enddo           
 
c	   read(data(1)(12:20),'(i2,i2,1x,f4.0)') h0,m0,s0
c	   read(data(1)(39:43),'(f5.0)') prof
c	   tol1=3.0    ! igual residuo maximo tolerado (para trás)
c	   tol2=20.0
c	   smodel=data(1)(21:21)   
	   
c	   call tempo_perc(h0,m0,s0,prof,dist,nest,tSmin,tSmax,smodel,
c     &	                   tol1,tol2)
     
c           do i=nhead+1,nrecord-1
c             if (data(i)(11:12).eq.'AM') then           
c               do j=1,nest
c                 if (data(i)(2:6).eq.EST(j)) then
c                    read(data(i)(19:28),'(i2,i2,1x,f5.0)') thS,tmS,tsS
c                   res1=thS*3600.0+tmS*60.0+tsS
c                   write(*,*) EST(j),dist(j),tSmin(j),res1,tSmax(j)
c                   if (res1.lt.tSmin(j).or.res1.gt.tSmax(j)) then
c                         data(i)(15:15)='5'
c                         write(*,'(a70)') data(i)(1:70)
c                   endif      
c                   goto 978
c                 endif
c               enddo
c             endif  
c978          continue
c           enddo
c
c
c   remove automatic amplitudes with weight 5, from where ?
c     
c           nout=0                      ! apagar fases peso 5 (repetido ... amps)
c           do i=nhead+1,nrecord-1-nout
c973          continue           
c             if (data(i)(11:12).eq.'AM') then
c               if (data(i)(15:16).eq.'5A') then                        
c                      do j=i,nrecord-1
c                         data(j)(1:80)=data(j+1)(1:80)
c                      enddo
c                      nout=nout+1
c                      goto 973
c               endif         
c             endif  
c           enddo  
c	   nrecord=nrecord-nout

c
c   save the data from the two iteratrion for s and sg
c	   

           if (IT.eq.1) then
             nrecord1=nrecord
             nhead1=nhead
             do i=1,nrecord
               data1(i)(1:80)=data(i)(1:80)
               data15(i)=data5(i)
             enddo  
            ndata1=ndata
             call sei close( close$, write11, code ) ! Close file (stop on error).
           else   
            nrecord2=nrecord
             nhead2=nhead
             do i=1,nrecord
               data2(i)(1:80)=data(i)(1:80)
               data25(i)=data5(i)  !nt
             enddo  
             ndata2=ndata
             call sei close( close$, write12, code ) ! Close file (stop on error).             
           endif
           
           ENDDO  ! it
c
ccccccccccccccc   end of loop for testing s and sg cccccccccccccccccccccccccc
c           


c
c   decide if using data set with s or sg. the data set with the highest number 
c   of accepted phases is selected. skip this section if NIT=1
c
        if(nit.gt.1) then
           if (ndata1.gt.ndata2) then
             isol=1
           elseif(ndata1.eq.ndata2.and.
     &     data1(1)(52:55).le.data2(1)(52:55)) then
             isol=1
           else
             isol=2
           endif  
           if (isol.eq.1) then
             do i=1,nrecord1
               data(i)(1:80)=data1(i)(1:80)
               data5(i)=data15(i)  !nt
             enddo
             nrecord=nrecord1
             nhead=nhead1
           else
             do i=1,nrecord2
               data(i)(1:80)=data2(i)(1:80)
               data5(i)=data25(i)   ! nt
             enddo
             nrecord=nrecord2
             nhead=nhead2      
           endif
        endif



c      call apagar_fases
           nout=0                 ! apagar residuos grosseiros ... again ...para garantir ...

           goto 982
c
c   it seems this section cannot be reached, remove ?
c
c           do i=1,nrecord
c             write(*,*) data(i)(1:77)
c           enddo        
c
c
c           do i=nhead+1,nrecord-nout
c 981          write(*,*) data(i)(1:77)
c981          continue
c             if (data(i)(11:12).ne.'AM') then
c              if (data(i)(11:13).ne.'apg') then
c               if (data(i)(64:68).ne.'     ') then
c                  read(data(i)(64:68),'(f5.0)') res1
c                  res1=abs(res1)                       
c               else 
c                  res1=9999.9
c               endif  
c               if (data(i)(16:16).eq.'A') then                        
c                 if (res1.gt.3.0) then     
c                   do j=i,nrecord-1
c                     data(j)(1:80)=data(j+1)(1:80)
c                   enddo
c                   nout=nout+1
c                   goto 981
c                 endif  
c               endif
c              endif
c             endif  
c           enddo
c            
cccccccccccccccccccccccccccc end of section aparently not used
c

982        continue
c           do i=nhead+1,nrecord
c              write(*,*) data(i)(1:77)    
c           enddo  
c
c   eliminate hypocenter info if no phases left
c         
           i=nrecord-nhead
           if (i.eq.1) then
             data(1)(24:50)='                          '             
             data(1)(51:79)='                             '
             nstat=0           ! added jh to avoid crash in hypocent
           endif
c
c   locate to check
c
           
           call hypocent(2,read2,write2,write1,write3,read3,
     &         init,'N',data,modnam,eventno,
     &         dlt,isort,test,yter1,maxline,nstat,locate,
     &         covar,oterr,nhead,nrecord,use_eev,nmessage,message,
     &         agency)

           if(nstat.eq.0)locate=.false.     !jh add
c
c   if depth less than 31 km and a test has been made for both s and sg, then
c   change both p and s phases to Pn or Sn phases  if distance is larger than 
c   145 km, change to Pg or Sg if less than 50 km and leave as is if in 
c   between.
c
c  **** this P->Pn labeling option as well the depth/distance limits could be LATER set as 
c  **** TEST parameters or kept in a parametric file

c   this seems bad practice:
c
c   -hardwired to a spcific model
c   -p is usually always a first arrival, so no need to specify as Pn, might
c    make location worse
c   -Sn is rarely seen, if S works it might be Sn anyway so no need to specify
c   -it might be that the phase is N-type but it does not have to be Moho
c
c    so i wonder if this phase change is not counter productive to the first 
c    automatic S and Sg selection.
c
c    disabled for now !!
c
           goto 1111
           read(data(1)(39:43),'(f5.0)') prof
           if (NIT.eq.2.and.prof.lt.31.0) then
             do i=nhead+1,nrecord
               if (data(i)(16:16).eq.'A') then
                 if (data(i)(11:12).eq.'P '.or.data(i)(11:12).eq.'S ')
     &            then
                   read(data(i)(71:75),'(f5.0)') distE
                   if (distE.gt.145.0) data(i)(12:12)='N'
                   if (distE.le.50.0.and.distE.gt.0.001) 
     &                   data(i)(12:12)='G'
                 endif  
               endif
             enddo            
           endif

           call hypocent(2,read2,write2,write1,write3,read3,
     &         init,'N',data,modnam,eventno,
     &         dlt,isort,test,yter1,maxline,nstat,locate,
     &         covar,oterr,nhead,nrecord,use_eev,nmessage,message,
     &         agency)


 1111 continue

c
c   assume sorting so set again
c
      test(71)=1.0   !nt

      return
      end
