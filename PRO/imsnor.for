c--------------------------------------------------------------------------
c  Program for reading GSE, IMS, ISF and ISF2  event files and converting to Nordic
c  format. 
c
c  - All events are assumed regional
c  - No check if header time corresponds to phase times
c  - Amplitudes are mm WA for nanometrics in albania system, else nm
c  - Do not read all error parameters
c  - Input file has no component unless ISF2 format
c
c
c    IASP amplitude phases used unchanged
c    LR is a Raleigh wave, assume Ms_20, ims calculates travel time
c        seisan not
c    LQ amplitudes are not associated with a magnitude scale and labled A, phase is
c        left as LQ, ims calculates travel time, seisan not 
c    MLR is non standard, seem to be the same as LR and treated the same way
c    L, LR and AMS as above.
c    Amplitude on a phase line is made into a new line. The time is the same
c         so it is possible to see which phase it was associated with. If
c         possible, it is given and IASP name, if not it is named A with extension of
c         the phase so oen can see which phase it came from.
c         Ml amplitudes from ctbto are calculated in a non standard way and 
c             cannot be used, give small Ml. Labled A.
c         Many amplitudes, even on Sg or Lg, are not standard and not labled IAML.
c             seems some could be mm WA. Only IAML is used
c         If amplitude on P and distance larger than 20 deg, assume IAmb.
c    Some amplitudes have no phase and no time, no time also in S-file.
c    AML, and old SEISAN name if often used in ISC , but the mmany  observartions
c         are wrong, ca  1000 times too high, maybe uncorrected. Not used.
c    AMB is assuem mb if magntude calculated.
c    pmax is assumed mb --------------------.
c    BAZ is put on separate line in order to be able to put in weight. This
c         is needed for ISC where many BAZ values are wrong. ISC does not use
c         BAZ so the ISC observations are identified with no BAZ residual and
c         the weight put to zero since many BAZ do not seem to be BAZ. This
c         unfortunate effect that good BAZ are not used, like for NORSAR array
c         or locally determined by 3 componenets.
c         If BAz has a residual it is used.
c    Phases from ISC not used by ISC are given weight zero, however phases
c         with a time defining flag and no residual is given weight 3 if residual 
c         is less than 5s.
c    P* and S* converted to Pb and Sb
c    SPECP and SPECS removed, probaly from SEISAN by mistake
c    Errors calcualated for errx and erry
c    Hypocenters are ordered according to a hardwired table in program. The same order
c         is used for the magnitudes. However, the prime solution (ISC) always
c         come first since residuals are referred to that solution.
c    Fault plane solutions are also ordered, different table, hardwired.
c    Only standard magnitudes are put on header lines, same order as for hypocenters.
c         The first 6 magnitudes are put on first hypcenter, the rest distributed among
c         following hypocenters and if more put in as comments.
c         The first magnitude is repeated in 3. posiiton, and the rest pushed down.      
c    Relocating ISC events gives higher rms. Part of the problem is that epicentral
c         distance is different, at certain distances up to 100 km. The travel time
c         model is also different.  Using residual weighting gives lower rms, might be
c         what ISC is using. 
c    Macroseismic data is not included, but if event is felt, it is marked with a 
c    type 2 line.
c
c    Phases not used by SEISAN a given weight zero. Not all has been checked, but often
c          exotic phases are not used by ISC so they will get weight zero.
c    In ISF2, there can also be component, network, location, phase agency, phase operator.
c         However, network often wrong so not used.
c
c    At ISC, the incoming phases are reinterpreted. This is particualrly unfortunate for 
c    crustal phases, like Pn, which then no longer is Pn as a refrated phase  but rather 
c    a Pg. However, for events not processed by isc, the origianl phases remain. This will create 
c    an inhomgenious data base for user who want to process data for a local area. 
c
c    In this program, all Pn, Pb and Pg  phases given by isc 
c    have been changed to P and corresponding for S. 
c    All other b and n are also converted to P and S since many observatories
c    do not have a consistent practice of setting n and b.
c
c Mail from Dmityry at isc:
c Regarding the crustal phases, it is true that Pn, in its IASPEI standard 
c understanding <http://www.isc.ac.uk/standards/phases/>, is also the P-wave that 
c bottoms in the uppermost mantle. The working group (which included yourself) has decided 
c at the time that we must follow the ak135 notation, simply because the 
c corresponding code did not differentiate between the classical Pn travelling as a 
c headwave alongside the Moho and the branch of direct P that leaves a source in 
c the uppermost mantle (~400km), bottoms no deeper than that and eventually going 
c up towards a station.
c
c I recall that Peter Bormann wasn't particularly easy about it, but connection 
c with ak135 and corresponding software was dearer, so he reluctantly agreed.
c
c  jh sep 11, 2020
c
c  updates:
c
c  2020 12 18 jh: more nanometrics cases, agency can be entered as an argument
c  2021 01 22 jh: check when last revised by isc, do not change phases Pb etc 
c                 when from isc after that date. bugs with amplitudes
c  2021 04 28 jh: a few more chenages in data selected, check if headers
c  2021 09 22 jh: fix duplicate, possible to gi different author for duplicate
c  2022 07 13 jh: more options on prompt: ARC ine, distance indicator.
c  2023 02 09 jh: fix so last event is included in case of no readings
c                 fix so locality comes with correct event -----------
c  2023 10 30 jh: error if sclar moment not there, reading magnitude without
c                 type was not working
c  
c--------------------------------------------------------------------------c
c
c
      implicit none                       ! force delcaration of all variables
      include 'seidim.inc'                ! dimensions for rea block
      include 'seisan.inc'
      include 'rea.inc'                   ! parameter common bliock
      character*80 data(max_data)         ! s-file with data in text array
      character*80 err_text               ! error text
      logical isc_review                  ! true if reviewed by ISC
      character*200 text
      character*80 infile                 ! input file
      character*8 amp_phase               ! amplitude phase name
      character*8 phase_save              ! phase 
      character*3 amp_com                 ! component of amplitude
      real strike,dip,rake                ! fps
      real smin,smaj,az                   ! error ellipse
      double precision  abstime,msec
      character*23 start_time_duration    ! for ARC line
      integer year,month,day,hour,min
      real sec
      integer doy                         ! day of year
c-- arguments
      character*80 args(10)    
c---number of arguments 
      integer nars     
      character*3  def_agency             ! agency to use if not given in input file
      character*3  def_agency_dup         ! agency to use for duplicate first event
C-- SYSTEM TIME FOR USED FOR TIME OF OPERATION
      CHARACTER*12 p_time
      character*14 proc_time
      logical nanometrics                 ! true if from a nanometrics system
      logical is_isc                      ! true if file form iSC
      character*22 isc_last_date          ! last date of isc revision
      character*22 event_last_date        ! last date in file
      logical isc_last_event              ! if true, this is the last event in file processed by isc
      logical all                         ! true: read all data, false: headers
      integer code                        ! error return code
      integer nevent                      ! number of events in file
      character*1 dist_indicator          ! distance indicator
      character*69 save_locality          ! locality
      logical arc                         ! if true, write arc line
      integer nfault                      ! counter for fps
      integer khyp                        ! counter for hypocenters
      integer i,j,k,n                     ! counters

      call get_seisan_def

      nanometrics=.false.
      isc_last_event=.false.
c
c   check s-file format
c
      if(new_nordic_format) then
         rea_new_out=.true.              ! write new format
      endif

      CALL SYSTIME(p_time,PROC_TIME)
c
c   open output file
c
       open(2,file='imsnor.out',status='unknown')
        
       infile=' '
       def_agency=' '
       def_agency_dup=' '
       dist_indicator='R'
       arc=.false.
c
c   check arguments
c
        call get_arguments(nars,args)

        do i=1,nars
          if(args(i).eq.'-infile') infile=args(i+1)
          if(args(i).eq.'-agency') def_agency=args(i+1)(1:3)         
c
c   check if agency to give to duplicate header if not the same
c   as header
c
          if(args(i).eq.'-def_dup') def_agency_dup=args(i+1)(1:3)
c
c   distance indicator
c
          if(args(i).eq.'-dist_indicator') dist_indicator=args(i+1)(1:1)
c
c   ARC line
c
          if(args(i).eq.'-arc') arc=.true.
      enddo

      if(dist_indicator.ne.'D'.and.dist_indicator.ne.'R'.and.
     *dist_indicator.ne.'L') then
         write(6,*) 'Wrong distance indicator'
         stop
      endif
c
c   get input file name, check if exist
c

 9    continue
      if(infile.eq.' ') then
         write(6,*) ' Give input file'
         read(5,'(a)') infile
      endif
      open(1,file=infile,status='old',err=10)
      goto 11
 10   continue
      write(6,*)' No such input file'
      infile=' '
      goto 9
 11   continue
 
      nevent=0                    ! initialize counter

c
c   find if this is a file from ISC and if so, last day of revision
c
    
      write(6,'(a)')'Checking input file..........'

      call isc_test(is_isc, isc_last_date,event_last_date,n,k)
      if(k.eq.0) then
         write(6,*)'No headers in file'
         write(6,*)
     *  'You must click on Output headers when selected data at ISC'
         stop
      endif

      if(is_isc) then
         write(6,'(a,i7)') 'This is an ISC file, number of lines: ',n
         write(6,'(a,a)') 'Date of last processed ISC event: ',
     *   isc_last_date
         write(6,'(a,a)') 'Date of last event in file      : ',
     *   event_last_date
         write(6,'(a)') 'Enter to continue'
         read(5,'(a)') text
      else
         write(6,'(a,i7)') 'This is not an ISC file, number of lines: '
     *   ,n
         write(6,'(a,a)') 'Date of last event in file      : ',
     *   event_last_date
         write(6,'(a)') 'Enter to continue'
         read(5,'(a)') text 
      endif

      k=0   ! counts phases
    

c
c-----------------------------------------------------------------
c  Loop to read events start here
c-----------------------------------------------------------------
c

c
c   new event
c
  12  continue

c
c   clear variables
c
      rea_nphase=0      ! count number of phase lines
      rea_nhyp=0        ! count number of hypocenters
      rea_nwav=0        ! --------------- waveform files
      rea_ncomment=0    ! --------------- comment lines
      rea_nmacro=0      ! --------------- macroseismic data
      rea_nfault=0      ! --------------- fault plane solutions
      nfault=101        ! index for --------------------------
      rea_nspec=0       ! --------------- spectral solutions
      rea_nmag=0        ! --------------- magnitudes
      rea_id_line=' '   ! no id line
      rea_locality=' '  ! no locality
      save_locality=' '
      mt_nmt=0          ! --------------  moment tensors
      khyp=101          ! count hypocenters, save at end to be able to copy when sorting
      isc_review=.true. ! assume reviewed

c
c   if, for isc, one of the previous events was the last reviewed, 
c   following events are not reviewed and phases written as is
c   if file is not from ISC, phases also used as is
c
      if(isc_last_event.and.is_isc) isc_review=.false.
      if(.not.is_isc) isc_review=.false.


      call rea_hyp_clear(1)

c
c  read one line
c
 20   continue
c
c   skip blank lines
c
      read(1,'(a)',end=100,err=300) text
      write(6,*) text(1:70)
      if(text.eq.' ') goto 20

      if(text(1:5).eq.'Event'.or.text(1:5).eq.'EVENT') then
         save_locality=text(6:74)
      endif
c
c  check if reviewed by ISC
c
              
      if(text(3:31).eq.'Event not reviewed by the ISC') then
         rea_ncomment=rea_ncomment+1
         rea_comment(rea_ncomment)=' '
         rea_comment(rea_ncomment)=' Event not reviewed by the ISC'
         rea_comment(rea_ncomment)(80:80)='3'
         isc_review=.false.
      endif
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   hyocenter lines, they might not be together
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      if(text(5:5).eq.'/'.and.text(8:8).eq.'/') then
c
c   only put in locality if there is header data
c
         rea_locality=save_locality
c
c   check if last reviewed event from ISC, then when processing next
c   event, turn off flag for isc reviewd event and all
c   phases will be output with full weight
c
         if(is_isc.and.isc_last_date.eq.text(1:22)) 
     *   isc_last_event=.true.

         write(6,*) text(1:70)
         khyp=khyp-1   ! put at end for sorting

         call rea_hyp_clear(khyp)
         read(text(1:4),'(i4)') hyp_year(khyp)
         read(text(6:7),'(i2)') hyp_month(khyp)
         read(text(9:10),'(i2)') hyp_day(khyp)
         read(text(12:13),'(i2)') hyp_hour(khyp)
         read(text(15:16),'(i2)') hyp_min(khyp)
         read(text(18:22),*) hyp_sec(khyp)
         read(text(89:92),'(i4)') hyp_nstat(khyp)
         if(text(77:77).eq.'f') hyp_depth_flag(khyp)='F'
         if(text(55:55).eq.'f') hyp_epi_flag(khyp)='F'
         if(text(23:23).eq.'f') hyp_fix_org(khyp)='F'
         
         hyp_agency(khyp)=text(119:121)
c
c   for ISC, it might be ISC-EHB, which is not prime, so agency will be
c   replaced with ISE so it does not come first
c
         if(text(123:125).eq.'EHB') hyp_agency(khyp)='ISE'
         hyp_dist_id(khyp)=dist_indicator
c 
c    hypocenter
c
         read(text(37:44),'(f8.3)') hyp_lat(khyp)
         read(text(46:54),'(f9.3)') hyp_lon(khyp)
         if(text(72:76).ne.' ') read(text(72:76),'(f5.2)') 
     *   hyp_depth(khyp)
         if(text(31:35).ne.' ') read(text(31:35),'(f5.2)') hyp_rms(khyp)
c
c   event type
c
c   suspected (s),  known(k), earthquake (e) 
c
         if(text(116:117).eq.'se') hyp_type(khyp)=' '
         if(text(116:117).eq.'ke') hyp_type(khyp)='Q'
         if(text(116:117).eq.'sh') hyp_type(khyp)='P'   ! chemical exp (h)
         if(text(116:117).eq.'kh') hyp_type(khyp)='E'   ! chemical exp (h)
         if(text(117:117).eq.'n')  hyp_type(khyp)='E'
c
c   suspected and known landslide are both assigned landslide
c
         if(text(116:117).eq.'sl'.or.text(116:117).eq.'kl') 
     *   hyp_type(khyp)='L'
c
c   suspected and known induced event are both assigned induced
c
         if(text(116:117).eq.'si'.or.text(116:117).eq.'ki') 
     *   hyp_type(khyp)='I'
c
c   macroseismic, to be finished, only indicate with type 2 line
c
         if(text(116:116).eq.'f'.or.text(116:116).eq.'d') then
            rea_nmacro=1
            rea_macro(1)(80:80)='2'
         endif
c
c
c   error lines, all are stored
c   
c
         if(text(25:29).ne.' '.or.
     *     (text(56:60).ne.' '.and.text(62:66).ne.' '.and.
     *      text(68:70).ne.' ').or.
     *      text(78:82).ne.' '.or.
     *      text(93:96).ne.' ') hyp_error(khyp)=.true.

         if(text(93:96).ne.' ') read(text(93:96),*) hyp_gap(khyp)
         if(text(24:29).ne.' ') read(text(24:29),*) hyp_sec_err(khyp)   

         if(text(56:60).ne.' '.and.text(62:66).ne.' '.
     *   and.text(68:70).ne.' ') then    ! all 3 must be there to calculate hor. errors
            read(text(56:70),*) smaj,smin,az
            call hyp_ellipse_err
     *      (smaj,smin,az,hyp_lon_err(khyp),hyp_lat_err(khyp))
         endif
         if(text(78:82).ne.' ') read(text(78:82),*) hyp_depth_err(khyp)

         goto 20  ! next line

       endif
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   felt information, first 79 chars
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
       if(text(1:6).eq. ' (Felt') then
          rea_ncomment=rea_ncomment+1
          rea_comment(rea_ncomment)(1:79)=text(1:79)
          rea_comment(rea_ncomment)(80:80)='3'
          rea_nmacro=rea_nmacro+1                                            
          rea_macro(rea_nmacro)=' '  
          rea_macro(rea_nmacro)(80:80)='2' 
          goto 20 ! next line
        endif
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   magnitudes, 6 first  stored with first hypocenter, the rest on following
c   hypocenters or comment lines
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
       if(text(1:14).eq.'Magnitude  Err') then
          rea_nmag=0
          j=201        ! store at end for sorting   
       
  23      continue     ! magnitude loop, from end of magnitude

          read(1,'(a)',end=300,err=300) text
          write(6,*) text(1:70)


          if(text(1:1).eq.'m'.or.text(1:1).eq.'M'.or.     ! the normal case
     *    (text(1:1).eq.' '.and.text(7:10).ne.' ')) then    ! magnitude type is blank
c
c   select out standard magnitudes and put in mag all array
c
             if(text(7:10).ne.' '.and.
     *       (text(1:5).eq.'mb   '.or.
     *        text(1:5).eq.'mB   '.or.
     *        text(1:5).eq.'Mb   '.or.
     *        text(1:5).eq.'Ms   '.or.
     *        text(1:5).eq.'MS   '.or.
     *        text(1:5).eq.'Ms_20'.or.
     *        text(1:5).eq.'MW   '.or.
     *        text(1:5).eq.'ML   '.or.
     *        text(1:5).eq.'MW   '.or.
     *        text(1:5).eq.'ML   '.or.
     *        text(1:5).eq.'Ml   '.or.
     *        text(1:5).eq.'mL   '.or.
     *        text(1:5).eq.'MD   '.or.
     *        text(1:5).eq.'Md   '.or.
     *        text(1:5).eq.'md   '.or.
     *        text(1:5).eq.'     '))
     *        then
                 rea_nmag=rea_nmag+1
                 j=j-1 
                 hyp_mag_type_all(j)=' '
                 read(text(7:10),'(f4.1)') hyp_mag_all(j)
                 if(text(2:2).eq.'l') hyp_mag_type_all(j)='L'
                 if(text(2:2).eq.'L') hyp_mag_type_all(j)='L'
                 if(text(2:2).eq.'d') hyp_mag_type_all(j)='C'
                 if(text(2:2).eq.'D') hyp_mag_type_all(j)='C'
                 if(text(2:2).eq.'b') hyp_mag_type_all(j)='b'
                 if(text(2:2).eq.'s') hyp_mag_type_all(j)='s'
                 if(text(2:2).eq.'S') hyp_mag_type_all(j)='s'  ! assume few use BB magnitude  
c
c   MB: can be mB or mB_BB
c
                 if(text(2:2).eq.'B') hyp_mag_type_all(j)='B'  ! assume more used BB b
                 if(text(2:2).eq.'W') hyp_mag_type_all(j)='W'
                 hyp_mag_agency_all(j)=text(21:23)
              else
c
c   put non standard magnitude on comment line
c
                 rea_ncomment=rea_ncomment+1
                 rea_comment(rea_ncomment)=' '
                 rea_comment(rea_ncomment)(2:38)=text(1:37)
                 rea_comment(rea_ncomment)(80:80)='3'
             endif
             goto 23     ! next magnitude
          endif        
       endif
c
c-----------------------------------------------------------------------------
c   fault plane solution, from ISC, ISF format
c-----------------------------------------------------------------------------
c
       if(text(4:14).eq.'FAULT_PLANE') then
          read(1,'(a)',end=300,err=300) text
          write(6,*) text(1:70)
          if(text(20:39).ne.' ') then
             read(text(20:39),*,end=24,err=24) strike,dip,rake
             nfault=nfault-1    ! store at end for sorting
             rea_fault(nfault)=' '
             rea_fault(nfault)(80:80)='F'
             write(rea_fault(nfault)(1:30),'(3f10.1)') 
     *       strike,dip,rake
             rea_fault(nfault)(67:69)=text(55:57)   ! agency
             rea_fault(nfault)(71:73)=text(16:18)   ! method or program
          endif
 24       continue
       endif

c-----------------------------------------------------------------------------
c   moment tensor solutions, from ISC, ISF format
c-----------------------------------------------------------------------------
c
       if(text(4:10).eq.'MOMTENS') then
          read(1,'(a)',end=300,err=300) text
          write(6,*) text(1:70)
          read(1,'(a)',end=300,err=300) text
          write(6,*) text(1:70)
          mt_nmt=mt_nmt+1
          call rea_mt_clear(mt_nmt)
          if(text(12:19).ne.' ')
c     *    read(text(12:19),*) mt_exp(mt_nmt),mt_moment(mt_nmt)
     *    read(text(12:19),'(i2,f5.3)') mt_exp(mt_nmt),mt_moment(mt_nmt)
          if(text(27:67).ne.' ')
     *    read(text(27:67),*) (mt_val(i,mt_nmt),i=1,6)


          do i=1,6
            mt_val(i,mt_nmt)=mt_val(i,mt_nmt)*10.0**mt_exp(mt_nmt)
         enddo

          mt_moment(mt_nmt)=mt_moment(mt_nmt)*10.0**mt_exp(mt_nmt)

          mt_coor(mt_nmt)='S'
          mt_agency(mt_nmt)=text(79:81)

c
c   isc does not give method. The corresponding hypocenter is in 
c   privious line
c
          mt_year(mt_nmt)=hyp_year(khyp)
          mt_month(mt_nmt)=hyp_month(khyp)
          mt_day(mt_nmt)=hyp_day(khyp)
          mt_hour(mt_nmt)=hyp_hour(khyp)
          mt_min(mt_nmt)=hyp_min(khyp)
          mt_sec(mt_nmt)=hyp_sec(khyp)
          mt_lat(mt_nmt)=hyp_lat(khyp)
          mt_lon(mt_nmt)=hyp_lon(khyp)
          mt_depth(mt_nmt)=hyp_depth(khyp)
c
c   remove hypocenter from list, not anymore, turned out that it could be the prime
c
c          khyp=khyp+1     ! remember, stored at end
       endif
c
c-----------------------------------------------------------------------------
c   finished with headers since next possible line is 'Sta' or end of file
c   if last event or a new header line if next event.
c   now sort according to agency and make id line
c   end of header can be when station lines appear or if no stations,
c   when next event header comes
c-----------------------------------------------------------------------------
c

      goto 101
c
c   come here from end of file at start since last event will not have
c   EVENT or Event in following line to identify next event
c

 100  continue
      text(1:5)='EVENT'   ! simulate new event to terminate
 101  continue

      if((text(1:8).eq.'Sta     '.or.                       ! phases
     *   text(1:5).eq.'Event'.or.text(1:5).eq.'EVENT').and. ! new event
     *   khyp.le.100 ) then                                 ! but only if at least 1 hyp line

          rea_nhyp=100-khyp+1
c
c   sort hypocenters according to priority list given in subroutine sort_source
c
             call sort_source_info(1)

c   sort magnitudes according to preferred agency
c
             call sort_source_info(2)
c
c   sort fault plane solutions
c
             rea_nfault=100-nfault+1  

             call sort_source_info(3)
c
c   id, use first hypocenter
c
             rea_id_line=' '
             rea_id_line(2:11)='ACTION:IMS' 
             rea_id_line(28:32)='OP:XX'
             rea_id_line(80:80)='I'    
             rea_id_line(36:42)='STATUS:'
             WRITE(rea_id_line(13:26),'(A)')PROC_TIME
             i=hyp_sec(1)
             write(rea_id_line(61:74),'(i4,6i2)')
     *       hyp_year(1),hyp_month(1),hyp_day(1),
     *       hyp_hour(1),hyp_min(1),i
             do i=61,74
                if(rea_id_line(i:i).eq.' ') rea_id_line(i:i)='0'
             enddo
c
c   if no phases, then go to write out
c
             if(text(1:5).eq.'Event'.or.text(1:5).eq.'EVENT') then
                 backspace(1)   ! back one line since new event and start again
                 goto 30   ! write out
             endif 
         endif
c
c-----------------------------------------------------------------------------
c   loop for stations and phases
c-----------------------------------------------------------------------------
c         
       if(text(1:8).eq.'Sta     ') then
          k=0
 25       continue    ! station loop
          read(1,'(a)',end=30) text    ! write out if end of file, goto 30
          write(6,*) text(1:70)
c
c   non valid phases to be skipped
c
          if(text(20:24).eq.'SPECP'.or.text(20:24).eq.'SPECS') goto 25  ! probably from SEISAN         

          if(text.eq.' ') goto 30    ! end of event, write out

c
c   case of amplitude without any information about time and station, go to amplitude
c
          if(text(1:40).eq.' '.and.text(84:92).ne.' ') goto 29
c
c   case of a phase with no information, skip
c
          if(text(20:27).ne.' '.and.text(29:73).eq.' '.and.
     *    text(84:92).eq.' ') goto 25
c
c  in isf format, there can be comments within the phases, skip
c
          if(text(2:2).eq.'(') goto 25
c
c   assume one more phase
c
          k=k+1
c
c   read phases 
c
           call rea_phase_clear(k)        
           rea_stat(k)=text(1:5)
c
c   componet, operator, agency, network and location is defined in ISF2
c   after normal content
c
           
           rea_com(k)=text(157:159)   ! ? is unknown
           do i=1,3,1
              if(rea_com(k)(i:i).eq.'?') rea_com(k)(i:i)=' '
           enddo
           rea_co(k)(1:1)=rea_com(k)(1:1)
           rea_co(k)(2:2)=rea_com(k)(3:3)
c
c   author or operator, ISC uses -- if not there
c
           rea_operator(k)=text(145:147)
           if(rea_operator(k).eq.'--') rea_operator(k)=' '
c
c   agency reporting
c
           rea_agency(k)=text(151:153)
c
c   network  fix  XXXXXXXXXX
c
           rea_network(k)=text(133:134)
c
c   for now it always seems to be IR so remove
c
           if(rea_network(k).eq.'IR') rea_network(k)=' '
c
c   location, if not, ISC uses --
c 
           rea_location(k)=text(142:143)
           if(rea_location(k).eq.'--') rea_location(k)=' '


           if(text(100:100).eq.'a') rea_auto(k)='auto'
           if(text(101:101).eq.'c') rea_polarity(k)='C'          
           if(text(101:101).eq.'d') rea_polarity(k)='D'
           if(text(102:102).eq.'i') rea_onset(k)='I'
           if(text(102:102).eq.'e') rea_onset(k)='E'

           if(text(6:12).ne.' ') read(text(6:12),'(f7.1)') rea_dist(k)
           rea_dist(k)=rea_dist(k)*111.2
      
c          read(text(79:81),'(f3.0)') rea_ain(k)
           if(text(14:18).ne.' ') read(text(14:18),'(f5.0)') rea_az(k)
           rea_phase(k)=text(20:27)
c
c   covert phase name P* and S* to seisan standard
c
           if(rea_phase(k)(1:2).eq.'P*') rea_phase(k)(1:2)='Pb' 
           if(rea_phase(k)(1:2).eq.'S*') rea_phase(k)(1:2)='Sb' 
c
c   save phase since it might be used to create a new amplitude or BAZ phase
c
           phase_save=rea_phase(k)
           if(text(35:40).ne.' ')
     *     read(text(35:40),*) rea_sec(k)
           if(text(43:46).ne.' ') 
     *     read(text(41:46),*) rea_res(k)   
c
c   if no residual and reviewed by ISC, phase not used so weight out phase
c
           if(text(43:46).eq.' '.and.isc_review)
     *     rea_weight_in(k)='4'   
c
c   if residual and event reviewed by ISC and time defining flag, use with full weight
c
           if(text(43:46).ne.' '.and.isc_review.and.text(74:74).eq.'T')
     *     rea_weight_in(k)=' ' 
c  
c   if residual less than 5 and events reviewed by ISC 
c   and no time defining flag, use weight 3, else 4
c
           if(text(43:46).ne.' '.and.isc_review.and.text(74:74).ne.'T'.
     *     and.abs(rea_res(k)).lt.5.0)
     *     rea_weight_in(k)='3'   

           if(text(43:46).ne.' '.and.isc_review.and.text(74:74).ne.'T'.
     *     and.abs(rea_res(k)).ge.5.0)
     *     rea_weight_in(k)='4'   
c
c   if event has not been reviewed by isc and no residual so then use and
c   do not weight out   
c
           if(text(43:46).eq.' '.and..not.isc_review)
     *     rea_weight_in(k)=' '  
c
c  if event has been reviewed by ISC but the location has been fixed to 
c  reporting agency, there is no time defining flag so give full weight
c
           if(text(43:46).ne.' '.and.isc_review.and.text(74:74).ne.'T'
     *     .and.hyp_agency(1).ne.'ISC')
     *     rea_weight_in(k)=' '  
c
c   if event is from  IDC and has residual and time defining flag
c   do not weight out   
c
           if(text(43:46).ne.' '.and.text(74:74).eq.'T'
     *     .and.hyp_agency(1).eq.'IDC')
     *     rea_weight_in(k)=' '  

c
c   if event is from agencies different from IDC and ISC and has residual
c   use event if no time defining flag  
c
           if(text(43:46).ne.' '.and.hyp_agency(1).ne.'ISC'
     *     .and.hyp_agency(1).ne.'IDC')
     *     rea_weight_in(k)=' '  

c
c   phases that cannot be used in seisan are weighted out, more to be added
c
           if(rea_phase(k)(1:4).eq.'PnPn'.or.
     *        rea_phase(k)(1:4).eq.'SnSn'.or.
     *        rea_phase(k)(1:5).eq.'PFAKE'.or.
     *        rea_phase(k)(1:4).eq.'PbPb'.or.
     *        rea_phase(k)(1:4).eq.'SbSb'.or.
     *        rea_phase(k)(1:3).eq.'pPN'.or.
     *        rea_phase(k)(1:3).eq.'pPn'.or.
     *        rea_phase(k)(1:3).eq.'sPN'.or.
     *        rea_phase(k)(1:3).eq.'sPn'.or.
     *        rea_phase(k)(1:2).eq.'PM')
     *        rea_weight_in(k)='4'  

c
c
c    convert all n and b phases to P or S
c
           if(rea_phase(k)(1:3).eq.'Pn')  rea_phase(k)(1:3)='P'  
           if(rea_phase(k)(1:3).eq.'Pb')  rea_phase(k)(1:3)='P' 
           if(rea_phase(k)(1:3).eq.'Pg')  rea_phase(k)(1:3)='P' 
           if(rea_phase(k)(1:3).eq.'Sn')  rea_phase(k)(1:3)='S' 
           if(rea_phase(k)(1:3).eq.'Sb')  rea_phase(k)(1:3)='S'
           if(rea_phase(k)(1:3).eq.'Sg')  rea_phase(k)(1:3)='S'
         

           if(text(29:40).ne.' ')
     *     read(text(29:40), '(i2,1x,i2)') 
     *     rea_hour(k),rea_min(k)

           rea_year(k)=hyp_year(1)
           rea_month(k)=hyp_month(1)
           rea_day(k)=hyp_day(1)   
c
c   slownes and baz
c
           if(text(61:65).ne.' ') then
               read(text(61:65),*) rea_vel(k)
               rea_vel(k)=111.2/rea_vel(k)
           endif
           if(text(48:52).ne.' ') then
               read(text(48:52),*) rea_baz_obs(k)
               if(text(53:58).ne.' ') read(text(53:58),*) rea_baz_res(k)
           endif
c
c   create a new BAZ phase, same time, if either baz or vel present
c
           if(rea_vel(k).ne.-999.0.or.rea_baz_obs(k).ne.-999.0) then
               k=k+1
               call rea_phase_clear(k) 
               call rea_copy_phase(k-1,k,0)
               rea_res(k)=-999.0            ! do not use phase residul in BAZ
               rea_polarity(k)=' '          ! ----------------- pol from phase
               rea_phase(k)='BAZ-'//rea_phase(k-1)(1:4)
               if(rea_phase(k)(5:5).eq.' ') rea_phase(k)(4:4)=' ' ! do not have BAZ- if no phase
c
c   if no BAZ residual, BAZ not used and might be wrong, seen in ISC data
c   so weight out, also if not reviewed by isc
c
               if(rea_baz_res(k).eq.-999.0) rea_weight_in(k)='4'
c
c   clear on previous BAZ info  so not written out 2 times
c
               rea_baz_obs(k-1)=-999.0
               rea_vel(k-1)=-999.0
               rea_baz_res(k-1)=-999.0
           endif

c
c   coda phase, so far only seen in nanometrics system
c          
          if(rea_phase(k)(1:3).eq.'AMd') then
             rea_phase(k)='END'
             if(text(93:98).ne.' ') read(text(93:98),*) rea_coda(k)
             rea_weight_in(k)=' '
             rea_per(k)=-999.0
             rea_amp(k)=-999.0
             nanometrics=.true.
           endif

c
c-----------------------------
c   amplitude phases like IAmb
c------------------------------
c
           if(rea_phase(k)(1:4).eq.'pmax'.or.           ! not standard, found in ISC
     *        rea_phase(k)(1:3).eq.'MLR'. or.           ! assume raleight wave
     *        rea_phase(k)(1:4).eq.'smax'.or.           ! not standard, found in ISC
     *        rea_phase(k)(1:2).eq.'M '.  or.
     *        rea_phase(k)(1:2).eq.'L '.  or.
     *        rea_phase(k)(1:2).eq.'IA'.  or.
     *        rea_phase(k)(1:2).eq.'IV'.  or.
     *        rea_phase(k)(1:1).eq.'A') then

              if(text(84:92).ne.' ') 
     *        read(text(84:92),*) rea_amp(k)
              if(text(93:98).ne.' ') 
     *        read(text(93:98),*) rea_per(k)
c
c   albania nanometrics system 
c
              if(rea_phase(k)(1:3).eq.'AMl') then
                 nanometrics=.true.
                 rea_phase(k)='IAML'
c
c   convert amplitudes in mm WA to nm, only albania, fix
c
                  rea_amp(k)=(rea_amp(k)/2800.0)*1000000.0
                  rea_auto(k)=' '
              endif
c
c    check that phase amp might be IASPEI, convert if likely
c   
              amp_phase='A'               !   default if not identified      
              if(rea_phase(k)(1:2).eq.'IA')   amp_phase=rea_phase(k)
              if(rea_phase(k)(1:2).eq.'IV')   amp_phase=rea_phase(k)
c
c   if magnitude Ms is calculated, assume amplitude is ok
c
              if((rea_phase(k)(1:3).eq.'MLR'.or.
     *            rea_phase(k)(1:2).eq.'M '. or.
     *            rea_phase(k)(1:2).eq.'L '. or.
     *            rea_phase(k)(1:2).eq.'LR'. or.
     *            rea_phase(k)(1:3).eq.'AMS')     
     *           .and.text(104:105).ne.' ')  
     *        amp_phase='IAMs_20'  ! amplitude on raleigh wave, assume Ms   

c
c   if magnitude Ms is not calculated, assume amplitude is ok but do indicate as Ms
c
              if((rea_phase(k)(1:3).eq.'MLR'.or.
     *            rea_phase(k)(1:2).eq.'M '. or.
     *            rea_phase(k)(1:2).eq.'L '. or.
     *            rea_phase(k)(1:2).eq.'LR'. or.
     *            rea_phase(k)(1:3).eq.'AMS')     
     *           .and.text(104:105).eq.' ')  
     *        amp_phase='A-'//rea_phase(k)(1:6)  

c
c   if magnitude mb is calculated assume amplitude ok
c
              if(rea_phase(k)(1:3).eq.'AMB'.and.text(104:105).ne.' ')
     *        amp_phase='IAmb' 
 
c
c   if magnitude mb is not calculated assume amplitude ok but do not assume
c   mb. could be broadband mb and magnitude not calculated due to period
c
              if(rea_phase(k)(1:3).eq.'AMB'.and.text(104:105).eq.' ')
     *        amp_phase='A-AMB' 
c
c   if magnitude mb is calculated assume amplitude ok
c
              if(rea_phase(k)(1:4).eq.'pmax'.and.text(104:105).ne.' ')
     *        amp_phase='IAmb' 
 
c
c   if magnitude mb is not calculated assume amplitude ok but do not assume
c   mb. 
c
              if(rea_phase(k)(1:4).eq.'pmax'.and.text(104:105).eq.' ')
     *        amp_phase='A-pmax'            
c
c   next should have been used but is often wrong in ISC
c 

c              if(rea_phase(k)(1:3).eq.'AML')  amp_phase='IAML'     ! older Ml
               if(rea_phase(k)(1:3).eq.'AML')  amp_phase='A-AML'    ! older Ml

              rea_phase(k)=amp_phase
              rea_weight_in(k)=' '         ! no weight on amplitudes
              goto 25   ! next phase
           endif

 29   continue         ! from above , blank station etc, but amplitude is there
c
c   case of no amplitude phase 
c   create a new amplitude phase, using info from previous phase. the amplitude
c   can have been on a phase line or an amplitude line without station info etc
c
           if(text(89:98).ne.' ') then         
              k=k+1
              call rea_phase_clear(k)
              rea_stat(k)=rea_stat(k-1)
              rea_comp(k)=rea_comp(k-1)  
              rea_com(k)=rea_com(k-1)
              rea_co(k)=rea_co(k-1)  
              rea_year(k)=rea_year(k-1)
              rea_month(k)=rea_month(k-1)
              rea_day(k)=rea_day(k-1)
              rea_hour(k)=rea_hour(k-1)
              rea_min(k)=rea_min(k-1)
              rea_sec(k)=rea_sec(k-1)
              rea_dist(k)=rea_dist(k-1)
              rea_az(k)=rea_az(k-1)
              rea_auto(k)=rea_auto(k-1)
              amp_phase='A'       ! default if not identified  
              rea_weight_in(k)=' ' ! no weight on amplitudes
c
c   local magnitude, was accepted before but too many bad amplitudes
c   so now only IAML is accepted
c
c              if(text(105:106).eq.'L '.or.text(105:106).eq.'l ')then   
c
c   if from IMS, amplitudes for local magnitude are not standard and give very
c   low manitude so do not use. this does not catch other agencies doing the same,
c   like NORSAR.
c
c                  if(hyp_agency(1).ne.'IDC')  amp_phase='IAML'
c
c   do not use amplitudes read on P
c
c                 if(phase_save(1:1).eq.'P') amp_phase='A'
c              endif

c
c   first assume amplitude is not identified so call amplitude A-phase
c   if a phase given
c
cxx
c              write(6,*)rea_stat(k),rea_phase(k), phase_save
              amp_phase='A-'//text(20:25)

c   case of amplitude on phase and time is given
c
              if(phase_save.ne.' ') amp_phase='A-'//phase_save(1:6)
c
c   case of phase with no time but amplitude is given
c
c              if(phase_save.eq.' ') amp_phase='A-'//text(20:25)

              if(text(104:105).eq.'b ')
     *        amp_phase='IAmb'

              if(text(104:105).eq.'s ')
     *        amp_phase='IAMs_20'

              if(phase_save(1:2).eq.'LR') amp_phase='IAMs_20'
              if(phase_save(1:2).eq.'P '.and.  ! assume mb if read on P at more then 20 deg
     *        rea_dist(k).ge.2224.0) amp_phase='IAmb'
              rea_phase(k)=amp_phase
c
c   read amplitude
c          
              read(text(84:92),'(f9.3)') rea_amp(k)
              if(text(93:98).ne.' ') 
     *        read(text(93:98),'(f6.3)') rea_per(k)
c
c   check if amplitude component is different from phase componenet
c
              amp_com=text(161:163)
              do i=1,3
                 if(amp_com(i:i).eq.'?') amp_com(i:i)=' '
              enddo
              if(amp_com.ne.' '.and.amp_com.ne.rea_com(k)) 
     *        rea_com(k)=amp_com
           endif
                  
c
c   go for next phase
c
           goto 25
       endif        ! end of phase reading
     
       goto 20      ! no lines were identified, try next      

c----------------------------------------------
c  end of one event, write out
c----------------------------------------------
 
 30       continue

          rea_nphase=k  
          nevent=nevent+1               ! count events

c
c   sometimes number of stations is not given for prime solution, 
c   if so count stations
c   with non zero weight phases, do not include amplitudes
c
          if(hyp_nstat(1).le.0.and.rea_nphase.gt.0) then
             call rea_count_stations(hyp_nstat(1))
          endif
c
c   check if header line has agency. if not use agency from prompt if entered
c
          if(def_agency.ne.' ') then
             if(hyp_agency(1).eq.' ') hyp_agency(1)(1:3)=def_agency
             do i=1,6
               if(hyp_mag_agency(i,1).eq.' '.
     *         and.hyp_mag(i,1).gt.-99.0) hyp_mag_agency(i,1)(1:3)
     *         =def_agency
             enddo
          endif
c
c  duplicate header line and move all down so duplicate comes second
c       
          do i=rea_nhyp,1,-1
             call rea_copy_hyp(i,i+1)
          enddo
c
c  check if agency given for dup
c
          if(def_agency_dup.ne.' ') then
             hyp_agency(2)=def_agency_dup
             do i=1,6
               if(hyp_mag(i,2).gt.-99.0) hyp_mag_agency(i,2)(1:3)
     *         =def_agency_dup
             enddo
          endif
          rea_nhyp=rea_nhyp+1
c
c   possibly an arc line
c
          if(arc) then
             rea_wav(1)=' '
             rea_wav(1)(80:80)='6'
             rea_wav(1)(2:4)='ARC'
             rea_wav(1)(6:6)='*'
c
c   if there are default values, put them in
c
         start_time_duration=' '
c
c   set start time, take origin time
c
 
            call timsec (hyp_year(1),hyp_month(1),hyp_day(1),
     *      hyp_hour(1),hyp_min(1),hyp_sec(1),abstime)
c
c   start 30 s before origin time of arc_start_time not defined
c   in seisan.def
c
            if(arc_start_time.eq.0.0) arc_start_time=30.0
            abstime=abstime-arc_start_time   ! start arc_start_time before origin time
            call sectim (abstime,year,doy,month,day,hour,min,sec)
            write(start_time_duration(1:17),
     *      '(i4,1x,2i2,1x,2i2,1x,i2)')
     *      year,month,day,hour,min,int(sec)
c
c   set duration, 600s if not defined in seisan.def
c
            if(arc_duration.eq.0.0) arc_duration=300.0
            write(start_time_duration(18:23),'(i6)') int(arc_duration) 
            rea_wav(1)(22:44)=start_time_duration
            rea_nwav=1
         endif         

          all=.true.
          call rea_event_out(2,all,data,code)
c
c   write the whole first header line to screen
c
          write(6,'(a)') data(1)(1:79)

c
c   get next event
c
          goto 12


c
c   end of file or other error reading text string
c

 300   continue

c
      write(6,*)            ! blank line
      close(1)
      close(2)              ! close output file
      write(6,*) 'Number of events in input file', nevent
      write(6,*) 'Output file name is imsnor.out'

      stop
      end
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine sort_source_info(key)
c
c   sort source info according to agency, different order for fps than
c   hypocenters and magnitudes
c
c   key=1   : hypocenters
c   key=2   : magnitudes
c   key=3   : fault plane solutions
c
c   march 2020 jh
c
      implicit none
      include 'seidim.inc'
      include 'rea.inc'
      integer i,k,j,key,kmag
      character*3 agency(10)          ! agencies to sort in order 1-10, hypocenter and mag     
      integer n_agency                ! number of agencies
      character*3 agency_fault(10)    ! agencies to sort in order 1-10, fps
      integer n_agency_fault          ! number of agencies, fps

      agency(1)='ISC'
      agency(2)='NEI'
      agency(3)='GCM'
      agency(4)='IDC'
      n_agency=4

      agency_fault(1)='GCM'
      agency_fault(2)='NEI'
      agency_fault(3)='ISC'
      n_agency_fault=3

      
      if(key.eq.1) continue
      if(key.eq.2) goto 10
      if(key.eq.3) goto 20

c
c   find priority agencies, hyp
c

      if(rea_nhyp.gt.50) then
         write(6,*)'Too many hypocenters, more then 50, stop'
         stop
      endif
c
c  the last hypocenter is considred the prime and must always come first
c  since the residuals are referred to that origin
c

      write(6,*) hyp_agency(100-rea_nhyp)
      call rea_copy_hyp(100-rea_nhyp+1,1)
c
c   sort the rest if more then 1
c
      if(rea_nhyp.eq.1) return
      
      j=1
      do k=1,n_agency
         do i=100,100-rea_nhyp+2,-1
            if(agency(k).eq.hyp_agency(i)(1:3)) then
                j=j+1
                call rea_copy_hyp(i,j)                
                hyp_agency(i)(1:3)='XXX'   ! indicate already used
            endif
         enddo
      enddo
c
c   put in the rest
c
      do i=100,100-rea_nhyp+1,-1
         if(hyp_agency(i)(1:3).ne.'XXX') then              
            j=j+1
            call rea_copy_hyp(i,j)
         endif
      enddo
      return
c
c--------------------------------------------------------------
c   sort magntudes so preferred come first
c--------------------------------------------------------------
c
 10   continue
      if(rea_nmag.eq.0) return
      if(rea_nmag.gt.100) then
         write(6,*)'Too many magnitudes, more then 100, stop'
         stop
      endif

      j=0
      do k=1,n_agency
         do i=200,200-rea_nmag+1,-1 
            if(agency(k).eq.hyp_mag_agency_all(i)(1:3)) then
                j=j+1
                hyp_mag_all(j)=hyp_mag_all(i)
                hyp_mag_type_all(j)=hyp_mag_type_all(i)
                hyp_mag_agency_all(j)=hyp_mag_agency_all(i)                
                hyp_mag_agency_all(i)(1:3)='XXX'   ! indicate already used
            endif
         enddo
      enddo
c
c   put in the rest
c
      do i=200,200-rea_nmag+1,-1
         if(hyp_mag_agency_all(i)(1:3).ne.'XXX') then              
            j=j+1
            hyp_mag_all(j)=hyp_mag_all(i)
            hyp_mag_type_all(j)=hyp_mag_type_all(i)
            hyp_mag_agency_all(j)=hyp_mag_agency_all(i)               
         endif
      enddo
c
c   move magnitudes so the first is repeated in 3. position
c
      if(rea_nmag.le.2) then
         hyp_mag_all(3)=hyp_mag_all(1)
         hyp_mag_type_all(3)=hyp_mag_type_all(1)
         hyp_mag_agency_all(3)=hyp_mag_agency_all(1)
         if(rea_nmag.eq.1) then
           hyp_mag_all(2)=-999.0
           hyp_mag_type_all(2)=' '
           hyp_mag_agency_all(2)=' '
         endif
         rea_nmag=3
      else
         do i=rea_nmag,3,-1
           hyp_mag_all(i+1)=hyp_mag_all(i)
           hyp_mag_type_all(i+1)=hyp_mag_type_all(i)
           hyp_mag_agency_all(i+1)=hyp_mag_agency_all(i)
         enddo
         hyp_mag_all(3)=hyp_mag_all(1)
         hyp_mag_type_all(3)=hyp_mag_type_all(1)
         hyp_mag_agency_all(3)=hyp_mag_agency_all(1)
         rea_nmag=rea_nmag+1
      endif
            
c
c   put magnitudes in hypocenter lines, if any
c
      if(rea_nmag.eq.0) return

      j=0
c
c   first 6 or less on header line
c
      if(rea_nmag.le.6) then
          kmag=rea_nmag
      else
          kmag=6
      endif

      do i=1,kmag
         j=j+1
         hyp_mag(i,1)=hyp_mag_all(j)
         hyp_mag_type(i,1)=hyp_mag_type_all(j)
         hyp_mag_agency(i,1)=hyp_mag_agency_all(j)
      enddo

      if(rea_nmag.le.6) return  ! only for one header
      if(rea_nhyp.eq.1) goto 15  
c
c   put following on next header lines if there, only 3 on each since
c   there might be several hypocenter lines from same agency 
c
      do k=2,rea_nhyp
         do i=1,3
            j=j+1
            hyp_mag(i,k)=hyp_mag_all(j)
            hyp_mag_type(i,k)=hyp_mag_type_all(j)
            hyp_mag_agency(i,k)=hyp_mag_agency_all(j)
            if(j.eq.rea_nmag)  return           ! no more magnitudes
         enddo
      enddo
c
c   if still magnitudes left, put on comment lines
c
 15   continue
      if(rea_nmag.gt.j) then
         do i=j+1,rea_nmag
           rea_ncomment=rea_ncomment+1
           write(rea_comment(rea_ncomment),'(1x,f4.1,a1,a4)') 
     *     hyp_mag_all(i),hyp_mag_type_all(i),hyp_mag_agency_all(i)
           rea_comment(rea_ncomment)(80:80)='3'
         enddo
      endif
      return
c          
c--------------------------------------------------------------
c   sort fault plane solutions so preferred come first
c--------------------------------------------------------------
c
 20   continue

      if(rea_nfault.gt.50) then
         write(6,*)'Too many fault plane solutions, more then 50, stop'
         stop
      endif
      
      if(rea_nfault.eq.0) return

      j=0
      do k=1,n_agency_fault
         do i=100,100-rea_nfault+1,-1 
            if(agency_fault(k).eq.rea_fault(i)(67:69)) then
                j=j+1
                rea_fault(j)=rea_fault(i)              
                rea_fault(i)(67:69)='XXX'   ! indicate already used
            endif
         enddo
      enddo
c
c   put in the rest
c
      do i=100,100-rea_nfault+1,-1
         if(rea_fault(i)(67:69).ne.'XXX') then              
            j=j+1
            rea_fault(j)=rea_fault(i)
         endif
      enddo

      return
      end
                
               
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine hyp_ellipse_err(smaj,smin,az,errx,erry)
c
c   from error ellipse, calculate xy errros.
c   the error are calculated as the ellipse crossing point
c   at the axis
c   
c   jh sep 2020
c
      implicit none
      real smaj,smin,az      ! major,min axis and azimuth
      real errx, erry        ! xy errors

     
      az=90.0-az
      az=az*0.01745329
      errx=1.0/sqrt((cos(az)/smaj)**2+(sin(az)/smin)**2)
      erry=1.0/sqrt((sin(az)/smaj)**2+(cos(az)/smin)**2)

      return
      end
      
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine isc_test(is_isc, isc_last_date,event_last_date,n,k)

c
c   finds if isc file and last date of revision, checks if headers
c
      implicit none
      logical is_isc                ! true if from isc
      character*22 isc_last_date    ! last date of time of isc revision
      character*22 event_last_date  ! date of last event
      character*200 text            ! text
      integer n,k
      n=0
      k=0

      isc_last_date=' '
      is_isc=.false.

 1    continue
      read(1,'(a)',end=2) text
      n=n+1
      if(text(119:121).eq.'ISC') then
          is_isc=.true.
          isc_last_date=text(1:22)
      endif
      if(text(1:18).eq.'   Date       Time') k=k+1
      if(text(1:9).eq.'Magnitude') k=k+1
      if(text(1:12).eq.'Sta     Dist') k=k+1
      if(text(5:5).eq.'/'.and.text(8:8).eq.'/') 
     *event_last_date=text(1:22)
      goto 1
 2    continue
      rewind 1
      return
      end
