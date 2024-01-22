c
c    Include file for SEISAN system
c
c  19.06.98  lo     : included merge_wav, and when was map variables put in ???
c  sep 16, 98 by jh : -------- version 7.0 check --------------------------
c  nov 26  98 jh    : add variables n_wav_dirs,wav_dirs
c  sep 20           : add geo_depth1, geo_depth2
c  sep 23           : add herkij_distance
c  feb           lo : add keep_auto
c  feb 28  2000     : add copy_wav_dir
c  mar 10        jh : add text_print
c  april 11         : fix common boundaries 
c  oct 20           : merge_wav to 5 chars
c  oct 24           : add high_accuracy
c  july 1        jh : common block boundaries screwed up again !!!!
c  oct 11 2001   lo : add cursortype
c  april 29 2002 bjb: add hypo71_offset parameter
c  may 13 2002   lo : add confirmation_level
c  may 27 2002   lo : add reg_auto_process
c  apr 10 2003   lo : add myxfont
c  oct 16 2007   jh : add gmap variables
cv oct 13 2007   jh : add arc variables
c  apr 27 2012   lo : add magnitude_order
c  oct    2012   pv : some changes ?
c  dec  2 2012   jh : add cont_year_position_file,cont_month_position_file
c  jan  4 2013   jh : add more ARC varuiables for start time, duratio, type 
c                     and default
c  jan 15 2013   jh : start and stop times of valid arcive channel
c                     increase arc dim to 3000
c  feb 05 2013   jh : change alignment
c  apr 09 2013   jh : add spec model
c  oct 30 2013   jh : add plot_picture_command, plot_pdf_command
c  mar 18 2014   jh : add q_below_1hz for p and s
c 2015 05 21     pv : added seisan_logging integer for turning logging on/off
c  jan 05 2015   lo : add eev comments
c  jan 17 2018   jh : add plot_ps_command, gmt_gridfile
c  jan 15 2019   jh : add new_nordic_format, new_nordic_format_only
c  mar 26 2020   fh : increased maxvirchan from 500 to 800
c  apr  8 2020   lo : maxvirchan to 1000
c  apr  8 2020   lo : maxcont to 600
c  jul 13 2020   pv : added verbosity, for debugging in pro and lib
c  jan 17 2021   jh : alertmail_sum and n_alertmail_sum
c  jan 28 2021   jh : add email_system,email_sum_title,seisan_agency,
c                     gmt_locality_file, epimap_locality_file,
c                     epimap_plot_locality,email_summary_map
c                     mailx_command_map
c  dec 27 2021   jh:  add alertemail_netdet,n_alertemail_netdet
c  jul 6  2022   jh:  add map_lat_div and map_lon_div
c  mar 12 2023   jh:  add alertemail_netdet_summary, n_alertemail_netdet_summary,
c                     alertemail_format

      integer n_wav_dirs         ! number of directories to search
c
      character*80 wav_dirs(100) ! waveform directories to search
      character*80 myxfont       ! name of x font
c
c   waveform directories
c
      integer n_wav_bases        ! number of waveform bases under WAV

      character*5 wav_base(100)  ! waveform base name
      character*80 full_path_wav_name  ! full path waveform file name
      character*5 merge_wav      ! code for waveform file when merging 
                                 ! from mulplt
      
      character*3 seisan_agency  ! agency

      real map_lat,map_lon       ! lat and long for starting epimap from eev
      real map_lat_div,map_lon_div ! division for ---------------------------
      character*10 map_file      ! name of epimap map file
      character*4 map_stations   ! print stations in epimap
      integer map_proj           ! number of projection
      real confirmation_level    ! 0 if skip confirmation

c  spectral
 
      real geo_depth1,geo_depth2 ! geometrical spreading depths
      real herkij_distance       ! herrnmann kijko geo spreading distance

c
c  spec model
c
       integer nspec_model! number of layers in spec model
       real kappa_p       ! kappa for p
       real kappa_s       ! kappa for s
       real spec_depth(32)! layer depth in spec model
       real qalpha_p(32)  ! ------------ vs depth for p
       real qalpha_s(32)  ! ------------------------- s
       real q0_p(32)      ! Q0 vs depth for p
       real q0_s(32)      ! ---------------- s
       real svelocity_depth(32) ! s-velocity vs depth 
       real pvelocity_depth(32) ! p-velocity vs depth
       real density_depth(32)   ! density vs depth
       real q_below_1Hz    ! 0: frequecy dependent below 1 hz :1: constant below x hz
       real qp_below_1Hz   ! 0: frequecy dependent below 1 hz :1: constant below x hz
       real qs_below_1Hz   ! 0: frequecy dependent below 1 hz :1: constant below x hz


c
      real keep_auto             ! keep auto phases when register is 1.
      character*5 copy_wav_dir   ! copy to this data base in mulplt 
      character*80 text_print    ! print command for a text file
      logical high_accuracy      ! if true, use high accuracy

c
c variables defined for working with continuos data
c

c-- maximum number of databases
      integer max_cont
c      parameter(max_cont=50)
      parameter(max_cont=600)
      integer max_cont_comp
c      parameter(max_cont_comp=150)
      parameter(max_cont_comp=300)
      character*5 cont_base(max_cont)    ! name of cont databases
      integer cont_base_def(max_cont)! 1 if default cont database
      character*5 cont_net_code          ! name of cont database for combined waveform file
      character*4 cont_comp(max_cont_comp)  ! component

      integer n_cont_base                ! number of ------
      integer n_cont_base_sel            ! number of selected ------
      integer n_cont_comp                ! number of components
      character*4 cont_operator          ! name of operator 
      real cont_before                   ! beginning of time interval for
      real cont_after                    ! end ----- reading waveform headers
      real cont_interval                 ! time interval to be plotted in sec 
      real cont_interval_org             ! original ------ 
      real cont_int_s                    ! small  ------
      integer cont_year_position_file,cont_month_position_file  ! start pos. in file name of year and month
c
c   for arc archive
c
      integer max_arc
      parameter(max_arc=3000)
      character*5       arc_stat(max_arc)               ! staiton name
      character*3       arc_comp(max_arc)               ! component
      character*2       arc_net(max_arc)                ! network
      character*2       arc_loc(max_arc)                ! location
      integer           arc_def(max_arc)                ! 1 gives default stations
      double precision  arc_start(max_arc),arc_stop(max_arc) ! start and stop time of arc
      character*80      arc_archive                  ! location of archive
      character*80      arc_archive_folder(99)       ! archive folder names
      character*9       arc_archive_label(99)        ! archive labels
      integer           arc_archive_n                ! counter for archive folders
      character*80      arc_folder(max_arc)          ! archive folder for one channel
      character*9       arc_label(max_arc)           ! archive label for one channel
      integer           arc_nchan                    ! number of channels in archive
      integer           arc_type                     ! 0: bud, 1: seiscomp
      integer           arc_vnet                     ! number of virtual archives
      integer           maxvir,maxvirchan
c      parameter         (maxvir=50) ! lo
      parameter         (maxvir=100)
c      parameter         (maxvirchan=800)
      parameter         (maxvirchan=1000)  ! lo
      character*5       arc_vnet_name(maxvir)      ! name of virtual networks
c      real              arc_vnet_duration(maxvir)  ! virtual networks data duration
      integer           arc_vnet_nchan(maxvir)     ! number of channels in virtual net
      character*5       arc_vnet_stat(maxvir,maxvirchan)  ! station name
      character*3       arc_vnet_comp(maxvir,maxvirchan)  ! component
      character*2       arc_vnet_net(maxvir,maxvirchan)   ! network
      character*2       arc_vnet_loc(maxvir,maxvirchan)   ! location
      real              arc_start_time             ! time in secs before origin time
      real              arc_duration               ! duration in secs
      integer           arc_by_default             ! if 1, look for archive even if not given
       
      integer seisan_logging             ! 0=logging off, 1=on

      integer verbosity                  ! 0=verbose off, 1,2,3,...=on and level
     
      integer cursortype                 ! choice of cursor

      real reg_autoprocess_flag          ! 0,1,2
      character*10 reg_autoprocess_name  ! name of process to run when register
c
c  gmap paramters
c
       character*10 gmap_type		 ! can be MAP, SATELLITE or HYBRID
       real gmap_size_lat                ! srtar map size lat
       real gmap_size_lon                ! start map size lon
       character*80 gmap_dir             ! place to put web file
c
c  gmt and epimap 
c
       character*80 gmt_gridfile         ! grid file for gmt
       character*80 gmt_locality_file    ! localities for gmt
       character*80 epimap_locality_file ! --------- epimap
       logical epimap_plot_locality      ! true if plotting
       

c parameter to offset station longitudes for events close to
c the Greenwich meridien
      real hypo71_offset
c maximum solutions in focmec
      integer focmec_maxsol
c order to sort magnitudes in mag_hyp_all
      character*4 magnitude_order(1000) ! list with type and agency
      integer n_magnitude_order         ! counter for the above

      character*80 plot_picture_command ! command to plot picture
      character*80 plot_pdf_command     ! command to plot pdf
      character*80 plot_ps_command      ! co\mmand to plot ps file

c eev comments
      integer n_eev_comments
      character*78 eev_comments(99)

c email alerts

      integer n_alertemail, n_alertemail_sum,n_alertemail_netdet,
     *        n_alertemail_sum_netdet
      character*80 email_sum_title   ! title for summary email
      character*80 alertemail(99),alertemail_sum(99),
     *             alertemail_netdet(99),alertemail_sum_netdet(99)
      character*80 mailx
      character*80 email_system ! system call optionally after email
      character*120 mailx_command_map  ! command to send mail with picture
      real email_summary_map           ! 0: no, 1:epimap, 2 mapg,  send picture with summary mail
      integer alertemail_format        ! 0: nordic, 2: nordic2
c nordic format
      logical new_nordic_format          ! if true, new format, ols can be input
      logical new_nordic_format_only      ! if true, only new format as input


C *********   KEEP PARAMETER TOGETHER WITH SAME BYTE LENGHT **************


C  TEXT
      common/seisan_text/    
     &                   wav_dirs,
     &                   full_path_wav_name,
     &                   text_print,myxfont,
     &                   reg_autoprocess_name,
     &                   cont_operator,cont_comp,
     &                   eev_comments,arc_vnet_name,alertemail,mailx,
     &                   alertemail_sum,email_system,email_sum_title,
     &                   alertemail_netdet,alertemail_sum_netdet

C  4 BYTES
      common/seisan4/    
     &                   cont_before,cont_after,
     &                   keep_auto,
     &                   cont_interval,cont_interval_org,
     &                   cont_int_s,n_cont_base_sel,
     &                   n_cont_base,n_wav_bases,n_wav_dirs,
     &                   n_cont_comp,
     &                   cursortype,confirmation_level,focmec_maxsol,
     &                   n_magnitude_order,
     &                   reg_autoprocess_flag, cont_base_def,
     *                   gmap_size_lat,gmap_size_lon,arc_archive,
     *                   arc_folder,arc_label,
     *                   arc_nchan,arc_type,magnitude_order,
     *                   cont_year_position_file,
     *                   cont_month_position_file, arc_start_time,
     *                   arc_duration,
     *                   arc_by_default,
     *                   seisan_logging,
     *                   verbosity
      common/more4/      arc_start,arc_stop,plot_picture_command,
     *                   plot_pdf_command,n_eev_comments,n_alertemail,
     *                   n_alertemail_netdet,
     *                   plot_ps_command,gmt_gridfile,n_alertemail_sum,
     *                   gmt_locality_file,epimap_locality_file,
     *                   mailx_command_map,n_alertemail_sum_netdet,
     *                   alertemail_format

c
c   spec model
c
      common/spec_model/
     *nspec_model,kappa_p,kappa_s,spec_depth,
     *qalpha_p,qalpha_s,q0_p,q0_s,
     *svelocity_depth, pvelocity_depth,density_depth,q_below_1hz,
     *qp_below_1hz,qs_below_1hz

   
c  5 bytes

      common/seisan5/ merge_wav,copy_wav_dir,cont_base,cont_net_code,
     &                wav_base,arc_stat,arc_vnet_stat

c  4 bytes

      common/epimap4/ map_lat,map_lon,map_stations,map_proj,
     *map_lat_div,map_lon_div

c  10 bytes

      common/epimap10/ map_file,gmap_type,gmap_dir
c
c  4 bytes

      common/allspec/ geo_depth1,geo_depth2,herkij_distance,
     *email_summary_map 
c
c  1 byte

      common/seisan1/ high_accuracy,epimap_plot_locality
     

      common/hypo71/ hypo71_offset

      common/new_nordic/new_nordic_format,new_nordic_format_only
c
c  2 bytes
c
      common /seisan2/ arc_loc,arc_net,arc_def,arc_vnet,
     &   arc_vnet_loc,arc_vnet_net,arc_vnet_nchan
c
c   3 bytes
c
      common /seisan3/ arc_comp,arc_vnet_comp,seisan_agency


