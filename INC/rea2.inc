c
c    include file and common block for parameter data for one event
c    in rea2 structure
c
c    the aprameter block is omitted since not used in rea
c
c    jh may 2020
c
c    updates
c
c 

c
c  general parameters
c
      integer      rea2_nstat                  ! number of stations
      integer      rea2_nphase                 ! number of phases lines
      integer      rea2_nhead                  ! number of header lines
      integer      rea2_nrecord                ! number of records
      integer      rea2_nspec                  ! number of spectra 
      integer      rea2_nhyp                   ! number of hypocenters
      integer      rea2_nmag                   ! number of magnitudes
      integer      rea2_nmacro                 ! number macroseismic lines
      integer      rea2_nwav                   ! number of waveform files
      integer      rea2_ncomment               ! number of comment lines
      integer      rea2_nfault                 ! number of fault plane solutions
      character*80 rea2_macro(100)             ! macroseismic solutions
      character*80 rea2_wav(500)               ! waveform file names lines
      character*80 rea2_fault(100)             ! fault plane solutions lines
      character*80 rea2_comment(1000)          ! comment lines
      character*80 rea2_picture(100)           ! picture lines
      integer      rea2_npicture               ! number of picture lines
      character*80 rea2_id_line                ! event id line
      integer      rea2_id_line_number         ! position in data for id line
c
c  next 3 not needed, left to keep common block the same 
c
      integer rea2_err_unit                    ! unit to write error messages
      integer rea2_read_err                    ! unspecified  read error, 0: ok, 1: error
      integer rea2_write_err                   ! unspecified write error, 0: ok, 1: error

      character*3 rea2_action                  ! action parameter from id line
      character*68 rea2_locality               ! locality
c
c  next 4 not needed, left to keep common block 
c
      character*80 rea2_message(1000)          ! messages from rea routines
      integer rea2_n_message                   ! number of messages
      logical      rea2_new_in                 ! if true, new format in
      logical      rea2_new_out                ! if true, new format out
      logical      rea2_old_format_required    ! if true, force old format in data array
      integer rea2_read_to_sfile_end           ! if -1, read to end in indata
c
c   hypocentral parameters, index 1 is the prime solution in first
c   line
c
      integer     hyp2_year(100)              ! hypocenter year
      integer     hyp2_month(100)
      integer     hyp2_day(100)
      integer     hyp2_hour(100)
      integer     hyp2_min(100)
      real        hyp2_sec(100)
      character*1 hyp2_model(100)             ! model indicator
      character*1 hyp2_dist_id(100)           ! distance indicator
      character*1 hyp2_type(100)              ! event type like E
      character*1 hyp2_fix_org(100)           ! fix origin time flag
      real        hyp2_lat(100)               ! latitude
      real        hyp2_lon(100)               ! longitude
      real        hyp2_depth(100)             ! depth
      character*1 hyp2_depth_flag(100)        ! depth flag
      character*1 hyp2_epi_flag(100)          ! epicenter flag
      character*5 hyp2_agency(100)            ! hypocenter agency, use 3 only
      integer     hyp2_nstat(100)             ! number of station
      real        hyp2_rms(100)               ! rms of hypocenter solution
      real        hyp2_mag(6,100)             ! magnitudes
      real        hyp2_mag_all(200)           ! all magnitudes, no hyp. assco.
      character*1 hyp2_mag_type(6,100)        ! magnitude types
      character*1 hyp2_mag_type_all(200)      ! all -----------
      character*5 hyp2_mag_agency(6,100)      ! magnitude agencies
      character*5 hyp2_mag_agency_all(200)    ! all
      integer     hyp2_mag_all_main(200)      ! all, indicate if from main hypocenter
      logical     hyp2_high_accuracy(100)     ! high accurcy flag
      logical     hyp2_error(100)             ! true if hypocenter has an error
      character*20 hyp2_auto(100)             ! name of auto process for parameter
c
c   hypocenter errors
c
      real        hyp2_gap(100)               ! gap, degrees
      real        hyp2_sec_err(100)           ! oriign time error (sec)
      real        hyp2_lat_err(100)           ! latitude error (km)
      real        hyp2_lon_err(100)           ! longitude error (km)
      real        hyp2_depth_err(100)         ! depth error (km)
      real        hyp2_cov(3,100)             ! covariance, xy,xz,yz (kmXkm)
c
c   Moment tensor parameters
c
      integer     mt2_nmt                    ! Number of Moment tensor solutions
      integer     mt2_year(100)              ! Moment tensor hypocenter year
      integer     mt2_month(100)
      integer     mt2_day(100)
      integer     mt2_hour(100)
      integer     mt2_min(100)
      real        mt2_sec(100)
      real        mt2_moment(100)            ! Scalar moment
      character*1 mt2_coor(100)              ! Moment tensor coordinate system Spherical or Cartesian
c                                           ! S=Spherical and C=Cartesian, see Aki y Richard 1980 p 118
      real        mt2_val(6,100)             ! Moment tensor values
      integer     mt2_exp(100)               ! Moment tensor exponential
      real        mt2_lat(100)               ! Moment tensor latitude
      real        mt2_lon(100)               ! Moment tensor longitude
      real        mt2_depth(100)             ! Moment tensor depth
      character*5 mt2_agency(100)            ! Moment tensor hypocenter agency, use 3 only
      character*7 mt2_method(100)            ! Moment tensor method
      character*1 mt2_quality(100)           ! Moment tensor quality
      real        mt2_mag(100)               ! Moment tensor magnitudes
      character*1 mt2_mag_type(100)          ! Moment tensor magnitude types

c
c data extension used in hypocenter, new nordic format also, not used in rea2
c
c      character*120 data5(max_data)
c
c  phase line parameters and associated parameters, all start with rea
c  and there can be nphase different parameters of each
c
      character*5 rea2_stat(max_data/2)       ! station codes
      character*4 rea2_comp(max_data/2)       ! componenets
      character*3 rea2_com(max_data/2)        ! 3 letter components
      character*2 rea2_co(max_data/2)         ! 2 letter componenets
      character*8 rea2_phase(max_data/2)      ! phase name
      character*1 rea2_onset(max_data/2)      ! onset I or E or blank
      character*1 rea2_weight_in(max_data/2)  ! input weight
      character*2 rea2_weight_out(max_data/2) ! weight out
      character*1 rea2_polarity(max_data/2)   ! polarity, D or C
      integer     rea2_year(max_data/2)
      integer     rea2_month(max_data/2)
      integer     rea2_day(max_data/2)
      integer     rea2_hour(max_data/2)
      integer     rea2_min(max_data/2)
      real        rea2_sec(max_data/2)
      real*8      rea2_abs_time(max_data/2)   ! abs time of phase time
      real        rea2_coda(max_data/2)       ! coda length in s
      real        rea2_amp(max_data/2)        ! amplitude in nm
      real        rea2_per(max_data/2)        ! period of amplitude
      real        rea2_baz_obs(max_data/2)    ! observed back azimuth
      real        rea2_baz_cal(max_data/2)    ! calculated back azimuth
      real        rea2_vel(max_data/2)        ! observed apparent velocity
      real        rea2_ain(max_data/2)        ! calculated angle of incidense
      real        rea2_baz_res(max_data/2)    ! back azimuth residual
      real        rea2_res(max_data/2)        ! travel time residual
      real        rea2_dist(max_data/2)       ! epicentral distance
      real        rea2_az(max_data/2)         ! azimuth
      character*2 rea2_location(max_data/2)   ! location code
      character*2 rea2_network(max_data/2)    ! network code
      character*5 rea2_agency(max_data/2)     ! agency of phase
      character*5 rea2_operator(max_data/2)   ! operator of phase
      character*20 rea2_auto(max_data/2)      ! name of auto process making par

c 
c parameters for SE 
c

c
c  next not needed, left to keep common block the same
c
      character*8 rea2_phase_cal(max_data/2)  ! calculated phase name
      real        rea2_wt(max_data/2)         ! weight used 
      real        hyp2_dx,hyp2_dy,hyp2_dz,hyp2_do! change in location
      real        rea2_time_obs(max_data/2)   ! observed travel time
      real        rea2_time_cal(max_data/2)   ! calculated travel time
      character*2 rea2_mag_type(max_data/2)   ! type of magnitude     
      real        rea2_mag(max_data/2)        ! magnitude value
      real        rea2_mag_res(max_data/2)    ! magnitude residual
      real        rea2_baz_wt(max_data/2)     ! azimuth weight
      integer     rea2_baz_di(max_data/2)     ! azimuth importance
      integer     rea2_di(max_data/2)         ! di (importance) of phase

c
c   spectral parameters, time is give in parameter above, spectra
c   have phase name SPECP or SPECS and are thus considered phases
c   although not written as such in s-file. the averge spectral values are
c   not considered a phase
c
      real        rea2_moment(max_data/2)     ! log moment, Nm
      real        rea2_spec_mw(max_data/2)    ! mw from each spec, different form mw from header lines
      real        rea2_sdrop(max_data/2)      ! stress drop, bar
      real        rea2_omega0(max_data/2)     ! log spectral flat level, ns
      real        rea2_cornerf(max_data/2)    ! corner f
      real        rea2_radius(max_data/2)     ! source radius
      real        rea2_swin(max_data/2)       ! window lenght used
      real        rea2_vs(max_data/2)         ! s-velocity at source, km/s
      real        rea2_vp(max_data/2)         ! p-velocity at source, km/s
      character*1 rea2_spec_phase(max_data/2) ! phase of spectrume, P or S
      real        rea2_q0(max_data/2)         ! q0
      real        rea2_qalpha(max_data/2)     ! q alpha
      real        rea2_q_below_1hz(max_data/2)! for q function below 1 hz
      real        rea2_kappa(max_data/2)      ! kappa
      real        rea2_density(max_data/2)    ! density g/cm**3
      real        rea2_slope(max_data/2)      ! - measured slope of spectrum
      real        rea2_geo_dist(max_data/2)   ! geo distance
c
c   same as above, the but averages, only one value
c
      real        rea2_av_moment              ! log moment, Nm
      real        rea2_av_sdrop               ! stress drop, bar
      real        rea2_av_omega0              ! log spectral flat level, ns
      real        rea2_av_cornerf             ! corner f
      real        rea2_av_radius              ! source radius
      real        rea2_av_swin                ! window lenght used
      real        rea2_av_mw                  ! moment mag
      real        rea2_av_slope               ! slope

c
c   same as above, the but SD, only one value
c
      real        rea2_sd_moment              ! log moment, Nm
      real        rea2_sd_sdrop               ! stress drop, bar
      real        rea2_sd_omega0              ! log spectral flat level, ns
      real        rea2_sd_cornerf             ! corner f
      real        rea2_sd_radius              ! source radius
      real        rea2_sd_swin                ! window lenght used
      real        rea2_sd_mw                  ! moment mag
      real        rea2_sd_slope               ! slope
c
c   magnitudes, not used, but kept for same common block
c
      real        rea2_mc(max_data/2)         ! coda
      real        rea2_ml(max_data/2)         ! local
      real        rea2_mb(max_data/2)         ! mb
      real        rea2_ms(max_data/2)         ! ms
      real        rea2_mw(max_data/2)         ! mw



c
c 
      common /hyp2_1/hyp2_model,hyp2_dist_id,hyp2_type,hyp2_fix_org,
     *             hyp2_depth_flag,hyp2_epi_flag,hyp2_mag_type,
     *             hyp2_high_accuracy,hyp2_error,hyp2_mag_type_all,
     *             mt2_coor
      common /hyp2_4/hyp2_year,hyp2_month,hyp2_day,hyp2_hour,
     *             hyp2_min,hyp2_sec,hyp2_lat,hyp2_lon,hyp2_depth,
     *             hyp2_nstat,hyp2_rms,hyp2_mag,hyp2_mag_all,
     *             hyp2_gap,hyp2_sec_err,hyp2_lat_err,hyp2_lon_err,
     *             hyp2_depth_err,hyp2_cov,hyp2_auto,hyp2_mag_all_main
      common /hyp2_5/hyp2_agency,hyp2_mag_agency,hyp2_mag_agency_all
      common /mt2_1/mt2_nmt,mt2_year,mt2_month,mt2_day,mt2_hour,
     *             mt2_min,mt2_sec,mt2_moment,mt2_val,mt2_exp,
     *             mt2_lat,mt2_lon,mt2_depth,mt2_agency,mt2_method,
     *             mt2_quality,mt2_mag,mt2_mag_type
c      common /hyp2_6/data5   ! cannot be defined twice
c
c 
      common /rea2_1/rea2_weight_in,rea2_onset,rea2_polarity
      common /rea2_2/rea2_co,rea2_weight_out,rea2_location,rea2_network
      common /rea2_3/rea2_action,rea2_com
      common /rea2_4/rea2_comp,rea2_hour,rea2_min,rea2_sec,rea2_coda,
     *             rea2_amp,rea2_per,rea2_baz_obs,rea2_baz_cal,
     *             rea2_vel,rea2_ain,rea2_baz_res,rea2_dist,rea2_az,
     *             rea2_nstat,rea2_nphase,rea2_res,rea2_year,rea2_month,
     *             rea2_day,rea2_moment,rea2_nmag,
     *             rea2_sdrop,rea2_omega0,rea2_cornerf,rea2_radius,
     *             rea2_swin,rea2_vs,rea2_vp,rea2_q_below_1hz,
     *             rea2_q0,rea2_qalpha,rea2_kappa,rea2_density,
     *             rea2_slope,
     *             rea2_mc,rea2_ml,rea2_mb,rea2_ms,rea2_mw,
     *             rea2_geo_dist,
     *             rea2_nhead,rea2_nrecord,rea2_nspec,rea2_nhyp,
     *             rea2_id_line,rea2_nmacro,rea2_nwav,rea2_nfault,
     *             rea2_ncomment,
     *             rea2_macro,rea2_wav,rea2_fault,rea2_comment,
     *             rea2_av_moment,rea2_av_sdrop,rea2_av_omega0,
     *             rea2_av_cornerf,rea2_av_radius,rea2_av_swin,
     *             rea2_av_mw,rea2_av_slope,rea2_auto,rea2_err_unit,
     *             rea2_read_err,rea2_write_err,rea2_locality,rea2_mag
 
      common /more_rea2_4/rea2_picture,rea2_npicture
c
c
      common /rea2_5/rea2_stat,rea2_agency,rea2_operator
      common /rea2_8/rea2_phase,rea2_abs_time
      common /rea2_9/rea2_phase_cal,rea2_mag_type,
     *             rea2_time_obs,rea2_time_cal,
     *             rea2_mag_res,rea2_wt,
     *             rea2_baz_wt,
     *             hyp2_dx,hyp2_dy,hyp2_dz,hyp2_do,rea2_di,rea2_baz_di


      common /rea2_mes/rea2_message,rea2_n_message

      common /rea2_new_format/rea2_new_in,rea2_new_out,
     *             rea2_old_format_required,     
     *             rea2_sd_moment,rea2_sd_sdrop,rea2_sd_omega0,
     *             rea2_sd_cornerf,rea2_sd_radius,rea2_sd_swin,
     *             rea2_sd_mw,rea2_sd_slope,rea2_spec_phase,
     *             rea2_spec_mw,rea2_id_line_number,
     *             rea2_read_to_sfile_end

  


