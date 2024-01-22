c
c   subroutines to copy rea to rea2,  rea2 to rea or merge rea2 into rea
c
c   all parameters are going through common block in rea.inc and rea2.inc
c
c   the intention is that all oprations follow the order of the include file
c   to make it easier to check
c   
c   jh may 2020
c
c   updates:

c   subroutines:
c
c      subroutine rea_copy_rea_to_rea2               copy rea to rea2
c      subroutine rea2_phase_clear(iphas)            clear rea2 phases
c      subroutine rea2_hyp_clear(ihyp)               clear rea2 hyp
c      subroutine rea2_mt2_clear(k)                  clear rea2 mt
c      subroutine rea_copy_hyp_to_hyp2(i)            copy hyp to hyp2
c      subroutine rea_copy_phase_rea_to_rea2(i)      copy phases to rea2
c      subroutine rea_copy_rea2_to_rea               copy rea2 to rea
c      subroutine rea_copy_phase_rea2_to_rea(i,j)    copy phases from rea2 to rea
c      subroutine rea_copy_hyp2_to_hyp(i,j)          copy hyp2 to hyp
c      subroutine rea_merge_rea2_to_rea              merge rea2 into rea


      subroutine rea_copy_rea_to_rea2
c
c   copy all parameters from rea to rea2
c
      implicit none
      include 'seidim.inc'
      include 'rea.inc'
      include 'rea2.inc'
      integer i,k
c
c  fixed parameters
c
      rea2_nstat=rea_nstat                  ! number of stations
      rea2_nphase=rea_nphase                ! number of phases lines
      rea2_nhead=rea_nhead                  ! number of header lines
      rea2_nrecord=rea_nrecord              ! number of records
      rea2_nspec=rea_nspec                  ! number of spectra 
      rea2_nhyp=rea_nhyp                    ! number of hypocenters
      rea2_nmag=rea_nmag                    ! number of magnitudes
      rea2_nmacro=rea_nmacro                ! number macroseismic lines
      rea2_nwav=rea_nwav                    ! number of waveform files
      rea2_ncomment=rea_ncomment            ! number of comment lines
      rea2_nfault=rea_nfault                ! number of fault plane solutions

      do i=1,rea_nmacro
         rea2_macro(i)=rea_macro(i)
      enddo    

      do i=1,rea_nwav
         rea2_wav(i)=rea_wav(i)             ! waveform file names lines
      enddo

      do i=1,rea_nfault
         rea2_fault(i)=rea_fault(i)         ! fault plane solutions lines
      enddo

      do i=1,rea_ncomment
         rea2_comment(i)=rea_comment(i)     ! comment lines
      enddo

      do i=1,rea_npicture
         rea2_picture(i)=rea_picture(i)     ! picture lines
      enddo

      rea2_npicture=rea_npicture            ! number of picture lines
      rea2_id_line=rea_id_line              ! event id line
      rea2_id_line_number=rea_id_line_number ! position in data for id line

c      rea_err_unit   not copied, only used in rea
c      rea_read_err   not copied, ----------------
c      rea_write_err  not copied, ----------------


      rea2_action=rea_action                ! action parameter from id line
      rea2_locality=rea_locality            ! locality

c
c   following 6 only used in rea
c
c      rea_message(1000)
c      rea_nmessage
c      rea_new_in                 ! if true, new format in
c      rea_new_out                ! if true, new format out
c      rea_old_format_required    ! if true, force old format in data array
c      rea_read_to_sfile_end      ! if -1, read to end in indata

c
c   hypocenters
c
      do i=1,rea_nhyp
        call rea_copy_hyp_to_hyp2(i)
      enddo

c
c   all mags
c
      do i=1,rea_nmag
         hyp2_mag_all(i)=hyp_mag_all(i)
         hyp2_mag_type_all(i)=hyp_mag_type_all(i)
         hyp2_mag_agency_all(i)=hyp_mag_agency_all(i)
         hyp2_mag_all_main(i)=hyp_mag_all_main(i)
      enddo
c
c  mt
c
      mt2_nmt=mt_nmt                        ! Number of Moment tensor solutions

      do i=1,mt_nmt
         mt2_year(i)=mt_year(i)             ! Moment tensor hypocenter year
         mt2_month(i)=mt_month(i)
         mt2_day(i)=mt_day(i)
         mt2_hour(i)=mt_hour(i)
         mt2_min(i)=mt_min(i)
         mt2_sec(i)=mt_sec(i)
         mt2_moment(i)=mt_moment(i)          ! Scalar moment
         mt2_coor(i)=mt_coor(i)              ! Moment tensor coordinate system Spherical or Cartesian
c        
         do k=1,6                            ! S=Spherical and C=Cartesian, see Aki y Richard 1980 p 118
            mt2_val(k,i)=mt_val(k,i)         ! Moment tensor values
         enddo

         mt2_exp(i)=mt_exp(i)                ! Moment tensor exponential
         mt2_lat(i)=mt_lat(i)                ! Moment tensor latitude
         mt2_lon(i)=mt_lon(i)                ! Moment tensor longitude
         mt2_depth(i)=mt_depth(i)            ! Moment tensor depth
         mt2_agency(i)=mt_agency(i)          ! Moment tensor hypocenter agency, use 3 only
         mt2_method(i)=mt_method(i)          ! Moment tensor method
         mt2_quality(i)=mt_quality(i)        ! Moment tensor quality
         mt2_mag(i)=mt_mag(i)                ! Moment tensor magnitudes
         mt2_mag_type(i)=mt_mag_type(i)      ! Moment tensor magnitude types
      enddo
c
c   phases, includes spectra but not average spectrum
c     
      do i=1,rea_nphase
         call rea_copy_phase_rea_to_rea2(i)
      enddo

c
c   spectral averages
c
      rea2_av_moment=rea_av_moment             ! log moment, Nm
      rea2_av_sdrop=rea_av_sdrop               ! stress drop, bar
      rea2_av_omega0=rea_av_omega0             ! log spectral flat level, ns
      rea2_av_cornerf=rea_av_cornerf           ! corner f
      rea2_av_radius=rea_av_radius             ! source radius
      rea2_av_swin=rea_av_swin                 ! window lenght used
      rea2_av_mw=rea_av_mw                     ! moment mag
      rea2_av_slope=rea_av_slope               ! slope

c
c   same as above, the but SD, only one value
c
      rea2_sd_moment=rea_sd_moment             ! log moment, Nm
      rea2_sd_sdrop=rea_sd_sdrop               ! stress drop, bar
      rea2_sd_omega0=rea_sd_omega0             ! log spectral flat level, ns
      rea2_sd_cornerf=rea_sd_cornerf           ! corner f
      rea2_sd_radius=rea_sd_radius             ! source radius
      rea2_sd_swin=rea_sd_swin                 ! window lenght used
      rea2_sd_mw=rea_sd_mw                     ! moment mag
      rea2_sd_slope=rea_sd_slope               ! slope

c
c   se, here since not a phase so not in phase copy routine
c        
       hyp2_dx=hyp_dx
       hyp2_dy=hyp_dy
       hyp2_dz=hyp_dz
       hyp2_do=hyp_do


      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine rea2_phase_clear(iphas)                                         
c                                                                               
c   initialize phase line parameters for one entry at index iphas,              
c   character items are set to blanks and numbers to -999                       
c   jh may 2020                                                              
c                                                                               
      implicit none                                                             
      include 'seidim.inc'                                                      
      include 'rea2.inc'                                                         
      integer iphas                                                             
c                                                                               
      rea2_stat(iphas)=' '        ! station codes                                
      rea2_comp(iphas)=' '        ! componenets                                  
      rea2_co(iphas)= ' '         ! 2 letter componenets 
      rea2_com(iphas)=' '         ! 3 letter component
      rea2_phase(iphas)= ' '      ! phase name                        
      rea2_onset(iphas)=  ' '     ! onset I or E or blank                        
      rea2_weight_in(iphas)= ' '  ! input weight                                 
      rea2_weight_out(iphas)=' '  ! weight out                                   
      rea2_polarity(iphas)=' '    ! polarity, D or C                        
      rea2_year(iphas)=-999                                                      
      rea2_month(iphas)=-999                                                     
      rea2_day(iphas)=-999                                                       
      rea2_hour(iphas)= -999                                                     
      rea2_min(iphas)= -999                                                      
      rea2_sec(iphas)= -999.0                                                      
      rea2_abs_time(iphas)=-999.0 ! abs phase time                               
      rea2_coda(iphas)= -999.0    ! coda length in s                             
      rea2_amp(iphas)=  -999.0    ! amplitude in nm                              
      rea2_per(iphas)=  -999.0    ! period of amplitude
      rea2_baz_obs(iphas)= -999.0 ! observed back azimuth                        
      rea2_baz_cal(iphas)= -999.0 ! calculated back azimuth                        
      rea2_vel(iphas)= -999.0     ! observed apparent velocity                   
      rea2_ain(iphas)=  -999.0    ! calcualated angle of incidence               
      rea2_baz_res(iphas)= -999.0 ! back azimuth residual                        
      rea2_res(iphas)=-999.0      ! travel time residual                         
      rea2_dist(iphas)= -999.0    ! epicentral distance                          
      rea2_az(iphas)=   -999.0    ! azimuth 
      rea2_location(iphas)=' '    ! location code 
      rea2_network(iphas)=' '     ! network code  
      rea2_agency(iphas)=' '      ! agency  
      rea2_operator(iphas)=' '    ! operator 
      rea2_auto(iphas)=' '         

c for se                          
  
      rea2_mag(iphas)=  -999.0    ! magnitude   
      rea2_mag_res(iphas)=-999.0  ! --- residual
      rea2_phase_cal(iphas)=' '   ! calculated phase name
      rea2_wt(iphas)=-999.0       ! weight used 
c     rea2_dx etc not initilized
      rea2_time_obs(iphas)=-999.0 ! observed travel time
      rea2_time_cal(iphas)=-999.0 ! calculated travel time
      rea2_mag_type(iphas)=' '    ! type of magnitude     
      rea2_mag(iphas)=999.0       ! magnitude value
      rea2_mag_res(iphas)=-999.0  ! residual
      rea2_baz_wt(iphas)= -999.0  ! azimuth weight
      rea2_baz_di(iphas)=-999     ! azimuth importance
      rea2_di(iphas)=-999         ! di (importance) of phase     

                                       
c   spectral                                                                              
                                                                                
      rea2_moment(iphas)=-999.0   ! log moment, Nm
      rea2_spec_mw(iphas)=-999.0  ! mw from spectrum                               
      rea2_sdrop(iphas)=-999.0    ! stress drop, bar                             
      rea2_omega0(iphas)=-999.0   ! log spectral flat level, ns                  
      rea2_cornerf(iphas)=-999.0  ! corner f                                     
      rea2_radius(iphas)=-999.0   ! source radius                                
      rea2_swin(iphas)=-999.0     ! window lenght used                                                   
      rea2_vs(iphas)=-999.0       ! s-velocity at source, km/s                   
      rea2_vp(iphas)=-999.0       ! p-velocity at source, km/s   
      rea2_spec_phase(iphas)=' '  ! type of phase                
      rea2_q0(iphas)=-999.0       ! q0                                           
      rea2_qalpha(iphas)=-999.0   ! q alpha                                      
      rea2_q_below_1hz(iphas)=-999.0 ! parameter for q function below 1 hz         
      rea2_kappa(iphas)=-999.0    ! kappa                                        
      rea2_density(iphas)=-999.0  ! density g/cm**3                              
      rea2_slope(iphas)=-999.0    ! measured slope of spectrum 
      rea2_geo_dist(iphas)=-999.0 ! geo distance, km      
c
              
      rea2_mc(iphas)=-999.0       ! coda                                         
      rea2_ml(iphas)=-999.0       ! local                                        
      rea2_mb(iphas)=-999.0       ! mb                                           
      rea2_ms(iphas)=-999.0       ! ms                                           
      rea2_mw(iphas)=-999.0       ! mw                                             

      return                                                                    
      end       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc   
 
      subroutine rea2_hyp_clear(ihyp)                                            
c                                                                               
c   initialize hyp1 line parameters for one entry at index ihyp,                
c   character items are set to blanks and numbers to -999 excepth               
c   covariace element which is set to -9.9e10                                   
c                                                                               
c   jh may 2020                                                              
c                                                                               
      implicit none                                                             
      include 'seidim.inc'                                                      
      include 'rea2.inc'                                                         
      integer ihyp,i                                                            
                                                                                
      hyp2_year(ihyp)=-999              ! hypocenter year                        
      hyp2_month(ihyp)=-999                                                      
      hyp2_day(ihyp)=-999                                                        
      hyp2_hour(ihyp)=-999                                                       
      hyp2_min(ihyp)=-999                                                        
      hyp2_sec(ihyp)=-999.0                                                      
      hyp2_model(ihyp)=' '             ! model indicator                         
      hyp2_dist_id(ihyp)=' '           ! distance indicator                      
      hyp2_type(ihyp)=' '              ! event type like E                       
      hyp2_fix_org(ihyp)=' '           ! fix origin time flag                    
      hyp2_lat(ihyp)=-999.0            ! latitude                                
      hyp2_lon(ihyp)=-999.0            ! longitude                               
      hyp2_depth(ihyp)=-999.0          ! depth                                   
      hyp2_depth_flag(ihyp)=' '        ! depth flag                              
      hyp2_epi_flag(ihyp)=' '          ! epicenter flag                          
      hyp2_agency(ihyp)=' '            ! hypocenter agency, use 3 only           
      hyp2_nstat(ihyp)=-999            ! number of station                       
      hyp2_rms(ihyp)=-999.0            ! rms of hypocenter solution              
      do i=1,6                                                                  
         hyp2_mag(i,ihyp)=-999.0       ! magnitudes                              
         hyp2_mag_type(i,ihyp)=' '     ! magnitude types                         
         hyp2_mag_agency(i,ihyp)=' '   ! magnitude agencies                      
      enddo                                                                     
      do i=1,200                                                                
         hyp2_mag_all(i)=-999.0       ! magnitudes                               
         hyp2_mag_type_all(i)=' '     ! magnitude types                          
         hyp2_mag_agency_all(i)=' '   ! magnitude agencies 
         hyp2_mag_all_main(i)=-999.0                      
      enddo                         
                                            
      hyp2_high_accuracy(ihyp)=.false. ! high accurcy flag                       
      hyp2_error(ihyp)=.false.         ! error flag 
      hyp2_auto(ihyp)=' '              ! name of automatic process                                
      hyp2_gap(ihyp)=-999.0            ! gap   
                                 
      hyp2_sec_err(ihyp)=-999.0        ! origin time error                       
      hyp2_lat_err(ihyp)=-999.0        ! hypocenter errors                       
      hyp2_lon_err(ihyp)=-999.0                                                  
      hyp2_depth_err(ihyp)=-999.0                                                
      do i=1,3                                                                  
         hyp2_cov(i,ihyp)= -9.9e10     ! covarieance elements                    
      enddo                                                                                
                                                                                
      return                                                                    
      end              

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc    

      subroutine rea2_mt2_clear(k)                                            
c                                                                               
c   initialize mt variables for index k            
c                                                                               
c   jh march 2020                                                               
c                                                                               
      implicit none                                                             
      include 'seidim.inc'                                                      
      include 'rea2.inc'                                                         
      integer i,k    

      mt2_year(k)=-999              
      mt2_month(k)=-999
      mt2_day(k)=-999
      mt2_hour(k)=-999
      mt2_min(k)=-999
      mt2_sec(k)=-999
      mt2_moment(k)=-999
      mt2_coor(k)=' '  
      do i=1,6           
         mt2_val(i,k)=-999
      enddo
      mt2_exp(k)=-999              
      mt2_lat(k)=-999          
      mt2_lon(k)=-999        
      mt2_depth(k)=-999         
      mt2_agency(k)=' '           
      mt2_method(k)=' '           
      mt2_quality(k)=' '                
      mt2_mag(k)=-999             
      mt2_mag_type(k)=' '          
                                                                                                                                         
                                                                                
      return                                                                    
      end                                                                     
                                                                                
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc    
                                                                          
      subroutine rea_copy_hyp_to_hyp2(i)
c
c   copy hyp to hyp2 for index i
c 
      implicit none
      include 'seidim.inc'
      include 'rea.inc'
      include 'rea2.inc'

      integer i,k

      call rea2_hyp_clear(i)  
c
      hyp2_year(i)=hyp_year(i)             
      hyp2_month(i)=hyp_month(i)
      hyp2_day(i)=hyp_day(i)
      hyp2_hour(i)=hyp_hour(i)
      hyp2_min(i)=hyp_min(i)
      hyp2_sec(i)=hyp_sec(i)
      hyp2_model(i)=hyp_model(i)
      hyp2_dist_id(i)=hyp_dist_id(i)           
      hyp2_type(i)=hyp_type(i)              
      hyp2_fix_org(i)=hyp_fix_org(i)           
      hyp2_lat (i)=hyp_lat(i)              
      hyp2_lon(i)=hyp_lon(i)              
      hyp2_depth(i)=hyp_depth(i)             
      hyp2_depth_flag(i)=hyp_depth_flag(i)        
      hyp2_epi_flag(i)=hyp_epi_flag(i)          
      hyp2_agency(i)=hyp_agency(i)           
      hyp2_nstat(i)=hyp_nstat(i)            
      hyp2_rms(i)=hyp_rms(i) 
      do k=1,6            
         hyp2_mag(k,i)=hyp_mag(k,i)
         hyp2_mag_type(k,i)=hyp_mag_type(k,i)
         hyp2_mag_agency(k,i)=hyp_mag_agency(k,i)
      enddo

      hyp2_high_accuracy(i)=hyp_high_accuracy(i)       ! high accurcy flag
      hyp2_error(i)=hyp_error(i)   
      hyp2_auto(i)=hyp_auto(i)
c
c
c   hypocenter errors
c
      hyp2_gap(i)=hyp_gap(i) 
      hyp2_sec_err(i)=hyp_sec_err(i)
      hyp2_lat_err(i)=hyp_lat_err(i)        
      hyp2_lon_err(i)=hyp_lon_err(i)         
      hyp2_depth_err(i)=hyp_depth_err(i)      
      hyp2_cov(1,i)=hyp_cov(1,i)   
      hyp2_cov(2,i)=hyp_cov(2,i) 
      hyp2_cov(3,i)=hyp_cov(3,i)

          
      return
      end      

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine rea_copy_phase_rea_to_rea2(i)
c
c   copy phase i from rea to rea2
c
      implicit none
      include 'seidim.inc'
      include 'rea.inc'
      include 'rea2.inc'

      integer option,i

      call rea2_phase_clear(i)   
c
c   input parameters
c
      
      rea2_stat(i)=rea_stat(i)
      rea2_com(i)=rea_com(i)
      rea2_comp(i)=rea_comp(i)
      rea2_co(i)=rea_co(i)
      rea2_phase(i)= rea_phase(i)
      rea2_onset(i)=rea_onset(i)
      rea2_weight_in(i)=rea_weight_in(i)
      rea2_weight_out(i)=rea_weight_out(i)
      rea2_polarity(i)=rea_polarity(i)
      rea2_year(i)=rea_year(i)
      rea2_month(i)=rea_month(i)
      rea2_day(i)=rea_day(i)
      rea2_hour(i)=rea_hour(i)
      rea2_min(i)=rea_min(i)
      rea2_sec(i)=rea_sec(i)
      rea2_abs_time(i)=rea_abs_time(i)
      rea2_coda(i)=rea_coda(i)
      rea2_amp(i)=rea_amp(i)
      rea2_per(i)=rea_per(i)
      rea2_baz_obs(i)=rea_baz_obs(i)
      rea2_baz_cal(i)=rea_baz_cal(i)
      rea2_vel(i)=rea_vel(i)
      rea2_ain(i)=rea_ain(i)
      rea2_baz_res(i)=rea_baz_res(i)
      rea2_res(i)=rea_res(i)
      rea2_dist(i)=rea_dist(i)
      rea2_az(i)=rea_az(i)
      rea2_location(i)=rea_location(i)
      rea2_network(i)=rea_network(i)
      rea2_agency(i)=rea_agency(i)
      rea2_operator(i)=rea2_operator(i)
      rea2_auto(i)=rea_auto(i)

c
c    se parameters
c
      rea2_phase_cal(i)=rea2_phase_cal(i)  ! calculated phase name
      rea2_wt(i)=rea2_wt(i)                ! weight used 
c     rea2_dx etc not a phase so in main copy routine
      rea2_time_obs(i)=rea_time_obs(i)     ! observed travel time
      rea2_time_cal(i)=rea_time_cal(i)     ! calculated travel time
      rea2_mag_type(i)=rea_mag_type(i)     ! type of magnitude     
      rea2_mag(i)=rea_mag(i)               ! magnitude value
      rea2_mag_res(i)=rea_mag_res(i)       ! residual
      rea2_baz_wt(i)=rea_baz_wt(i)         ! azimuth weight
      rea2_baz_di(i)=rea_baz_di(i)         ! azimuth importance
      rea2_di(i)=rea_di(i)                 ! di (importance) of phase     

c
c   spectral parameters
c
      rea2_moment(i)=rea_moment(i)      ! log moment, Nm
      rea2_spec_mw(i)=rea_spec_mw(i)    ! mw from each spec, different form mw from header lines
      rea2_sdrop(i)=rea_sdrop(i)        ! stress drop, bar
      rea2_omega0(i)=rea_omega0(i)      ! log spectral flat level, ns
      rea2_cornerf(i)=rea_cornerf(i)    ! corner f
      rea2_radius(i)=rea_radius(i)      ! source radius
      rea2_swin(i)=rea_swin(i)          ! window lenght used
      rea2_vs(i)=rea_vs(i)              ! s-velocity at source, km/s
      rea2_vp(i)=rea_vp(i)              ! p-velocity at source, km/s
      rea2_spec_phase(i)=rea_spec_phase(i) ! phase of spectrume, P or S
      rea2_q0(i)=rea_q0(i)              ! q0
      rea2_qalpha(i)=rea_qalpha(i)      ! q alpha
      rea2_q_below_1hz(i)=rea_q_below_1hz(i)! for q function below 1 hz
      rea2_kappa(i)=rea_kappa(i)        ! kappa
      rea2_density(i)=rea_density(i)    ! density g/cm**3
      rea2_slope(i)=rea_slope(i)        ! measured slope of spectrum
      rea2_geo_dist(i)=rea_geo_dist(i)  ! geo distance

c
c   spectral averages are in main routine
c

c
c   magnitudes, probably never used
c
      rea2_mc(i)=rea_mc(i)              ! coda
      rea2_ml(i)=rea_ml(i)              ! local
      rea2_mb(i)=rea_mb(i)              ! mb
      rea2_ms(i)=rea_ms(i)              ! ms
      rea2_mw(i)=rea_mw(i)              ! mw


      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine rea_copy_rea2_to_rea
c
c   copy all parameters from rea2 to rea
c
      implicit none
      include 'seidim.inc'
      include 'rea.inc'
      include 'rea2.inc'
      integer i,j,k
c
c  fixed parameters
c
      rea_nstat=rea2_nstat                  ! number of stations
      rea_nphase=rea2_nphase                ! number of phases lines
      rea_nhead=rea2_nhead                  ! number of header lines
      rea_nrecord=rea2_nrecord              ! number of records
      rea_nspec=rea2_nspec                  ! number of spectra 
      rea_nhyp=rea2_nhyp                    ! number of hypocenters
      rea_nmag=rea2_nmag                    ! number of magnitudes
      rea_nmacro=rea2_nmacro                ! number macroseismic lines
      rea_nwav=rea2_nwav                    ! number of waveform files
      rea_ncomment=rea2_ncomment            ! number of comment lines
      rea_nfault=rea2_nfault                ! number of fault plane solutions

      do i=1,rea2_nmacro
         rea_macro(i)=rea2_macro(i)
      enddo

      do i=1,rea2_nwav
         rea_wav(i)=rea2_wav(i)             ! waveform file names lines
      enddo
      
      do i=1,rea2_nfault
         rea_fault(i)=rea2_fault(i)         ! fault plane solutions lines
      enddo

      do i=1,rea2_ncomment
         rea_comment(i)=rea2_comment(i)     ! comment lines
      enddo

      do i=1,rea2_npicture
         rea_picture(i)=rea2_picture(i)     ! picture lines
      enddo

      rea_npicture=rea2_npicture            ! number of picture lines
      rea_id_line=rea2_id_line              ! event id line
      rea_id_line_number=rea2_id_line_number ! position in data for id line

c     rea_err_unit      ! only in rea
c     rea_read_err
c     rea_write_err

      rea_action=rea2_action                ! action parameter from id line
      rea_locality=rea2_locality            ! locality 
 
      rea_action=rea2_action
      rea_locality=rea2_locality
c
c   next only used in rea
c
c     rea_message
c     rea_n_message
c     rea_new_in
c     rea_new_out 
c     rea_old_format_required
c     rea_read_to_sfile_end   
c
c   hypocenters
c
      do i=1,rea2_nhyp
        call rea_copy_hyp2_to_hyp(i,i)
      enddo

c
c  mt
c
      mt_nmt=mt2_nmt                        ! Number of Moment tensor solutions

      do i=1,mt2_nmt
         mt_year(i)=mt2_year(i)             ! Moment tensor hypocenter year
         mt_month(i)=mt2_month(i)
         mt_day(i)=mt2_day(i)
         mt_hour(i)=mt2_hour(i)
         mt_min(i)=mt2_min(i)
         mt_sec(i)=mt2_sec(i)
         mt_moment(i)=mt2_moment(i)          ! Scalar moment
         mt_coor(i)=mt2_coor(i)              ! Moment tensor coordinate system Spherical or Cartesian
c        
         do k=1,6                            ! S=Spherical and C=Cartesian, see Aki y Richard 1980 p 118
            mt_val(k,i)=mt2_val(k,i)         ! Moment tensor values
         enddo

         mt_exp(i)=mt2_exp(i)                ! Moment tensor exponential
         mt_lat(i)=mt2_lat(i)                ! Moment tensor latitude
         mt_lon(i)=mt2_lon(i)                ! Moment tensor longitude
         mt_depth(i)=mt2_depth(i)            ! Moment tensor depth
         mt_agency(i)=mt2_agency(i)          ! Moment tensor hypocenter agency, use 3 only
         mt_method(i)=mt2_method(i)          ! Moment tensor method
         mt_quality(i)=mt2_quality(i)        ! Moment tensor quality
         mt_mag(i)=mt2_mag(i)                ! Moment tensor magnitudes
         mt_mag_type(i)=mt2_mag_type(i)      ! Moment tensor magnitude types
      enddo

c
c   phases
c     
      do i=1,rea2_nphase
         call rea_copy_phase_rea2_to_rea(i,i)
      enddo

c
c   spectral averages
c
      rea_av_moment=rea2_av_moment             ! log moment, Nm
      rea_av_sdrop=rea2_av_sdrop               ! stress drop, bar
      rea_av_omega0=rea2_av_omega0             ! log spectral flat level, ns
      rea_av_cornerf=rea2_av_cornerf           ! corner f
      rea_av_radius=rea2_av_radius             ! source radius
      rea_av_swin=rea2_av_swin                 ! window lenght used
      rea_av_mw=rea2_av_mw                     ! moment mag
      rea_av_slope=rea2_av_slope               ! slope

c
c   same as above, the but SD, only one value
c
      rea_sd_moment=rea2_sd_moment             ! log moment, Nm
      rea_sd_sdrop=rea2_sd_sdrop               ! stress drop, bar
      rea_sd_omega0=rea2_sd_omega0             ! log spectral flat level, ns
      rea_sd_cornerf=rea2_sd_cornerf           ! corner f
      rea_sd_radius=rea2_sd_radius             ! source radius
      rea_sd_swin=rea2_sd_swin                 ! window lenght used
      rea_sd_mw=rea2_sd_mw                     ! moment mag
      rea_sd_slope=rea2_sd_slope               ! slope

c
c   se, not a phase so not in phase copy routine
c        
       hyp_dx=hyp2_dx
       hyp_dy=hyp2_dy
       hyp_dz=hyp2_dz
       hyp_do=hyp2_do
c
c   all mags
c
      do i=1,rea2_nmag
         hyp_mag_all(i)=hyp2_mag_all(i)
         hyp_mag_type_all(i)=hyp2_mag_type_all(i)
         hyp_mag_agency_all(i)=hyp2_mag_agency_all(i)
         hyp_mag_all_main(i)=hyp2_mag_all_main(i)
      enddo

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine rea_copy_phase_rea2_to_rea(i,j)
c
c   copy phase i from rea2 to rea j
c
      implicit none
      include 'seidim.inc'
      include 'rea.inc'
      include 'rea2.inc'

      integer option,i,j

      call rea_phase_clear(j)  
c
c   input parameters
c
      
      rea_stat(j)=rea2_stat(i)
      rea_com(j)=rea2_com(i)
      rea_comp(j)=rea2_comp(i)
      rea_co(j)=rea2_co(i)
      rea_phase(j)= rea2_phase(i)
      rea_onset(j)=rea2_onset(i)
      rea_weight_in(j)=rea2_weight_in(i)
      rea_weight_out(j)=rea2_weight_out(i)
      rea_polarity(j)=rea2_polarity(i)
      rea_year(j)=rea2_year(i)
      rea_month(j)=rea2_month(i)
      rea_day(j)=rea2_day(i)
      rea_hour(j)=rea2_hour(i)
      rea_min(j)=rea2_min(i)
      rea_sec(j)=rea2_sec(i)
      rea_abs_time(j)=rea2_abs_time(i)
      rea_coda(j)=rea2_coda(i)
      rea_amp(j)=rea2_amp(i)
      rea_per(j)=rea2_per(i)
      rea_baz_obs(j)=rea2_baz_obs(i)
      rea_baz_cal(j)=rea2_baz_cal(i)
      rea_vel(j)=rea2_vel(i)
      rea_ain(j)=rea2_ain(i)
      rea_baz_res(j)=rea2_baz_res(i)
      rea_res(j)=rea2_res(i)
      rea_dist(j)=rea2_dist(i)
      rea_az(j)=rea2_az(i)
      rea_location(j)=rea2_location(i)
      rea_network(j)=rea2_network(i)
      rea_agency(j)=rea2_agency(i)
      rea_operator(j)=rea2_operator(i)
      rea_auto(j)=rea2_auto(i)           
c
c    se parameters
c
      rea_phase_cal(j)=rea2_phase_cal(i)   ! calculated phase name
      rea_wt(j)=rea2_wt(i)                 ! weight used 
c     dx,dy etc in main
      rea_time_obs(j)=rea2_time_obs(i)     ! observed travel time
      rea_time_cal(j)=rea2_time_cal(i)     ! calculated travel time
      rea_mag_type(j)=rea2_mag_type(i)     ! type of magnitude     
      rea_mag(j)=rea2_mag(i)               ! magnitude value
      rea_mag_res(j)=rea2_mag_res(i)       ! magnitude residual
      rea_baz_wt(j)=rea2_baz_wt(i)         ! azimuth weight
      rea_baz_di(j)=rea2_baz_di(i)         ! azimuth importance
      rea_di(j)=rea2_di(i)                 ! di (importance) of phase
     
c
c   spectral parameters
c
      rea_moment(j)=rea2_moment(i)      ! log moment, Nm
      rea_spec_mw(j)=rea2_spec_mw(i)    ! mw from each spec, different form mw from header lines
      rea_sdrop(j)=rea2_sdrop(i)        ! stress drop, bar
      rea_omega0(j)=rea2_omega0(i)      ! log spectral flat level, ns
      rea_cornerf(j)=rea2_cornerf(i)    ! corner f
      rea_radius(j)=rea2_radius(i)      ! source radius
      rea_swin(j)=rea2_swin(i)          ! window lenght used
      rea_vs(j)=rea2_vs(i)              ! s-velocity at source, km/s
      rea_vp(j)=rea2_vp(i)              ! p-velocity at source, km/s
      rea_spec_phase(j)=rea2_spec_phase(i) ! phase of spectrum, P or S
      rea_q0(j)=rea2_q0(i)              ! q0
      rea_qalpha(j)=rea2_qalpha(i)      ! q alpha
      rea_q_below_1hz(j)=rea2_q_below_1hz(i)! for q function below 1 hz
      rea_kappa(j)=rea2_kappa(i)        ! kappa
      rea_density(j)=rea2_density(i)    ! density g/cm**3
      rea_slope(j)=rea2_slope(i)        ! measured slope of spectrum
      rea_geo_dist(j)=rea2_geo_dist(i)  ! geo distance

c
c   magnitudes, probably never used
c
      rea_mc(j)=rea2_mc(i)              ! coda
      rea_ml(j)=rea2_ml(i)              ! local
      rea_mb(j)=rea2_mb(i)              ! mb
      rea_ms(j)=rea2_ms(i)              ! ms
      rea_mw(j)=rea2_mw(i)              ! mw

      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc    
                                                                          
      subroutine rea_copy_hyp2_to_hyp(i,j)
c
c   copy hyp to hyp2 for index i to j
c 
      implicit none
      include 'seidim.inc'
      include 'rea.inc'
      include 'rea2.inc'

      integer i,j,k

      call rea_hyp_clear(j)   
c

      hyp_year(j)=hyp2_year(i)             
      hyp_month(j)=hyp2_month(i)
      hyp_day(j)=hyp2_day(i)
      hyp_hour(j)=hyp2_hour(i)
      hyp_min(j)=hyp2_min(i)
      hyp_sec(j)=hyp2_sec(i)
      hyp_model(j)=hyp2_model(i)
      hyp_dist_id(j)=hyp2_dist_id(i)           
      hyp_type(j)=hyp2_type(i)              
      hyp_fix_org(j)=hyp2_fix_org(i)           
      hyp_lat (j)=hyp2_lat(i)              
      hyp_lon(j)=hyp2_lon(i)              
      hyp_depth(j)=hyp2_depth(i)             
      hyp_depth_flag(j)=hyp2_depth_flag(i)        
      hyp_epi_flag(j)=hyp2_epi_flag(i)          
      hyp_agency(j)=hyp2_agency(i)           
      hyp_nstat(j)=hyp2_nstat(i)            
      hyp_rms(j)=hyp2_rms(i) 
      do k=1,6            
         hyp_mag(k,j)=hyp2_mag(k,i)
         hyp_mag_type(k,j)=hyp2_mag_type(k,i)
         hyp_mag_agency(k,j)=hyp2_mag_agency(k,i)
      enddo

c   all mags is done in main

      hyp_high_accuracy(j)=hyp2_high_accuracy(i) 
      hyp_error(j)=hyp2_error(i) 
      hyp_auto(j)=hyp2_auto(i)
c
c   hypocenter errors
c

      hyp_gap(j)=hyp2_gap(i) 
      hyp_sec_err(j)=hyp2_sec_err(i)
      hyp_lat_err(j)=hyp2_lat_err(i)        
      hyp_lon_err(j)=hyp2_lon_err(i)         
      hyp_depth_err(j)=hyp2_depth_err(i)      
      hyp_cov(1,j)=hyp2_cov(1,i)   
      hyp_cov(2,j)=hyp2_cov(2,i) 
      hyp_cov(3,j)=hyp2_cov(3,i)
  


            
      return
      end      

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine rea_merge_rea2_to_rea
c
c   merge all parameters from rea2 to rea 
c   id line in rea2 is made to a comment
c   rea2 is put at end
c
      implicit none
      include 'seidim.inc'
      include 'rea.inc'
      include 'rea2.inc'
      integer i,k
c
c  fixed parameters
c
c
c  not used at output so do not copy, might not be just the sum
c
c      rea_nstat=rea2_nstat+rea_nstat                    ! number of stations
c      rea_nrecord=rea2_nrecord+rea_nrecord              ! number of records   
c      
c      rea_action=rea2_action           not used not copied, anywy in id line
c

c
c   normally only one locality so only copied if not there from before
c   and the locality in rea2 is given name NER-LOC
c  
c
      if(rea_locality.eq.' '.and.rea2_locality.ne.' ') then
         rea_locality=rea2_locality           
      endif
c
c   both are there so make rea2 to a comment
c
      if(rea_locality.ne.' '.and.rea2_locality.ne.' ') then
         rea_ncomment=rea_ncomment+1
         rea_comment(rea_ncomment)(12:79)=rea2_locality(1:68)
         rea_comment(rea_ncomment)(80:80)='3'
         rea_comment(rea_ncomment)(2:10)='MER-LOCA:'
      endif
         
      
      do i=1,rea2_nmacro
         rea_macro(i+rea_nmacro)=rea2_macro(i)
      enddo
      rea_nmacro=rea_nmacro+rea2_nmacro                ! number macroseismic lines

      do i=1,rea2_nwav
         rea_wav(i+rea_nwav)=rea2_wav(i)               ! waveform file names lines
      enddo
      rea_nwav=rea_nwav+rea2_nwav

      do i=1,rea2_nfault
         rea_fault(i+rea_nfault)=rea2_fault(i)         ! fault plane solutions lines
      enddo
      rea_nfault=rea_nfault+rea2_nfault

      do i=1,rea2_ncomment
         rea_comment(i+rea_ncomment)=rea2_comment(i)     ! comment lines
      enddo
      rea_ncomment=rea_ncomment+rea2_ncomment            ! number of comment lines

      do i=1,rea2_npicture
         rea_picture(i+rea_npicture)=rea2_picture(i)     ! picture lines
      enddo
      rea_npicture=rea2_npicture+rea_npicture            ! number of picture lines
c
c   make rea2 id line to a comment, if there
c

      if(rea2_id_line_number.gt.0) then
         rea_ncomment=rea_ncomment+1
         rea_comment(rea_ncomment)=rea2_id_line          ! event id line
         rea_comment(rea_ncomment)(80:80)='3'
      endif
c
c   hypocenters
c
      do i=1,rea2_nhyp
        call rea_copy_hyp2_to_hyp(i,i+rea_nhyp)
      enddo
      rea_nhyp=rea_nhyp+rea2_nhyp

c
c   phases, put at end
c     
      do i=1,rea2_nphase
         call rea_copy_phase_rea2_to_rea(i,i+rea_nphase)
      enddo
      rea_nphase=rea2_nphase+rea_nphase                ! number of phases lines

c
c   spectral averages and sd are only copied if none in rea
c
      if(rea_nspec.eq.0.and.rea_av_moment.lt.0) then
         rea_av_moment=rea2_av_moment             ! log moment, Nm
         rea_av_sdrop=rea2_av_sdrop               ! stress drop, bar
         rea_av_omega0=rea2_av_omega0             ! log spectral flat level, ns
         rea_av_cornerf=rea2_av_cornerf           ! corner f
         rea_av_radius=rea2_av_radius             ! source radius
         rea_av_swin=rea2_av_swin                 ! window lenght used
         rea_av_mw=rea2_av_mw                     ! moment mag
         rea_av_slope=rea2_av_slope               ! slope

c
c   same as above, the but SD, only one value
c
         rea_sd_moment=rea2_sd_moment             ! log moment, Nm
         rea_sd_sdrop=rea2_sd_sdrop               ! stress drop, bar
         rea_sd_omega0=rea2_sd_omega0             ! log spectral flat level, ns
         rea_sd_cornerf=rea2_sd_cornerf           ! corner f
         rea_sd_radius=rea2_sd_radius             ! source radius
         rea_sd_swin=rea2_sd_swin                 ! window lenght used
         rea_sd_mw=rea2_sd_mw                     ! moment mag
         rea_sd_slope=rea2_sd_slope               ! slope
      endif
c
c  mt
c

      do i=1,mt2_nmt
         mt_year(i+mt_nmt)=mt2_year(i)             ! Moment tensor hypocenter year
         mt_month(i+mt_nmt)=mt2_month(i)
         mt_day(i+mt_nmt)=mt2_day(i)
         mt_hour(i+mt_nmt)=mt2_hour(i)
         mt_min(i+mt_nmt)=mt2_min(i)
         mt_sec(i+mt_nmt)=mt2_sec(i)
         mt_moment(i+mt_nmt)=mt2_moment(i)          ! Scalar moment
         mt_coor(i+mt_nmt)=mt2_coor(i)              ! Moment tensor coordinate system Spherical or Cartesian
c        
         do k=1,6                                   ! S=Spherical and C=Cartesian, see Aki y Richard 1980 p 118
            mt_val(k,i+mt_nmt)=mt2_val(k,i)         ! Moment tensor values
         enddo

         mt_exp(i+mt_nmt)=mt2_exp(i)                ! Moment tensor exponential
         mt_lat(i+mt_nmt)=mt2_lat(i)                ! Moment tensor latitude
         mt_lon(i+mt_nmt)=mt2_lon(i)                ! Moment tensor longitude
         mt_depth(i+mt_nmt)=mt2_depth(i)            ! Moment tensor depth
         mt_agency(i+mt_nmt)=mt2_agency(i)          ! Moment tensor hypocenter agency, use 3 only
         mt_method(i+mt_nmt)=mt2_method(i)          ! Moment tensor method
         mt_quality(i+mt_nmt)=mt2_quality(i)        ! Moment tensor quality
         mt_mag(i+mt_nmt)=mt2_mag(i)                ! Moment tensor magnitudes
         mt_mag_type(i+mt_nmt)=mt2_mag_type(i)      ! Moment tensor magnitude types
      enddo

      mt_nmt=mt2_nmt+mt_nmt                         ! Number of Moment tensor solutions
c
c   SE dex etc, nothing in s-file
c        
c
c   all mags, added so no check if duplications. no probelm 
c   after write out. 
c
      do i=1,rea2_nmag
         hyp_mag_all(i+rea_nmag)=hyp2_mag_all(i)
         hyp_mag_type_all(i+rea_nmag)=hyp2_mag_type_all(i)
         hyp_mag_agency_all(i+rea_nmag)=hyp2_mag_agency_all(i)
         hyp_mag_all_main(i+rea_nmag)=hyp2_mag_all_main(i)
      enddo
      rea_nmag=rea_nmag+rea2_nmag

      return
      end
