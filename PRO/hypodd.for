      program hypoDD

c Author: Felix Waldhauser, felixw@ldeo.columbia.edu
c Version 1.3 - 11/2010 - FW
c
c started 03/1999 
c 01-03/2001  clean up & bug fixes by Bruce Julian, Fred Klein, Keith
c             Richards-Dinger, Felix Waldhauser 
c 10/2004     Version 1.1: fixed errors listed in BugList to V 1.0.
c 06/2007     Version 1.2 started. fixed errors listed in Buglist 1.1
c 06/2007     accomodate negative magnitudes in output format.
c             real -> doubleprecision: covar,lsfit_svd,matmult2,matmutl3,svd
c 07/2010     fixed bug in computing rms values for hypoDD.reloc (Zhonhe Zhao)
c 07/2010     lsfit_svd: Fixed apparent bug in getting 95% confidence errors 
c             from standard errors. Factor 2.7955 was used, but it should be 
c             1.96, assuming a t-distribution of the residuals (Hilary Martens)
c 07/2010     version 1.2
c 08/2010     now compiles with gfortran  (rcs removed and mod problem fixed)
c 08/2010     version 1.3
c 11/2010     fixed /0 problem in rms reporting (NaN in hypoDD.reloc) (c101116)
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c 02/2020  jh Include all fortran subroutines, convert output to nordic 
c             format, look for seisan, fix array overflow in matmult2
c 06/2021  lo increased dimension for elat/elon to match partials
c
c Purpose:
c Program to determine high-resolution hypocenter locations using the
c double-difference algorithm. hypoDD incorporates catalog and/or cross
c correlation P- and/or S-wave relative travel-time measurements.
c Residuals between observed and theoretical travel time differences
c (or double-differences = DD) are minimized for pairs
c of earthquakes at each station while linking together all observed
c event/station pairs. A least squares solution (SVD or LSQR) is found
c by iteratively adjusting the vector difference between hypocentral pairs.
c
c References:
c For a detailed description of the algorithm see:
c    Waldhauser, F. and W.L. Ellsworth, A double-difference earthquake
c    location algorithm: Method and application to the northern Hayward
c    fault, Bull. Seismol. Soc. Am., 90, 1353-1368, 2000.
c
c For a user guide to hypoDD see USGS open-file report: 
c    Waldhauser, F., HypoDD: A computer program to compute double-difference
c    earthquake locations,  U.S. Geol. Surv. open-file report , 01-113,
c    Menlo Park, California, 2001.
c
c The code is continuously being updated and improved, so feel
c free to send me an occasional request for the newest version:
c felixw@ldeo.columbia.edu
c or goto http://www.ldeo.columbia.edu/~felixw/hypoDD.html

	implicit none

	include'hypoDD.inc'

	real		acond
	real		adamp(10)
	real		adep
	integer		aiter(0:10)
	real		alat
	real		alon
	real		amaxdcc(10)
	real		amaxdct(10)
	real		amaxres_cross(10)
	real		amaxres_net(10)
	integer		amcusp(1000)
	real		awt_ccp(10)
	real		awt_ccs(10)
	real		awt_ctp(10)
	real		awt_cts(10)
	integer		clust(MAXCL,MAXEVE)
	real		cohav
	real		damp
	character	dattim*25
	real		dtav
	integer		dt_c1(MAXDATA)
	integer		dt_c2(MAXDATA)
	real		dt_cal(MAXDATA)
	real		dt_dt(MAXDATA)
	integer		dt_ic1(MAXDATA)
	integer		dt_ic2(MAXDATA)
	integer		dt_idx(MAXDATA)
	integer		dt_ista(MAXDATA)
	real		dt_offs(MAXDATA)
	real		dt_qual(MAXDATA)
	real		dt_res(MAXDATA)
	character	dt_sta(MAXDATA)*7
	real		dt_wt(MAXDATA)
	real		dxav
	real		dyav
	real		dzav
	real		etav
	integer		ev_cusp(MAXEVE)
	integer		ev_date(MAXEVE)
	real		ev_dep(MAXEVE)
	real		ev_herr(MAXEVE)
	real		ev_lat(MAXEVE)
	real		ev_lon(MAXEVE)
	real		ev_mag(MAXEVE)
	real		ev_res(MAXEVE)
	integer		ev_time(MAXEVE)
	real		ev_x(MAXEVE)
	real		ev_y(MAXEVE)
	real		ev_zerr(MAXEVE)
	real		ev_z(MAXEVE)
	logical		ex
	real		exav
	real		eyav
	real		ezav
	character	fn_cc*80
	character	fn_ct*80
	character	fn_eve*80
	character	fn_inp*80
	character	fn_loc*80
	character	fn_reloc*80
	character	fn_res*80
	character	fn_srcpar*80
	character	fn_sta*80
	character	fn_stares*80
	integer		fu0
	integer		fu1
	integer		fu3
	integer		i
	integer		iargc
	integer		ibeg
	integer		iclust
	integer		icusp(MAXEVE)
	integer		idata
	integer		idy
	integer		iend
	integer		ihr
	integer		imn
	integer		imo
	integer		ineg
	integer		iphase
	integer		isolv
	integer		istart
	integer		iter
	integer		itf
	integer		iunit
	integer		iyr
	integer		j
	integer		jiter
	integer		juliam
	integer		k
	integer		kiter
	integer		l
	doubleprecision	lat
	integer		log
	doubleprecision	lon
	real		maxdcc
	real		maxdct
	real 		maxdist
	integer		maxiter
	real		maxres_cross
	real		maxres_net
	integer		mbad
	integer		minobs_cc
	integer		minobs_ct
	real		minwght
	integer		mod_nl
	real		mod_ratio
	real		mod_top(MAXLAY)
	real		mod_v(MAXLAY)
	integer		n
	integer		narguments
	integer		ncc
	integer		nccold
	integer		nccp
	integer		nccs
	integer		nclust
	integer		nct
	integer		nctold
	integer		nctp
	integer		ncts
	integer		ncusp
	integer		ndt
	integer		nev
	integer		nevold
	integer		niter
	integer		noclust(MAXEVE)
	real		noisef_dt
	integer		nsrc
	integer		nsta
	real		picav
	real		resvar1
	real		rms_cc
	real		rms_cc0
	real		rms_cc0old
	real		rms_ccold
	real		rms_ct
	real		rms_ct0
	real		rms_ct0old
	real		rms_ctold
	real		sc
	real		sdc0_dep
	real		sdc0_lat
	real		sdc0_lon
	integer		src_cusp(MAXEVE)
	real		src_dep(MAXEVE)
	real		src_dt(MAXEVE)
	real		src_dx(MAXEVE)
	real		src_dy(MAXEVE)
	real		src_dz(MAXEVE)
	real		src_et(MAXEVE)
	real		src_ex(MAXEVE)
	real		src_ey(MAXEVE)
	real		src_ez(MAXEVE)
	real		src_lat0(MAXEVE)
	doubleprecision	src_lat(MAXEVE)
	real		src_lon0(MAXEVE)
	doubleprecision	src_lon(MAXEVE)
	integer		src_nnp(MAXEVE)
	integer		src_nns(MAXEVE)
	integer		src_np(MAXEVE)
	integer		src_ns(MAXEVE)
	real		src_rmsc(MAXEVE)
	real		src_rmsn(MAXEVE)
	real		src_t0(MAXEVE)
	real		src_t(MAXEVE)
	real		src_x0(MAXEVE)
	real		src_x(MAXEVE)
	real		src_y0(MAXEVE)
	real		src_y(MAXEVE)
	real		src_z0(MAXEVE)
	real		src_z(MAXEVE)
	real		sta_az(MAXSTA)
	real		sta_dist(MAXSTA)
	character	sta_lab(MAXSTA)*7
	real		sta_lat(MAXSTA)
	real		sta_lon(MAXSTA)
	integer		sta_nnp(MAXSTA)
	integer		sta_nns(MAXSTA)
	integer		sta_np(MAXSTA)
	integer		sta_ns(MAXSTA)
	real		sta_rmsc(MAXSTA)
	real		sta_rmsn(MAXSTA)
	character	str1*60
	character	str80*80
	character	str3*3
	real		tav
	real		tmpr1
	real		tmpr2
	real		tmp_ttp(MAXSTA,MAXEVE)
	real		tmp_tts(MAXSTA,MAXEVE)
	real		tmp_xp(MAXSTA,MAXEVE)
	real		tmp_yp(MAXSTA,MAXEVE)
	real		tmp_zp(MAXSTA,MAXEVE)
	integer		trimlen
	real		wt_ccp
	real		wt_ccs
	real		wt_ctp
	real		wt_cts
	real		x
	real		xav
	real		y
	real		yav
	real		zav

      minwght= 0.00001
      rms_ccold= 0
      rms_ctold= 0
      rms_cc0old= 0
      rms_ct0old=  0
c--- open log file:
      call freeunit(log)
      open(log,file='hypoDD.log',status='unknown')
      str1= 'starting hypoDD (v1.3 - 11/2010)...'
      call datetime(dattim)
      write(6,'(a45,a)') str1, dattim
      write(log,'(a45,a)') str1, dattim

c--- get input parameter file name:
      narguments = iargc()
      if(narguments.lt.1) then
        write(*,'(/,a)') 'PARAMETER INPUT FILE [hypoDD.inp <ret>]:'
        read(5,'(a)') fn_inp
        if(trimlen(fn_inp).le.1) then
           fn_inp= 'hypoDD.inp'            !default input file name
        else
           fn_inp= fn_inp(1:trimlen(fn_inp))
        endif
      else
          call getarg(1,fn_inp)
      endif
      inquire(FILE= fn_inp,exist=ex)
      if(.not. ex) stop' >>> ERROR OPENING INPUT PARAMETER FILE.'

c--- get input parameters:
      call getinp(MAXEVE,MAXLAY,log,fn_inp,
     & fn_cc,fn_ct,fn_sta,fn_eve,
     & fn_loc,fn_reloc,fn_res,fn_stares,fn_srcpar,
     & idata,iphase,
     & minobs_cc,minobs_ct,
     & amaxres_cross,amaxres_net,amaxdcc,amaxdct,
     & noisef_dt,maxdist,
     & awt_ccp,awt_ccs,awt_ctp,awt_cts,adamp,
     & istart,maxiter,isolv,niter,aiter,
     & mod_nl,mod_ratio,mod_v,mod_top,
     & iclust,ncusp,icusp)

c--- get data:
      call getdata(
     & log,fn_cc,fn_ct,fn_sta,fn_eve,fn_srcpar,
     & idata,iphase,ncusp,icusp,
     & maxdist,amaxdct(1),amaxdcc(1),
     & noisef_dt,mod_nl,mod_ratio,mod_v,mod_top,
     & ev_date,ev_time,ev_cusp,ev_lat,ev_lon,ev_dep,
     & ev_mag,ev_herr,ev_zerr,ev_res,
     & sta_lab,sta_lat,sta_lon,
     & dt_sta,dt_dt,dt_qual,dt_c1,dt_c2,dt_idx,
     & dt_ista,dt_ic1,dt_ic2,dt_offs,
     & nev,nsta,ndt,nccp,nccs,nctp,ncts,
     & tmp_xp,tmp_yp,tmp_zp,tmp_ttp,tmp_tts)

c--- clustering:
      if((idata.eq.1.and.minobs_cc.eq.0).or.
     &   (idata.eq.2.and.minobs_ct.eq.0).or.
     &   (idata.eq.3.and.minobs_ct+minobs_cc.eq.0)) then
         nclust= 1
         clust(1,1)= nev
         do i=1,nev
             clust(1,i+1)= ev_cusp(i)
         enddo
          write(*,'(/,"no clustering performed.")')
          write(log,'(/,"no clustering performed.")')
      else

         call cluster1(log, nev, ndt,
     & idata, minobs_cc, minobs_ct,
     & dt_c1, dt_c2, ev_cusp,
     & clust, noclust, nclust)

      endif

c--- open files
      call freeunit(fu0)
      open(fu0,file=fn_loc,status='unknown')
      call freeunit(fu1)
      open(fu1,file=fn_reloc,status='unknown')
      if(trimlen(fn_stares).gt.1) then
         call freeunit(fu3)
         open(fu3,file=fn_stares,status='unknown')
      endif

      jiter = 0  ! counter for iter with no updating (air quakes)
c--- big loop over clusters starts here:
      if(iclust.ne.0) then
        if (iclust.lt.0 .or. iclust.gt.nclust) then
           write(*,*) 'error: invalid cluster number ',iclust
           write(*,*) 'must be between 1 and nclust (',nclust,')'
           stop
        endif
        ibeg= iclust
        iend= iclust
      else
        ibeg= 1
        iend= nclust
      endif
      do iclust= ibeg,iend
      call datetime(dattim)
      write(log,'(/,"RELOCATION OF CLUSTER:",i2,5x,a,/,
     &"----------------------")')iclust,dattim
      write(*,'(/,"RELOCATION OF CLUSTER:",i2,5x,a,/,
     &"----------------------")')iclust,dattim

c--- get data for each cluster if clustering was invoked:
      if((nclust.eq.1.and.clust(iclust,1).eq.nev).or.
     &   (idata.eq.1.and.minobs_cc.eq.0).or.
     &   (idata.eq.2.and.minobs_ct.eq.0).or.
     &   (idata.eq.3.and.minobs_ct+minobs_cc.eq.0)) goto 50

      ncusp= clust(iclust,1)
      do i=1,ncusp
         icusp(i)= clust(iclust,i+1)
      enddo

      if(idata.ne.0) call getdata(
     & log,fn_cc,fn_ct,fn_sta,fn_eve,fn_srcpar,
     & idata,iphase,ncusp,icusp,
     & maxdist,amaxdct(1),amaxdcc(1),
     & noisef_dt,mod_nl,mod_ratio,mod_v,mod_top,
     & ev_date,ev_time,ev_cusp,ev_lat,ev_lon,ev_dep,
     & ev_mag,ev_herr,ev_zerr,ev_res,
     & sta_lab,sta_lat,sta_lon,
     & dt_sta,dt_dt,dt_qual,dt_c1,dt_c2,dt_idx,
     & dt_ista,dt_ic1,dt_ic2,dt_offs,
     & nev,nsta,ndt,nccp,nccs,nctp,ncts,
     & tmp_xp,tmp_yp,tmp_zp,tmp_ttp,tmp_tts)

50    continue
      nccold= nccp+nccs
      nctold= nctp+ncts
      ncc= nccp+nccs
      nct= nctp+ncts
      nevold= nev

c--- get cluster centroid:
      sdc0_lat= 0
      sdc0_lon= 0
      sdc0_dep= 0
      do i=1,nev
         sdc0_lat= sdc0_lat + ev_lat(i)
         sdc0_lon= sdc0_lon + ev_lon(i)
         sdc0_dep= sdc0_dep + ev_dep(i)
      enddo
      sdc0_lat= sdc0_lat/nev
      sdc0_lon= sdc0_lon/nev
      sdc0_dep= sdc0_dep/nev

      write(log,'("Cluster centroid at:",1x,f10.6,2x,f11.6,2x,f9.6)')
     & sdc0_lat,sdc0_lon,sdc0_dep

c--- Set up cartesian coordinate system (build common block for subr. SDC):
      call setorg(sdc0_lat,sdc0_lon,0.0,0)

c--- get cartesian coordinates for epicenters
      do i=1,nev
         lat= ev_lat(i)
         lon= ev_lon(i)
         call SDC2(x,y,lat,lon,-1)
         ev_x(i)= x *1000
         ev_y(i)= y *1000
         ev_z(i)= (ev_dep(i) - sdc0_dep)*1000
      enddo

      write(log,'("# events:",i5)')nev

c--- write output (mdat.loc):
      write(fu0,'(i9,1x,f10.6,1x,f11.6,1x,f9.3,1x,f10.1,1x,f10.1,
     & 1x,f10.1,
     & 1x,f8.1,1x,f8.1,1x,f8.1,1x,i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,f5.2,
     & 1x,f4.1,1x,i3)')
cfw     & 1x,f3.1,1x,i3)')	neg mag format
     & (ev_cusp(i),ev_lat(i),ev_lon(i),ev_dep(i),ev_x(i),ev_y(i),
     & ev_z(i),ev_herr(i)*1000,ev_herr(i)*1000,ev_zerr(i)*1000,
     & int(ev_date(i)/10000),
     & int(mod(ev_date(i),10000)/100),mod(ev_date(i),100),
     & int(ev_time(i)/1000000),int(mod(ev_time(i),1000000)/10000),
     & mod(real(ev_time(i)),10000.)/100,ev_mag(i),iclust,i=1,nev)
cfw100806     & mod(real(ev_time(i)),10000)/100,ev_mag(i),iclust,i=1,nev)

c--- get initial trial sources:
      call trialsrc(istart,sdc0_lat,sdc0_lon,sdc0_dep,
     & nev,ev_cusp,ev_lat,ev_lon,ev_dep,
     & nsrc,src_cusp,src_lat0,src_lon0,
     & src_x0,src_y0,src_z0,src_t0,
     & src_lat,src_lon,src_dep,
     & src_x,src_y,src_z,src_t)

      write(*,'("Initial trial sources =",i6)')nsrc
      write(log,'("# initial trial sources:",i6)')nsrc

c--- loop over iterations starts here:
c define iteration step at which re-weighting starts: this is dynam. since
c it depends on the number of neg depths runs before.

c first reset aiter() and maxiter
      do i=1,niter
         aiter(i) = aiter(i) - jiter
      enddo
      maxiter = maxiter - jiter

      kiter= 0		! counter for iter with data skipping
      jiter= 0		! counter for iter with no updating (air quakes)
      mbad= 0		! counter for air quakes

      iter= 1
55    call datetime(dattim)
      write(log,'(/,"===ITERATION ",i3," (",i3,") ",a)')
     & iter-jiter, iter, dattim

c--- get weighting parameters for this iteration:
      do i=1,niter
        if(iter.le.aiter(i)) goto 75
      enddo
75    maxres_cross= amaxres_cross(i)
      maxres_net= amaxres_net(i)
      maxdcc= amaxdcc(i)
      maxdct= amaxdct(i)
      wt_ccp= awt_ccp(i)
      wt_ccs= awt_ccs(i)
      wt_ctp= awt_ctp(i)
      wt_cts= awt_cts(i)
      damp= adamp(i)

      write(log, '(/,"Weighting parameters for this iteration:",/,
     &"  wt_ccp= ",f7.4,2X,"wt_ccs= ",f7.4,2X,
     &"maxr_cc= ",f7.4,2X,"maxd_cc= ",f7.2,2X,/,
     &"  wt_ctp= ",f7.4,2x,"wt_cts= ",f7.4,2x,"maxr_ct= ",f7.4,2x,
     &"maxd_ct= ",f7.2,/,"  damp= ",f5.1)')
     & wt_ccp,wt_ccs,maxres_cross,
     & maxdcc,wt_ctp,wt_cts,maxres_net,
     & maxdct,damp

c--- calculate travel times  and slowness vectors:
      write(log,'(/,"~ getting partials for ",i5,
     & " stations and ",i5," source(s) ...")') nsta,nsrc
      call partials(fn_srcpar,
     & nsrc,src_cusp,src_lat,src_lon,src_dep,
     & nsta,sta_lab,sta_lat,sta_lon,
     & mod_nl,mod_ratio,mod_v,mod_top,
     & tmp_ttp,tmp_tts,
     & tmp_xp,tmp_yp,tmp_zp)

c--- get double difference vector:
      call dtres(log,ndt,MAXSTA,nsrc,
     & dt_dt,dt_idx,
     & dt_ista,dt_ic1,dt_ic2,
     & src_cusp,src_t,tmp_ttp,tmp_tts,
     & dt_cal,dt_res)

c--- get a priori weights and reweight residuals
      call weighting(log,ndt,mbad,amcusp,idata,kiter,ineg,
     &               maxres_cross,maxres_net,maxdcc,maxdct,minwght,
     &               wt_ccp,wt_ccs,wt_ctp,wt_cts,
     &               dt_c1,dt_c2,dt_idx,dt_qual,dt_res,dt_offs,
     &               dt_wt)

c--- skip outliers and/or air quakes:
      if(ineg.gt.0) then
          call skip(log,kiter,minwght,
     & ndt,nev,nsrc,nsta,
     & ev_cusp,ev_date,ev_time,ev_mag,
     & ev_lat,ev_lon,ev_dep,ev_x,ev_y,ev_z,
     & ev_herr,ev_zerr,ev_res,
     & src_cusp, src_lat, src_lon, src_dep,
     & src_lat0, src_lon0,
     & src_x, src_y, src_z, src_t, src_x0, src_y0, src_z0, src_t0,
     & sta_lab,sta_lat,sta_lon,sta_dist,sta_az,
     & sta_rmsc,sta_rmsn,sta_np,sta_ns,sta_nnp,sta_nns,
     & dt_sta,dt_c1,dt_c2,dt_idx,dt_dt,dt_qual,dt_cal,
     & dt_ista,dt_ic1,dt_ic2,
     & dt_res,dt_wt,dt_offs,
     & tmp_ttp,tmp_tts,tmp_xp,tmp_yp,tmp_zp,nct,ncc)

c--Dont mess anymore with this cluster if we have wiped out all events
        if(nev.lt.2) then 
          write(log,*)' Cluster has less than 2 events.'
          write(*,*)' Cluster has less than 2 events.'
          goto 778
        endif
      else
         write(log,'("no data skipped.")')

      endif

c--- get initial residual statistics (avrg,rms,var..)
      if(iter.eq.1) then
       resvar1= -999
       call resstat(log,idata,ndt,nev,dt_res,dt_wt,dt_idx,
     & rms_cc,rms_ct,rms_cc0,rms_ct0,
     & rms_ccold,rms_ctold,rms_cc0old,rms_ct0old,
     &              resvar1)
      endif

c--- least square fitting:

      if(isolv.eq.1) then
         call lsfit_SVD(log,iter,ndt,nev,nsrc,damp,mod_ratio,
     & idata,ev_cusp,src_cusp,
     & dt_res,dt_wt,
     & dt_ista,dt_ic1,dt_ic2,   !new
     & src_dx,src_dy,src_dz,src_dt,src_ex,src_ey,src_ez,src_et,
     & exav,eyav,ezav,etav,dxav,dyav,dzav,dtav,
     & rms_cc,rms_ct,rms_cc0,rms_ct0,
     & rms_ccold,rms_ctold,rms_cc0old,rms_ct0old,
     & tmp_xp,tmp_yp,tmp_zp,dt_idx)

      else
         call lsfit_lsqr(log,iter,ndt,nev,nsrc,damp,mod_ratio,
     & idata,ev_cusp,src_cusp,
     & dt_res,dt_wt,
     & dt_ista,dt_ic1,dt_ic2,   !new
     & src_dx,src_dy,src_dz,src_dt,src_ex,src_ey,src_ez,src_et,
     & exav,eyav,ezav,etav,dxav,dyav,dzav,dtav,
     & rms_cc,rms_ct,rms_cc0,rms_ct0,
     & rms_ccold,rms_ctold,rms_cc0old,rms_ct0old,
     & tmp_xp,tmp_yp,tmp_zp,dt_idx,acond)
      endif

c--- check for air quakes:
      mbad= 0
      k= 1
      do i= 1,nsrc
        if(src_dep(i) + (src_dz(i)/1000).lt.0) then
            write(log,'(">>>Warning: negative depth - ",i12)')ev_cusp(i)
            amcusp(k)= ev_cusp(i)
            k=k+1
            if(k.gt.1000) stop'>>> More than 1000 air quakes. Too many!'
        endif
      enddo
      mbad= k-1    ! number of neg depth events

c update iteration numbers:
      if(mbad.gt.0) then
         do i= 1,niter
              aiter(i)= aiter(i)+1
         enddo
         jiter= jiter+1	  ! iteration with no update
         maxiter= maxiter+1

         write(log,*)'Number of air quakes (AQ) =',mbad
         if(nsrc-mbad .le. 1) then
           write(*,*)'Warning: number of non-airquakes < 2'
                   write(*,*)'   skipping this cluster'
           write(log,*)'Warning: number of non-airquakes < 2'
                   write(log,*)'   skipping this cluster'
                   goto 778
         endif       
         goto 500   ! skip the updating step
      endif


c--- update source parameters:
      xav= 0 ! mean centroid shift
      yav= 0
      zav= 0
      tav= 0
      alon= 0
      alat= 0
      adep= 0
      if(nsrc.eq.1) nsrc= nev
      do i= 1,nsrc
        src_cusp(i)= ev_cusp(i)
c update absolute source parameters (cart)
        src_x(i)= src_x(i) + src_dx(i)
        src_y(i)= src_y(i) + src_dy(i)
        src_z(i)= src_z(i) + src_dz(i)
        src_t(i)= src_t(i) + src_dt(i)

c update absolute source locations (geogr)
        src_dep(i)= src_dep(i) + (src_dz(i)/1000)
        call SDC2(src_x(i)/1000,src_y(i)/1000,lat,lon,1)
        src_lon(i)= lon
        src_lat(i)= lat
        alon= lon+alon	
        alat= lat+alat
        adep= adep+src_dep(i)

c get mean centroid shift
        xav= xav + (src_x(i) - src_x0(i))	
        yav= yav + (src_y(i) - src_y0(i))
        zav= zav + (src_z(i) - src_z0(i))
        tav= tav + (src_t(i) - src_t0(i))
      enddo
      xav= xav/nsrc
      yav= yav/nsrc
      zav= zav/nsrc
      tav= tav/nsrc
      alon= alon/nsrc
      alat= alat/nsrc
      adep= adep/nsrc

      write(log,'("  cluster centroid at:",1x,f10.6,2x,f11.6,2x,f9.6)')
     & alat,alon,adep
      write(log,'("  mean centroid (origin) shift in x,y,z,t [m,ms]: ",/
     & f7.1,f7.1,f7.1,f7.1)'),xav,yav,zav,tav
      write(log,'("  (OS in std output gives maximum value.)")')

c--- get interevent distance for each observation and average signal coherency:
      cohav= 0
      picav= 0
      j= nct
      k= ncc
      ncc= 0
      nct= 0
      do i= 1,ndt
         dt_offs(i)= sqrt((src_x(dt_ic1(i))-src_x(dt_ic2(i)))**2 +
     &                  (src_y(dt_ic1(i))-src_y(dt_ic2(i)))**2 +
     &                  (src_z(dt_ic1(i))-src_z(dt_ic2(i)))**2)

         if(dt_idx(i).le.2) then
            cohav= cohav + sqrt(dt_qual(i))
            ncc= ncc+1
         else
            picav= picav + dt_qual(i)
            nct= nct+1
         endif

      enddo
      cohav= cohav/ncc
      picav= picav/nct
      write(log,'(/,"More:")')
      write(log,'("  mean phase coherency = ",f5.3)')cohav
      write(log,'("  mean pick quality = ",f5.3)')picav

c--- get number of observations and mean residual at each station
      tmpr1= 0
      tmpr2= 0
      do i= 1,nsta
         sta_np(i)= 0
         sta_ns(i)= 0
         sta_nnp(i)= 0
         sta_nns(i)= 0
         sta_rmsc(i)= 0
         sta_rmsn(i)= 0
         do j= 1,ndt
            if(i.eq.dt_ista(j)) then
               if(dt_idx(j).le.2) then
                 sta_rmsc(i)= sta_rmsc(i)+dt_res(j)**2
                 if(dt_idx(j).eq.1) then
                    sta_np(i)= sta_np(i)+1
                 else
                    sta_ns(i)= sta_ns(i)+1
                 endif
               else
                 sta_rmsn(i)= sta_rmsn(i)+dt_res(j)**2
                 if(dt_idx(j).eq.3) then
                   sta_nnp(i)= sta_nnp(i)+1
                 else
                   sta_nns(i)= sta_nns(i)+1
                 endif
               endif
            endif
         enddo

         if(sta_np(i)+sta_ns(i).gt.0)
     &     sta_rmsc(i)= sqrt(sta_rmsc(i)/(sta_np(i)+sta_ns(i)))
         if(sta_nnp(i)+sta_nns(i).gt.0)
     &     sta_rmsn(i)= sqrt(sta_rmsn(i)/(sta_nnp(i)+sta_nns(i)))
         if(sta_rmsc(i).gt.tmpr1) then
            tmpr1= sta_rmsc(i)
            k= i
         endif
         if(sta_rmsn(i).gt.tmpr2) then
            tmpr2= sta_rmsn(i)
            l= i
         endif
      enddo
      tmpr1= tmpr1*1000
      tmpr2= tmpr2*1000
      if(idata.eq.1.or.idata.eq.3) then
         write(log,'("  station with largest cc rms: ",a7,"=",
     & f7.0," ms (RMSST)")')
     &    sta_lab(k),tmpr1
      endif
      if(idata.eq.2.or.idata.eq.3) then
         write(log,'("  station with largest ct rms: ",a7,"=",
     & f7.0," ms (RMSST)")')
     &    sta_lab(l),tmpr2
      endif

c--- write output scratch mdat.reloc:
      n= trimlen(fn_reloc)
      i=iter-jiter
      write(str80,'(a,".",i3.3,".",i3.3)')fn_reloc(1:n),iclust,i
      call freeunit(iunit)
      open(iunit,file=str80,status='unknown')
      write(iunit,'(i9,1x,f10.6,1x,f11.6,1x,f9.3,1x,f10.1,1x,f10.1,
     & 1x,f10.1,
     & 1x,f8.1,1x,f8.1,1x,f8.1,1x,i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,f6.3,
     & 1x,f4.1,1x,i3)')
cfw     & 1x,f3.1,1x,i3)')		! negative mag
     & (src_cusp(i),src_lat(i),src_lon(i),src_dep(i),src_x(i),src_y(i),
     & src_z(i),src_ex(i),src_ey(i),src_ez(i),int(ev_date(i)/10000),
     & int(mod(ev_date(i),10000)/100),mod(ev_date(i),100),
     & int(ev_time(i)/1000000),int(mod(ev_time(i),1000000)/10000),
     & mod(real(ev_time(i)),10000.)/100,ev_mag(i),iclust,i=1,nev)
cfw100806     & mod(real(ev_time(i)),10000)/100,ev_mag(i),iclust,i=1,nev)
      close(iunit)
c WARNING: variable "str" is set to zero value by default
      write(log,'(/,"Relocation results for this iteration are"
     & " stored in ",a)')str80(1:trimlen(str80))

500   continue  ! case of air quakes

c standard output:
      if(mbad.gt.0) then
         str3='   '
      else
         n= iter-jiter
         if(n.lt.1000) write(str3,'(i3)')n
         if(n.lt.100) write(str3,'(1x,i2)')n
         if(n.lt.10) write(str3,'(2x,i1)')n
      endif
      if(isolv.eq.1.and.idata.eq.3) then
       if(iter.eq.1) write(*,'(/,"  IT   EV  CT  CC",
     & "    RMSCT      RMSCC   RMSST   DX   DY   DZ   DT   OS  AQ",/,
     &                           "        %   %   %",
     & "   ms     %   ms     %    ms    m    m    m   ms    m ")')
       write(*,'(i2,a3,3(1x,i3),i5,f6.1,i5,f6.1,i6,4i5,i5,i4)')
     & iter,str3,
     & nint(nev*100./nevold),nint(nct*100.0/nctold),
     & nint(ncc*100.0/nccold),
     & nint(rms_ct*1000),(rms_ct-rms_ctold)*100/rms_ctold,
     & nint(rms_cc*1000),(rms_cc-rms_ccold)*100/rms_ccold,
     & nint(max(tmpr1,tmpr2)),
     & nint(dxav),nint(dyav),nint(dzav),nint(dtav),
     & nint(max(abs(xav),abs(yav),abs(zav))),mbad
      endif
      if(isolv.eq.1.and.idata.eq.1) then
       if(iter.eq.1) write(*,'(/,"  IT   EV  CC",
     & "    RMSCC   RMSST   DX   DY   DZ   DT   OS  AQ",/,
     &                          "        %   %",
     & "   ms     %    ms    m    m    m   ms    m ")')
       write(*,'(i2,a3,2(1x,i3),i5,f6.1,i6,4i5,i5,i4)')
     & iter,str3,
     & nint(nev*100./nevold),
     & nint(ncc*100.0/nccold),
     & nint(rms_cc*1000),(rms_cc-rms_ccold)*100/rms_ccold,
     & nint(max(tmpr1,tmpr2)),
     & nint(dxav),nint(dyav),nint(dzav),nint(dtav),
     & nint(max(abs(xav),abs(yav),abs(zav))),mbad
      endif
      if(isolv.eq.1.and.idata.eq.2) then
       if(iter.eq.1) write(*,'(/,"  IT   EV  CT",
     & "    RMSCT     RST   DX   DY   DZ   DT   OS  AQ",/,
     &                           "        %   %",
     & "   ms     %    ms    m    m    m   ms    m ")')
       write(*,'(i2,a3,2(1x,i3),i5,f6.1,i6,4i5,i5,i4)')
     & iter,str3,
     & nint(nev*100./nevold),
     & nint(nct*100.0/nctold),
     & nint(rms_ct*1000),(rms_ct-rms_ctold)*100/rms_ctold,
     & nint(max(tmpr1,tmpr2)),
     & nint(dxav),nint(dyav),nint(dzav),nint(dtav),
     & nint(max(abs(xav),abs(yav),abs(zav))),mbad
      endif

      if(isolv.eq.2.and.idata.eq.3) then
       if(iter.eq.1) write(*,'(/,"  IT   EV  CT  CC",
     & "    RMSCT      RMSCC   RMSST   DX   DY   DZ   DT   ",
     & "OS  AQ  CND",/,
     &                           "        %   %   %",
     & "   ms     %   ms     %    ms    m    m    m   ms   ",
     & " m     ")')
       write(*,'(i2,a3,3(1x,i3),i5,f6.1,i5,f6.1,i6,4i5,i5,i4,i5)')
     & iter,str3,
     & nint(nev*100./nevold),nint(nct*100.0/nctold),
     & nint(ncc*100.0/nccold),
     & nint(rms_ct*1000),(rms_ct-rms_ctold)*100/rms_ctold,
     & nint(rms_cc*1000),(rms_cc-rms_ccold)*100/rms_ccold,
     & nint(max(tmpr1,tmpr2)),
     & nint(dxav),nint(dyav),nint(dzav),nint(dtav),
     & nint(max(abs(xav),abs(yav),abs(zav))),mbad,nint(acond)
      endif
      if(isolv.eq.2.and.idata.eq.1) then
       if(iter.eq.1) write(*,'(/,"  IT   EV  CC",
     & "    RMSCC   RMSST   DX   DY   DZ   DT   OS  AQ  CND",/,
     &                           "        %   %",
     & "   ms     %    ms    m    m    m   ms    m ")')
       write(*,'(i2,a3,2(1x,i3),i5,f6.1,i6,4i5,i5,i4,i5)')
     & iter,str3,
     & nint(nev*100./nevold),
     & nint(ncc*100.0/nccold),
     & nint(rms_cc*1000),(rms_cc-rms_ccold)*100/rms_ccold,
     & nint(max(tmpr1,tmpr2)),
     & nint(dxav),nint(dyav),nint(dzav),nint(dtav),
     & nint(max(abs(xav),abs(yav),abs(zav))),mbad,nint(acond)
      endif
      if(isolv.eq.2.and.idata.eq.2) then
       if(iter.eq.1) write(*,'(/,"  IT   EV  CT",
     & "    RMSCT   RMSST   DX   DY   DZ   DT   OS  AQ  CND",/,
     &                           "        %   %",
     & "   ms     %    ms    m    m    m   ms    m ")')
       write(*,'(i2,a3,2(1x,i3),i5,f6.1,i6,4i5,i5,i4,i5)')
     & iter,str3,
     & nint(nev*100./nevold),
     & nint(nct*100.0/nctold),
     & nint(rms_ct*1000),(rms_ct-rms_ctold)*100/rms_ctold,
     & nint(max(tmpr1,tmpr2)),
     & nint(dxav),nint(dyav),nint(dzav),nint(dtav),
     & nint(max(abs(xav),abs(yav),abs(zav))),mbad,nint(acond)
      endif

      call datetime(dattim)
      write(log,'("Iteration ",i2," finished ",a)') iter, dattim

      if(iter.eq.maxiter) goto 600	! all iterations done.
      iter= iter+1
      goto 55	! next iteration

c--- update origin time (this is only done for final output!!)
600   continue
      write(*,'(/,"writing out results ...")')
      do i= 1,nev
         src_t(i)= src_t(i)/1000	!from here on src_t in sec!!
         if(src_t(i).gt.5) then
            write(*,*)'WARNING: org time diff > 5s for ',src_cusp(i)
         endif
         iyr= int(ev_date(i)/10000)
         imo= int(mod(ev_date(i),10000)/100)
         idy= int(mod(ev_date(i),100))
         ihr= int(ev_time(i)/1000000)
         imn= int(mod(ev_time(i),1000000)/10000)
         itf= JULIAM(iyr,imo,idy,ihr,imn)

cfw         sc= (mod(real(ev_time(i)),10000)/100) + src_t(i)
         sc= (mod(real(ev_time(i)),10000.)/100) - src_t(i)
cfw100806         sc= (mod(real(ev_time(i)),10000)/100) - src_t(i)
         itf= itf + int(sc / 60.)
         sc=  sc  - int(sc / 60.)*60.
         if(sc.lt.0) then
            itf= itf-1
            sc= 60. + sc
         endif
         call DATUM(itf,iyr,imo,idy,ihr,imn)
         ev_date(i)= iyr*10000 + imo*100 + idy
         ev_time(i)= ihr*1000000 + imn*10000 + nint(sc*100)
      enddo

c--- get # of obs per event:
      do i=1,nev
         src_np(i)= 0
         src_ns(i)= 0
         src_nnp(i)= 0
         src_nns(i)= 0
         src_rmsc(i)= 0
         src_rmsn(i)= 0
      enddo
      do i=1,ndt
         if(dt_idx(i).eq.1) then
             src_np(dt_ic1(i))= src_np(dt_ic1(i))+1
             src_np(dt_ic2(i))= src_np(dt_ic2(i))+1
         endif
         if(dt_idx(i).eq.2) then
             src_ns(dt_ic1(i))= src_ns(dt_ic1(i))+1
             src_ns(dt_ic2(i))= src_ns(dt_ic2(i))+1
         endif
         if(dt_idx(i).le.2) then
             src_rmsc(dt_ic1(i))= src_rmsc(dt_ic1(i))+dt_res(i)**2
             src_rmsc(dt_ic2(i))= src_rmsc(dt_ic2(i))+dt_res(i)**2
         endif
         if(dt_idx(i).eq.3) then
             src_nnp(dt_ic1(i))= src_nnp(dt_ic1(i))+1
             src_nnp(dt_ic2(i))= src_nnp(dt_ic2(i))+1
         endif
         if(dt_idx(i).eq.4) then
             src_nns(dt_ic1(i))= src_nns(dt_ic1(i))+1
             src_nns(dt_ic2(i))= src_nns(dt_ic2(i))+1
         endif
         if(dt_idx(i).ge.3) then
             src_rmsn(dt_ic1(i))= src_rmsn(dt_ic1(i))+dt_res(i)**2
             src_rmsn(dt_ic2(i))= src_rmsn(dt_ic2(i))+dt_res(i)**2
         endif
      enddo
      do i=1,nev
c100710         src_rmsc(i)= sqrt(src_rmsc(i)/nev)
c100710         src_rmsn(i)= sqrt(src_rmsn(i)/nev)
c101116         src_rmsc(i)= sqrt(src_rmsc(i)/(src_np(i)+src_ns(i)))
c101116         src_rmsn(i)= sqrt(src_rmsn(i)/(src_nnp(i)+src_nns(i)))
         if(src_np(i)+src_ns(i).gt.0) then
            src_rmsc(i)= sqrt(src_rmsc(i)/(src_np(i)+src_ns(i)))
         else
            src_rmsc(i)= -9 
         endif
         if(src_nnp(i)+src_nns(i).gt.0) then
            src_rmsn(i)= sqrt(src_rmsn(i)/(src_nnp(i)+src_nns(i)))
         else
            src_rmsn(i)= -9 
         endif
      enddo

c--- output final residuals: mdat.res
      if(trimlen(fn_res).gt.1) then
         call freeunit(iunit)
         open(iunit,file=fn_res,status='unknown')
         write(iunit,'("STA",11x,"DT",8x,
     &"C1",8x,"C2",4x,"IDX",5x,"QUAL",4x,"RES [ms]",3x,"WT",9x,
     &"OFFS")')
         write(iunit,'(a7,1x,f12.7,1x,i9,1x,i9,1x,i1,1x,
     & f9.4,1x,f12.6,1x,f11.6,1x,f8.1)')
     & (dt_sta(j),dt_dt(j),dt_c1(j),dt_c2(j),dt_idx(j),dt_qual(j),
     & dt_res(j)*1000,dt_wt(j),dt_offs(j),j=1,ndt)
         close(iunit)
      endif

c--- output final locations (mdat.reloc):
      write(fu1,'(i9,1x,f10.6,1x,f11.6,1x,f9.3,1x,f10.1,1x,f10.1,
     & 1x,f10.1,
     & 1x,f8.1,1x,f8.1,1x,f8.1,1x,i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,f6.3,
     & 1x,f4.1,1x,i5,1x,i5,1x,i5,1x,i5,1x,f6.3,1x,f6.3,1x,i3)')
cfw     & 1x,f3.1,1x,i5,1x,i5,1x,i5,1x,i5,1x,f6.3,1x,f6.3,1x,i3)') !neg mag
     & (src_cusp(i),src_lat(i),src_lon(i),src_dep(i),src_x(i),src_y(i),
     & src_z(i),src_ex(i),src_ey(i),src_ez(i),int(ev_date(i)/10000),
     & int(mod(ev_date(i),10000)/100),mod(ev_date(i),100),
     & int(ev_time(i)/1000000),int(mod(ev_time(i),1000000)/10000),
     & mod(real(ev_time(i)),10000.)/100,ev_mag(i),
cfw100806     & mod(real(ev_time(i)),10000)/100,ev_mag(i),
     & src_np(i),src_ns(i),src_nnp(i),src_nns(i),
     & src_rmsc(i),src_rmsn(i), iclust,i=1,nev)

c--- output stations (mdat.station):
      if(trimlen(fn_stares).gt.1) then
cfw         write(fu3,'(a5,1x,f9.4,1x,f9.4,1x,f9.4,1x,f9.4,1x,i7,1x,
         write(fu3,'(a7,1x,f9.4,1x,f9.4,1x,f9.4,1x,f9.4,1x,i7,1x,
     & i7,1x,i7,1x,i7,1x,f9.4,1x,f9.4,1x,i3)')
     & (sta_lab(i),sta_lat(i),sta_lon(i),sta_dist(i),sta_az(i),
     & sta_np(i),sta_ns(i),sta_nnp(i),sta_nns(i),
     & sta_rmsc(i),sta_rmsn(i),iclust,i=1,nsta)
      endif

778   continue
      enddo  ! loop over clusters (iclust)

      close(fu0)
      if(trimlen(fn_stares).gt.1) close(fu3)
      close(fu1)
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  seisan add
c
      call systemc('nor2dd n',8)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      
      end !of main routine
c Multiply a matrix by a vector
c  Version for use with sparse matrix specified by
c  output of subroutine sparse for use with LSQR

	subroutine aprod(mode, m, n, x, y, leniw, lenrw, iw, rw)

	implicit none

c	Parameters:
	integer	mode		! ==1: Compute  y = y + a*x
				! 	y is altered without changing x
				! ==2: Compute  x = x + a(transpose)*y
				!	x is altered without changing y
	integer	m, n		! Row and column dimensions of a
	real	x(n), y(m)	! Input vectors
	integer	leniw, lenrw
	integer	iw(leniw)	! Integer work vector containing:
				! iw[1]  Number of non-zero elements in a
				! iw[2:iw[1]+1]  Row indices of non-zero elements
				! iw[iw[1]+2:2*iw[1]+1]  Column indices
	real	rw(lenrw)	! [1..iw[1]] Non-zero elements of a

c	Local variables:
	integer i1
	integer j1
	integer k
	integer kk

c	set the ranges the indices in vector iw

	kk=iw(1)
	i1=1
	j1=kk+1

c	main iteration loop

	do 100 k = 1,kk

	if (mode.eq.1) then

c	compute  y = y + a*x

	y(iw(i1+k)) = y(iw(i1+k)) + rw(k)*x(iw(j1+k))

	else

c	compute  x = x + a(transpose)*y

	x(iw(j1+k)) = x(iw(j1+k)) + rw(k)*y(iw(i1+k))

	endif

  100	continue

	return
	end
	subroutine cluster1(log, nev, ndt,
     &	idata, minobs_cc, minobs_ct,
     &	dt_c1, dt_c2, ev_cusp,
     &	clust, noclust, nclust)

	implicit none

	include'hypoDD.inc'

c	Parameters:
	integer		log		! Log-file identifier
	integer		nev		! No. of events
	integer		ndt		! No. of data
	integer		idata
	integer		minobs_cc	! Min. obs./pair for ccor. data
	integer		minobs_ct	! Min. obs./pair for cat. data
	integer		dt_c1(MAXDATA)	! [1..ndt] Event keys
	integer		dt_c2(MAXDATA)	! [1..ndt] Event keys
	integer		ev_cusp(MAXEVE)	! [1..nev] Event keys
	integer		clust(MAXCL,MAXEVE) ! [1..nclust,1..] Event keys
					! clust[i,1] = Size of ith cluster
	integer		noclust(MAXEVE)	! [1..MAXEVE] Event keys
					! noclust[1] = No. of keys
	integer		nclust		! No. of cllusters

c	Local variables:
	integer		acli(MAXEVE)
	integer		acl(MAXEVE)
	integer		apair_n((MAXEVE*(MAXEVE-1))/2)
	integer		i
	integer		ia
	integer		icusp(MAXEVE)	! [1..nev] Event keys
	integer		ifindi
	integer		ii
	integer		j
	integer		k
	integer		kk
	integer		n
	integer		nn

      write(*,'("clustering ...  ")')
      write(log,'(/,"~ clustering ...  ")')

c     Set up event-pair arrays
      k = 0
      do i=2,nev
         do j=1,i-1		!lower triangle of matrix
            k = k+1
            apair_n(k) = 0
         enddo
      enddo

      do i=1,nev
         icusp(i) = ev_cusp(i)
      enddo
      call sorti(nev, icusp)

      do i=1, ndt
         j = ifindi(nev, icusp, dt_c1(i))
         k = ifindi(nev, icusp, dt_c2(i))
         if (k.gt.j) then
c           Map into lower triangle
            kk = k
            k = j
            j = kk
         endif
         apair_n(((j-1)**2-(j-1))/2+k)=
     &          apair_n(((j-1)**2-(j-1))/2+k) + 1
      enddo
      if (idata.eq.0 .or. idata.eq.1) minobs_ct = 0
      if (idata.eq.0 .or. idata.eq.2) minobs_cc = 0

c     Initialize array acl to store cluster index for each event
      do i=1,nev
         acl(i) = nev+1
      enddo
      k = 0
      n = 0
      do i=2,nev
         do j=1,i-1
            k = k+1
            if (apair_n(k).ge.(minobs_cc+minobs_ct)) then
               if (acl(i).lt.acl(j)) then
                  if (acl(j).eq.nev+1) then
                     acl(j) = acl(i)
                  else
                     ia = acl(j)
                     do ii=1,i
                       if (acl(ii).eq.ia) acl(ii) = acl(i)
                     enddo
                  endif
               elseif (acl(j).lt.acl(i)) then
                  if (acl(i).eq.nev+1) then
                     acl(i) = acl(j)
                  else
                     ia = acl(i)
                     do ii=1,i
                       if (acl(ii).eq.ia) acl(ii) = acl(j)
                     enddo
                  endif
               elseif (acl(i).eq.nev+1) then
                  n = n+1
                  acl(i) = n
                  acl(j) = n
               endif
            endif
         enddo
      enddo

c     Store event keys in cluster matrix clust[]
      call indexxi(nev,acl,acli)
      n = 1
      nn = 2
      if (acl(acli(1)).eq.nev+1) then
c        No events clustered
         i = 1
         goto 300
      else
         clust(n,nn) = icusp(acli(1))
      endif
      do i=2,nev
         if (acl(acli(i)).gt.acl(acli(i-1))) then
            clust(n,1) = nn-1
            n = n+1
            nn = 1
         endif
         if (acl(acli(i)).eq.nev+1) goto 300	! events not clustered
         nn = nn+1
         clust(n,nn) = icusp(acli(i))
      enddo
      clust(n,1) = nn-1
      nclust = n
      noclust(1) = 0
      goto 310
300   nclust = n-1
      do j=i,nev
         noclust(j-i+2) = icusp(acli(j))
      enddo
      noclust(1) = nev-i+1
310   continue
      if (nclust.ge.MAXCL) stop'>>> Increase MAXCL in hypoDD.inc.'

c     Sort - biggest cluster first
      if (nclust.gt.1) then
         do i=1,nclust-1
            do j=i+1,nclust
               if (clust(i,1).le.clust(j,1)) then
                  do k=1,clust(i,1)+1
                     clust(MAXCL,k) = clust(i,k)
                  enddo
                  do k=1,clust(j,1)+1
                     clust(i,k) = clust(j,k)
                  enddo
                  do k=1,clust(MAXCL,1)+1
                     clust(j,k) = clust(MAXCL,k)
                  enddo
               endif
            enddo
         enddo
      endif

      k = 0
      do i=1,nclust
         k = k + clust(i,1)
      enddo

      write(*,'("Clustered events: ",i5)') k
      write(*,'("Isolated events: ",i5)') noclust(1)
      write(*,'("# clusters:",i5)')nclust
      k = 0
      do i=1,nclust
         write(*,'("Cluster",i4,": ",i5," events")') i, clust(i,1)
         k = k+clust(i,1)
      enddo

      write(log,'("# clustered events =",i5)')k
cfw      write(log,'("# isolated events =",i5,/,8(i8))')
      write(log,'("# isolated events =",i5,/,8(i9))')
     & noclust(1),(noclust(j),j=2,noclust(1)+1)
      write(log,'("# clusters =",i5,"  for min. number of links "
     & "set to ",i5)')nclust,minobs_ct+minobs_cc
      k = 0
      do i=1,nclust
         write(log,'("Cluster",i5,": ",i5," events",/,8(i9,2x))')
     & i,clust(i,1),(clust(i,j),j=2,clust(i,1)+1)
         k = k+clust(i,1)
      enddo
      write(log,*)
      if (nclust.eq.0) stop

      end !of subroutine cluster1

	subroutine covar(maxev0, v, n, q, cvm)

	implicit none

c	Parameters:
	integer	maxev0
	doubleprecision	v(maxev0*4, maxev0*4)
	integer	n
	doubleprecision	q(maxev0*4)
	real	cvm(maxev0*4, maxev0*4)

c	Local variables:
	integer	i, j, k		! Dummy loop indices
	real	sum

      do i=1,n
         do j=1,i
           sum= 0
           do k=1,n
              if(q(k).ne.0) sum= sum + v(i,k)*v(j,k) * (1/(q(k)*q(k)))
           enddo
           cvm(i,j)= sum
           cvm(j,i)= sum
         enddo
      enddo
      return
      end
c Compute calendar date from count of minutes (since when?)

	subroutine datum(itf, iyr, imo, idy, ihr, imn)

	implicit none

c	Parameters:
	integer	itf
	integer	iyr
	integer	imo
	integer	idy
	integer	ihr
	integer	imn

C UMRECHNEN DES DATUMS IN MINUTEN (CF. JULIAM) IN YR-MO-DY-HR-MI
C   (MIT IMN<2**31, JAHR < 4000

c	Local variables:
	integer	i
	integer	iyr4
	integer	iyrh
	integer	iyrt
	integer	kh
	integer	l
	integer	ld
	integer id
	integer k
	integer kmo(12)

	data kmo/31,28,31,30,31,30,31,31,30,31,30,31/

      k = itf/60
      imn = itf-k*60
      kh = k/24
      ihr = k-kh*24
      iyr = kh/365
5     id = kh-iyr*365
      l = 0
      iyr4 = iyr/4
      iyrh = iyr/100
      iyrt = iyr/1000
      ld = iyr4-iyrh+iyrt
      if (iyr4*4.eq.iyr.and.(iyrh*100.ne.iyr.or.iyrt*1000.eq.iyr)) l = 1
      id = id-ld+l
      if (id.gt.0) goto 10
      if (id.eq.0.and.ihr.eq.0.and.imn.eq.0) then
          idy = 0
          imo = 0
          return
      endif
      iyr = iyr-1
      goto 5
10    kmo(2) = 28+l
      do i=1,12
         id = id- kmo(i)
         if (id.le.0) goto 30
      enddo
      i =12
30    idy = id+kmo(i)
      imo = i
      return
      end ! of subr. datum
c-------------------------------------------------------------------------
      subroutine delaz(a1lat,a1lon,a2lat,a2lon,del,dist,az)
c
c     by Bill Ellsworth
c
c        computes distance and azimuth from a1 to a2
c        a1 and a2 are in decimal degrees and n-e coordinates
c        del -- delta in degrees
c        dist -- distance in km
c        az -- azimuth from a to b clockwise from north in degrees

c     changes by Felix Waldhauser (fw)
c
      real*8 pi2,rad,flat
      real*8 alatr,alonr,blatr,blonr
      real*8 tana,geoa,acol,tanb,geob,bcol
      real*8 diflon,cosdel,delr,top,den,azr,colat,radius
cfw      real*8 dtan,datan,dsin,dcos,darcos,dcotan,datan2
      real*8 dtan,datan,dsin,dcos,datan2
      data pi2/1.570796d0/
      data rad/1.745329d-02/
      data flat/.993231d0/

c-----convert to radians
      alatr=a1lat*rad
      alonr=a1lon*rad
      blatr=a2lat*rad
      blonr=a2lon*rad
c-----convert latitudes to geocentric colatitudes
      tana=flat*dtan(alatr)
      geoa=datan(tana)
      acol=pi2-geoa
      tanb=flat*dtan(blatr)
      geob=datan(tanb)
      bcol=pi2-geob
c-----calcuate delta
      diflon=blonr-alonr
      cosdel=dsin(acol)*dsin(bcol)*dcos(diflon)+dcos(acol)*dcos(bcol)
cfw      delr=darcos(cosdel)
      delr=dacos(cosdel)
c-----calcuate azimuth from a to b
      top=dsin(diflon)
cfw      den=dsin(acol)*dcotan(bcol)-dcos(acol)*dcos(diflon)
      den=dsin(acol)*(1/dtan(bcol))-dcos(acol)*dcos(diflon)
      azr=datan2(top,den)
c-----convert to degrees
      del=delr/rad
      az=azr/rad
      if (az.lt.0.0) az=360+az
c-----compute distance in kilometers
      colat=pi2-(alatr+blatr)/2
      radius=6371.227*(1.0+3.37853d-3*(1/3-((dcos(colat))**2)))
cfw      radius=6378.140*(1.0+3.37853d-3*(1/3-((dcos(colat))**2)))
      dist=delr*radius
      return
      end
c Compute distance and azimuth on a sphere
c changes by Felix Waldhauser (fw)

	subroutine delaz2(alat, alon, blat, blon, del, dist, az)

	implicit none

	doubleprecision	alat, alon	! Coordinates of first point
	real		blat, blon	! Coordinates of second point
	real		del		! Sentral angle (degrees)
	real		dist		! Distance (km)
	real		az		! Azimuth from a to b (degrees)

C	Local variables:
	doubleprecision	acol, bcol
	doubleprecision	azr
	doubleprecision	blatr, blonr
	doubleprecision	colat
	doubleprecision	cosdel
	doubleprecision	delr
	doubleprecision	flat
	doubleprecision	geoa
	doubleprecision	geob
	doubleprecision	rad
	doubleprecision	radius
	doubleprecision alatr, alonr
	doubleprecision diflon
	doubleprecision pi2
	doubleprecision tana, tanb
cfw	real		xtop, xden	! see comments below
	doubleprecision top, den

c	Built-in functions: Declarations not needed
	doubleprecision dtan
	doubleprecision	datan
	doubleprecision	dsin
	doubleprecision	dcos
	doubleprecision	dacos

c	doubleprecision top,den

	data pi2/1.570796d0/
	data rad/1.745329d-02/
	data flat/.993231d0/

c-----convert to radians
	alatr=alat*rad
	alonr=alon*rad
	blatr=blat*rad
	blonr=blon*rad
c-----convert latitudes to geocentric colatitudes
	tana=flat*dtan(alatr)
	geoa=datan(tana)
	acol=pi2-geoa
	tanb=flat*dtan(blatr)
	geob=datan(tanb)
	bcol=pi2-geob
c-----calculate delta
	diflon=blonr-alonr
	cosdel=dsin(acol)*dsin(bcol)*dcos(diflon)+dcos(acol)*
     &	dcos(bcol)
	delr=dacos(cosdel)
c-----calculate azimuth from a to b

c*****	Note the use of single precision xtop and xden instead
c	of the double precision top and den in the original
c	program.
c*****	Note also the call to atan2 instead of datan2.
c	Both of these changes were made so that dyn.load
c	would work in Splus.  For some reason, the ld command
c	ld -r -d didn't find _d_atan2
c						WLE 10/16/91
cfw	xtop = dsin(diflon)
cfw	xden=(dsin(acol)/dtan(bcol))-dcos(diflon)*dcos(acol)
cfw	azr=atan2(xtop,xden)
c changes reversed: fw 02/05/17
	top = dsin(diflon)
	den=(dsin(acol)/dtan(bcol))-dcos(diflon)*dcos(acol)
	azr=datan2(top,den)

c----- convert to degrees
	del=delr/rad
	az=azr/rad
	if(az.lt.0.0) az=360.+az
c-----compute distance in kilometers
	colat=pi2-(alatr+blatr)/2.d0
cfw the equatorial radius of the Earth is 6378.137 km (IUGG value)
cfw the mean equatorial radius from Bott, 1982, is 6378.140 km 
cfw	radius=6378.163d0*
cfw     & 	(1.d0+3.35278d-3*((1.d0/3.d0)-(dcos(colat)**2)))
	radius=6378.140*(1.0+3.37853d-3*((1/3)-((dcos(colat))**2)))
	dist=delr*radius
	return
c  ***** end of subroutine delaz *****
	end
c Compute travel time, etc., for direct (upward-departing) ray

	subroutine direct1(nl,v,vsq,thk,jl,tkj,delta,depth,tdir,u,x)

	implicit none

c	Parameters:
	integer	nl	! Number of layers		(input)
	real	v(nl)	! Layer wave speeds		(input)
	real	vsq(nl)	! Squares of wave speeds	(input)
	real	thk(nl)	! Layer thicknesses		(input)
	integer	jl	! Event layer			(input)
	real	tkj	! Event depth within layer jl	(input)
	real	delta	! Epicentral distance		(input)
	real	depth	! Event depth			(input)
	real	tdir	! Direct-ray travel time	(output)
	real	u	! Sine of take-off angle	(output)
	real	x	! Horizontal travel distance in event layer (output)

c       For the direct seismic ray from an event to a receiver in
c  a layered velocity structure, direct predicts the travel time, the
c  sine of the takeoff angle, and the horizontal distance of travel in
c  the event layer.  The receiver must be located at the top of layer
c  1 and the event must be located below layer 1.  Low velocity
c  layers are permitted.
c       To find the takeoff angle of the ray, a numerical approach
c  is required.  The basic scheme adopted here is the method of false
c  position.  (see acton, 1970, 'numerical methods that work,' for
c  example.)  First, the characteristics of the fastest layer
c  between the event and the surface are determined.  These permit
c  placing definite lower and upper bounds, ua and ub, on the
c  sine of the takeoff angle.  In turn, lower and upper bounds, xa
c  and xb, on the horizontal travel distance in the event layer are
c  determined.  The total horizontal travel distance for a ray with
c  with horizontal travel distance x in the event layer is denoted
c  by del, and the zero of del - delta is obtained by using xa and
c  xb as initial guesses for x in the method of false position
c  from x and tkj, the depth of the event below the top of the event
c  layer, the sine of the takeoff angle, u , is calculated.
c       From u and x, tdir is found by summing the travel time in
c  each layer.  finally, a slight correction to tdir is made, based
c  on the misfit between the final del and delta.

c	Local variables:
	real		del		! Computed distance
	real		dela, delb	! Distances corresponding to xa, xb
	doubleprecision	hypot		! Hypoteneuse function
	integer		j1
	integer		kount
	integer		l
	integer		lmax
	real		r
	real		tklmax
	real		usq
	real		ua, uasq
	real		ub, ubsq
	real		ubdiv
	real		vlmax
	real		xa, xb		! Bounds on x
	real		xtest

	if (jl .eq. 1) then
c	   Focus in surface layer
	   r = hypot(depth, delta)
	   tdir = r/v(1)
	   u = delta/r
	   x = delta
	   return
	endif

c     Find the fastest layer, lmax, above and including jl
      lmax = jl
      tklmax = tkj
      vlmax = v(jl)
      j1 = jl-1
      do 23184 l=1,j1
         if (.not.(v(l).gt.vlmax)) goto 23186
            lmax = l
            tklmax = thk(l)
            vlmax = v(l)
23186    continue
23184 continue

C CHANGE BY E.KISSLING MARCH 1984
      IF(tklmax.le.0.05) tklmax = 0.05

c     Find initial bounds on the sine of the takeoff angle
      ua = (v(jl)/vlmax)*delta/sqrt(delta**2+depth**2)
      ub = (v(jl)/vlmax)*delta/sqrt(delta**2+tklmax**2)

c     Calculate horizontal travel distances
      uasq = ua**2
      ubsq = ub**2
C CHANGE BY E.KISSLING MARCH 1984
      if (ubsq.ge.1.) ubsq = 0.99999
      if (uasq.ge.1.) uasq = 0.99999
      xa = tkj*ua/sqrt(1.0-uasq)
      if (.not.(lmax.eq.jl)) goto 23188
         xb = delta
         goto 23189
23188 continue
      xb = tkj*ub/sqrt(1.0-ubsq)
23189 continue
      dela = xa
      delb = xb
      do 23190 l=1,j1
         dela = dela+thk(l)*ua/sqrt(vsq(jl)/vsq(l)-uasq)
         ubdiv = sqrt(vsq(jl)/vsq(l)-ubsq)
         if (ubdiv.GT.1.e-20) GOTO 1002
c	    No write statements for Splus!
            ubdiv = 1.e-20
 1002    continue
         delb = delb+thk(l)*ub/sqrt(vsq(jl)/vsq(l)-ubsq)
23190 continue

c     Loop to find the zero of del-delta by the method of false position
      do 23192 kount=1,25
         if (.not.((delb-dela).lt.0.02)) goto 23194
            x = 0.5*(xa+xb)
            u = x/sqrt(x**2+tkj**2)
            usq = u**2
            goto 23193	! break
23194    continue
         x = xa+(delta-dela)*(xb-xa)/(delb-dela)
         u = x/sqrt(x**2+tkj**2)
         usq = u**2
         del = x
         do 23196 l=1,j1
            del = del+thk(l)*u/sqrt(vsq(jl)/vsq(l)-usq)
23196    continue
         xtest = del-delta
         if (abs(xtest).lt.0.02) goto 23193	! break
         if (.not.(xtest.lt.0.0)) goto 23200
            xa = x
            dela = del
            goto 23201
23200    continue
            xb = x
            delb = del
23201    continue
23192 continue
23193 continue

c     Calculate direct-ray travel time
      tdir = sqrt(x**2+tkj**2)/v(jl)
      do 23202 l=1,j1
         tdir = tdir+thk(l)*v(jl)/(vsq(l)*sqrt(vsq(jl)/vsq(l)-usq))
23202 continue
      tdir = tdir-(u/v(jl))*(del-delta)
      return
c  ***** end of subroutine direct1 *****
      end
c Convert latitude and longitude to kilometers relative
c to center of coordinates by short distance conversion.

	subroutine dist(xlat, xlon, xkm, ykm)

	implicit none

c	Parameters:
	doubleprecision	xlat, xlon	! (input)
	real		xkm, ykm	! (output)

c	Local variables:
	doubleprecision lat1, lat2, lat3
	real	q
	real	xx
	real	yp

	include "geocoord.inc"

c Set up short distance conversion by subr. SETORG
      q=60*xlat-olat
      yp=q+olat
      lat1=datan(rlatc*dtan(RAD*yp/60.0))
      lat2=datan(rlatc*dtan(RAD*OLAT/60.0))
      LAT3=(LAT2+LAT1)/2.
      xx=60*xlon-olon  !  - wegen LON E
      q=q*aa
      xx = xx*bb*dcos(LAT3)
      IF(rotate.ne.0.) then
c** rotate coordinate system anticlockwise
        yp=cost*q+sint*xx
        xx=cost*xx-sint*q
        q=yp
      ENDIF

      xkm=xx
      ykm=q

      return
      end
	subroutine dtres(log, ndt, stdim, nsrc,
     &	dt_dt, dt_idx,
     &	dt_ista, dt_ic1, dt_ic2,
     &	src_cusp, src_t, tmp_ttp, tmp_tts,
     &	dt_cal, dt_res)

	implicit none

	include'hypoDD.inc'

c	Parameters:
	integer		log		! Log-file identifier
	integer		ndt		! No. of data
	integer		stdim		! Column dimenson of arrays tmp_tt[ps]
	integer		nsrc		! No. of sources
	real		dt_dt(MAXDATA)	! [1..ndt] Observed time differences
	integer		dt_idx(MAXDATA)	! [1..ndt]
	integer		dt_ista(MAXDATA)! [1..ndt] Station indices
	integer		dt_ic1(MAXDATA)	! [1..ndt] Event indices
	integer		dt_ic2(MAXDATA)	! [1..ndt] Event indices
	integer		src_cusp(MAXEVE)! [1..nsrc] Event keys
	real		src_t(MAXEVE)	! [1..nsrc] Event times
	real		tmp_ttp(stdim,MAXEVE)! [1.., 1..nsrc]
	real		tmp_tts(stdim,MAXEVE)! [1.., 1..nsrc]
	real		dt_cal(MAXDATA)	! [1..ndt] Theoretical time differences
	real		dt_res(MAXDATA)	! [1..ndt] Time-difference residuals

c	Local variables:
	integer		i
	real		tt1, tt2

      write(log,'("~ getting residual vector...")')

      if (nsrc.eq.1) then
c        Single source
         do i=1,ndt
            dt_res(i) = dt_dt(i)
         enddo
      else
c        Mulitple sources
	 tt1 = 0.0
	 tt2 = 0.0
         do i=1,ndt
            if (dt_idx(i).eq.1 .or. dt_idx(i).eq.3) then
c              P phase
               tt1 = tmp_ttp(dt_ista(i),dt_ic1(i)) -
     &              src_t(dt_ic1(i))/1000
               tt2 = tmp_ttp(dt_ista(i),dt_ic2(i)) -
     &              src_t(dt_ic2(i))/1000
            elseif (dt_idx(i).eq.2 .or. dt_idx(i).eq.4) then
c              S phase
               tt1 = tmp_tts(dt_ista(i),dt_ic1(i)) -
     &              src_t(dt_ic1(i))/1000
               tt2 = tmp_tts(dt_ista(i),dt_ic2(i)) -
     &              src_t(dt_ic2(i))/1000
            endif
            if (tt1.eq.0 .or. tt2.eq.0) then
               write(*,'("FATAL ERROR (theor tt). Please report to ",
     &                   "felix@andreas.wr.usgs.gov")')
               stop
            endif
            dt_cal(i) = tt1 - tt2
            dt_res(i) = dt_dt(i) - dt_cal(i)
         enddo
      endif

      end !of subroutine dtres
	subroutine exist(fn)

	implicit none

c	Parameters:
	character	fn*80

c	Local variables
	logical	ex

      inquire(FILE=fn,exist=ex)
      if (.not.ex) then
          write(*,'("FILE DOES NOT EXIST / CHECK IDATA,IPHASE: ",a)')fn
          stop
      endif
      end
c Find a free fortran i/o unit-number

	subroutine FREEUNIT(iunit)

	implicit none

c	Parameters:
	integer	iunit

c	Local variables:
	logical	lopen

      do iunit=10,999
         if(iunit.eq.999) stop'FREEUNIT>>> no free unit found!'
         inquire(unit=iunit,opened=lopen)
         if(.not.lopen) RETURN
      enddo
      RETURN
      end ! of subr. freeunit
c Read in data

	subroutine getdata(
     &	log, fn_cc, fn_ct, fn_sta, fn_eve, fn_srcpar,
     &	idata, iphase, ncusp, icusp,
     &  maxdist,maxsep_ct,maxsep_cc,
     &	noisef_dt, mod_nl, mod_ratio, mod_v, mod_top,
     &	ev_date, ev_time, ev_cusp, ev_lat, ev_lon, ev_dep,
     &	ev_mag, ev_herr, ev_zerr, ev_res,
     &	sta_lab, sta_lat, sta_lon,
     &	dt_sta, dt_dt, dt_qual, dt_c1, dt_c2, dt_idx,
     &	dt_ista, dt_ic1, dt_ic2,dt_offs,
     &	nev, nsta, ndt, nccp, nccs, nctp, ncts,
     &	tmp_xp, tmp_yp, tmp_zp, tmp_ttp, tmp_tts)

	implicit none

	include 'hypoDD.inc'

c	Parameters:
	doubleprecision	atoangle	! ASCII-to-angle function
	integer		log
	character*80	fn_cc, fn_ct, fn_sta, fn_eve, fn_srcpar
	integer		idata
	integer		iphase
	integer		ncusp		! No. of events to relocate
	integer		icusp(MAXEVE)	! [1..ncusp] Keys of events to relocate
	real		maxdist
	real		maxsep_ct
	real		maxsep_cc
	real		noisef_dt
	integer		mod_nl
	real		mod_ratio
	real		mod_v(MAXLAY)	! [1..MAXLAY]
	real		mod_top(MAXLAY)	! [1..MAXLAY]
	integer		ev_date(MAXEVE)	! [1..MAXEVE]
	integer		ev_time(MAXEVE)	! [1..MAXEVE]
	integer		ev_cusp(MAXEVE)	! [1..nev] Event keys
	real		ev_lat(MAXEVE)	! [1..nev]
	real		ev_lon(MAXEVE)	! [1..nev]
	real		ev_dep(MAXEVE)	! [1..nev]
	real		ev_mag(MAXEVE)	! [1..nev]
	real		ev_herr(MAXEVE)	! [1..nev]
	real		ev_zerr(MAXEVE)	! [1..nev]
	real		ev_res(MAXEVE)	! [1..nev]
	character	sta_lab(MAXSTA)*7	! [1..MAXSTA]
	real		sta_lat(MAXSTA)	! [1..MAXSTA]
	real		sta_lon(MAXSTA)	! [1..MAXSTA]
	character	dt_sta(MAXDATA)*7	! [1..MAXDATA]
	real		dt_dt(MAXDATA)	! [1..MAXDATA]
	real		dt_qual(MAXDATA)	! [1..MAXDATA]
        real            dt_offs(MAXDATA)   	! [1..MAXDATA] 
	integer		dt_c1(MAXDATA)	! [1..MAXDATA]
	integer		dt_c2(MAXDATA)	! [1..MAXDATA]
	integer		dt_idx(MAXDATA)	! [1..MAXDATA]
	integer		dt_ista(MAXDATA)	! [1..MAXDATA]
	integer		dt_ic1(MAXDATA)	! [1..MAXDATA]
	integer		dt_ic2(MAXDATA)	! [1..MAXDATA]
	integer		nev
	integer		nsta
	integer		ndt
	integer		nccp
	integer		nccs
	integer		nctp
	integer		ncts
	integer		sscanf3		! Formatted string-reading function
	real		tmp_xp(MAXSTA,MAXEVE)! [1..MAXSTA, 1..MAXEVE]
	real		tmp_yp(MAXSTA,MAXEVE)! [1..MAXSTA, 1..MAXEVE]
	real		tmp_zp(MAXSTA,MAXEVE)! [1..MAXSTA, 1..MAXEVE]
	real		tmp_ttp(MAXSTA,MAXEVE)! [1..MAXSTA, 1..MAXEVE]
	real		tmp_tts(MAXSTA,MAXEVE)! [1..MAXSTA, 1..MAXEVE]

c	Local variables:

	real		azim
	character	buf1*20		! Input buffer
	character	buf2*20		! Input buffer
	real		clat
	real		clon
	integer		cusperr(34000)	! [1..nerr] Event keys to not locate
	character	dattim*25
c	doubleprecision	elon(20), elat(20)
	doubleprecision	elon(2000), elat(2000)
	integer		nerr
	real		del
	real		dist
	real		dt1, dt2
	real		dtn
	logical		ex
	integer		i
	integer		ic1
	integer		ic2
	integer		ifindi
	integer		ii
	integer		iicusp(MAXEVE)
	integer		iiotc
	integer		iskip
	integer		iunit
	integer		j
	integer		k
	integer		l
	character	line*200
	real		otc
	character	pha*1
	integer		sta_itmp(MAXSTA)
	character	str1*1
	real		tmp
	integer		trimlen
        real		offs
        real		dlat	
        real	 	dlon	
	integer		k1
	integer		k2
        real 		PI
        parameter       (PI=3.141593)

      call datetime (dattim)
      write (*,'("Reading data ...   ",a)') dattim
      write (log,'(/,"~ Reading data ...   ",a)') dattim

c--Read file with events not to be considered in the relocations
c--At the moment, hypoDD appears to simply warn you that you are trying
c  to relocate events you had marked as bad in a file called 'cuspid.err'
      inquire (FILE='cuspid.err',exist=ex)
      if (.not. ex) then
         nerr = 0
      else
         call freeunit (iunit)
         open (iunit,file='cuspid.err', status='unknown')
         i = 1
5        read (iunit,*,end=6) cusperr(i)
         i = i+1
         goto 5
6        continue
         nerr = i-1
         close(iunit)
      endif

c     Read event file
      call freeunit(iunit)
      open (iunit,file=fn_eve,status='unknown')
      i = 1

c--Begin earthquake read loop
c----------------------------------------------------------
10    read (iunit,'(a)',end=20) line
      read (line,*,err=1010) ev_date(i), ev_time(i), ev_lat(i),
     &   ev_lon(i), ev_dep(i), ev_mag(i), ev_herr(i), ev_zerr(i),
     &   ev_res(i), ev_cusp(i)

      if (ev_date(i).lt.10000000) ev_date(i) = ev_date(i)+19000000

c--If earthquake shallower than 10m, force it to 10m
c--This may not be necessary
      if (ev_dep(i) .lt. 0.01) ev_dep(i) = 1   ! no 0-depth for ttime!!!

c--Check if event is on error list. This appears to be just a warning.
      do j=1,nerr
         if (ev_cusp(i).eq.cusperr(j)) then
            write(*,*) 'NOTE: event in error list:', ev_cusp(i)
            write(log,*) 'NOTE: event in error list:', ev_cusp(i)
            goto 15
         endif
      enddo
15    continue

      if (ncusp.gt.1) then
c        Read selected events, skip others
         do j=1,ncusp
            if (icusp(j).eq.ev_cusp(i)) then
                i = i+1
                goto 16
            endif
         enddo
c        From now on, icusp free for work space
      else
c        Read all events
         i = i+1
      endif

16    continue
      if (i.gt.MAXEVE) stop'>>> Increase MAXEVE in hypoDD.inc.'
      goto 10
c----------------------------------------------------------
c--End earthquake read loop

20    nev = i-1
      write (*,'("# events = ",i5)') nev
      if (ncusp.gt.0 .and. ncusp.ne.nev) then
         write (*,'(//,">>> Events repeated in selection list '//
     &      'or missing/repeated in event file!")')
         do i=1,ncusp
            k = 0
            do j=1,nev
               if (icusp(i).eq.ev_cusp(j)) k = k+1
            enddo
            if (k.eq.0) write(*,*) icusp (i),' is missing.'
            if (k.ge.2) then
                write(*,*) icusp (i),' is non-unique.'  
                stop'Event ID must be unique!'
            endif
         enddo
      endif
      close(iunit)

c--Get center of event cluster
      clat = 0
      clon = 0
      do i=1,nev
         clat = clat + ev_lat(i)
         clon = clon + ev_lon(i)
      enddo
      clat = clat/nev
      clon = clon/nev

c--Read station list
      call freeunit (iunit)
      open (iunit,file=fn_sta,status='unknown')
      i = 1
      ii = 1

30    read (iunit,'(a)',end=40) line

c--Split into fields separated by white space
         if (sscanf3(line, "%s%s%s", sta_lab(i), buf1, buf2).ne.3) then
            write (6,*) line
            stop '** Bad station line'
         endif
         call rpad(sta_lab(i))

c--Convert strings to numbers, interpreting colons, if any.
         sta_lat(i) = atoangle(buf1)
         sta_lon(i) = atoangle(buf2)

c--Skip stations at distances larger than maxdist:
         call delaz(clat, clon, sta_lat(i), sta_lon(i), del, dist, azim)
         if (dist.le.maxdist) i = i+1
         if (i.gt.MAXSTA) then
            write (*,*)'>>> Increase station array dimension (MAXSTA)'//
     &      'in hypoDD.inc or decrease search radius for stations '//
     &      '(maxdist) in hypoDD.inp.'
            stop
         endif
         ii = ii+1
      goto 30

c--We now have read the entire station file
40    nsta = i-1
      write (log,'("# stations total = ",i6,/,
     & "# stations < maxdist = ",i6)')ii-1,nsta
      write (*,'("# stations < maxdist = ",i6)') nsta
      close(iunit)

c--Check for duplicated stations
      do i=1,nsta-1
         do j=i+1,nsta
            if (sta_lab(i).eq.sta_lab(j)) then
               write (*,*)sta_lab(i)
               stop'>>> This station is listed twice in station file.'
            endif
         enddo
      enddo

      if (idata.eq.0) goto 150   	!synthetics

      nccp = 0
      nccs = 0
      nctp = 0
      ncts = 0

c--Read cross-correlation dtimes
      call indexxi(nev,ev_cusp,iicusp)
      do i=1,nev
         icusp(i) = ev_cusp(iicusp(i)) ! icusp is workspace array here
      enddo
      i = 1
      iiotc = 0
      if ((idata.eq.1.or.idata.eq.3).and.trimlen(fn_cc).gt.1) then
         call freeunit(iunit)
         open (iunit,file=fn_cc,status='unknown')
50       read (iunit,'(a)',end=60) line
         if (line(1:1).eq.'#') then
            read (line,*,err=1051) str1, ic1, ic2, otc
            iskip = 0
c	skip event pairs with no origin time correction:
            if (abs(otc + 999).lt.0.001) then
               write (log,*)'No OTC for ', ic1, ic2, '. Pair skiped'
               iiotc = iiotc+1
               iskip = 1
               goto 50
            endif
c	skip event pairs with events not in event list: 
            k1= ifindi(nev,icusp,ic1)
            k2= ifindi(nev,icusp,ic2)
            if(k1.eq.0.or.k2.eq.0) then
               iskip=1 
               goto 50
            endif
c	skip event pairs with large separation distance: 
            dlat= ev_lat(iicusp(k1)) - ev_lat(iicusp(k2))
            dlon= ev_lon(iicusp(k1)) - ev_lon(iicusp(k2))
            offs= sqrt( (dlat*111)**2 +
     &            (dlon*(cos(ev_lat(iicusp(k1))*PI/180)*111))**2 +
     &            (ev_dep(iicusp(k1))-ev_dep(iicusp(k2)))**2)
            if(maxsep_cc.gt.0 .and. offs.gt.maxsep_cc) iskip= 1
            goto 50
         else
c           New format, body...
            if (iskip.eq.1) goto 50
            read (line,*,err=1051) dt_sta(i), dt_dt(i), dt_qual(i), pha
            dt_dt(i) = dt_dt(i) - otc
            dt_c1(i) = ic1
            dt_c2(i) = ic2
         endif

c--Skip far-away stations
         do j=1,nsta
            if (dt_sta(i).eq.sta_lab(j)) goto 58
         enddo
         goto 50

c--Only accept P or S phase codes
58       if (pha.eq.'P') then
            if (iphase.eq.2) goto 50
            dt_idx(i) = 1
            nccp = nccp+1
         elseif (pha.eq.'S') then
            if (iphase.eq.1) goto 50
            dt_idx(i) = 2
            nccs = nccs+1
         else
            stop '>>> Phase identifier format error.'
         endif
         dt_offs(i)= offs

         i = i+1
         if (i.gt.MAXDATA) stop'>>> Increase MAXDATA in hypoDD.inc.'
         goto 50

60       if (iphase.ne.2) then
            write (*,'("# cross corr P dtimes = ",i7,
     &      " (no OTC for",i7," event pairs)")') nccp, iiotc
            write (log,'("# cross corr P dtimes = ",i7,
     &      " (no org. time corr. for",i7," event pairs)")') nccp, iiotc
         endif
         if (iphase.ne.1) then
            write (*,'("# cross corr S dtimes = ",i7,
     &      " (no OTC for",i7," event pairs)")') nccs, iiotc
            write (log,'("# cross corr S dtimes = ",i7,
     &      " (no org. time corr. for",i7," event pairs)")') nccs, iiotc
         endif
         close(iunit)
      endif

      if (i.gt.MAXDATA) stop'>>> Increase MAXDATA in hypoDD.inc.'

c--Read catalog P and S absolute times
      if ((idata.eq.2.or.idata.eq.3) .and.
     &   trimlen(fn_ct).gt.1) then
         call freeunit(iunit)
         open (iunit,file=fn_ct,status='unknown')

90       read (iunit,'(a)',end=100) line
         if (line(1:1).eq.'#') then 	
            read(line,*,err=1091) str1, ic1, ic2

            iskip= 0
c	skip event pairs with events not in event list: 
            k1= ifindi(nev,icusp,ic1)
            k2= ifindi(nev,icusp,ic2)
            if(k1.eq.0.or.k2.eq.0) then
               iskip=1 
               goto 90
            endif
c	skip event pairs with large separation distance: 
            dlat= ev_lat(iicusp(k1)) - ev_lat(iicusp(k2))
            dlon= ev_lon(iicusp(k1)) - ev_lon(iicusp(k2))
            offs= sqrt( (dlat*111)**2 +
     &            (dlon*(cos(ev_lat(iicusp(k1))*PI/180)*111))**2 +
     &            (ev_dep(iicusp(k1))-ev_dep(iicusp(k2)))**2)
            if(maxsep_ct.gt.0 .and. offs.gt.maxsep_ct) iskip= 1
            goto 90
         else
            if (iskip.eq.1) goto 90
            read (line,*,err=1091)dt_sta(i), dt1,dt2, dt_qual(i),pha
            dt_c1(i) = ic1
            dt_c2(i) = ic2
         endif

c--Skip far-away data
         do j=1,nsta
            if (dt_sta(i).eq.sta_lab(j)) goto 95
         enddo
         goto 90

c--Store time difference
95       dt_dt(i) = dt1 - dt2
         if (pha.eq.'P') then
            if (iphase.eq.2) goto 90
            dt_idx(i) = 3
            nctp = nctp+1
         elseif (pha.eq.'S') then
            if (iphase.eq.1) goto 90
            dt_idx(i) = 4
            ncts = ncts+1
         else
            write(*,*)line
            stop '>>> Phase identifier format error.'
         endif
         dt_offs(i)= offs

         i = i+1
         if (i.gt.MAXDATA) stop'>>> Increase MAXDATA in hypoDD.inc.'
         goto 90

100      if (iphase.ne.2) then
             write (*,'("# catalog P dtimes = ",i7)') nctp
             write (log,'("# catalog P dtimes = ",i7)') nctp
         endif
         if (iphase.ne.1) then
             write (*,'("# catalog S dtimes = ",i7)') ncts
             write (log,'("# catalog S dtimes = ",i7)') ncts
         endif
         close(iunit)
      endif

      goto 160   ! jump over synthetics

150   continue
c--Generate synthetics dtimes
      if (nev.gt.20) stop'>>> Increase elon/elat array in getdata!'
c     Copy into double precision
      do i=1,nev
         elon(i) = ev_lon(i)
         elat(i) = ev_lat(i)
      enddo
      call partials (fn_srcpar,
     & nev, ev_cusp, elat, elon, ev_dep,
     & nsta, sta_lab, sta_lat, sta_lon,
     & mod_nl, mod_ratio, mod_v, mod_top,
     & tmp_ttp, tmp_tts,
     & tmp_xp, tmp_yp, tmp_zp)

c--Open synthetic dtime file
      nccp = 0
      nccs = 0
      nctp = 0
      if (iphase.eq.1.or.iphase.eq.3) then
         open(20,file='dtime.P.syn',status='unknown')
         l = 1
         do i=1,nsta
            do j=1,nev
               do k=j+1,nev
                  dt_sta(l) = sta_lab(i)
                  dt_c1(l) = ev_cusp(j)
                  dt_c2(l) = ev_cusp(k)
                  dt_qual(l) = 100
                  dt_idx(l) = 1
                  dt_dt(l) = (tmp_ttp(i,j)-tmp_ttp(i,k))
                  tmp = noisef_dt
                  call ran(-tmp, tmp, dtn)
                  dt_dt(l) = dt_dt(l)+dtn
                  write (20,'(a7,2f15.7,2i4,f9.1,2a)')
     &            dt_sta(l), dt_dt(l), -dt_dt(l),
     &            dt_c1(l), dt_c2(l), dt_qual(l), ' 0'
                  l = l+1
               enddo
            enddo
         enddo
         close(20)
         nccp = l-1
      endif

      if (iphase.eq.2.or.iphase.eq.3) then
         open (21,file='dtime.S.syn',status='unknown')
         l = 1
         do i=1,nsta
            do j=1,nev
               do k=j+1,nev
                  dt_sta(l) = sta_lab(i)
                  dt_c1(l) = ev_cusp(j)
                  dt_c2(l) = ev_cusp(k)
                  dt_qual(l) = 100
                  dt_idx(l) = 2
                  dt_dt(l) = (tmp_tts(i,j)-tmp_tts(i,k))
                  tmp = noisef_dt
                  call ran(-tmp, tmp, dtn)
                  dt_dt(l) = dt_dt(l)+dtn
                  write (21,'(a7,2f15.7,2i4,f9.1,2a)')
     &            dt_sta(l), dt_dt(l), -dt_dt(l),
     &            dt_c1(l), dt_c2(l), dt_qual(l), ' 0'
                  l = l+1
               enddo
            enddo
         enddo
         close(21)
         nccs = l-1
      endif

      write (*,'("# synthetic P dtimes ",i6)') nccp
      write (*,'("# synthetic S dtimes ",i6)') nccs
      write (log,'("# synthetic P dtimes ",i6)') nccp
      write (log,'("# synthetic S dtimes ",i6)') nccs
      if (nccp+nccs.gt.MAXDATA)
     &   stop '>>> Increase MAXDATA in hypoDD.inc'

160   ndt = nccp+nccs+nctp+ncts
      write (*,'("# dtimes total = ",i8)') ndt
      write (log,'("# dtimes total = ",i8)') ndt
      if (ndt.gt.MAXDATA) stop'>>> Increase MAXDATA in hypoDD.inc.'
      if (ndt.eq.0) stop

c--Clean events: dtime match
      do i=1,ndt
         dt_ic1(i) = dt_c1(i)   !dt_ic1 is just a workspace array here!
         dt_ic2(i) = dt_c2(i)   !dt_ic2 is just a workspace array here!
      enddo
      call sorti (ndt, dt_ic1)
      call sorti (ndt, dt_ic2)
      k = 1
      do i=1,nev
         if (ifindi(ndt, dt_ic1, ev_cusp(i)).gt.0) goto 174
         if (ifindi(ndt, dt_ic2, ev_cusp(i)).eq.0) goto 175
174      ev_date(k) = ev_date(i)
         ev_time(k) = ev_time(i)
         ev_lat(k) = ev_lat(i)
         ev_lon(k) = ev_lon(i)
         ev_dep(k) = ev_dep(i)
         ev_mag(k) = ev_mag(i)
         ev_herr(k) = ev_herr(i)
         ev_zerr(k) = ev_zerr(i)
         ev_res(k) = ev_res(i)
         ev_cusp(k) = ev_cusp(i)
         k = k+1
175      continue
      enddo
      nev = k-1
      write (*,'("# events after dtime match = ",i10)') nev
      write (log,'("# events after dtime match = ",i10)') nev

c--New & fast: clean stations
      do i=1,nsta
         sta_itmp(i) = 0
      enddo
      do j=1,ndt
         do i=1,nsta
            if (dt_sta(j).eq.sta_lab(i)) then
               sta_itmp(i) = 1
               goto 176
            endif
         enddo
176      continue
      enddo
      k = 1

      do i=1,nsta
         if (sta_itmp(i).eq.1) then
            sta_lab(k) = sta_lab(i)
            sta_lat(k) = sta_lat(i)
            sta_lon(k) = sta_lon(i)
            k = k+1
            goto 177
         endif
177      continue
      enddo

      nsta = k-1
      write(*,'("# stations = ",i6)') nsta
      write(log,'("# stations = ",i6)') nsta

c--New & fast indexing station labels and cuspids
      call indexxi (nev, ev_cusp, iicusp)
      do i=1,nev
         icusp(i) = ev_cusp(iicusp(i)) !icusp is just a workspace array here!
      enddo
      do i=1,ndt
        do j=1,nsta
            if (dt_sta(i).eq.sta_lab(j)) then
               dt_ista(i) = j
               dt_ic1(i) = iicusp(ifindi(nev, icusp, dt_c1(i)))
               dt_ic2(i) = iicusp(ifindi(nev, icusp, dt_c2(i)))
               goto 200
            endif
        enddo
        stop'FATAL ERROR (indexing). Please report to felix'
200     continue
      enddo
      return

c--Error processing
1010  write (*,*) '** Bad earthquake data, so stop:'
      write (*,*) line
      stop

1051  write (*,'(">>> Format error in cross data file,",/,
     & "OR no origin time corrections ",
     & "available for combined use of cat and cross data.")')
       write (*,*) line
       stop 'Program aborted.'

1091   write(*,*)'>>> Format error in catalog data file.'
       write(*,*)line
       stop 'Program aborted.'


      end  ! of subroutine getdata
c Get input parameters

	subroutine getinp (maxev,maxlyr,log,fn_inp,
     &	fn_cc, fn_ct, fn_sta, fn_eve,
     &	fn_loc, fn_reloc, fn_res, fn_stares, fn_srcpar,
     &	idata, iphase,
     &	minobs_cc, minobs_ct,
     &	amaxres_cross, amaxres_net, amaxdcc, amaxdct,
     &	noisef_dt, maxdist,
     &	awt_ccp, awt_ccs, awt_ctp, awt_cts, adamp,
     &	istart, maxiter, isolv, niter, aiter,
     &	mod_nl, mod_ratio, mod_v, mod_top,
     &	iclust, ncusp, icusp)

	implicit none

c	Parameters:
	integer		maxev		! Array dimension
	integer		maxlyr		! Array dimension
	integer		log		! Log-file identifier
	character	fn_inp*80	! File of control info.
	character	fn_cc*80	! File of cross-corr. times
	character	fn_ct*80	! File of catalog times
	character	fn_sta*80	! Station file
	character	fn_eve*80	! Event file
	character	fn_loc*80	! Output file of original locs.
	character	fn_reloc*80	! Output file of final locs.
	character	fn_res*80	! Output residual file
	character	fn_stares*80	! Output station file
	character	fn_srcpar*80	! Output source-parameter file
	integer		idata		! 0: Synthetics
					! 1: Cross-correlation
					! 2: Catalog
					! 3: Both
	integer		iphase		! 1: P; 2: S; 3: Both
	integer		minobs_cc	! Min. obs./pair for ccor. data
	integer		minobs_ct	! Min. obs./pair for cat. data
	real		amaxres_cross(10)! [1..niter] Ccor. res. thresh.
	real		amaxres_net(10)	! [1..niter] Cat. res. thresh.
	real		amaxdcc(10)	! [1..niter] Ccor. link-dist. limit
	real		amaxdct(10)	! [1..niter] Cat. link-dist. limit
	real		noisef_dt	! Synthetic noise
	real		maxdist		! Max. cluster-station distance
	real		awt_ccp(10)	! [1..niter] Wts. for ccor. P
	real		awt_ccs(10)	! [1..niter] Wts. for ccor. S
	real		awt_ctp(10)	! [1..niter] Wts. for cat. P
	real		awt_cts(10)	! [1..niter] Wts. for cat. S
	real		adamp(10)	! [1..niter] Damping (lsqr only)
	integer		istart		! 1: From single source
					! 2: From network sources
	integer		maxiter
	integer		isolv		! 1: SVD; 2: LSQR
	integer		niter		! No. of iteration sets
	integer		aiter(0:10)	! [1..niter] Iterations/set
	integer		mod_nl		! No. of layers
	real		mod_ratio	! Vp/Vs
	real		mod_v(maxlyr)	! [1..mod_nl] Vp values
	real		mod_top(maxlyr)	! [1..mod_nl] Depths to layers
	integer		iclust		! Cluster to relocate (0: all).
	integer		ncusp		! No. of event keys in icusp[]
	integer		icusp(maxev)	! [1..ncusp] Events to relocate

c	Local variables:
	integer		fu_inp
	integer		i
	integer		ii
	integer		l
	character	line*200
	integer		trimlen

c--- newest format: 083000 with iteration step dependent weighting
c-- open input file:
      call freeunit (fu_inp)
      open (fu_inp,status='unknown',file=fn_inp,err=998)
      ncusp= 0
      niter= 0  ! number of iteration blocks
      l = 1
      ii= 1

c-- Loop to read each parameter lines, skipping comments
 210  read (fu_inp,'(a)',end=220) line
      if (line(1:1).eq.'*' .or. line(2:2).eq.'*') goto 210
      if (l.eq.1) read (line,'(a)',err=999) fn_cc
      if (l.eq.2) read (line,'(a)',err=999) fn_ct
      if (l.eq.3) read (line,'(a)',err=999) fn_eve
      if (l.eq.4) read (line,'(a)',err=999) fn_sta
      if (l.eq.5) read (line,'(a)',err=999) fn_loc
      if (l.eq.6) read (line,'(a)',err=999) fn_reloc
      if (l.eq.7) read (line,'(a)',err=999) fn_stares
      if (l.eq.8) read (line,'(a)',err=999) fn_res
      if (l.eq.9) read (line,'(a)',err=999) fn_srcpar
      if (l.eq.10) read (line,*,err=999) idata, iphase,maxdist
      if (l.eq.11) read (line,*,err=999) minobs_cc,minobs_ct
      if (l.eq.12) read (line,*,err=999) istart, isolv, niter

c--Read iteration instructions
      if (l.ge.13 .and. l.le.12+niter) then
         i=l-12
         read (line,*,err=999) aiter(i),
     &  awt_ccp(i), awt_ccs(i), amaxres_cross(i), amaxdcc(i),
     &  awt_ctp(i), awt_cts(i), amaxres_net(i), amaxdct(i), adamp(i)
      endif

      if (l.eq.13+niter) then
        read(line,*,err=999) mod_nl, mod_ratio
      endif
      if (l.eq.14+niter) read(line,*,err=999) (mod_top(i),i=1,mod_nl)
      if (l.eq.15+niter) read(line,*,err=999) (mod_v(i),i=1,mod_nl)

c--Read specific clusters/events to relocate
      if (l.eq.16+niter) read (line,*,err=999) iclust
      if (l.ge.17+niter) then
         read (line,*,err=999,end=230) (icusp(i),i=ii,ii+7)
230      ii= i
      endif
      l= l+1
      goto 210
220   close (fu_inp)
      ncusp= ii-1

c- rearrange aiter:
      do i=2,niter
        aiter(i)= aiter(i-1)+aiter(i)
      enddo

c- check files
      call exist (fn_eve)
      call exist (fn_sta)
      if ((idata.eq.1 .or.idata.eq.3).and.trimlen(fn_cc).gt.1)
     & call exist(fn_cc)
      if ((idata.eq.2 .or.idata.eq.3).and.trimlen(fn_ct).gt.1)
     & call exist (fn_ct)

      maxiter= aiter(niter)
c synthetic noise:
      noisef_dt= 0.002

c write log output: of newest format
600   if (trimlen(fn_loc).lt.2) fn_loc= 'hypoDD.loc'
      if (trimlen(fn_reloc).lt.2) fn_reloc= 'hypoDD.reloc'
      write (6,'("INPUT FILES:",/,
     &"cross dtime data: ",a,/,"catalog dtime data: ",a,/,
     &"events: ",a,/,"stations: ",a,/,"OUTPUT FILES:",/,
     &"initial locations: ",a,/,"relocated events: ",a,/,
     &"event pair residuals: ",a,/,"station residuals: ",a,/,
     &"source parameters: ",a)')
     &fn_cc(1:trimlen(fn_cc)),
     &fn_ct(1:trimlen(fn_ct)),
     &fn_eve(1:trimlen(fn_eve)),
     &fn_sta(1:trimlen(fn_sta)),fn_loc(1:trimlen(fn_loc)),
     &fn_reloc(1:trimlen(fn_reloc)),fn_res(1:trimlen(fn_res)),
     &fn_stares(1:trimlen(fn_stares)),fn_srcpar(1:trimlen(fn_srcpar))

      write (log,'("Input parameters: (from ",a,")",/,
     &"  cross dtime file: ",a,/,"  catalog dtime file: ",a,/,
     &"  station file: ",a,/,"  event file: ",a,/,
     &"  initial locations: ",a,/,"  relocated events: ",a)')
     &fn_inp(1:trimlen(fn_inp)),
     &fn_cc(1:trimlen(fn_cc)),
     &fn_ct(1:trimlen(fn_ct)),
     &fn_sta(1:trimlen(fn_sta)),
     &fn_eve(1:trimlen(fn_eve)),fn_loc(1:trimlen(fn_loc)),
     &fn_reloc(1:trimlen(fn_reloc))

      write (log,'(
     &"  event pair file: ",a,/,"  station residual file: ",a,/,
     &"  source parameter file: ",a,/,
     &"  IDATA= ",i2,2X,"IPHASE= ",i2,2x,"MAXDIST= ",f5.0,/,
     &"  MINOBS_CC= ",i3,2x,"MINOBS_CT= ",i3,/,"  ISTART= ",i1,2x,
     &"ISOLV= ",i1,2x)')
     &fn_res(1:trimlen(fn_res)),
     &fn_stares(1:trimlen(fn_stares)),fn_srcpar(1:trimlen(fn_srcpar)),
     &idata,iphase,maxdist,minobs_cc,minobs_ct,istart,isolv

      aiter(0)=0
      write (log, '("  ITER ",i2,"-",i2,
     &": DAMP= "f5.1,/,"    WT_CCP= ",f7.4,2X,"WT_CCS= ",f7.4,2x,
     &"MAXR_CC= ",f7.4,2X,"MAXD_CC= ",f7.2,2X,/,
     &"    WT_CTP= ",f7.4,2x,"WT_CTS= ",f7.4,2x,"MAXR_CT= ",f7.4,2x,
     &"MAXD_CT= ",f7.2)')
     &(aiter(i-1)+1,aiter(i),adamp(i),awt_ccp(i),awt_ccs(i),
     & amaxres_cross(i),
     & amaxdcc(i),awt_ctp(i),awt_cts(i), amaxres_net(i), amaxdct(i),
     & i=1,niter)

c--Write crust model
      write (log, '("  MOD_NL= ",i2,2x,"MOD_RATIO= ",f4.2)')
     & mod_nl,mod_ratio
      write(log,'("    MOD_TOP    MOD_V")')
      do i=1,mod_nl
          write(log,'(2x,2f9.3)')mod_top(i),mod_v(i)
      enddo

c--Repeat number of clusters, events to relocate
      if (iclust.eq.0) then
        write (*,*) 'Relocate all clusters'
        write (log,*) 'Relocate all clusters'
      else
        write (*,*) 'Relocate cluster number ',iclust
        write (log,*) 'Relocate cluster number ',iclust
      end if

      if (ncusp.eq.0) then
        write (*,*) 'Relocate all events'
        write (log,*) 'Relocate all events'
      else
        write (*,*) 'Relocate ',ncusp,' events'
        write (log,*) 'Relocate ',ncusp,' events'
      end if
      return

c--Input error handling
998   write(*,*)'>>> ERROR OPENING CONTROL PARAMETER FILE'
      goto 1000

999   write (*,*)'>>> ERROR READING CONTROL PARAMETERS IN LINE ',l
      write (*,*) line
1000  stop 'Program run aborted.'
      end  ! of subroutine getinp
c Find specified value in ordered integer vector

	integer function ifindi(n, ia, iv)

	implicit none

c	Parameters:
	integer		n
	integer		ia(n)	! [1..n] Vector to search
	integer		iv	! Value to find

c	Local variables:
	integer		i
	integer		k	! 2^(no. of chops)

      if (n.le.0) then
         ifindi = 0
         return
      endif

      if (iv.lt.ia(1) .or. iv.gt.ia(n)) then
c        Outside range of vector
         ifindi=0
         return
      endif

      k = 2
      i = nint(real(n)/k)
10    if (k.gt.2*n) then
c        Value not in vector
         ifindi = 0
         return
      endif
      k = k*2
      if (iv.lt.ia(i)) then
c        Value smaller:  Search below
         i = i-nint(real(n)/k)
         goto 10
      endif
      if (iv.gt.ia(i)) then
c        Value larger:  Search above
         i = i+nint(real(n)/k)
         goto 10
      endif

c     Value found: iv == ia[i]
      ifindi = i
      return
      end
	subroutine indexxi(n, iarrin, indx)

	implicit none

	integer	n
	integer	iarrin(n)
	integer	indx(n)

	integer	i
	integer	indxt
	integer	ir
	integer	j
	integer	l
	integer	q

      if (n.lt.1) then
         return
      else if (n.eq.1) then
         indx(1) = 1
         return
      endif

      do 11 j=1,n
         indx(j) = j
11    continue
      l = n/2+1
      ir = n
10    continue
         if (l.gt.1) then
            l = l-1
            indxt = indx(l)
            q = iarrin(indxt)
         else
            indxt = indx(ir)
            q = iarrin(indxt)
            indx(ir) = indx(1)
            ir = ir-1
            if (ir.eq.1) then
               indx(1) = indxt
               return
            endif
         endif
         i = l
         j = l+l
20       if (j.le.ir) then
            if (j.lt.ir) then
               if (iarrin(indx(j)).lt.iarrin(indx(j+1))) j = j+1
            endif
            if (q.lt.iarrin(indx(j))) then
               indx(i) = indx(j)
               i = j
               j = j+j
            else
               j = ir+1
            endif
         go to 20
         endif
         indx(i) = indxt
      go to 10
      end
c Convert calendar date & time to minutes

	integer function juliam(iyr, imo, idy, ihr, imn)

	implicit none

c	Parameters:
	integer	iyr, imo, idy, ihr, imn		! (input)
						! iyr < 4000 for 32-bit int

c	Local variables:
	integer	kl
	integer	kmo(12)
	integer	ky, km, kd
	integer	ky0
	integer	ky1
	integer	ky4
	integer	l
	integer	leap

	data kmo/0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334/
	data leap/1/

      ky = iyr
      km = imo
      kd = idy
      if (km.le.0) km = 1
      juliam = 365*ky
      kd = kmo(km)+kd
      ky4 = ky/4
      ky1 = ky/100
      ky0 = ky/1000
      kl = leap*(ky4-ky1+ky0)
      l = 0
      if (ky4*4.eq.ky.and.(ky1*100.ne.ky.or.ky0*1000.eq.ky)) l = leap
      if (l.ne.0.and.km.lt.3) kl = kl-leap
      juliam = juliam+kd+kl
      juliam = juliam*24+ihr
      juliam = juliam*60+imn
      return
      end ! of integer function juliam
	subroutine lsfit_lsqr(log, iter, ndt, nev, nsrc,
     &	damp, mod_ratio,
     &	idata, ev_cusp, src_cusp,
     &	dt_res, dt_wt,
     &	dt_ista, dt_ic1, dt_ic2,
     &	src_dx, src_dy, src_dz, src_dt, src_ex, src_ey, src_ez, src_et,
     &	exav, eyav, ezav, etav, dxav, dyav, dzav, dtav,
     &	rms_cc, rms_ct, rms_cc0, rms_ct0,
     &	rms_ccold, rms_ctold, rms_cc0old, rms_ct0old,
     &	tmp_xp, tmp_yp, tmp_zp, dt_idx, acond)

	implicit none

	include'hypoDD.inc'

c	Parameters:
	integer		log		! Log-file identifier
	integer		iter
	integer		ndt
	integer		nev
	integer		nsrc
	real		damp
	real		mod_ratio
	integer		idata
	integer		ev_cusp(MAXEVE)	! [1..nev] Event keys
	integer		src_cusp(MAXEVE)! [1..nev] Event keys
	real		dt_res(MAXDATA)	! [1..ndt]
	real		dt_wt(MAXDATA)	! [1..ndt]
	integer		dt_ista(MAXDATA)! [1..ndt]
	integer		dt_ic1(MAXDATA)	! [1..ndt]
	integer		dt_ic2(MAXDATA)	! [1..ndt]
	real		src_dx(MAXEVE)	! [1..nev]
	real		src_dy(MAXEVE)	! [1..nev]
	real		src_dz(MAXEVE)	! [1..nev]
	real		src_dt(MAXEVE)	! [1..nev]
	real		src_ex(MAXEVE)	! [1..nev]
	real		src_ey(MAXEVE)	! [1..nev]
	real		src_ez(MAXEVE)	! [1..nev]
	real		src_et(MAXEVE)	! [1..nev]
	real		exav
	real		eyav
	real		ezav
	real		etav
	real		dxav
	real		dyav
	real		dzav
	real		dtav
	real		rms_cc
	real		rms_ct
	real		rms_cc0
	real		rms_ct0
	real		tmp_xp(MAXSTA,MAXEVE)! [1.., 1..nev]
	real		tmp_yp(MAXSTA,MAXEVE)! [1.., 1..nev]
	real		tmp_zp(MAXSTA,MAXEVE)! [1.., 1..nev]
	integer		dt_idx(MAXDATA)	! [1..ndt]
	real		acond		! Condition number

c	Local variables:
	real		anorm
	real		arnorm
	real		atol
	real		btol
	real		conlim
	character	dattim*25
	real		d(MAXDATA+4)	! Data vector
	real		dtavold
	real		dxavold
	real		dyavold
	real		dzavold
	real		etavold
	real		exavold
	real		eyavold
	real		ezavold
	real		factor
	integer		i
	integer		istop
	integer		itnlim
	integer		iw(2*(8*MAXDATA+4*MAXEVE)+1)	! lsqr index array
	integer		k1
	integer		k2
	integer		leniw
	integer		lenrw
	integer		m
	integer		n
	integer		nar
	integer		nndt
	real		norm(MAXEVE*4)
	real		norm_test(MAXEVE*4)
	real		resvar1
	real		rms_cc0old
	real		rms_ccold
	real		rms_ct0old
	real		rms_ctold
	real		rnorm
	real		rw(8*MAXDATA+4*MAXEVE)
	real		se(MAXEVE*4)	! Solution error
	real		w1(MAXEVE*4)	! Work space
	real		w2(MAXEVE*4)	! Work space
	real		wtinv(MAXDATA+4)! +4 = mean shift constr
	real		wt(MAXDATA+4)
	real		x(MAXEVE*4)	! Solution vector
	real		xnorm

      write(log,'(/,"~ setting up G matrix.. ")')

c     If mean shift not contstrained
      nar = 8*ndt
      nndt = ndt

      iw(1) = nar

c     Prepare sparse data and design vector
      do i=1,ndt
c        Weight data first
         wt(i) = dt_wt(i)
         if (wt(i).ne.0) then
            wtinv(i) = 1.0/wt(i)
         else
            wtinv(i) = 1.0
         endif
         d(i) = dt_res(i)*1000.0 * wt(i)
         iw(1+i) = i
         iw(1+ndt+i) = i
         iw(1+2*ndt+i) = i
         iw(1+3*ndt+i) = i
         iw(1+4*ndt+i) = i
         iw(1+5*ndt+i) = i
         iw(1+6*ndt+i) = i
         iw(1+7*ndt+i) = i

c        Set up non-zero G matrix elements and apply weights
         if (nsrc.eq.1) then
            k1 = 1
            k2 = 1
         else
            k1 = dt_ic1(i)
            k2 = dt_ic2(i)
         endif
         if (dt_idx(i).eq.2 .or. dt_idx(i).eq.4) then
            rw(i)       = tmp_xp(dt_ista(i),k1) * wt(i) * mod_ratio
            rw(ndt+i)   = tmp_yp(dt_ista(i),k1) * wt(i) * mod_ratio
            rw(2*ndt+i) = tmp_zp(dt_ista(i),k1) * wt(i) * mod_ratio
            rw(3*ndt+i) = wt(i)
            rw(4*ndt+i) = -tmp_xp(dt_ista(i),k2) * wt(i) * mod_ratio
            rw(5*ndt+i) = -tmp_yp(dt_ista(i),k2) * wt(i) * mod_ratio
            rw(6*ndt+i) = -tmp_zp(dt_ista(i),k2) * wt(i) * mod_ratio
            rw(7*ndt+i) = -wt(i)
         else
            rw(i)       = tmp_xp(dt_ista(i),k1) * wt(i)
            rw(ndt+i)   = tmp_yp(dt_ista(i),k1) * wt(i)
            rw(2*ndt+i) = tmp_zp(dt_ista(i),k1) * wt(i)
            rw(3*ndt+i) = wt(i)
            rw(4*ndt+i) = -tmp_xp(dt_ista(i),k2) * wt(i)
            rw(5*ndt+i) = -tmp_yp(dt_ista(i),k2) * wt(i)
            rw(6*ndt+i) = -tmp_zp(dt_ista(i),k2) * wt(i)
            rw(7*ndt+i) = -wt(i)
         endif

c        Set up column indexes with non-zero elements
         iw(1+nar+      i) = 4*dt_ic1(i) - 3
         iw(1+nar+  ndt+i) = 4*dt_ic1(i) - 2
         iw(1+nar+2*ndt+i) = 4*dt_ic1(i) - 1
         iw(1+nar+3*ndt+i) = 4*dt_ic1(i)
         iw(1+nar+4*ndt+i) = 4*dt_ic2(i) - 3
         iw(1+nar+5*ndt+i) = 4*dt_ic2(i) - 2
         iw(1+nar+6*ndt+i) = 4*dt_ic2(i) - 1
         iw(1+nar+7*ndt+i) = 4*dt_ic2(i)

      enddo

c     Scale G matrix so the L2 norm of each column is 1.
      write(log,'("~ scaling G columns ... ")')

c     G array scaling
      do i=1,4*nev
         norm(i) = 0.0
      enddo
      do i=1,nar
         norm(iw(1+nar+i)) = norm(iw(1+nar+i)) + rw(i)**2
      enddo
      do i=1,nev*4
         norm(i) = sqrt(norm(i)/nndt)
      enddo
      do i=1,nar
         rw(i) = rw(i) / norm(iw(1+nar+i))
      enddo

c     Testing...
      do i=1,4*nev
         norm_test(i) = 0.0
      enddo
      do i=1,nar
         norm_test(iw(1+nar+i)) = norm_test(iw(1+nar+i)) + rw(i)**2
      enddo
      do i=1,nev*4
         norm_test(i) = sqrt(norm_test(i)/nndt)
         if (abs(norm_test(i)-1).gt.0.001) then
            write(*,'("FATAL ERROR (lsqr: G scaling). Please report to ",   
     &                "felix@andreas.wr.usgs.gov")')
            stop
         endif
      enddo

c     Least square fitting using the algorithm of
c     Paige and Saunders, acm-trans. math. software, vol.8, no. 2,
c     jun., 1982, p. 195.

c     Set up input parameter first
      m = nndt
      n = nev*4
      leniw = 2*nar+1
      lenrw = nar
      do i= 1,n
         w1(i) = 0.0
         w2(i) = 0.0
         x(i) = 0.0
         se(i) = 0.0
      enddo
      atol = 0.000001
      btol = 0.000001
      conlim = 100000.0
      itnlim = 100*n
      istop = 0
      anorm = 0.0
      acond = 0.0
      rnorm = 0.0
      arnorm = 0.0
      xnorm = 0.0

      call datetime(dattim)
      write(log,'("~ lsqr ...    ", a)') dattim

c d= data vector; w1,w2 = workspace; x= solution vector; se=solution error
      call lsqr(m, n, damp, 
     & leniw, lenrw, iw, rw, 
     & d, w1, w2, x, se, 
     & atol, btol, conlim, itnlim, 
     & istop, anorm, acond, rnorm, arnorm, xnorm)

      write(log,'("  istop = ",i1,"; acond (CND)=",f8.1,"; anorm =",f8.1,
     & "; arnorm =",f8.1,"; xnorm =",f8.1)')
     & istop, acond, anorm, arnorm, xnorm

      if (nsrc.eq.1) nsrc = nev

c     Rescale model vector
      do i=1,4*nev
         x(i) = x(i) / norm(i)
         se(i) = se(i) / norm(i)
      enddo

c     Unweight and rescale G matrix
      do i=1,ndt
         rw(i)       = rw(i)       * wtinv(i) * norm(iw(1+nar+i))
         rw(ndt+i)   = rw(ndt+i)   * wtinv(i) * norm(iw(1+nar+ndt+i))
         rw(2*ndt+i) = rw(2*ndt+i) * wtinv(i) * norm(iw(1+nar+2*ndt+i))
         rw(3*ndt+i) = rw(3*ndt+i) * wtinv(i) * norm(iw(1+nar+3*ndt+i))
         rw(4*ndt+i) = rw(4*ndt+i) * wtinv(i) * norm(iw(1+nar+4*ndt+i))
         rw(5*ndt+i) = rw(5*ndt+i) * wtinv(i) * norm(iw(1+nar+5*ndt+i))
         rw(6*ndt+i) = rw(6*ndt+i) * wtinv(i) * norm(iw(1+nar+6*ndt+i))
         rw(7*ndt+i) = rw(7*ndt+i) * wtinv(i) * norm(iw(1+nar+7*ndt+i))
      enddo

c     Compute residuals from d = G*x
      do i=1,ndt
         d(i) = -dt_res(i)*1000.0
      enddo
      call aprod(1, m, n, x, d, leniw, lenrw, iw, rw)
      do i=1,ndt
         dt_res(i) = -d(i)/1000.0
      enddo

c     Get residual statistics (avrg, rms, var..)
      call resstat(log, idata, ndt, nev, dt_res, wt, dt_idx, 
     & rms_cc, rms_ct, rms_cc0, rms_ct0, 
     & rms_ccold, rms_ctold, rms_cc0old, rms_ct0old, 
     &             resvar1)


c     Scale errors
c The standard error estimates returned by LSQR increase monotonically
c with the iterations.  If LSQR shuts down early because of loose tolerances,
c or because the rhs-vector is special, the estimates will be too small.
c (I think they are most likely to be accurate if the rhs is random.)
c
c Remember that se(j) is covariance(j) / (m - n)
c where m - n = 1000000.  I've never quite understood why we
c divide by that number.

c Errors for the 95% confidence level,
c thus multiply the standard errors by 2.7955
      factor = 2.7955

c     Store solution and errors
      do i=1,nev
         src_dx(i) = -x(4*i-3)
         src_dy(i) = -x(4*i-2)
         src_dz(i) = -x(4*i-1)
         src_dt(i) = -x(4*i)
         src_cusp(i) = ev_cusp(i)

c        Take weighted variance
         src_ex(i) = sqrt(se(4*i-3)) * sqrt(resvar1) *factor
         src_ey(i) = sqrt(se(4*i-2)) * sqrt(resvar1) *factor
         src_ez(i) = sqrt(se(4*i-1)) * sqrt(resvar1) *factor
         src_et(i) = sqrt(se(4*i))   * sqrt(resvar1) *factor
      enddo

c     Get average errors and vector changes
      exavold = exav
      eyavold = eyav
      ezavold = ezav
      etavold = etav
      dxavold = dxav
      dyavold = dyav
      dzavold = dzav
      dtavold = dtav
      exav = 0.0
      eyav = 0.0
      ezav = 0.0
      etav = 0.0
      dxav = 0.0
      dyav = 0.0
      dzav = 0.0
      do i=1,nev
         exav = exav + src_ex(i)
         eyav = eyav + src_ey(i)
         ezav = ezav + src_ez(i)
         etav = etav + src_et(i)
         dxav = dxav + abs(src_dx(i))
         dyav = dyav + abs(src_dy(i))
         dzav = dzav + abs(src_dz(i))
         dtav = dtav + abs(src_dt(i))
      enddo
      exav = exav/nev
      eyav = eyav/nev
      ezav = ezav/nev
      etav = etav/nev
      dxav = dxav/nev
      dyav = dyav/nev
      dzav = dzav/nev
      dtav = dtav/nev

      if (iter.eq.1) then
         exavold = exav
         eyavold = eyav
         ezavold = ezav
         etavold = etav
         dxavold = dxav
         dyavold = dyav
         dzavold = dzav
         dtavold = dtav
      endif

c     Output location statistics
      write(log,'(/,"Location summary:")')
      write(log,'(
     & " mean 2sig-error (x,y,z,t) [m,ms]: ",/,f7.1,f7.1,f7.1,f7.1,
     & " (",f7.1,f7.1,f7.1,f7.1")",/,
     & " mean shift (x,y,z,t) [m,ms] (DX,DY,DZ,DT): ",/,
     & f7.1,f7.1,f7.1,f7.1," (",f7.1,f7.1,f7.1,f7.1,")")')
     & exav, eyav, ezav, etav, exav-exavold, eyav-eyavold, 
     & ezav-ezavold, etav-etavold, 
     & dxav, dyav, dzav, dtav, dxav-dxavold, dyav-dyavold, 
     & dzav-dzavold, dtav-dtavold

      end !of subroutine lsfit_lsqr



	subroutine lsfit_svd(log, iter, ndt, nev, nsrc, damp, mod_ratio,
     &	idata, ev_cusp, src_cusp,
     &	dt_res, dt_wt,
     &	dt_ista, dt_ic1, dt_ic2,   
     &	src_dx, src_dy, src_dz, src_dt, src_ex, src_ey, src_ez, src_et,
     &	exav, eyav, ezav, etav, dxav, dyav, dzav, dtav,
     &	rms_cc, rms_ct, rms_cc0, rms_ct0,
     &	rms_ccold, rms_ctold, rms_cc0old, rms_ct0old,
     &	tmp_xp, tmp_yp, tmp_zp, dt_idx)

	implicit none

	include 'hypoDD.inc'

c	Parameters:
	integer		log
	integer		iter
	integer		ndt
	integer		nev
	integer		nsrc
	real		damp
	real		mod_ratio
	integer		idata
	integer		ev_cusp(MAXEVE)	! (1..MAXEVE)
	integer		src_cusp(MAXEVE)	! (1..MAXEVE)
	real		dt_res(MAXDATA)	! (1..MAXDATA)
	real		dt_wt(MAXDATA)	! (1..MAXDATA)
	integer		dt_ista(MAXDATA)	! (1..MAXDATA)
	integer		dt_ic1(MAXDATA)	! (1..MAXDATA)
	integer		dt_ic2(MAXDATA)	! (1..MAXDATA)
	real		src_dx(MAXEVE)	! (1..MAXEVE)
	real		src_dy(MAXEVE)	! (1..MAXEVE)
	real		src_dz(MAXEVE)	! (1..MAXEVE)
	real		src_dt(MAXEVE)	! (1..MAXEVE)
	real		src_ex(MAXEVE)	! (1..MAXEVE)
	real		src_ey(MAXEVE)	! (1..MAXEVE)
	real		src_ez(MAXEVE)	! (1..MAXEVE)
	real		src_et(MAXEVE)	! (1..MAXEVE)
	real		exav
	real		eyav
	real		ezav
	real		etav
	real		dxav
	real		dyav
	real		dzav
	real		dtav
	real		rms_cc
	real		rms_ct
	real		rms_cc0
	real		rms_ct0
	real		rms_ccold
	real		rms_ctold
	real		rms_cc0old
	real		rms_ct0old
	real		tmp_xp(MAXSTA,MAXEVE)! (1..maxsta, 1..MAXEVE)
	real		tmp_yp(MAXSTA,MAXEVE)! (1..maxsta, 1..MAXEVE)
	real		tmp_zp(MAXSTA,MAXEVE)! (1..maxsta, 1..MAXEVE)
	integer		dt_idx(MAXDATA)	! (1..MAXDATA)

c	Local	variables:
	real		cvm(MAXEVE0*4,MAXEVE0*4)
	real		dd(MAXDATA0)
	real		d(MAXDATA0)
	real		dxavold, dyavold, dzavold, dtavold
	real		exavold, eyavold, ezavold, etavold
	real		factor
	doubleprecision		g(MAXDATA0,MAXEVE0*4)
	integer		i, j
	integer		izero
	integer		k1, k2
	integer		nndt
	real		norm(MAXEVE*4)
	real		norm_test(MAXEVE*4)
	doubleprecision		q(MAXEVE0*4)
	real		qmin, qmax
	real		resvar1
	real		s
	real		se(MAXEVE0*4)
	real		tmp(MAXEVE0*4)
	doubleprecision		u(MAXDATA0,MAXEVE0*4)
	doubleprecision		v(MAXEVE0*4,MAXEVE0*4)
	real		wtinv(MAXDATA0)
	real		wt(MAXDATA0)
	real		x(MAXEVE0*4)

      if (ndt.gt.MAXDATA0-4) stop'>>> Increase MAXDATA0 in hypoDD.inc.'
      if (nev.gt.MAXEVE0) stop'>>> Increase MAXEVE0 in hypoDD.inc.'

c     SVD
c     Set up full G matrix
      do i=1,ndt
         do j=1,nev*4
            g(i,j)= 0
         enddo
      enddo

      do i=1,ndt
         if (nsrc.eq.1) then
            k1 = 1
            k2 = 1
         else
            k1 = dt_ic1(i)
            k2 = dt_ic2(i)
         endif
         if (dt_idx(i).eq.2 .or. dt_idx(i).eq.4) then
           g(i,dt_ic1(i)*4-3) = tmp_xp(dt_ista(i),k1)*mod_ratio
           g(i,dt_ic1(i)*4-2) = tmp_yp(dt_ista(i),k1)*mod_ratio
           g(i,dt_ic1(i)*4-1) = tmp_zp(dt_ista(i),k1)*mod_ratio
           g(i,dt_ic1(i)*4) = 1.0
           g(i,dt_ic2(i)*4-3) = -tmp_xp(dt_ista(i),k2)*mod_ratio
           g(i,dt_ic2(i)*4-2) = -tmp_yp(dt_ista(i),k2)*mod_ratio
           g(i,dt_ic2(i)*4-1) = -tmp_zp(dt_ista(i),k2)*mod_ratio
           g(i,dt_ic2(i)*4) = -1.0
         else
           g(i,dt_ic1(i)*4-3) = tmp_xp(dt_ista(i),k1)
           g(i,dt_ic1(i)*4-2) = tmp_yp(dt_ista(i),k1)
           g(i,dt_ic1(i)*4-1) = tmp_zp(dt_ista(i),k1)
           g(i,dt_ic1(i)*4) = 1.0
           g(i,dt_ic2(i)*4-3) = -tmp_xp(dt_ista(i),k2)
           g(i,dt_ic2(i)*4-2) = -tmp_yp(dt_ista(i),k2)
           g(i,dt_ic2(i)*4-1) = -tmp_zp(dt_ista(i),k2)
           g(i,dt_ic2(i)*4) = -1.0
         endif
      enddo

c     Weight data
      do i=1,ndt
c        wt[] must be same as dt_wt[], but with different dimensions, and
c        should not be changed so statistics in resstat will not be screwed up!
         wt(i) = dt_wt(i)
         if (wt(i).ne.0) then
           wtinv(i) = 1.0/wt(i)
         else
           wtinv(i) = 1.0
         endif
         d(i) = dt_res(i)*1000.0 * wt(i) ! data in ms, so results are in m
      enddo

c     Weight G matrix
      call matmult3(MAXDATA0,MAXEVE0*4,ndt,nev*4,wt,g)

c     Add four extra rows to make mean shift zero.
c     This should make the design matrix non-singular.
      do i=1,4
         d(ndt+i) = 0.0
         wt(ndt+i) = 1.0
         do j=1,nev
            g(ndt+i,j*4-4+i) = 1.0
            g(ndt+i,j*4-3+i) = 0.0
            g(ndt+i,j*4-2+i) = 0.0
            g(ndt+i,j*4-1+i) = 0.0
         enddo
      enddo
      nndt = ndt+4

c     Column scaling
      do j=1,4*nev
         norm(j) = 0
      enddo
      do j=1,4*nev
         do i=1,nndt
            norm(j) = norm(j) + g(i,j)**2
         enddo
      enddo
      do j=1,nev*4
         norm(j) = sqrt(norm(j)/nndt)
      enddo
      do j=1,nev*4
         do i=1,nndt
            g(i,j) =  g(i,j) / norm(j)
         enddo
      enddo

c     Testing...
      do j=1,4*nev
         norm_test(j) = 0
      enddo
      do j=1,4*nev
         do i=1,nndt
            norm_test(j) = norm_test(j) + g(i,j)**2
         enddo
      enddo
      do j=1,nev*4
         norm_test(j) = sqrt(norm_test(j)/nndt)
         if (abs(norm_test(j)-1).gt.0.001) then
            write(*,'("FATAL ERROR (svd: G scaling). Please report to ",
     &                "felix@andreas.wr.usgs.gov")')
            stop
         endif
      enddo

c     Do singular-value decomposition
      write(log,'("~ singular value decomposition of G (",i6,
     & "x",i6," matrix) ...")') nndt,nev*4
      call SVD(MAXDATA0,MAXEVE0*4,nndt,nev*4,g,u,v,q,1)

c     Check for singular values close to zero
      write(log,'("~ backsubstitution ...")')
      izero = 0
      qmax = 0.0
      do i=1,nev*4
         if (q(i).lt.0) then
            write(*,'("FATAL ERROR (svd: neg sing val). Please report ",
     &                "to felix@andreas.wr.usgs.gov")')
            stop
          endif
         if (q(i).gt.qmax) qmax = q(i)
      enddo
      qmin = qmax * 0.0000001
      do i=1,nev*4
         if (q(i).lt.qmin) then
            q(i) = 0.0
            izero = izero+1
         endif
      enddo
      if (izero.gt.0)then
         write(*,'(/,">>> ",i3," singular values close/= zero !!",/)')
     &   izero
         write(log,'(/,">>> ",i3," singular values close/= zero !!",/)')
     &   izero
      endif

c     Back substitution (get x' from Gx=d: x=v*diag(1/q)*t(u)*d))

c     Compute diag(1/q)*t(U)*d
      do j=1,nev*4
        s = 0.0
        if (q(j).ne.0) then
           do i=1,nndt
              s = s+u(i,j)*d(i)
           enddo
           s = s/q(j)
        endif
        tmp(j) = s
      enddo

c     Multiply by V
      do i=1,nev*4
        s = 0.0
        do j=1,nev*4
           s = s+v(i,j)*tmp(j)
        enddo
        x(i) = s
      enddo

c     Rescale model vector and G
      do j=1,4*nev
         x(j) = x(j) / norm(j)
         do i=1,ndt
           g(i,j) = g(i,j) * norm(j)
         enddo
      enddo

c     Unweight G matrix
      call matmult3(MAXDATA0,MAXEVE0*4,ndt,nev*4,wtinv,g)

c     Predict data dd = G*x', get residuals (sec)
      call matmult2(MAXDATA0,ndt,nev*4,g,x,dd)
      do i=1,ndt
        dt_res(i) = dt_res(i) - dd(i)/1000
      enddo

c     Get covariance matrix: cvm = v*(1/q**2)*vt
      call covar(MAXEVE0,v,nev*4,q,cvm)

c     Get residual statistics (avrg,rms,var..)
c     At this point, wt must be = dt_wt; just different dimension to save
c     memory for SVD runs (see above).
      call resstat(log,idata,ndt,nev,dt_res,dt_wt,dt_idx,
     & rms_cc,rms_ct,rms_cc0,rms_ct0,
     & rms_ccold,rms_ctold,rms_cc0old,rms_ct0old,
     &             resvar1)

c     Errors for the 95% confidence level,
c     thus multiply the standard errors by 2.7955 
c100710 Not sure anymore where this number comes from. It should be 1.96 
c100710 assuming a t-distribution of the residuals. This means that errors were 
c100710 overestimated by about 40%. see emails by Hilary Martens May, 2010.
c100710      factor = 2.7955
      factor = 1.96 
      do i=1,nev*4
         se(i) = sqrt(cvm(i,i))*sqrt(resvar1)*factor  ! Weighted variance
      enddo

c     Rescale errors
      do i=1,nev*4
         se(i) = se(i) / norm(i)
      enddo

c     Store solution and errors
      do i=1,nev
         src_dx(i) = -x(4*i-3)
         src_dy(i) = -x(4*i-2)
         src_dz(i) = -x(4*i-1)
         src_dt(i) = -x(4*i)
         src_ex(i) = se(4*i-3)
         src_ey(i) = se(4*i-2)
         src_ez(i) = se(4*i-1)
         src_et(i) = se(4*i)
         src_cusp(i) = ev_cusp(i)
      enddo

c     Output statistics....
c     Get average errors and vector changes
      exavold = exav
      eyavold = eyav
      ezavold = ezav
      etavold = etav
      dxavold = dxav
      dyavold = dyav
      dzavold = dzav
      dtavold = dtav
      exav = 0.0
      eyav = 0.0
      ezav = 0.0
      etav = 0.0
      dxav = 0.0
      dyav = 0.0
      dzav = 0.0
      dtav = 0.0
      do i=1,nev
         exav = exav + src_ex(i)
         eyav = eyav + src_ey(i)
         ezav = ezav + src_ez(i)
         etav = etav + src_et(i)
         dxav = dxav + abs(src_dx(i))
         dyav = dyav + abs(src_dy(i))
         dzav = dzav + abs(src_dz(i))
         dtav = dtav + abs(src_dt(i))
      enddo
      exav = exav/nev
      eyav = eyav/nev
      ezav = ezav/nev
      etav = etav/nev
      dxav = dxav/nev
      dyav = dyav/nev
      dzav = dzav/nev
      dtav = dtav/nev

      if (iter.eq.1) then
         exavold = exav
         eyavold = eyav
         ezavold = ezav
         etavold = etav
         dxavold = dxav
         dyavold = dyav
         dzavold = dzav
         dtavold = dtav
      endif
      write(log,'(/,"Location summary:",/,
     & "  mean 2sig-error (x,y,z,t) [m,ms]: ",/,f7.1,f7.1,f7.1,f7.1,
     & " (",f7.1,f7.1,f7.1,f7.1")",/,
     & "  mean shift (x,y,z,t) [m,ms] (DX,DY,DZ,DT): ",/,
     & f7.1,f7.1,f7.1,f7.1," (",f7.1,f7.1,f7.1,f7.1,")")')
     & exav,eyav,ezav,etav,exav-exavold,eyav-eyavold,
     & ezav-ezavold,etav-etavold,
     & dxav,dyav,dzav,dtav,dxav-dxavold,dyav-dyavold,
     & dzav-dzavold,dtav-dtavold

      end !of subroutine lsfit_SVD
c     algorithm 583, collected algorithms from acm.
c     algorithm appeared in acm-trans. math. software, vol.8, no. 2,
c     jun., 1982, p. 195.

c	Splus callable version			may 27, 1997 wle

	subroutine lsqr(m, n, damp,
     1	                leniw, lenrw, iw, rw,
     2	                u, v, w, x, se,
     3	                atol, btol, conlim, itnlim,
     4	                istop, anorm, acond, rnorm, arnorm, xnorm)

	implicit none

c	Parameters:
	integer	m
	integer	n
	real	damp
	integer	leniw, lenrw
	integer	iw(leniw)
	real	rw(lenrw)
	real	u(m), v(n), w(n)
	real	x(n)
	real	se(n)
	real	atol, btol
	real	conlim
	integer	itnlim
	integer	istop
	real	anorm
	real	acond
	real	rnorm
	real	arnorm
	real	xnorm

c     ------------------------------------------------------------------
c
c     lsqr  finds a solution  x  to the following problems...
c
c     1. unsymmetric equations --    solve  a*x = b
c
c     2. linear least squares  --    solve  a*x = b
c                                    in the least-squares sense
c
c     3. damped least squares  --    solve  (   a    )*x = ( b )
c                                           ( damp*i )     ( 0 )
c                                    in the least-squares sense
c
c     where  a  is a matrix with  m  rows and  n  columns, b  is an
c     m-vector, and  damp  is a scalar (all quantities real).
c     the matrix  a  is intended to be large and sparse.  it is accessed
c     by means of subroutine calls of the form
c
c                call aprod( mode,m,n,x,y,leniw,lenrw,iw,rw )
c
c     which must perform the following functions...
c
c                if mode = 1, compute  y = y + a*x.
c                if mode = 2, compute  x = x + a(transpose)*y.
c
c     the vectors x and y are input parameters in both cases.
c     if mode = 1, y should be altered without changing x.
c     if mode = 2, x should be altered without changing y.
c     the parameters leniw, lenrw, iw, rw may be used for workspace
c     as described below.
c
c     the rhs vector  b  is input via  u,  and subsequently overwritten.
c
c
c     note.  lsqr uses an iterative method to approximate the solution.
c     the number of iterations required to reach a certain accuracy
c     depends strongly on the scaling of the problem.  poor scaling of
c     the rows or columns of  a  should therefore be avoided where
c     possible.
c
c     for example, in problem 1 the solution is unaltered by
c     row-scaling.  if a row of  a  is very small or large compared to
c     the other rows of  a,  the corresponding row of  ( a  b )  should
c     be scaled up or down.
c
c     in problems 1 and 2, the solution  x  is easily recovered
c     following column-scaling.  in the absence of better information,
c     the nonzero columns of  a  should be scaled so that they all have
c     the same euclidean norm (e.g.  1.0).
c
c     in problem 3, there is no freedom to re-scale if  damp  is
c     nonzero.  however, the value of  damp  should be assigned only
c     after attention has been paid to the scaling of  a.
c
c     the parameter  damp  is intended to help regularize
c     ill-conditioned systems, by preventing the true solution from
c     being very large.  another aid to regularization is provided by
c     the parameter  acond,  which may be used to terminate iterations
c     before the computed solution becomes very large.
c
c
c     notation
c     --------
c
c     the following quantities are used in discussing the subroutine
c     parameters...
c
c     abar   =  (   a    ),          bbar  =  ( b )
c               ( damp*i )                    ( 0 )
c
c     r      =  b  -  a*x,           rbar  =  bbar  -  abar*x
c
c     rnorm  =  sqrt( norm(r)**2  +  damp**2 * norm(x)**2 )
c            =  norm( rbar )
c
c     relpr  =  the relative precision of floating-point arithmetic
c               on the machine being used.  for example, on the ibm 370,
c               relpr  is about 1.0e-6 and 1.0d-16 in single and double
c               precision respectively.
c
c     lsqr  minimizes the function  rnorm  with respect to  x.
c
c
c     parameters
c     ----------
c
c     m       input      the number of rows in  a.
c
c     n       input      the number of columns in  a.
c
c     damp    input      the damping parameter for problem 3 above.
c                        (damp  should be 0.0 for problems 1 and 2.)
c                        if the system  a*x = b  is incompatible, values
c                        of  damp  in the range 0 to sqrt(relpr)*norm(a)
c                        will probably have a negligible effect.
c                        larger values of  damp  will tend to decrease
c                        the norm of  x  and to reduce the number of
c                        iterations required by lsqr.
c
c                        the work per iteration and the storage needed
c                        by lsqr are the same for all values of  damp.
c
c     leniw   input      the length of the workspace array  iw.
c     lenrw   input      the length of the workspace array  rw.
c     iw      workspace  an integer array of length  leniw.
c     rw      workspace  a real array of length  lenrw.
c
c             note.  lsqr does not explicitly use the previous four
c             parameters, but passes them to subroutine aprod for
c             possible use as workspace.  if aprod does not need
c             iw  or  rw,  the values  leniw = 1  or  lenrw = 1  should
c             be used, and the actual parameters corresponding to
c             iw  or  rw  may be any convenient array of suitable type.
c
c     u(m)    input      the rhs vector  b.  beware that  u  is
c                        over-written by lsqr.
c
c     v(n)    workspace
c     w(n)    workspace
c
c     x(n)    output     returns the computed solution  x.
c
c     se(n)   output     returns standard error estimates for the
c                        components of  x.  for each  i,  se(i)  is set
c                        to the value  rnorm * sqrt( sigma(i,i) / t ),
c                        where  sigma(i,i)  is an estimate of the i-th
c                        diagonal of the inverse of abar(transpose)*abar
c                        and  t = 1      if  m .le. n,
c                             t = m - n  if  m .gt. n  and  damp = 0,
c                             t = m      if  damp .ne. 0.
c
c     atol    input      an estimate of the relative error in the data
c                        defining the matrix  a.  for example,
c                        if  a  is accurate to about 6 digits, set
c                        atol = 1.0e-6 .
c
c     btol    input      an estimate of the relative error in the data
c                        defining the rhs vector  b.  for example,
c                        if  b  is accurate to about 6 digits, set
c                        btol = 1.0e-6 .
c
c     conlim  input      an upper limit on  cond(abar),  the apparent
c                        condition number of the matrix  abar.
c                        iterations will be terminated if a computed
c                        estimate of  cond(abar)  exceeds  conlim.
c                        this is intended to prevent certain small or
c                        zero singular values of  a  or  abar  from
c                        coming into effect and causing unwanted growth
c                        in the computed solution.
c
c                        conlim  and  damp  may be used separately or
c                        together to regularize ill-conditioned systems.
c
c                        normally,  conlim  should be in the range
c                        1000  to  1/relpr.
c                        suggested value --
c                        conlim = 1/(100*relpr)  for compatible systems,
c                        conlim = 1/(10*sqrt(relpr))  for least squares.
c
c             note.  if the user is not concerned about the parameters
c             atol, btol  and  conlim,  any or all of them may be set
c             to zero.  the effect will be the same as the values
c             relpr, relpr  and  1/relpr  respectively.
c
c     itnlim  input      an upper limit on the number of iterations.
c                        suggested value --
c                        itnlim = n/2     for well conditioned systems,
c                        itnlim = 4*n     otherwise.
c
c     istop   output     an integer giving the reason for termination...
c
c                0       x = 0  is the exact solution.
c                        no iterations were performed.
c
c                1       the equations  a*x = b  are probably
c                        compatible.  norm(a*x - b)  is sufficiently
c                        small, given the values of  atol  and  btol.
c
c                2       the system  a*x = b  is probably not
c                        compatible.  a least-squares solution has
c                        been obtained which is sufficiently accurate,
c                        given the value of  atol.
c
c                3       an estimate of  cond(abar)  has exceeded
c                        conlim.   the system  a*x = b  appears to be
c                        ill-conditioned.  otherwise, there could be an
c                        an error in subroutine aprod.
c
c                4       the equations  a*x = b  are probably
c                        compatible.  norm(a*x - b)  is as small as
c                        seems reasonable on this machine.
c
c                5       the system  a*x = b  is probably not
c                        compatible.  a least-squares solution has
c                        been obtained which is as accurate as seems
c                        reasonable on this machine.
c
c                6       cond(abar)  seems to be so large that there is
c                        not much point in doing further iterations,
c                        given the precision of this machine.
c                        there could be an error in subroutine aprod.
c
c                7       the iteration limit  itnlim  was reached.
c
c     anorm   output     an estimate of the frobenius norm of  abar.
c                        this is the square-root of the sum of squares
c                        of the elements of  abar.
c                        if  damp  is small and if the columns of  a
c                        have all been scaled to have length  1.0,
c                        anorm  should increase to roughly  sqrt(n).
c                        a radically different value for  anorm  may
c                        indicate an error in subroutine aprod (there
c                        may be an inconsistency between modes 1 and 2).
c
c     acond   output     an estimate of  cond(abar),  the condition
c                        number of  abar.  a very high value of  acond
c                        may again indicate an error in aprod.
c
c     rnorm   output     an estimate of the final value of norm(rbar),
c                        the function being minimized (see notation
c                        above).  this will be small if  a*x = b  has
c                        a solution.
c
c     arnorm  output     an estimate of the final value of
c                        norm( abar(transpose)*rbar ), the norm of
c                        the residual for the usual normal equations.
c                        this should be small in all cases.  (arnorm
c                        will often be smaller than the true value
c                        computed from the output vector  x.)
c
c     xnorm   output     an estimate of the norm of the final
c                        solution vector  x.
c
c
c     subroutines and functions used
c     ------------------------------
c
c     user       aprod
c     lsqr       normlz
c     blas       scopy,snrm2,sscal  (see lawson et al. below)
c                (snrm2 is used only in normlz)
c     fortran    abs,mod,sqrt
c
c
c     precision
c     ---------
c
c     the number of iterations required by lsqr will usually decrease
c     if the computation is performed in higher precision.  to convert
c     lsqr  and  normlz  between single- and double-precision, change
c     the words
c                scopy, snrm2, sscal
c                abs, real, sqrt
c     to the appropriate blas and fortran equivalents.
c
c
c     references
c     ----------
c
c     paige, c.c. and saunders, m.a.  lsqr: an algorithm for sparse
c        linear equations and sparse least squares.
c        acm transactions on mathematical software 8, 1 (march 1982).
c
c     lawson, c.l., hanson, r.j., kincaid, d.r. and krogh, f.t.
c        basic linear algebra subprograms for fortran usage.
c        acm transactions on mathematical software 5, 3 (sept 1979),
c        308-323 and 324-325.
c
c
c     lsqr.      this version dated 22 february 1982.
c     ------------------------------------------------------------------

c	Functions and local variables
	real	abs
	real	alfa
	real	bbnorm
	real	beta
	real	bnorm
	real	cs
	real	cs1,cs2
	real	ctol
	real	dampsq
	real	ddnorm
	real	delta
	real	gambar
	real	gamma
	integer	i
	integer	itn
	integer	nconv
	integer	nout
	integer	nstop
	real	one
	real	phi
	real	phibar
	real	psi
	real	res1,res2
	real	rhbar1,rhbar2
	real	rho
	real	rhobar
	real	rhs
	real	rtol
	real	sn
	real	sn1,sn2
	real	sqrt
	real	t
	real	t1,t2,t3
	real	tau
	real	test1,test2,test3
	real	theta
	real	xxnorm
	real	z
	real	zbar
	real	zero

c     initialize.
      zero   = 0.0
      one    = 1.0
      ctol   = zero
      if (conlim .gt. zero) ctol = one/conlim
      dampsq = damp**2
      anorm  = zero
      acond  = zero
      bbnorm = zero
      ddnorm = zero
      res2   = zero
      xnorm  = zero
      xxnorm = zero
      cs2    = -one
      sn2    = zero
      z      = zero
      itn    = 0
      istop  = 0
      nstop  = 0

      do 10 i = 1, n
         v(i) = zero
         x(i) = zero
        se(i) = zero
   10 continue

c     set up the first vectors for the bidiagonalization.
c     these satisfy   beta*u = b,   alfa*v = a(transpose)*u.

      call normlz( m,u,beta )
      call aprod ( 2,m,n,v,u,leniw,lenrw,iw,rw )
      call normlz( n,v,alfa )
      call scopy ( n,v,1,w,1 )

      rhobar = alfa
      phibar = beta
      bnorm  = beta
      rnorm  = beta
      arnorm = alfa*beta
      if (arnorm .le. zero) go to 800
      if (nout   .le.  0  ) go to 100
      test1  = one
      test2  = alfa/beta

c     ------------------------------------------------------------------
c     main iteration loop.
c     ------------------------------------------------------------------
  100 itn = itn + 1

c     perform the next step of the bidiagonalization to obtain the
c     next  beta, u, alfa, v.  these satisfy the relations
c                beta*u  =  a*v  -  alfa*u,
c                alfa*v  =  a(transpose)*u  -  beta*v.

      call sscal ( m,(-alfa),u,1 )
      call aprod ( 1,m,n,v,u,leniw,lenrw,iw,rw )
      call normlz( m,u,beta )
      bbnorm = bbnorm + alfa**2 + beta**2 + dampsq
      call sscal ( n,(-beta),v,1 )
      call aprod ( 2,m,n,v,u,leniw,lenrw,iw,rw )
      call normlz( n,v,alfa )

c     use a plane rotation to eliminate the damping parameter.
c     this alters the diagonal (rhobar) of the lower-bidiagonal matrix.

      rhbar2 = rhobar**2 + dampsq
      rhbar1 = sqrt(rhbar2)
      cs1    = rhobar/rhbar1
      sn1    = damp/rhbar1
      psi    = sn1*phibar
      phibar = cs1*phibar

c     use a plane rotation to eliminate the subdiagonal element (beta)
c     of the lower-bidiagonal matrix, giving an upper-bidiagonal matrix.

      rho    = sqrt(rhbar2 + beta**2)
      cs     = rhbar1/rho
      sn     = beta/rho
      theta  =  sn*alfa
      rhobar = -cs*alfa
      phi    =  cs*phibar
      phibar =  sn*phibar
      tau    =  sn*phi

c     update  x, w  and the standard error estimates.

      t1 =    phi/rho
      t2 = -theta/rho
      t3 =    one/rho

      do 200 i = 1, n
         t     = w(i)
         x(i)  = t1*t + x(i)
         w(i)  = t2*t + v(i)
         t     =(t3*t)**2
         se(i) = t + se(i)
         ddnorm= t + ddnorm
  200 continue

c     use a plane rotation on the right to eliminate the
c     super-diagonal element (theta) of the upper-bidiagonal matrix.
c     then use the result to estimate  norm(x).

      delta  =  sn2*rho
      gambar = -cs2*rho
      rhs    = phi - delta*z
      zbar   = rhs/gambar
      xnorm  = sqrt(xxnorm + zbar**2)
      gamma  = sqrt(gambar**2 + theta**2)
      cs2    = gambar/gamma
      sn2    = theta/gamma
      z      = rhs/gamma
      xxnorm = xxnorm + z**2

c     test for convergence.
c     first, estimate the norm and condition of the matrix  abar,
c     and the norms of  rbar  and  abar(transpose)*rbar.

      anorm  = sqrt(bbnorm)
      acond  = anorm*sqrt(ddnorm)
      res1   = phibar**2
      res2   = res2 + psi**2
      rnorm  = sqrt(res1 + res2)
      arnorm = alfa*abs(tau)

c     now use these norms to estimate certain other quantities,
c     some of which will be small near a solution.

      test1  = rnorm/bnorm
      test2  = arnorm/(anorm*rnorm)
      test3  = one/acond
      t1     = test1/(one + anorm*xnorm/bnorm)
      rtol   = btol +  atol*anorm*xnorm/bnorm

c     the following tests guard against extremely small values of
c     atol, btol  or  ctol.  (the user may have set any or all of
c     the parameters  atol, btol, conlim  to zero.)
c     the effect is equivalent to the normal tests using
c     atol = relpr,  btol = relpr,  conlim = 1/relpr.

      t3 = one + test3
      t2 = one + test2
      t1 = one + t1
      if (itn .ge. itnlim) istop = 7
      if (t3  .le. one   ) istop = 6
      if (t2  .le. one   ) istop = 5
      if (t1  .le. one   ) istop = 4

c     allow for tolerances set by the user.

      if (test3 .le. ctol) istop = 3
      if (test2 .le. atol) istop = 2
      if (test1 .le. rtol) istop = 1
c     ==================================================================

c     stop if appropriate.
c     the convergence criteria are required to be met on  nconv
c     consecutive iterations, where  nconv  is set below.
c     suggested value --   nconv = 1, 2  or  3.

      if (istop .eq. 0) nstop = 0
      if (istop .eq. 0) go to 100
      nconv = 1
      nstop = nstop + 1
      if (nstop .lt. nconv  .and.  itn .lt. itnlim) istop = 0
      if (istop .eq. 0) go to 100
c     ------------------------------------------------------------------
c     end of iteration loop.
c     ------------------------------------------------------------------


c     finish off the standard error estimates.

      t = one
      if (m .gt. n) t = m - n
      if (dampsq .gt. zero) t = m
      t = rnorm/sqrt(t)

      do 700 i = 1, n
         se(i) = t*sqrt(se(i))
  700 continue

c     end of execution

c	set itnlim = itn upon return		6/18/97 wle
	itnlim = itn
  800 return
c     ------------------------------------------------------------------
c     end of lsqr
      end
c Matrix mutliply: c(m,m) = a(m,m)*b(m,m)

	subroutine matmult1(maxm, m, a, b, c)

	implicit none

c	Parameters:
	integer	maxm
	integer	m
	real	a(maxm,maxm)	! (1..maxm, 1..maxm)
	real	b(maxm,maxm)	! (1..maxm, 1..maxm)
	real	c(maxm,maxm)	! (1..maxm, 1..maxm)

c	Local variables:
	integer	j, l, k		! Dummy loop indices
	real	sum

      do j=1,m
         do k=1,m
            sum=0.0
            do l=1,m
               sum=sum+a(j,l)*b(l,k)
            end do
            c(j,k)=sum
         end do
      end do
      return
      end
c Matrix mutliply: c(n) = a(m,n)*b(n)

	subroutine matmult2(maxm, m, n, a, b, c)

	implicit none

c	Parameters:
	integer	maxm
	integer	m
	integer	n
cjh	doubleprecision	a(maxm,n)	! (1..m, 1..n)
        double precision	a(maxm,*)	! (1..m, 1..n)

cjh	real	b(n)		! (1..n)
cjh	real	c(n)		! (1..n)

	real	b(*)		! (1..n)
	real	c(*)		! (1..n)

c	Local variables:
	integer	j, l
	real	sum

      do j=1,m
            sum=0.0
            do l=1,n
               sum=sum+a(j,l)*b(l)
            end do
            c(j)=sum
      end do
      return
      end
c Matrix mutliply: c(m,n) = a(m) * b(m,n)
c overwrites matrix b

	subroutine matmult3(maxm, maxn, m, n, a, b)

	implicit none

c	Parameters:
	integer	maxm
	integer	maxn
	integer	m
	integer	n
	real	a(maxm)
	doubleprecision	b(maxm,maxn)

c	Local variables:
	integer i, j		! Dummy loop indices

      do i=1,m
            do j=1,n
                  b(i,j) = a(i)*b(i,j)
            enddo
      enddo
      return
      end
c Median value in float vector
c Sorts the vector as a side-effect.

	subroutine mdian1(x, n, xmed)

	implicit none

c	Parameters:
	integer	n
	real	x(n)
	real	xmed

c	Local variables:
	integer	n2

      CALL SORT(N,X)
      N2=N/2
      IF(2*N2.EQ.N)THEN
        XMED=0.5*(X(N2)+X(N2+1))
      ELSE
        XMED=X(N2+1)
      ENDIF
      RETURN
      END
c Compute Euclidean norm of float vector, and normalize vector if norm != 0.
c (Required by subroutine lsqr.)

	subroutine normlz(n, x, beta)

	implicit none

c	Parameters:
	integer	n
	real	x(n)
	real	beta	! Euclidean norm of x	(output)

c	Local variables:
	real	one
	real	snrm2	! BLAS function
	real	zero

      zero = 0.0
      one  = 1.0
      beta = snrm2(n, x, 1)
      if (beta .gt. zero) call sscal(n, (one/beta), x, 1)
      return

c     end of normlz
      end
	subroutine partials(fn_srcpar,
     &	nsrc, src_cusp, src_lat, src_lon, src_dep,
     &	nsta, sta_lab, sta_lat, sta_lon,
     &	mod_nl, mod_ratio, mod_v, mod_top,
     &	tmp_ttp, tmp_tts,
     &	tmp_xp, tmp_yp, tmp_zp)

	implicit none

	include'hypoDD.inc'

c	Parameters:
	character	fn_srcpar*80	! Source-parameter file
	integer		nsrc		! No. of sources
	integer		src_cusp(MAXEVE)! [1..nsrc]
	doubleprecision	src_lat(MAXEVE)	! [1..nsrc]
	doubleprecision	src_lon(MAXEVE)	! [1..nsrc]
	real		src_dep(MAXEVE)	! [1..nsrc]
	integer		nsta		! No. of stations
	character	sta_lab(MAXSTA)*7! [1..nsta]
	real		sta_lat(MAXSTA)	! [1..nsta]
	real		sta_lon(MAXSTA)	! [1..nsta]
	integer		mod_nl		! No. of layers
	real		mod_ratio	! Vp/Vs
	real		mod_v(MAXLAY)	! [1..mod_nl]
	real		mod_top(MAXLAY)	! [1..mod_nl]
	real		tmp_ttp(MAXSTA,MAXEVE)! [1..nsta,1..nsrc]
	real		tmp_tts(MAXSTA,MAXEVE)! [1..nsta,1..nsrc]
	real		tmp_xp(MAXSTA,MAXEVE)! [1..nsta,1..nsrc]
	real		tmp_yp(MAXSTA,MAXEVE)! [1..nsta,1..nsrc]
	real		tmp_zp(MAXSTA,MAXEVE)! [1..nsta,1..nsrc]

c	Local variables:
	real		ain
	real		az
	real		del
	real		dist
	integer		i, j, k
	integer		iunit		! Output unit number
	real		pi
	integer		trimlen
        real            vs(MAXLAY)

	parameter(pi=3.141593)

      iunit = 0
      if (trimlen(fn_srcpar).gt.1) then
c        Open source-parameter file
         call freeunit(iunit)
         open(iunit,file=fn_srcpar,status='unknown')
      endif

c     Make sure hypocenters don't fall on layer boundaries
      do i=1,nsrc
         do j=1,mod_nl
            if (abs(src_dep(i)-mod_top(j)).lt.0.0001)
     &         src_dep(i) = src_dep(i)-0.001
         enddo
      enddo

c     Get S velocity model
      do i=1,mod_nl
         vs(i) = mod_v(i)/mod_ratio
      enddo

c     Compute epicentral distances, azimuths, angles of incidence,
c     and P/S-travel times from sources to stations
      do i=1,nsta
         do j=1,nsrc
            call delaz2(src_lat(j), src_lon(j), sta_lat(i), sta_lon(i), 
     &                 del, dist, az)

c           1D ray tracing
            call ttime(dist, src_dep(j), mod_nl, mod_v, mod_top, 
     &                 tmp_ttp(i, j), ain)
            call ttime(dist, src_dep(j), mod_nl, vs, mod_top, 
     &                 tmp_tts(i, j), ain)
            
c           Determine wave speed at the hypocenter
            do k=1,mod_nl
               if (src_dep(j).le.mod_top(k)) goto 10	! break
            enddo
10          continue

c           Depth derivative
            tmp_zp(i,j) = cos((ain * pi)/180.0)/mod_v(k-1)
c           Epicentral derivatives
	    tmp_xp(i,j) = (sin((ain * pi)/180.0) *
     &               cos(((az - 90) * pi)/180.0))/mod_v(k-1)
	    tmp_yp(i,j) = (sin((ain * pi)/180.0) *
     &               cos((az * pi)/180.0))/mod_v(k-1)

c           Write to source-parameter file
            if (iunit .ne. 0)
     &         write(iunit,'(i9,2x,f9.4,2x,f9.4,2x,a7,2x,f9.4,
     &         2x,f9.4,2x,f9.4)')
     &         src_cusp(j), src_lat(j), src_lon(j), sta_lab(i), 
     &         dist, az, ain

         enddo
      enddo

      if (iunit .ne. 0) close(iunit)	! Source-parameter file

      end !of subroutine partials
c Pseudo-random number generator
c Numerical Recipes, p. 274

	subroutine ran(jlo, jhi, j)

	implicit none

c	Parameters:
	real	jlo, jhi	! Limit values (changed)
	real	j

c	Local variables:
	integer	im, ia, ic
	real	jran

	parameter (im= 714025, ia= 4096, ic= 150889)

      jhi=jhi-1.0
      jran= mod(jran*ia+ic,real(im))         ! generator
      j= jlo+((jhi-jlo+1)*jran)/im
      return
      end
c Convert from local Cartesian coordinates to latitude & longitude

	subroutine redist(xkm, ykm, xlat, xlon)

	implicit none

c	Parameters:
	real		xkm, ykm	! km
	doubleprecision	xlat, xlon	! Degrees

	include "geocoord.inc"

c	Local variables:
	real		bcl
	integer		lat, lon
	doubleprecision lat1, lat2, lat3, clat1
	real		p, q
	real		x, y
	real		yp
	real		xx, yy

      xx=xkm
      yy=ykm

c     Rotate coordinates anticlockwise back
      y = yy*cost-xx*sint
      x = yy*sint+xx*cost
      if (abs(aa).lt.0.0000001) goto 900
      q = y/aa
      lat = (q+olat)/60.
      xlat = q+olat - 60.*lat
      yp = 60.*lat+xlat
      lat1 = datan(rlatc*dtan(yp*rad/60.0))
      lat2 = datan(rlatc*dtan(olat*rad/60.))
      lat3 = (lat1+lat2)/2.
      clat1 = dcos(lat3)
      bcl = bb*clat1
      if (abs(bcl).lt.0.000001) goto 900
      p = x/(bb*clat1)
      lon = (p+olon)/60.
      xlon = p+olon - 60.*lon
      xlat = lat+xlat/60.
      xlon = lon+xlon/60.
      return
  900 write(6,1000) aa,bb,clat1
 1000 format(/,2x,' subr. redist: aa=',f10.5,2x,'bb=',f10.5,2x,
     1'cos(lat1)=',f10.7,5x,'division by zero, run stops here',/)
      stop'redist>>> division by zero!!'
      end
c Find "refracted" ray with smallest travel time

	subroutine refract (nl, v, vsq, thk, jl, tkj, delta,
     &	kk, tref, xovmax)

	implicit none

	include "hypoDD.inc"

c	Parameters:
	integer	nl
	real	v(MAXLAY)
	real	vsq(MAXLAY)
	real	thk(MAXLAY)
	integer	jl
	real	tkj
	integer	kk
	real	tref
	real	xovmax

c       For refracted rays in a layered earth model, refract
c  determines the fastest travel time, tref, the layer
c  in which the fastest ray is refracted, kk, the
c  critical distance for refraction in that layer,
c  didjkk, and an upper bound on delta for which a
c  direct ray can be a first arrival, xovmax.  Refract
c  allows for the possibility of low velocity layers.
c       Note that there may not be a refracted ray, either because
c  all layers below the event layer are low velocity layers or
c  because for all layers below the event layer which are not low
c  velocity layers the critical distance exceeds delta.  In such
c  cases tref, didjkk, and xovmax are set very large, kk is set to
c  zero, and refract returns to the calling program.
c
c  input:  nl - number of layers
c        v(l) - velocity of layer l
c      vsq(l) - v(l) ** 2
c      thk(l) - thickness of layer l
c          jl - event layer
c         tkj - depth of event in event layer
c       delta - horizontal distance between event and receiver
c
c  output:   kk - refracting layer for fastest refracted ray
c          tref - travel time of fastest refracted ray
c        didjkk - critical distance for refraction in layer kk
c        xovmax - an upper bound on delta for which the direct ray can
c                       be the first arrival
c  internal arrays:
c
c       tr(m) - travel time for refraction in layer m
c     tinj(m) - traveltime intercept
c      tid(m) - terms in travel time intercept which are
c                     independent of tkj
c     didj(m) - critical distance
c      did(m) - terms in critical distance which are
c                     independent of tkj
c
c
c  Call subroutine tiddid to evaluate tid(m) and
c  did(m), the terms in the travel time intercept and
c  critical distance for a ray refracted in layer m
c  that are independent of tkj.

c	Local variables:
	real	delta
	real	did(20)
	real	didj(20)
	real	j1
	real	jx
	integer	l
	real	lx
	integer	m
	integer	m1
	real	sqt
	real	tid(20)
	real	tim
	real	tinj(20)
	real	tr(20)

c  determine tref, kk, didjkk
      call tiddid(jl,nl,v,vsq,thk,tid,did)
      tref=100000.
      j1=jl+1
      do 23151m=j1,nl
      if(.not.(tid(m).eq.100000.))goto 23153
      tr(m)=100000.
      goto 23154
23153 continue
      sqt=sqrt(vsq(m)-vsq(jl))
      tinj(m)=tid(m)-tkj*sqt/(v(m)*v(jl))
      didj(m)=did(m)-tkj*v(jl)/sqt
      tr(m)=tinj(m)+delta/v(m)
      if(.not.(didj(m).gt.delta))goto 23155
      tr(m)=100000.
23155 continue
23154 continue
      if(.not.(tr(m).lt.tref))goto 23157
      tref=tr(m)
      kk=m
23157 continue
23151 continue

c   if there is no refracted ray:

      if(.not.(tref.eq.100000.))goto 23159
      xovmax=100000.
      kk=0
      return
23159 continue

c   if there is a refracted ray, determine xovmax:
c   find lx, the 1st layer below the event layer which
c   is not a low velocity layer

      m=jl+1
      continue
23161 if(.not.(tid(m).eq.100000.))goto 23162
      m=m+1
      goto 23161
23162 continue
      lx=m

c   check whether the event is in the 1st layer

      if(.not.(jl.eq.1))goto 23163
      xovmax=tinj(lx)*v(lx)*v(1)/(v(lx)-v(1))
      return
23163 continue
      m=jl

c   find jx, the 1st layer above and including the event
c   layer which is not a low velocity layer

      continue
23165 continue
      tid(m)=0.
      m1=m-1
      do 23168l=1,m1
      if(.not.(vsq(m).le.vsq(l)))goto 23170
      tid(m)=100000.
      goto 23171
23170 continue
      sqt=sqrt(vsq(m)-vsq(l))
      tim=thk(l)*sqt/(v(l)*v(m))
      tid(m)=tid(m)+tim
23171 continue
23168 continue
      m=m-1

c  decide whether or not jx=1 and calculate xovmax

      if(.not.(tid(m+1).lt.100000..or.m.eq.1))goto 23165
      if(.not.(tid(m+1).lt.100000.))goto 23172
      jx=m+1
      xovmax=(tinj(lx)-tid(jx))*v(lx)*v(jx)/(v(lx)-v(jx))
      goto 23173
23172 continue

c   jx=1

      xovmax=tinj(lx)*v(lx)*v(1)/(v(lx)-v(1))
23173 continue
      return
c  ***** end of subroutine refract *****
      end
c Calculate residual statistics

	subroutine resstat(log, idata, ndt, nev, d, w, idx,
     &	rms_cc, rms_ct, rms_cc0, rms_ct0,
     &	rms_ccold, rms_ctold, rms_cc0old, rms_ct0old,
     &                   dum)

	implicit none

	include "hypoDD.inc"

c	Parameters:
	integer	log
	integer	idata
	integer	ndt
	integer	nev
	real	d(MAXDATA)	! (1..ndt)
	real	w(MAXDATA)	! (1..ndt)
	integer	idx(MAXDATA)	! (1..ndt)
	real	rms_cc
	real	rms_ct
	real	rms_cc0
	real	rms_ct0
	real	rms_ccold
	real	rms_ctold
	real	rms_cc0old
	real	rms_ct0old
	real	dum

c	Local variables:
	real	av_cc
	real	av_cc0
	real	av_ct
	real	av_ct0
	real	dav
	real	dav0
	real	dav1
	real	dav1old
	real	davold
	real	dvar
	real	dvar1
	real	dvar1old
	real	dvarold
	real	f
	real	f_cc
	real	f_ct
	integer	i, j
	real	s
	real	s1
	real	ss
	real	ss1
	real	sw
	real	sw_cc
	real	sw_ct

c--- get rms:
      rms_cc0old= rms_cc0
      rms_ct0old= rms_ct0
      rms_ccold= rms_cc
      rms_ctold= rms_ct
      j= 0
      sw_cc= 0
      sw_ct= 0
      do i= 1,ndt
         if(idx(i).le.2) then
             sw_cc= sw_cc + w(i)
             j= j+1
         else
             sw_ct= sw_ct + w(i)
         endif
      enddo
      f_cc= j/sw_cc  ! factor to scale weights for rms value
      f_ct= (ndt-j)/sw_ct  ! factor to scale weights for rms value

      rms_cc0= 0
      rms_ct0= 0
      av_cc0= 0
      av_ct0= 0
      rms_cc= 0
      rms_ct= 0
      av_cc= 0
      av_ct= 0
      j= 0
      do i= 1,ndt
         if(idx(i).le.2) then
             rms_cc0= rms_cc0 + d(i)**2
             av_cc0= av_cc0 + d(i)
             rms_cc= rms_cc + (f_cc*w(i)*d(i))**2	! weighted and scaled
             av_cc= av_cc + f_cc*w(i)*d(i)   	! weighted and scaled
             j= j+1
         else
             rms_ct0= rms_ct0 + d(i)**2
             av_ct0= av_ct0 + d(i)
             rms_ct= rms_ct + (f_ct*w(i)*d(i))**2	! weighted and scaled
             av_ct= av_ct + f_ct*w(i)*d(i)   	! weighted and scaled
         endif
      enddo
      av_cc0= av_cc0/j
      av_ct0= av_ct0/(ndt-j)
      rms_cc0= sqrt( (rms_cc0 - av_cc0**2/j) / (j-1) )
      rms_ct0= sqrt( (rms_ct0 - av_ct0**2/(ndt-j)) / (ndt-j-1) )
      av_cc= av_cc/j
      av_ct= av_ct/(ndt-j)
      rms_cc= sqrt( (rms_cc - av_cc**2/j) / (j-1) )
      rms_ct= sqrt( (rms_ct - av_ct**2/(ndt-j)) / (ndt-j-1) )

c--- more: residual average, rms, and variance:
      davold= dav
      dvarold= dvar
      dav1old= dav1
      dvar1old= dvar1
      dav= 0    ! unweighted
      dvar= 0   ! unweighted
      dav1= 0   ! weighted
      dvar1= 0  ! weighted
      sw= 0
      dav0= 0    ! unweighted

      do i= 1,ndt
         sw= sw + w(i)
      enddo
      f= ndt/sw  ! factor to scale weights for rms value

      do i= 1,ndt
         dav= dav + d(i)		! unweighted
         dav0= dav0 + w(i)*d(i)   	! weighted
         dav1= dav1 + f*w(i)*d(i)   	! weighted and scaled
      enddo
      dav= dav/ndt
      dav0= dav0/ndt
      dav1= dav1/ndt

      s= 0
      ss= 0
      ss1= 0
      do i=1,ndt
         s= d(i)*1000 - dav*1000 		! in msec
         s1= w(i)*d(i)*1000 - dav0*1000 	! weighted, in msec
         ss= ss + s
         ss1= ss1 + s1
         dvar= dvar + s**2
         dvar1= dvar1 + s1**2
      enddo
      if(ndt.gt.4*nev) then
         dvar= (dvar - ss**2/ndt) / (ndt - 4*nev) ! / by the # of deg of freedom
         dvar1= (dvar1 - ss1**2/ndt) / (ndt - 4*nev)
      else
         dvar= dvar / 1   ! / by the # of degrees of freedom
         dvar1= dvar1 / 1
c         write(*,*)'>>> ndt < 4*nev'
c         write(log,*)'>>> ndt < 4*nev'
         write(*,*)'>>> Warning: ndt < 4*nev'
         write(log,*)'>>> Warning: ndt < 4*nev'
      endif

      if(abs(dum+999).lt.0.0001) then   ! orginal data
         write(log,'(/,"Residual summary of initial data:")')
         write(log,'(a,f7.4)')' absolute mean [s] = ',dav
         write(log,'(a,f7.4)')' weighted mean [s] = ',dav1
         write(log,'(a,f10.4)')' absolute variance [s] = ',dvar/1000
         write(log,'(a,f7.4)')' weighted variance [s] = ',dvar1/1000
         if(idata.eq.1.or.idata.eq.3) then
           write(log,'(a,f7.4)')' absolute cc rms [s] = ',rms_cc0
           write(log,'(a,f7.4)')' weighted cc rms [s] (RMSCC) = ',rms_cc
         endif
         if(idata.eq.2.or.idata.eq.3) then
           write(log,'(a,f7.4)')' absolute ct rms [s] = ',rms_ct0
           write(log,'(a,f7.4)')' weighted ct rms [s] (RMSCT) = ',rms_ct
         endif
      else
         write(log,'(/,"Residual summary:")')
         write(log,'(a,f7.4,a,f7.2,a)')' absolute mean [s] = ',dav,
     & ' (',(dav-davold)*100/abs(davold),' %)'
         write(log,'(a,f7.4,a,f7.2,a)')' weighted mean [s] = ',dav1,
     & ' (',(dav1-dav1old)*100/abs(dav1old),' %)'
         write(log,'(a,f10.4,a,f7.2,a)')' absolute variance [s] = ',
     & dvar/1000, ' (',(dvar-dvarold)*100/dvarold,' %)'
         write(log,'(a,f10.4,a,f7.2,a)')' weighted variance [s] = ',
     & dvar1/1000,' (',(dvar1-dvar1old)*100/dvar1old,' %)'
         if(idata.eq.1.or.idata.eq.3) then
           write(log,'(a,f7.4,a,f7.2,a)')' absolute cc rms [s] = ',
     & rms_cc0,' (',(rms_cc0-rms_cc0old)*100/rms_cc0old,' %)'
           write(log,'(a,f7.4,a,f7.2,a)')' weighted cc rms [s] = ',
     & rms_cc,' (',(rms_cc-rms_ccold)*100/rms_ccold,' %)'
         endif
         if(idata.eq.2.or.idata.eq.3) then
           write(log,'(a,f7.4,a,f7.2,a)')' absolute ct rms [s] = ',
     & rms_ct0,' (',(rms_ct0-rms_ct0old)*100/rms_ct0old,' %)'
           write(log,'(a,f7.4,a,f7.2,a)')' weighted ct rms [s] = ',
     & rms_ct, ' (',(rms_ct-rms_ctold)*100/rms_ctold,' %)'
         endif
      endif

      dum= dvar1
      end !of tine resstat
c     Copy single precision sx to single precision sy.
c     for i = 0 to n-1, copy  sx(lx+i*incx) to sy(ly+i*incy),
c     where lx = 1 if incx .ge. 0, else lx = (-incx)*n, and ly is
c     defined in a similar way using incy.

	subroutine scopy(n, sx, incx, sy, incy)

	implicit none

c	Parameters:
	integer	n
	real	sx(*)
	integer	incx
	real	sy(*)
	integer	incy

c	Local variables:
	integer	i
	integer	ix, iy
	integer	m
	integer	mp1
	integer	ns

      if(n.le.0)return
      if(incx.eq.incy) if(incx-1) 5,20,60
    5 continue

c        code for unequal or nonpositive increments.

      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        sy(iy) = sx(ix)
        ix = ix + incx
        iy = iy + incy
   10 continue
      return

c        code for both increments equal to 1


c        clean-up loop so remaining vector length is a multiple of 7.

   20 m = mod(n,7)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        sy(i) = sx(i)
   30 continue
      if( n .lt. 7 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,7
        sy(i) = sx(i)
        sy(i + 1) = sx(i + 1)
        sy(i + 2) = sx(i + 2)
        sy(i + 3) = sx(i + 3)
        sy(i + 4) = sx(i + 4)
        sy(i + 5) = sx(i + 5)
        sy(i + 6) = sx(i + 6)
   50 continue
      return

c        code for equal, positive, nonunit increments.

   60 continue
      ns = n*incx
          do 70 i=1,ns,incx
          sy(i) = sx(i)
   70     continue
      return
      end
c Convert coordinates of a point by short distance conversion (SDC)

	subroutine sdc2(x, y, xlat, xlon, i)     ! 22.Okt. 1990

	implicit none

c	Parameters:
	real		x, y
	doubleprecision xlat, xlon
	integer		i		!  1: (x, y) -> (xlat, xlon)
					! -1: (xlat, xlon) -> (x, y)

	include "geocoord.inc"

      if(.not.(i.eq.-1.or.i.eq.1)) stop'SDC>>> specify conversion !!!'

      if(i.eq.1)  call redist(x,y,xlat,xlon)
      if(i.eq.-1) call dist(xlat,xlon,x,y)
      end
c Set up Cartesian coordinate system by short distance conversion.
c Unrotated coordinate system with pos. x-axis toward WEST
c and pos. y-axis toward NORTH.
c pos. z-axis toward EARTH`s CENTER.

	subroutine setorg(orlat, orlon, rota, ifil)  !22. Okt. 1990

	implicit none

c	Parameters:
	real	orlat, orlon
	real	rota		! Anticlockwise rotation (degrees)
	integer	ifil

	include "geocoord.inc"

	real	arota
	doubleprecision r, lat1, lat2, dela, delb, PHI, BETA

c     O(r)LAT & O(r)LON : origin of cartesian coordinate system
c      north  w e s t

c if orlat and orlon are both set to zero , the Swiss Cartesian
c coordinate system will be used (this system cannot be rotated).
c  For all other cases, orlat and orlon denote the origin of
c  the SDC.

       rad = 0.017453292d0

      if(orlat.eq.0.0.and.orlon.eq.0.0)then
         olat=46.95240  ! BERN North
         olon=-7.439583  ! BERN West
         rotate=0.0
      else
         olat=orlat
         olon=orlon
         rotate=rota*rad
      endif

      olat=olat*60. ! minutes N
      olon=olon*60. ! minutes W

C  NEW ELLIPSOID FOR WHOLE EARTH:   WGS72 == WORLD GEODETIC SYSTEM 1972

C  ALSO SET RLATC ACCORDING TO ORIGIN

      REARTH=6378.135D0
      ELLIP=298.26         ! (flattening)


C CALCULATE RLATC:  CONVERSION FROM GEOGRAPHICAL LAT TO GEOCENTRICAL LAT

      PHI=OLAT*RAD/60.0              !  phi=geogr. lat
      BETA=PHI-DSIN(PHI*2.)/ELLIP    !  beta=geoc. lat
      RLATC=DTAN(BETA)/DTAN(PHI)

C WRITE ELLIPSOIDE CONSTANTS

      if(ifil.gt.0)then
         write(ifil,*)
         write(ifil,*)
         write(ifil,*)'SHORT DISTANCE CONVERSION on ELLIPSOIDE of'//
     &                ' WORLD GEODETIC SYSTEM 1972 (WGS72)'
         write(ifil,*)'=========================================='//
     &                '==================================='
         write(ifil,*)
         write(ifil,'('' Radius at equator (REARTH)= '',f10.5,
     &                ''  km'')') rearth
         write(ifil,'(''   1. / (ellipticity)      = '',f10.3)') ellip
         write(ifil,*)
         write(ifil,*)'Origin of cartesian coordinates [degrees]:'
         if(orlat.eq.0.0.and.orlon.eq.0.0)then
            write(ifil,*)
            write(ifil,*)' SWISS COORDINATE SYSTEM (we have to be'
            write(ifil,*)'                               special)'
            write(ifil,*)' (Origin = city of BERNE, Switzerland)'
            write(ifil,*)
            write(ifil,*) ' no rotation of grid, pos. y-axis toward N'
            write(ifil,*) '                      pos. x-axis toward E'
            write(ifil,*)
         else
           write(ifil,'(1x,f12.7,'' N'',5x,f12.7,'' W'')')
     &               olat/60.,olon/60.

           write(ifil,*)
           write(ifil,*) ' without rotation of grid, '
           write(ifil,*) '             pos. x-axis toward WEST'
           write(ifil,*) '             pos. y-axis toward NORTH'
           write(ifil,*)
           write(ifil,*) ' Rotation of y-axis from North anticlock-'
           write(ifil,*) ' wise with pos. angle given in degrees'
           write(ifil,*)
           IF(ROTA.GE.0.) THEN
             write(ifil,*) ' Rotation of grid anticlockwise by'
             write(ifil,*)'  ', rota,' degrees'
             write(ifil,*)
           ELSE
             write(ifil,*) ' Rotation of grid clockwise by'
             arota=-1.*rota
             write(ifil,*)'  ', arota,' degrees'
             write(ifil,*)
           ENDIF
         endif
      endif

c   calculate aa &  bb
c   length of one minute of lat and lon in km

      lat1 = datan(rlatc*dtan(olat*rad/60.0))       ! geoc. lat for OLAT
      lat2 = datan(rlatc*dtan((olat+1.)*rad/60.0))  ! geoc. lat for (OLAT+1min)
      dela = lat2 - lat1
      r = rearth*(1.0 - (dsin(lat1)**2)/ellip)      ! kugelradius fuer lat=OLAT
      aa = r*dela   ! aa = 1 min geogr. lat
      delb = dacos(dsin(lat1)**2 + dcos(rad/60.)*dcos(lat1)**2)
      bc=r*delb     ! bc = 1 min geogr. lon
      bb = r*delb/dcos(lat1)
      if(ifil.gt.0)then
         write(ifil,'('' Radius of sphere at OLAT = '',f10.3,'' km'')')r
         write(ifil,*)
         write(ifil,*)'Conversion of GEOGRAPHICAL LATITUDE to '//
     &                'GEOCENTRICAL LATITUDE:'
         write(ifil,*)'RLATC = TAN(GEOCENTR.LAT) / TAN(GEOGRAPH.LAT)'
         write(ifil,'(1x,''RLATC = '',f12.8)') rlatc
         write(ifil,*)
         write(ifil,4) aa, bc
 4       format (10x,'Short distance conversions:',/,
     &           10x,'one min lat ~ ', f7.4,' km ',/,
     &           10x,'one min lon ~ ', f7.4,' km ',/)
         write(ifil,*)
         write(ifil,*)
      endif

c***  convert coordinates with rotation cosines (stored in Common)
      sint=sin(rotate)
      cost=cos(rotate)

      return
      end
	subroutine skip(log, kiter, minwght,
     &	ndt, nev, nsrc, nsta,
     &	ev_cusp, ev_date, ev_time, ev_mag,
     &	ev_lat, ev_lon, ev_dep, ev_x, ev_y, ev_z,
     &	ev_herr, ev_zerr, ev_res,
     &	src_cusp, src_lat, src_lon, src_dep,
     &	src_lat0, src_lon0,
     &	src_x, src_y, src_z, src_t, src_x0, src_y0, src_z0, src_t0,
     &	sta_lab, sta_lat, sta_lon, sta_dist, sta_az,
     &	sta_rmsc, sta_rmsn, sta_np, sta_ns, sta_nnp, sta_nns,
     &	dt_sta, dt_c1, dt_c2, dt_idx, dt_dt, dt_qual, dt_cal,
     &	dt_ista, dt_ic1, dt_ic2,
     &	dt_res, dt_wt, dt_offs,
     &	tmp_ttp, tmp_tts, tmp_xp, tmp_yp, tmp_zp, nct, ncc)

	implicit none

	include'hypoDD.inc'

c	Parameters:
	integer		log
	integer		kiter
	real		minwght
	integer		ndt
	integer		nev
	integer		nsrc
	integer		nsta
	integer		ev_cusp(MAXEVE)	! [1..MAXEVE]
	integer		ev_date(MAXEVE)	! [1..MAXEVE]
	integer		ev_time(MAXEVE)	! [1..MAXEVE]
	real		ev_mag(MAXEVE)	! [1..MAXEVE]
	real		ev_lat(MAXEVE)	! [1..MAXEVE]
	real		ev_lon(MAXEVE)	! [1..MAXEVE]
	real		ev_dep(MAXEVE)	! [1..MAXEVE]
	real		ev_x(MAXEVE)	! [1..MAXEVE]
	real		ev_y(MAXEVE)	! [1..MAXEVE]
	real		ev_z(MAXEVE)	! [1..MAXEVE]
	real		ev_herr(MAXEVE)	! [1..MAXEVE]
	real		ev_zerr(MAXEVE)	! [1..MAXEVE]
	real		ev_res(MAXEVE)	! [1..MAXEVE]
	integer		src_cusp(MAXEVE)! [1..MAXEVE]
	doubleprecision	src_lat(MAXEVE)	! [1..MAXEVE]
	doubleprecision	src_lon(MAXEVE)	! [1..MAXEVE]
	real		src_dep(MAXEVE)	! [1..MAXEVE]
	real		src_lat0(MAXEVE)! [1..MAXEVE]
	real		src_lon0(MAXEVE)! [1..MAXEVE]
	real		src_x(MAXEVE)	! [1..MAXEVE]
	real		src_y(MAXEVE)	! [1..MAXEVE]
	real		src_z(MAXEVE)	! [1..MAXEVE]
	real		src_t(MAXEVE)	! [1..MAXEVE]
	real		src_x0(MAXEVE)	! [1..MAXEVE]
	real		src_y0(MAXEVE)	! [1..MAXEVE]
	real		src_z0(MAXEVE)	! [1..MAXEVE]
	real		src_t0(MAXEVE)	! [1..MAXEVE]
	character	sta_lab(MAXSTA)*7! [1..MAXSTA]
	real		sta_lat(MAXSTA)	! [1..MAXSTA]
	real		sta_lon(MAXSTA)	! [1..MAXSTA]
	real		sta_dist(MAXSTA)! [1..MAXSTA]
	real		sta_az(MAXSTA)	! [1..MAXSTA]
	real		sta_rmsc(MAXSTA)! [1..MAXSTA]
	real		sta_rmsn(MAXSTA)! [1..MAXSTA]
	integer		sta_np(MAXSTA)	! [1..MAXSTA]
	integer		sta_ns(MAXSTA)	! [1..MAXSTA]
	integer		sta_nnp(MAXSTA)	! [1..MAXSTA]
	integer		sta_nns(MAXSTA)	! [1..MAXSTA]
	character	dt_sta(MAXDATA)*7! [1..MAXDATA]
	integer		dt_c1(MAXDATA)	! [1..MAXDATA]
	integer		dt_c2(MAXDATA)	! [1..MAXDATA]
	integer		dt_idx(MAXDATA)	! [1..MAXDATA]
	real		dt_dt(MAXDATA)	! [1..MAXDATA]
	real		dt_qual(MAXDATA)! [1..MAXDATA]
	real		dt_cal(MAXDATA)	! [1..MAXDATA]
	integer		dt_ista(MAXDATA)! [1..MAXDATA]
	integer		dt_ic1(MAXDATA)	! [1..MAXDATA]
	integer		dt_ic2(MAXDATA)	! [1..MAXDATA]
	real		dt_res(MAXDATA)	! [1..MAXDATA]
	real		dt_wt(MAXDATA)	! [1..MAXDATA]
	real		dt_offs(MAXDATA)! [1..MAXDATA]
	real		tmp_ttp(MAXSTA,MAXEVE)! [1..MAXSTA,1..MAXEVE]
	real		tmp_tts(MAXSTA,MAXEVE)! [1..MAXSTA,1..MAXEVE]
	real		tmp_xp(MAXSTA,MAXEVE)! [1..MAXSTA,1..MAXEVE]
	real		tmp_yp(MAXSTA,MAXEVE)! [1..MAXSTA,1..MAXEVE]
	real		tmp_zp(MAXSTA,MAXEVE)! [1..MAXSTA,1..MAXEVE]
	integer		nct
	integer		ncc

c	Local variables:
	integer		i, j, k
	integer		icusp(MAXEVE)	! [1..nev] Event keys
	integer		ifindi
	integer		iicusp(MAXEVE)	! [1..nev] Index table into ev_cusp[]
	integer		nccold
	integer		nctold
	integer		ndtold
	integer		sta_itmp(MAXSTA)

      write(log,'("skipping data...")')

c     Skip data with large resiudals
      if (kiter.eq.1) then
          ndtold = ndt
          nccold = 0
          nctold = 0
      endif
      ncc = 0
      nct = 0
      j = 1
      do i=1,ndt
         if (kiter.eq.1) then
            if (dt_idx(i).le.2) then
               nccold = nccold+1
            else
               nctold = nctold+1
            endif
         endif
         if (dt_wt(i).ge.minwght) then
            dt_sta(j) = dt_sta(i)
            dt_c1(j) = dt_c1(i)
            dt_c2(j) = dt_c2(i)
            dt_idx(j) = dt_idx(i)
            dt_qual(j) = dt_qual(i)
            dt_dt(j) = dt_dt(i)
            dt_cal(j) = dt_cal(i)
            dt_res(j) = dt_res(i)
            dt_wt(j) = dt_wt(i)
            dt_offs(j) = dt_offs(i)
            if (dt_idx(i).le.2) then
                ncc = ncc+1
            else
                nct = nct+1
            endif
            j = j+1
         endif
      enddo
      ndt = j-1
      write(log,'("# obs = ",i9," (",f5.1,"%)")')
     &ndt, (ndt*100.0/ndtold)
      if (nccold.gt.0.and.nctold.gt.0) then
         write(log,'("# obs cc = ",i9," (",f5.1,"%)")')
     &   ncc, (ncc*100.0/nccold)
         write(log,'("# obs ct = ",i9," (",f5.1,"%)")')
     &   nct, (nct*100.0/nctold)
      endif

c     Skip events
      do i=1,ndt
         dt_ic1(i) = dt_c1(i)   !dt_ic1 is just a workspace array here!
         dt_ic2(i) = dt_c2(i)   !dt_ic2 is just a workspace array here!
      enddo
      call sorti(ndt, dt_ic1)
      call sorti(ndt, dt_ic2)
      k = 1
      do i=1,nev
         if (ifindi(ndt, dt_ic1, ev_cusp(i)).gt.0 .or.
     &       ifindi(ndt, dt_ic2, ev_cusp(i)).gt.0) then
            ev_date(k) = ev_date(i)
            ev_time(k) = ev_time(i)
            ev_cusp(k) = ev_cusp(i)
            ev_lat(k) = ev_lat(i)
            ev_lon(k) = ev_lon(i)
            ev_dep(k) = ev_dep(i)
            ev_mag(k) = ev_mag(i)
            ev_herr(k) = ev_herr(i)
            ev_zerr(k) = ev_zerr(i)
            ev_res(k) = ev_res(i)
            ev_x(k) = ev_x(i)
            ev_y(k) = ev_y(i)
            ev_z(k) = ev_z(i)
            k = k+1
         endif
      enddo
      nev = k-1
      write(log,'("# events = ",i9)') nev

c     Skip sources
c     Uses sorted dt_ic[12] arrays from above
      if (nsrc.ne.1) then
         k = 1
         do i=1,nsrc
            if (ifindi(ndt, dt_ic1, src_cusp(i)).gt.0 .or.
     &          ifindi(ndt, dt_ic2, src_cusp(i)).gt.0) then
               src_cusp(k) = src_cusp(i)
               src_lat(k) = src_lat(i)
               src_lon(k) = src_lon(i)
               src_lat0(k) = src_lat0(i)
               src_lon0(k) = src_lon0(i)
               src_dep(k) = src_dep(i)
               src_x(k) = src_x(i)
               src_y(k) = src_y(i)
               src_z(k) = src_z(i)
               src_t(k) = src_t(i)
               src_x0(k) = src_x0(i)
               src_y0(k) = src_y0(i)
               src_z0(k) = src_z0(i)
               src_t0(k) = src_t0(i)
               do j=1,nsta
                  tmp_ttp(j,k) = tmp_ttp(j,i)
                  tmp_tts(j,k) = tmp_tts(j,i)
                  tmp_xp(j,k) = tmp_xp(j,i)
                  tmp_yp(j,k) = tmp_yp(j,i)
                  tmp_zp(j,k) = tmp_zp(j,i)
               enddo
               k = k+1
            endif
         enddo
         nsrc = k-1
      endif

c    Clean stations
      do i=1,nsta
         sta_itmp(i) = 0
      enddo
      do j=1,ndt
         do i=1,nsta
            if (dt_sta(j).eq.sta_lab(i)) then
               sta_itmp(i) = 1
               goto 200	! break
            endif
         enddo
200      continue
      enddo
      k = 1
      do i=1,nsta
         if (sta_itmp(i).eq.1) then
            sta_lab(k) = sta_lab(i)
            sta_lat(k) = sta_lat(i)
            sta_lon(k) = sta_lon(i)
            sta_dist(k) = sta_dist(i)
            sta_az(k) = sta_az(i)
            sta_np(k) = sta_np(i)
            sta_ns(k) = sta_ns(i)
            sta_nnp(k) = sta_nnp(i)
            sta_nns(k) = sta_nns(i)
            sta_rmsc(k) = sta_rmsc(i)
            sta_rmsn(k) = sta_rmsn(i)
            do j=1,nsrc
               tmp_ttp(k,j) = tmp_ttp(i,j)
               tmp_tts(k,j) = tmp_tts(i,j)
               tmp_xp(k,j) = tmp_xp(i,j)
               tmp_yp(k,j) = tmp_yp(i,j)
               tmp_zp(k,j) = tmp_zp(i,j)
            enddo
            k = k+1
         endif
      enddo
      nsta = k-1
      write(log,'("# stations = ",i9)') nsta

c     Index station labels and cuspids
      call indexxi(nev, ev_cusp, iicusp)
      do i=1,nev
         icusp(i) = ev_cusp(iicusp(i)) !icusp is just a workspace array here!
      enddo
      do i=1,ndt
         do j=1,nsta
            if (dt_sta(i).eq.sta_lab(j)) then
               dt_ista(i) = j
               dt_ic1(i) = iicusp(ifindi(nev, icusp, dt_c1(i)))
               dt_ic2(i) = iicusp(ifindi(nev, icusp, dt_c2(i)))
               goto 300	! continue 2
            endif
         enddo
         write(*,'("FATAL ERROR (indexing). Please report to ",
     &             "felix@andreas.wr.usgs.gov")')
         stop   
300      continue
      enddo

      end !of subroutine skip
c Euclidean norm of the n-vector stored in sx() with storage increment incx.

	real function snrm2(n, sx, incx)

	implicit none

c	Parameters:
	integer	n
	real	sx(n)	! (1..n)
	integer	incx

c	Local variables:
	real	cuthi
	real	cutlo
	real	hitest
	integer	i, j
	integer	next
	integer	nn
	real	one
	real	sum
	real	xmax
	real	zero

	data   zero, one /0.0e0, 1.0e0/

c     if    n .le. 0 return with result = 0.
c     if n .ge. 1 then incx must be .ge. 1
c
c           c.l.lawson, 1978 jan 08
c
c     four phase method     using two built-in constants that are
c     hopefully applicable to all machines.
c         cutlo = maximum of  sqrt(u/eps)  over all known machines.
c         cuthi = minimum of  sqrt(v)      over all known machines.
c     where
c         eps = smallest no. such that eps + 1. .gt. 1.
c         u   = smallest positive no.   (underflow limit)
c         v   = largest  no.            (overflow  limit)
c
c     brief outline of algorithm..
c
c     phase 1    scans zero components.
c     move to phase 2 when a component is nonzero and .le. cutlo
c     move to phase 3 when a component is .gt. cutlo
c     move to phase 4 when a component is .ge. cuthi/m
c     where m = n for x() real and m = 2*n for complex.
c
c     values for cutlo and cuthi..
c     from the environmental parameters listed in the imsl converter
c     document the limiting values are as follows..
c     cutlo, s.p.   u/eps = 2**(-102) for  honeywell.  close seconds are
c                   univac and dec at 2**(-103)
c                   thus cutlo = 2**(-51) = 4.44089e-16
c     cuthi, s.p.   v = 2**127 for univac, honeywell, and dec.
c                   thus cuthi = 2**(63.5) = 1.30438e19
c     cutlo, d.p.   u/eps = 2**(-67) for honeywell and dec.
c                   thus cutlo = 2**(-33.5) = 8.23181d-11
c     cuthi, d.p.   same as s.p.  cuthi = 1.30438d19
c     data cutlo, cuthi / 8.232d-11,  1.304d19 /
c     data cutlo, cuthi / 4.441e-16,  1.304e19 /
      data cutlo, cuthi / 4.441e-16,  1.304e19 /

      if(n .gt. 0) go to 10
         snrm2  = zero
         go to 300

   10 assign 30 to next
      sum = zero
      nn = n * incx
c                                                 begin main loop
      i = 1
   20    go to next,(30, 50, 70, 110)
   30 if( abs(sx(i)) .gt. cutlo) go to 85
      assign 50 to next
      xmax = zero

c                        phase 1.  sum is zero

   50 if( sx(i) .eq. zero) go to 200
      if( abs(sx(i)) .gt. cutlo) go to 85

c                                prepare for phase 2.
      assign 70 to next
      go to 105

c                                prepare for phase 4.

  100 i = j
      assign 110 to next
      sum = (sum / sx(i)) / sx(i)
  105 xmax = abs(sx(i))
      go to 115

c                   phase 2.  sum is small.
c                             scale to avoid destructive underflow.

   70 if( abs(sx(i)) .gt. cutlo ) go to 75

c                     common code for phases 2 and 4.
c                     in phase 4 sum is large.  scale to avoid overflow.

  110 if( abs(sx(i)) .le. xmax ) go to 115
         sum = one + sum * (xmax / sx(i))**2
         xmax = abs(sx(i))
         go to 200

  115 sum = sum + (sx(i)/xmax)**2
      go to 200


c                  prepare for phase 3.

   75 sum = (sum * xmax) * xmax


c     for real or d.p. set hitest = cuthi/n
c     for complex      set hitest = cuthi/(2*n)

   85 hitest = cuthi/float( n )

c                   phase 3.  sum is mid-range.  no scaling.

      do 95 j =i,nn,incx
      if(abs(sx(j)) .ge. hitest) go to 100
   95    sum = sum + sx(j)**2
      snrm2 = sqrt( sum )
      go to 300

  200 continue
      i = i + incx
      if ( i .le. nn ) go to 20

c              end of main loop.

c              compute square root and adjust for scaling.

      snrm2 = xmax * sqrt(sum)
  300 continue
      return
      end
c Sort a float vector

	subroutine sort(n, ra)

	implicit none

c	Parameters:
	integer	n
	real	ra(n)	! (1..n)

c	Local variables:
	integer	i, j, l
 	integer	ir
	real	rra

      if (n.le.1) then
         return
      endif


      l = n/2+1
      ir = n
10    continue
         if (l.gt.1) then
            l = l-1
            rra = ra(l)
         else
            rra = ra(ir)
            ra(ir) = ra(1)
            ir = ir-1
            if (ir.eq.1) then
               ra(1) = rra
               return
            endif
         endif
         i = l
         j = l+l
20       if (j.le.ir) then
            if (j.lt.ir) then
               if (ra(j).lt.ra(j+1)) j = j+1
            endif
            if (rra.lt.ra(j)) then
               ra(i) = ra(j)
               i = j
               j = j+j
            else
               j = ir+1
            endif
         go to 20
         endif
         ra(i) = rra
      go to 10
      end
c Sort an int array

	subroutine sorti(n, ia)

	implicit none

c	Parameters:
	integer	n
	integer ia(n)	! (1..n)

c	Local variables:
	integer	i, j, l
	integer	iia
	integer	ir

      if (n.le.1) then
         return
      endif

      L=N/2+1
      IR=N
10    CONTINUE
        IF(L.GT.1)THEN
          L=L-1
          IIA=IA(L)
        ELSE
          IIA=IA(IR)
          IA(IR)=IA(1)
          IR=IR-1
          IF(IR.EQ.1)THEN
            IA(1)=IIA
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
20      IF(J.LE.IR)THEN
          IF(J.LT.IR)THEN
            IF(IA(J).LT.IA(J+1))J=J+1
          ENDIF
          IF(IIA.LT.IA(J))THEN
            IA(I)=IA(J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 20
        ENDIF
        IA(I)=IIA
      GO TO 10
      END
c Multiply a float vector by a scalar
c     Replace single precision sx by single precision sa*sx.
c     for i = 0 to n-1, replace sx(1+i*incx) with  sa * sx(1+i*incx)

	subroutine  sscal(n, sa, sx, incx)

	implicit none

c	Parameters:
	integer	n
	integer	incx
	real	sa, sx(1+(n-1)*incx)

c	Local variables:
	integer	i
	integer	m
	integer	mp1
	integer	ns

      if(n.le.0)return
      if(incx.eq.1)goto 20

c        code for increments not equal to 1.

      ns = n*incx
          do 10 i = 1,ns,incx
          sx(i) = sa*sx(i)
   10     continue
      return

c        code for increments equal to 1.


c        clean-up loop so remaining vector length is a multiple of 5.

   20 m = mod(n,5)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        sx(i) = sa*sx(i)
   30 continue
      if( n .lt. 5 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,5
        sx(i) = sa*sx(i)
        sx(i + 1) = sa*sx(i + 1)
        sx(i + 2) = sa*sx(i + 2)
        sx(i + 3) = sa*sx(i + 3)
        sx(i + 4) = sa*sx(i + 4)
   50 continue
      return
      end
c Singular-value decomposition of rectangular float matrix

      subroutine svd(m1, n1, m, n, a, u, v, q, index)
C$$$$$ CALLS NO OTHER ROUTINES
C  SINGULAR VALUE DECOMPOSITION)  FOR ALGOL PROGRAM SEE WILKINSON+REINSCH
C  HANDBOOK FOR AUTOMATIC COMPUTATION VOL 2 - LINEAR ALGEBRA, PP140-144
C  TRANSLATED FROM ALGOL BY R.L.PARKER
C  THE MATRIX A(M,N) IS DECOMPOSED.  SINGULAR VALUES IN Q, PRE-MATRIX IN U,
C  POST-MATRIX IN V.   INDEX MAY BE 1,2,3 OR 4.  IF 1, FIND U,V. IF 2, FIND
C  ONLY U. IF 3, FIND ONLY V. IF 4, FIND NEITHER. IN ALL CASES, THE ARRAY  U
C  MUST BE SUPPLIED AS IT IS USED AS WORKING SPACE FOR THE ROUTINE.
C  PROGRAM ALTERED BY PAUL SILVER 4/15 TO HANDLE UNPACKED ARRAYS.
C  M1,N1 ARE DIMENSIONS IN MAIN ROUTINE.M,N ARE ACTUAL DIMENSIONS TO
C  BE USED IN THE SUBROUTINE.

	implicit none

c	Parameters:
	integer	m1, n1
	integer	m, n
	integer	index
	doubleprecision	a(m1,n)	! (1..m, 1..n)
	doubleprecision	u(m1,n)	! (1..m, 1..n)
	doubleprecision	v(n1,n)	! (1..n, 1..n)
	doubleprecision	q(n)	! (1..n)

c	Local variables
	doubleprecision	c
	doubleprecision	e(1000)
	doubleprecision	eps
	doubleprecision	f
	doubleprecision	g
	doubleprecision	h
	integer	i, j, k, l
	integer	iback
	integer	kback
	integer	lback
	integer	lplus
	integer	l1
	doubleprecision	s
	doubleprecision	tol
	doubleprecision	x
	doubleprecision	y
	doubleprecision	z

      EPS=1.0E-10
      TOL=1.0E-35
      DO 1100 I=1,M
      DO 1100 J=1,N
 1100 U(I,J)=A(I,J)
C  HOUSEHOLDER REDUCTION TO BI-DIAGONAL FORM
      G=0.0
      X=0.0
      DO 2900 I=1,N
      E(I)=G
      S=0.0
      L=I+1
      DO 2100 J=I,M
 2100 S=U(J,I)**2 + S
      IF (S .LT. TOL) GO TO 2500
      F=U(I,I)
      G=-SIGN(SQRT(S),F)
      H=F*G - S
      U(I,I)=F - G
      IF (L.GT.N) GO TO 2501
      DO 2400 J=L,N
      S=0.0
      DO 2200 K=I,M
 2200 S=U(K,I)*U(K,J) + S
      F=S/H
      DO 2300 K=I,M
 2300 U(K,J)=U(K,J) + F*U(K,I)
 2400 CONTINUE
      GO TO 2501
 2500 G=0.0
C
 2501 CONTINUE
      Q(I)=G
      S=0.0
      IF (L.GT.N) GO TO 2601
      DO 2600 J=L,N
 2600 S=U(I,J)**2 + S
 2601 IF (S.LT.TOL) GO TO 2800
      F=U(I,I+1)
      G=-SIGN(SQRT(S),F)
      H=F*G - S
      U(I,I+1)=F - G
      IF (L.GT.N) GO TO 2651
      DO 2650 J=L,N
 2650 E(J)=U(I,J)/H
 2651 CONTINUE
      IF (L.GT.M) GO TO 2850
      DO 2700 J=L,M
      S=0.0
      IF (L.GT.N) GO TO 2700
      DO 2670 K=L,N
 2670 S=U(J,K)*U(I,K) + S
      DO 2690 K=L,N
 2690 U(J,K)=U(J,K) + S*E(K)
 2700 CONTINUE
      GO TO 2850
 2800 G=0.0
 2850 Y=ABS(Q(I)) + ABS(E(I))
      IF (Y .GT. X) X=Y
 2900 CONTINUE
C
C  ACCUMULATION OF RIGHT-HAND TRANSFORMS (V)
C
      GO TO (3000,3701,3000,3701       ),INDEX
 3000 CONTINUE
      DO 3700 IBACK=1,N
      I=N+1-IBACK
      IF (G .EQ. 0.0) GO TO 3500
      H=U(I,I+1)*G
      IF (L.GT.N) GO TO 3500
      DO 3100 J=L,N
 3100 V(J,I)=U(I,J)/H
      DO 3400 J=L,N
      S=0.0
      DO 3200 K=L,N
 3200 S=U(I,K)*V(K,J) + S
      DO 3300 K=L,N
 3300 V(K,J)=V(K,J) + S*V(K,I)
 3400 CONTINUE
 3500 CONTINUE
      IF (L.GT.N) GO TO 3601
      DO 3600 J=L,N
      V(J,I)=0.0
 3600 V(I,J)=0.0
 3601 V(I,I)=1.0
      G=E(I)
      L=I
 3700 CONTINUE
 3701 CONTINUE
C
C  ACCUMULATION OF LEFT-HAND TRANSFORMS
      GO TO (4000,4000,4701,4701       ),INDEX
 4000 CONTINUE
      DO 4700 IBACK=1,N
      I=N+1-IBACK
      L=I+1
      G=Q(I)
      IF (L.GT.N) GO TO 4101
      DO 4100 J=L,N
 4100 U(I,J)=0.0
 4101 IF (G.EQ. 0.0) GO TO  4500
      H=U(I,I)*G
      IF (L.GT.N) GO TO 4401
      DO 4400 J=L,N
      S=0.0
      DO 4200 K=L,M
 4200 S=U(K,I)*U(K,J) + S
      F=S/H
      DO 4300 K=I,M
 4300 U(K,J)=U(K,J) + F*U(K,I)
 4400 CONTINUE
 4401 CONTINUE
      DO 4550 J=I,M
 4550 U(J,I)=U(J,I)/G
      GO TO 4700
 4500 CONTINUE
      DO 4600 J=I,M
 4600 U(J,I)=0.0
 4700 U(I,I)=U(I,I) + 1.0
C
C  DIAGONALIZATION OF BI-DIAGONAL FORM
 4701 EPS=EPS*X
      DO 9000 KBACK=1,N
      K=N+1-KBACK
C  TEST F-SPLITTING
 5000 CONTINUE
      DO 5100 LBACK=1,K
      L=K+1-LBACK
      IF (ABS(E(L)).LE. EPS) GO TO 6500
      IF (ABS(Q(L-1)) .LE. EPS) GO TO 6000
 5100 CONTINUE
C  CANCELLATION OF E(L), IF L.GT. 1
 6000 C=0.0
      S=1.0
      L1=L - 1
      DO 6200 I=L,K
      F=S*E(I)
                   E(I)=C*E(I)
      IF (ABS(F) .LE. EPS) GO TO 6500
      G=Q(I)
      Q(I)=SQRT(F*F + G*G)
      H=Q(I)
      C=G/H
      S=-F/H
      GO TO (6050,6050,6200,6200       ),INDEX
 6050 CONTINUE
      DO 6100 J=1,M
      Y=U(J,L1)
      Z=U(J,I)
      U(J,L1)=Y*C + Z*S
      U(J,I)=-Y*S + Z*C
 6100 CONTINUE
 6200 CONTINUE
C  TEST F-CONVERGENCE
 6500 Z=Q(K)
      IF (L .EQ. K) GO TO  8000
C  SHIFT FROM BOTTOM 2 X 2 MINOR
      X=Q(L)
      Y=Q(K-1)
      G=E(K-1)
      H=E(K)
      F=((Y-Z)*(Y+Z) + (G-H)*(G+H))/(2.0*H*Y)
      G=SQRT(F*F + 1.0)
      F=((X-Z)*(X+Z) + H*(Y/(F + SIGN(G,F))-H))/X
C  NEXT Q-R TRANSFORMATION
      C=1.0
      S=1.0
      LPLUS=L + 1
      DO 7500 I=LPLUS,K
      G=E(I)
      Y=Q(I)
      H=S*G
      G=C*G
      Z=SQRT(F*F + H*H)
      E(I-1)=Z
      C=F/Z
      S=H/Z
      F=X*C + G*S
      G=-X*S + G*C
      H=Y*S
      Y=Y*C
      GO TO (7100,7201,7100,7201       ),INDEX
 7100 DO 7200 J=1,N
      X=V(J,I-1)
      Z=V(J,I)
      V(J,I-1)=X*C + Z*S
      V(J,I)=-X*S + Z*C
 7200 CONTINUE
 7201 Z=SQRT(F*F + H*H)
      Q(I-1)=Z
      C=F/Z
      S=H/Z
      F=C*G + S*Y
      X=-S*G + C*Y
      GO TO (7300,7300,7500,7500       ),INDEX
 7300 DO 7400 J=1,M
      Y=U(J,I-1)
      Z=U(J,I)
      U(J,I-1)=Y*C + Z*S
      U(J,I)=-Y*S + Z*C
 7400 CONTINUE
 7500 CONTINUE
      E(L)=0.0
      E(K)=F
      Q(K)=X
      GO TO  5000
C  CONVERGENCE
 8000 IF (Z .GE. 0.0) GO TO 9000
C  Q IS MADE NON-NEGATIVE
      Q(K)=-Z
      GO TO (8100,9000,8100,9000       ),INDEX
 8100 DO 8200 J=1,N
 8200 V(J,K)=-V(J,K)
 9000 CONTINUE
      RETURN
      END
c Compute intercept times and critical distances for "refracted" rays

	subroutine tiddid (jl, nl, v, vsq, thk, tid, did)

	implicit none

	include "hypoDD.inc"

c	Parameters:
	integer	jl
	integer	nl
	real	v(MAXLAY)	! (1..nl)
	real	vsq(MAXLAY)	! (1..nl)
	real	thk(MAXLAY)	! (1..nl)
	real	tid(20)	! (1..20)
	real	did(20)	! (1..20)

c       Determines the travel time intercept and critical
c  distance for a seismic ray in a layered earth model
c  originating at the top of layer jl, refracting in
c  layer m, and terminating at the top of layer 1.
c
c  input:       jl - event layer
c               nl - number of layers
c             v(l) - velocity of layer l
c           vsq(l) - velocity squared
c           thk(l) - thickness of layer l
c  output:
c           tid(m) - travel time intercept for
c                      refraction in layer m
c           did(m) - critical distance

c	Local variables:
	real	did1, did2
	real	dimm
	integer	j1
	integer	l
	integer	m
	integer	m1
	real	sqt
	real	tid1, tid2
	real	tim

      j1=jl+1
      do 23174m=j1,nl
      tid(m)=0.
      did(m)=0.
      tid1=0.
      tid2=0.
      did1=0.
      did2=0.
      m1=m-1
      do 23176l=1,m1
      if(.not.(vsq(m).le.vsq(l)))goto 23178

c   if m is a low velocity layer, set tid and did to
c   very large values

      tid(m)=100000.
      did(m)=100000.
      goto 23179
23178 continue
      sqt=sqrt(vsq(m)-vsq(l))
      tim=thk(l)*sqt/(v(l)*v(m))
      dimm=thk(l)*v(l)/sqt
      if(.not.(l.lt.jl))goto 23180

c   sum for layers above event layer

      tid1=tid1+tim
      did1=did1+dimm
      goto 23181
23180 continue

c   sum for layers below and including the event layer

      tid2=tid2+tim
      did2=did2+dimm
23181 continue
23179 continue
23176 continue
      if(.not.(tid(m).ne.100000.))goto 23182

c   calculate tid and did if m is not a low velocity layer

      tid(m)=tid1+2*tid2
      did(m)=did1+2*did2
23182 continue
23174 continue
      return
c  ***** end of subroutine tiddid *****
      end

	subroutine trialsrc(istart, sdc0_lat, sdc0_lon, sdc0_dep,
     &	nev, ev_cusp, ev_lat, ev_lon, ev_dep,
     &	nsrc, src_cusp, src_lat0, src_lon0,
     &	src_x0, src_y0, src_z0, src_t0,
     &	src_lat, src_lon, src_dep,
     &	src_x, src_y, src_z, src_t)

	implicit none

	include "hypoDD.inc"

c	Parameters:
	integer		istart
	real		sdc0_lat	! Cluster center
	real		sdc0_lon	! Cluster center
	real		sdc0_dep	! Cluster center
	integer		nev		! No. of events
	integer		ev_cusp(MAXEVE)	! [1..nev]
	real		ev_lat(MAXEVE)	! [1..nev]
	real		ev_lon(MAXEVE)	! [1..nev]
	real		ev_dep(MAXEVE)	! [1..nev]
	integer		nsrc		! No of trial sources
	integer		src_cusp(MAXEVE)! [1..nev]
	real		src_lat0(MAXEVE)! [1..nev]
	real		src_lon0(MAXEVE)! [1..nev]
	real		src_x0(MAXEVE)	! [1..nev]
	real		src_y0(MAXEVE)	! [1..nev]
	real		src_z0(MAXEVE)	! [1..nev]
	real		src_t0(MAXEVE)	! [1..nev]
	doubleprecision	src_lat(MAXEVE)	! [1..nev]
	doubleprecision	src_lon(MAXEVE)	! [1..nev]
	real		src_dep(MAXEVE)	! [1..nev]
	real		src_x(MAXEVE)	! [1..nev]
	real		src_y(MAXEVE)	! [1..nev]
	real		src_z(MAXEVE)	! [1..nev]
	real		src_t(MAXEVE)	! [1..nev]

c	Local variables:
	integer	i
	real	x, y

c     Set up parameters for initial inversion
      if (istart.eq.1) then
c        Cluster center as initial trial source
         nsrc = 1
         do i=1,nev
            src_cusp(i) = ev_cusp(i)
            src_lon(i) = sdc0_lon
            src_lat(i) = sdc0_lat
            src_dep(i) = sdc0_dep
            src_x(i) = 0.0
            src_y(i) = 0.0
            src_z(i) = 0.0
            src_t(i) = 0.0
c           Store initial trial source
            src_lon0(i) = sdc0_lon
            src_lat0(i) = sdc0_lat
            src_x0(i) = 0.0
            src_y0(i) = 0.0
            src_z0(i) = 0.0
            src_t0(i) = 0.0
         enddo
      else
c        Catalog sources as initial trial source
c        Add noise for synthetic data mode
         nsrc = nev
         do i=1,nev
            src_cusp(i) = ev_cusp(i)
            src_lon(i) = ev_lon(i)
            src_lat(i) = ev_lat(i)
            src_dep(i) = ev_dep(i)
            src_lon0(i) = ev_lon(i)
            src_lat0(i) = ev_lat(i)
            call SDC2(x,y,src_lat(i),src_lon(i),-1)
            src_x(i) = x *1000.0
            src_y(i) = y *1000.0
            src_z(i) = ((ev_dep(i) - sdc0_dep)*1000)
            src_t(i) = 0.0
            src_x0(i) = src_x(i)
            src_y0(i) = src_y(i)
            src_z0(i) = src_z(i)
            src_t0(i) = src_t(i)
         enddo
      endif
      end !of subroutine trialsrc
c Length of character string, excluding trailing blanks

	integer function trimlen(t)

	implicit none

c	Parameter:
	character t*(*)

      do 1 trimlen=LEN(t),1,-1
    1    if(t(trimlen:trimlen).ne.' ')RETURN
      trimlen=1
      end ! of integer function trimlen
c Determine the fastest traveltime between a source
c at depth=depth(km) and a receiver at distance=delta(km).

	subroutine ttime(delta, depth, nl, v, top, t, ain)

	implicit none

	include "hypoDD.inc"

c	Parameters:
	real	delta
	real	depth
	integer	nl
	real	v(MAXLAY)
	real	top(MAXLAY)
	real	t
	real	ain

c	Local variables:
	integer	jl
	integer	kk
	real	tdir
	real	thk(20)
	real	tkj
	real	tref
	real	u
	real	vsq(20)
	real	x
	real	xovmax

c	compile and link for S
c	f77 -c ttime.f
c	ld -r -dn ttime.o
c	mv a.out ttime.o

c	subroutine direct1 is used to compute the direct ray
c	traveltime and sine of takeoff angle.

c	subroutine refract is used to compute the fastest
c	refracted ray traveltime.  It calls subroutine tiddid.

c	subroutine vmodel extract needed information from the
c	layered velocity model.

c	input:
c	delta	epicentral distance in km
c	depth	focal depth of source in km
c	nl	number of layers in velocity model
c	v	velocity in each layer
c	top	depth to top of layer

c	output:
c	t	minimum traveltime
c	ain	angle of emergence at source


c	call vmodel to set-up model and locate source in it

	call vmodel(nl,v,top,depth,vsq,thk,jl,tkj)

c  output:
c      vsq(l) - v(l) ** 2
c      thk(l) - thickness of layer l
c          jl - event layer
c         tkj - depth of event in event layer

c	call refract to find fastest refracted arrival

	call refract(nl,v,vsq,thk,jl,tkj,delta,
     &			kk,tref,xovmax)

c  output:   kk - refracting layer for fastest refracted ray
c          tref - travel time of fastest refracted ray
c        xovmax - an upper bound on delta for which the direct ray
c                 can be the first arrival


c	if delta <= xovmax, them
c	call direct1 to find the direct ray traveltime
c	otherwise tref is the minimum traveltime

c	assume for now refracted path is faster

	t=tref

c	compute the takeoff angle
	if (kk.gt.0) then

	u=v(jl)/v(kk)
	ain=asin(u)*57.2958
	endif

	if (delta.le.xovmax) then

	call direct1(nl,v,vsq,thk,jl,tkj,delta,depth,tdir,u,x)

c  output:  tdir - direct ray travel time
c              u - sine of the takeoff angle
c              x - horizontal travel distance in the event layer
c

c	compare the traveltimes

	if (tref.gt.tdir) then

c	direct time is the minimum traveltime

	t=tdir
	ain=180-asin(u)*57.2958

	endif
	endif

	return
c *****	end of subroutine ttime *****
	end
c Extract needed information from the layered velocity model.

	subroutine vmodel(nl, v, top, depth, vsq, thk, jl, tkj)

	implicit none

	include "hypoDD.inc"

c	Parameters:
	integer	nl
	real	v(MAXLAY)
	real	vsq(MAXLAY)
	real	top(MAXLAY)
	real	depth
	real	thk(MAXLAY)
	integer	jl
	real	tkj

c  input:     nl - number of layers
c           v(l) - velocity of layer l
c	     top - depth to top of layer l
c          depth - depth of event

c         vsq(l) = v(l) ** 2
c         thk(l) - thickness of layer l
c             jl - event layer
c            tkj - depth of event from top of event layer

c	Local variables:
	integer	i

c	compute square of layer velocity
	do 10 i=1,nl
   10	vsq(i)=v(i)*v(i)

c	determine layer thickness and
c	find layer containing event,

	jl=nl

	do 20 i=1,nl

c	Important note:  if (depth.lt.top(i)) will
c	lead to incorrect results for traveltime
	if (depth.le.top(i)) then
	jl=i-1
	goto 25
	endif
   20	continue
   25	continue

	do 30 i=1,nl-1
   30	thk(i)=top(i+1)-top(i)

c	compute depth from top of layer to source

	tkj=depth-top(jl)

	return
c *****	end of subroutine vmodel *****
	end
c 10/04/00 determines a priori weights and re-weights them ....

	subroutine weighting(log, ndt, mbad, amcusp, idata, kiter, ineg,
     &	maxres_cross, maxres_net, maxdcc, maxdct, minwght,
     &	wt_ccp, wt_ccs, wt_ctp, wt_cts,
     &	dt_c1, dt_c2, dt_idx, dt_qual, dt_res, dt_offs,
     &	dt_wt)

	implicit none

	include "hypoDD.inc"

c	Parameters:
	integer	log
	integer	ndt
	integer	mbad
	integer	amcusp(1000)
	integer	idata
	integer	kiter
	integer	ineg
	real	maxres_cross
	real	maxres_net
	real	maxdcc
	real	maxdct
	real	minwght
	real	wt_ccp
	real	wt_ccs
	real	wt_ctp
	real	wt_cts
	integer	dt_c1(MAXDATA)		! (1..MAXDATA)
	integer	dt_c2(MAXDATA)		! (1..MAXDATA)
	integer	dt_idx(MAXDATA)		! (1..MAXDATA)
	real	dt_qual(MAXDATA)		! (1..MAXDATA)
	real	dt_res(MAXDATA)		! (1..MAXDATA)
	real	dt_offs(MAXDATA)		! (1..MAXDATA)
	real	dt_wt(MAXDATA)		! (1..MAXDATA)

c	Local variables:
	character	dattim*25
	real	dt_tmp(MAXDATA)
	integer	i, j, k
	real	mad_cc
	real	mad_ct
	real	maxres_cc
	real	maxres_ct
	real	med_cc
	real	med_ct
	integer	ncc
	integer	nct
	integer	nncc
	integer	nnct

        call datetime(dattim)

c synthetics:
      if(idata.eq.0) then
            do i=1,ndt
               dt_wt(i)=1 	
            enddo
      endif

c--- get a priori data weights:
c intial (a priori) weights:
c s=linspace(0.0,100.0,101); ss= (s/100).^2; plot(s/100,ss); % coherency
c s=linspace(0.0,2.0,101); ss= (1./(2.^s)); plot(s,ss); % pick qual

c all the quality transf is done in getdata. old format listed qualities,
c new format list weights directly.
      ineg= 0  		!flag, =1 if neg weights exist
      do i=1,ndt
          if(dt_idx(i).eq.1)
     & dt_wt(i)= wt_ccp * dt_qual(i)	! compat. with new format
          if(dt_idx(i).eq.2)
     & dt_wt(i)= wt_ccs * dt_qual(i)	! compat. with new format
          if(dt_idx(i).eq.3)
     & dt_wt(i)= wt_ctp * dt_qual(i) ! compatib with new format 17/01/00
          if(dt_idx(i).eq.4)
     & dt_wt(i)= wt_cts * dt_qual(i) ! compatib with new format 17/01/00

          do j=1,mbad
             if(dt_c1(i).eq.amcusp(j).or.dt_c2(i).eq.amcusp(j)) then
                 dt_wt(i)= 0.0
                 ineg= 1
             endif
          enddo
      enddo

c--- re-weighting: :
      if(((idata.eq.1.or.idata.eq.3).and.
     &    (maxres_cross.ne.-9.or.maxdcc.ne.-9)).or.
     &   ((idata.eq.2.or.idata.eq.3).and.
     &    (maxres_net.ne.-9.or.maxdct.ne.-9))) then
          write(log,'("re-weighting ... ", a)') dattim

c--- get median and MAD of residuals
         if(idata.eq.3) then
            if(maxres_cross.ge.1) then
c cross data:
               k= 1
               do i=1,ndt
                  if(dt_idx(i).le.2) then
                     dt_tmp(k)= dt_res(i)
                     k= k+1
                  endif
               enddo
               call mdian1(dt_tmp,k-1,med_cc)
c 071200...
               do i=1,k-1
                  dt_tmp(i)= abs(dt_tmp(i)-med_cc)
               enddo
               call mdian1(dt_tmp,k-1,mad_cc)
               mad_cc= mad_cc/0.67449    !MAD for gaussian noise
            endif
            if(maxres_net.ge.1) then
c- catalog data:
               k= 1
               do i=1,ndt
                 if(dt_idx(i).ge.3) then
                    dt_tmp(k)= dt_res(i)
                    k= k+1
                 endif
               enddo
               call mdian1(dt_tmp,k-1,med_ct)
               do i=1,k-1
                  dt_tmp(i)= abs(dt_tmp(i)-med_ct)
               enddo
               call mdian1(dt_tmp,k-1,mad_ct)
               mad_ct= mad_ct/0.67449    !MAD for gaussian noise
            endif
         elseif((idata.eq.1.and.maxres_cross.ge.1).or.
     &       (idata.eq.2.and.maxres_net.ge.1)) then
            do i=1,ndt
                  dt_tmp(i)= dt_res(i)
            enddo
            call mdian1(dt_tmp,ndt,med_cc)
c new... see email from jim larsen...
            do i=1,ndt
                dt_tmp(i)= abs(dt_tmp(i)-med_cc)
            enddo
            call mdian1(dt_tmp,ndt,mad_cc)
            mad_cc= mad_cc/0.67449
            if(idata.eq.2) mad_ct= mad_cc
         endif

c--- define residual cutoff value:
         maxres_cc= maxres_cross	! absolute cutoff value
         maxres_ct= maxres_net		! absolute cutoff value
         if(maxres_cross.ge.1) maxres_cc= mad_cc*maxres_cross
         if(maxres_net.ge.1) maxres_ct= mad_ct*maxres_net

c--- apply residual/offset dependent weights to a priori weights
         nncc= 0
         nnct= 0
         ncc= 0
         nct= 0
         do i=1,ndt
            if(dt_idx(i).le.2) then
c--- cross data:
               ncc= ncc+1

c bi ^5 offset weighting for cross data:
c    exp needs to be uneven so weights become negative for offsets larger
c    than 2 km. 2km is hardwired, >>>not anymore since 03/23/00
c s=linspace(0,2.2,30);ss=(1-(s/2).^5).^5;plot(s,ss);axis([0 2.0 -0.0 1]);
               if(maxdcc.ne.-9)
     & dt_wt(i)= dt_wt(i) * (1 - (dt_offs(i)/(maxdcc*1000))**5)**5

c bi-cube residual weighting:
c     needs to be cube so that res > cutoff become negative.
c s=linspace(-0.2,0.2,101); ss= (1- (abs(s)/0.1).^3).^3;
c plot(abs(s),ss);  axis([0 0.11 -0.1 1]);

               if(maxres_cross.gt.0.and.dt_wt(i).gt.0.000001)
     & dt_wt(i)= dt_wt(i) * (1- (abs(dt_res(i))/maxres_cc)**3)**3
               if(dt_wt(i).lt.minwght) nncc= nncc+1

            else	
c--- catalog data:
               nct= nct+1

c bi ^3 offset weighting for catalog data:
c    exp needs to be uneven so weights become negative for offsets larger
c    than 10 km. 10km is hardwired. not anymore since 03/23/00
c s=linspace(0,11,100);ss=(1-(s/10).^3).^3;plot(s,ss);axis([0 11 -0.1 1]);
               if(maxdct.ne.-9)
     &  dt_wt(i)= dt_wt(i) * (1 - (dt_offs(i)/(maxdct*1000))**3)**3

c bi-cube residual weighting:
c     needs to be cube so that res > cutoff become negative.
c s=linspace(-0.2,0.2,101); ss= (1- (abs(s)/0.1).^3).^3;
c plot(abs(s),ss);  axis([0 0.11 -0.1 1]);
               if(dt_wt(i).gt.0.000001 .and. maxres_net.gt.0)
     & dt_wt(i)= dt_wt(i) * (1- (abs(dt_res(i))/maxres_ct)**3)**3	
               if(dt_wt(i).lt.minwght) nnct= nnct+1
            endif
         enddo

c--- check if neg residuals exist
         ineg= 0
         do j=1,ndt
            if(dt_wt(j).lt.minwght) then
               ineg= 1
               goto  100
            endif
         enddo
100      continue

         if(idata.eq.1.or.idata.eq.3) then
            write(log,'(" cc res/dist cutoff:",
     & f7.3,"s/",f6.2,"km (",f5.1,"%)")')
     & maxres_cc,maxdcc,(nncc*100.0/ncc)
         endif
         if(idata.eq.2.or.idata.eq.3) then
            write(log,'(" ct res/dist cutoff [s]:",
     & f7.3,"s/",f6.2,"km (",f5.1,"%)")')
     & maxres_ct,maxdct,(nnct*100.0/nct)
         endif
      endif  ! re-weighting

      if(ineg.gt.0) kiter= kiter+1
      end ! of subroutine weighting
