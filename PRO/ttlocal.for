
       program localtt
       implicit none
       include 'seisan.inc'

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c program to calculate  travel times for given stations
c and hypocenter location at local distances. First arrival and a
c possibel secon arrival is calculated like PN2 and Pg. the model
c used ofr calculation is the standard STATIONx.HYP.
c the program operates in two modes:
c
c   -if iasp.inp is present arrival times for staitions in iasp.inp
c    are calcualted and put in file iasp.out to be used with mulplt
c
c   -an argument with an s-file is given. the program calculates 
c    theoretical arrivals for the phases given in the s-file and 
c    write it in an output file with the original arrival times 
c    replaced by the theoretical times. All weights etc are kept. 
c    relocating the output file should give same locations
c    and zero residuals.
c    in this mode there can be many events in the input s-file. 
c    The station file used is the one from the first event.
c

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c changes:
c   15/03/2018 lo include fold_lat, and use correct ref height (check again)
c   06/06/2018 lo skip calculation if station not found rather than exit
c   22/09/2020 jh  new nordic format, nordic2
c   27/11/2020 jh always calculate g-phases in addion to first arrival
c   15/12/2020 jh calculate theroreticla time for phases given in s-file

      include 'hypparm.inc'
      include 'rea.inc'

      character*80 infile,sfile
      character*12 stfile
      character*80 indat,testdat
      character*5 station_name
      character*1 reff,ucase,prmd,prm2
      character*4 stat
      character*8 phsid      ! type of phase found
      character*8 phase_out  ! phase to write out
      integer dlat,dlon,height,iustat,j,i,nmoho,n
      integer nconrad,nn,nd,iulst,iflag,minflag
      real mlat,mlon,pi,rearth,degtorad,delta
      integer mlat1,mlat2,mlon1,mlon2
      real xnear,xfar,depth,dist,tmin,ann,testj
      character*1 clat,clon,c
      logical station_found
c latitude and longitude of station and epicenter in radians
      real slat,slon,eqlat,eqlon,sslat,sslon
c input of eq lat and lon in degrees
      real deqlat,deqlon
c distance from earthqauke to station in degree
      real dedeg
c         az0      azimuth from earthquake to station measured clockwise
c                     from north in degrees
      real azi,baz
      real noise                 ! noise factor for adding random noise
      real pnoise,snoise         ! noise for p and s
      integer nars               ! number of arguments
      character*80 args(5)       ! arguments
c output
      integer iout
c file exist
      logical exists
c top directory
      character*60 top_directory
c dlimin...
      character*1 dchar
      external sei clen
      integer  sei clen
      integer nistat
      character*5 istat(9999)
      character*80 text
      logical exist
      integer code
      logical all
      integer maxh
      double precision osec
      integer year,month,day,doy,hour,min
      real sec

c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver
     
      call get_seisan_def
      call srand(50)    ! seed random noise if noise is added
c
c   get arguments, can be an s-file
c

      call get_arguments(nars,args)

      noise=0.0
      if(nars.gt.0) then     ! multi event
         sfile=args(1)
         open(10,file='ttlocal.out',status='unknown')
         if(nars.eq.2) read(args(2),*) noise
      endif
c 
c   get directory structure
c
      call topdir(top_directory)
      call dir_char(dchar)         ! dirctory delimiter character

      pi=3.141593
      degtorad=pi/180.
      rearth=6371.
      nmoho=0
      nconrad=0
      maxh=-999

c
c operated from mulplt, get s-file name
c
      if(nars.eq.0) then
         call get_env_event(sfile)
      endif      
c
c   open event sfile from data base or file
c
      all=.true.
      open(23,file=sfile,status='old')

c
c try to read iasp.inp which has station list to overwrite sfile
c only if not an sfile as argument
c
      if(nars.eq.0) then
         inquire(file='iasp.inp',exist=exist)
         if(exist) then
           write(*,*) ' reading stations from iasp.inp '
           nistat=1
           open(26,file='iasp.inp',status='old')
224        continue
           read(26,'(a)',end=225) text
           istat(nistat)=text(1:5)
           nistat=nistat+1
           goto 224
         endif
225      continue
         nistat=nistat-1
         close(26)
      endif

c output file for mulplt

      open(24,file='iasp.out',status='unknown')


c
c  set name of station file
c
      c=' '
      c=hyp_dist_id(1)
      c='0'
      if(c.eq.' '.or.c.eq.'0')then
        stfile='STATION0.HYP'
      else
        stfile='STATION'  // c // '.HYP'
      endif
c
c open input station file
c
      infile=stfile

      iustat=9         
      inquire(file=infile,exist=exists)
      if (exists) then
        open(iustat,status='old',file=infile)
      else
c
c check in DAT directory
c
        infile = top_directory(:seiclen(top_directory)) //
     &           dchar // 'DAT' //dchar                 //
     &           stfile
        inquire(file=infile,exist=exists)
        if (.not.exists) then
          write(*,*) infile,' does not exist'
          stoP
        endif
      endif

c---------------------------------------------------------
c  back here to read next event if multi event calculation
c---------------------------------------------------------

 1    continue

      call rea_event_in(23,all,data,code)
c
c   check if end of file (code=1), if so jump out of loop
c
      if(code.eq.1.and.nars.gt.0) goto 1000
c
c   header for iasp.out
c
      write(24,'(1x,i4,1x,2i2,11x,a1,57x,a1)')
     &   hyp_year(1),hyp_month(1),hyp_day(1),'L','1'

c
c convert input into radians
c
      deqlat=hyp_lat(1)
      deqlon=hyp_lon(1)
      depth=hyp_depth(1)
      call fold_lat(deqlat)
      eqlat=deqlat*pi/180
      eqlon=deqlon*pi/180

c
c    set test parameter defaults
c
      call settest(test)

      if(nars.gt.0) nistat=rea_nphase       ! calculate theoretical times for given phases 

      do n=1,nistat ! loop over stations
        open(iustat,status='old',file=infile)  ! station file
        station_name=istat(n)

        if(nars.gt.0) then
           if(rea_phase(n)(1:1).eq.'P'.or.rea_phase(n)(1:1).eq.'S') then
              station_name=rea_stat(n)
           else
              goto 999
           endif
        else
           station_name=istat(n)
        endif

        stat='xxxxx'
        j=-1
        
        do while (stat.ne. '    '.and.j.ne.0)
         read(iustat,'(a80)')testdat
         if(testdat(14:14).eq.')')then      ! read reset test data
          read(testdat,'(a4,t12,i2,t16,f9.4)')stat,j,testj
         elseif(testdat(13:13).eq.')')then
          read(testdat,'(a4,t12,i1,t15,f9.4)')stat,j,testj
         else
          read(testdat,'(a4)')stat
          j=-1
         endif
         if(j.gt.0)then
          test(j)=testj
         endif
        end do
c
c read station coordinates
c
        indat='XXXXXXXX'
        station_found=.false.
        do while (indat(1:8).ne.'        ')
          read(iustat,'(a80)')indat
          if (indat(2:6).eq.station_name) then   ! 5 char station
            if (indat(11:11).eq.'.') then
              read(indat(7:27),'(i2,f5.2,a1,i3,f5.2,a1,i4)')
     &           dlat,mlat,clat,dlon,mlon,clon,height
            else
              read(indat(7:27),'(i2,i2,i3,a1,i3,i2,i3,a1,i4)')
     &           dlat,mlat1,mlat2,clat,dlon,mlon1,mlon2,clon,height
                 mlat=mlat1+mlat2*.001
                 mlon=mlon1+mlon2*.001
            endif
            station_found=.true.
          elseif (indat(3:6).eq.station_name) then   ! 4 char station
            if (indat(11:11).eq.'.') then
              read(indat(7:27),'(i2,f5.2,a1,i3,f5.2,a1,i4)')
     &           dlat,mlat,clat,dlon,mlon,clon,height
            else
              read(indat(7:27),'(i2,i2,i3,a1,i3,i2,i3,a1,i4)')
     &           dlat,mlat1,mlat2,clat,dlon,mlon1,mlon2,clon,height
                 mlat=mlat1+mlat2*.001
                 mlon=mlon1+mlon2*.001
            endif
           station_found=.true.
          endif
        end do
c
c max height
c
        if (height.gt.maxh) maxh=height

        if (.not.station_found) then
           write(*,*) 'station not in list ',station_name
           close(iustat)
           goto 999
c           stop
        else
c
c  convert coordinates
c
         slat = dlat + real(mlat)/60. 
         if(clat .eq. 'S') slat = -slat
         slon = dlon + real(mlon)/60. 
         if(clon .eq. 'W') slon = -slon
c         write(*,*) ' station ',slat,slon

         call fold_lat(slat)
         sslat=slat
         sslon=slon

         slat=slat*pi/180
         slon=slon*pi/180
c get distance
         call delaz(slat,slon,dist,dedeg,az0,eqlat,eqlon)
         delta=dedeg
        endif

c read the velocity model
        i=1
        iustat=9
        
c    reff is used to specify the moho layer for PN calculation
5       read(iustat,105,end=99)v(i),d(i),vs(i),reff
105     format(3f7.3,a1)
        if(ucase(reff).eq.'N')nmoho=i

c 4/94: added nconrad variable
        if(ucase(reff).eq.'B')nconrad=i
        
        if(v(i).eq.0.0)go to 6
        i=i+1
        go to 5

c    nl is the number of layers in the velocity model
6       nl=i-1

c    read in trial depth and vp/vs ratio
        read(iustat,'(3f5.0,f5.2)',end=99)ztr,xnear,xfar,pos
99      continue
        close(9)

c    if vs(1)=0 then set vs(i)=v(i)/pos
        if(vs(1).eq.0.0)then
          do i=1,nl
            vs(i)=v(i)/pos
c            write(*,*) i,v(i),pos
          end do
        endif

c  store thicknesses in parm
        do i=1,nl-1
          parm(nl+i)=d(i+1)-d(i)
          if (i.eq.1) parm(nl+i)=parm(nl+i)+10.  ! models reference is 10km
        end do

        do i=1,nl
          parm(i)=v(i)
        end do
        nn=2*nl-1

        minflag=int(test(63))      
        
c    xs(1), xs(2) are the station long. and lat.
        x0(1)=slon
        x0(2)=slat
c        x0(3)=height/1000.
        x0(3)=10.-height/1000.
      
        prmd=' '                        
        prm2=' '

        xh(1)=eqlon
        xh(2)=eqlat
        xh(3)=10.+depth


         do i=1,nl
           parm(i)=v(i)
         end do

c
c  calculate travel time
c

        if (dist.le.test(57)) then
c
c  local, added N and G to phase names and always calculate G-phase
c  in addition to first arrival, jh nov 2020
c
          if(nars.gt.0) then  ! for multievent, use phase defined
             prmd=rea_phase(n)(2:2)
             if(prmd.eq.'g') prmd='G'
             if(prmd.eq.'n') prmd='N'
             if(prmd.eq.'b') prmd='B'
          endif

          phsid=' '
          call dtdx2(xh,x0,prmd,nmoho,nconrad,0,tmin,
     &    dx,delta,ann,iflag,phsid)
          write(*,*) n,station_name,dist,phsid(2:3),tmin

          prmd=' '

          call timsec(hyp_year(1),hyp_month(1),
     &  hyp_day(1),hyp_hour(1),hyp_min(1),hyp_sec(1),osec)

c
c   add noise if required, between -0.5 0.5 multipled by factor noise
c
          pnoise=(0.5-rand())*noise
          snoise=(0.5-rand())*pos*noise      
 
          if(nars.eq.2) then
             osec=osec+tmin+pnoise
          else
             osec=osec+tmin
          endif

          call sectim(osec,year,doy,month,day,hour,min,sec)
          phase_out='P'//phsid(2:3)//'     '
          write(24,200)station_name,'  ',phase_out,
     +          hour,min,sec

          if(rea_phase(n)(1:1).eq.'P') then
             rea_hour(n)=hour
             rea_min(n)=min
             rea_sec(n)=sec
          endif
c
c   S
c
          call timsec(hyp_year(1),hyp_month(1),
     &  hyp_day(1),hyp_hour(1),hyp_min(1),hyp_sec(1),osec) 
          
          tmin=tmin*pos

          if(nars.eq.2) then
             osec=osec+tmin+snoise
          else
             osec=osec+tmin
          endif

          call sectim(osec,year,doy,month,day,hour,min,sec)
          phase_out='S'//phsid(2:3)//'     '
          write(24,200)station_name,'  ',phase_out,
     +          hour,min,sec
c
c   put into rea structure
c
          if(rea_phase(n)(1:1).eq.'S') then
          rea_hour(n)=hour
          rea_min(n)=min
          rea_sec(n)=sec
          endif        

c
c   calculate G-phases if not done above
c
          if(phsid(2:2).ne.'G') then

             prmd='G'
             phsid=' '
             call dtdx2(xh,x0,prmd,nmoho,nconrad,0,tmin,
     &       dx,delta,ann,iflag,phsid)
             write(*,*) n,station_name,dist,phsid(2:3),tmin

             call timsec(hyp_year(1),hyp_month(1),
     &       hyp_day(1),hyp_hour(1),hyp_min(1),hyp_sec(1),osec) 
             osec=osec+tmin
             call sectim(osec,year,doy,month,day,hour,min,sec)
             phase_out='P'//phsid(2:3)//'     '
             write(24,200)station_name,'  ',phase_out,
     +          hour,min,sec

             call timsec(hyp_year(1),hyp_month(1),
     &       hyp_day(1),hyp_hour(1),hyp_min(1),hyp_sec(1),osec) 
             osec=osec+tmin*pos
             call sectim(osec,year,doy,month,day,hour,min,sec)
             phase_out='S'//phsid(2:3)//'     '
             write(24,200)station_name,'  ',phase_out,
     +          hour,min,sec
          endif


200       format(1x,a5,a2,1x,'Y',a8,i2,i2,1x,f5.2)
        endif  

999     continue
      enddo

      call rea_event_out(10,all,data,i)
c
c  if multi file calculation of times, get next event
c
      if(nars.gt.0) goto 1

1000  continue
 
      close(23)
      close(24)
      close(10)
      if(nars.gt.1) write(6,*)'Output file is ttlocal.out'

      stop
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine fold_lat(alat)
c
c-------- given geographic lat compute geocentric lat
c
c input:  la     degree portion of latitude in degrees
c         ins    n for north, s for south
c         ala    minutes portion of latitude
c         lo     degree portion of longitude
c         iew    e for east, w for west
c         alo    minutes portion of longitude
c output: alat   geocentric latitude in radians
c         alon   longitude in radians
      parameter (pi = 3.14159265)
      parameter (twopi = 2.0*pi)
      parameter (halfpi = 0.5*pi)
      parameter (rad = pi/180.)
      parameter (deg = 1.0/rad)
      parameter (equrad = 6378.2064)
      parameter (polrad = 6356.5838)
      parameter (flat = (equrad - polrad)/equrad)
      parameter (c1 = (1.0 - flat)**2)
      parameter (c2 = halfpi*(1.0/c1 - 1.0))
c
c
      alat = alat*rad
c  convert from geographic to geocentric latitude
      if (halfpi-abs(alat) .ge. 0.02) goto 201
         alat = alat/c1-sign(c2,alat)
         goto 202
  201    alat = atan(c1*tan(alat))
  202    continue
         alat=alat/rad
      return
      end


