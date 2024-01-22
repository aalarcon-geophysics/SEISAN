       program ph2dt

c   for seisan 2020, all subroutines put into main. routine datetime renamed
c   since it conflicts with c routine of same name, interactive input fixed
c   so CR can be used by adding end=

c Version 1.3 - 08/2010  same as 1.2, but now compiles with gfortran
c                         (rcs removed)
c Version 1.2 - 07/2010
c Version 1.1 - 10/2004
c Author: Felix Waldhauser, felixw@ldeo.columbia.edu
c
c started 03/1999
c 01-03/2001  clean up & bug fixes by Bruce Julian, Fred Klein, Keith
c             Richards-Dinger, Felix Waldhauser 
c 10/2004     Version 1.1: fixed errors listed in BugList to V 1.0.
c 05/2005     accomodate format for neg magnitudes 
c 07/2010     version 1.2
c 08/2010     now compiles with gfortran  (rcs removed and mod problem fixed)
c 08/2010     version 1.3 
c
c Purpose:
c Reads and filters absolute travel-time data from network catalogs
c to form travel-time data for pairs of earthquakes. Control file or
c interactive input of parameters to optimize linkage between events 
c (i.e. optimize quality and minimize number of links between events).
c See hypoDD user guide for information on how to use ph2dt.

c For a user guide to ph2dt see USGS open-file report:
c    Waldhauser, F., HypoDD: A computer program to compute double-difference
c    earthquake locations,  U.S. Geol. Surv. open-file report , 01-113,
c    Menlo Park, California, 2001.
c

c--Reads the input file specified om the command line (ph2dt ph2dt.inp)
c  to get the input station and phase file names, and the numerical parameters.
c--Writes the files (input files to hypoDD):
c  dt.ct	the travel times for the common stations for close 
c               earthquake pairs
c  event.dat	earthquake list in dd format
c  event.sel	selected earthquake list in dd format (should be the same 
c               as event.dat)
c  ph2dt.log	log file
c--Most earthquake selection (dates, magnitudes, etc) happens before ph2dt 
c  processes phase to differential time data.
c--Use the files events.select to select events by ID (one per line).
c--FWK 2/9/01

c The standard input format (free format) is:
c#  yr  mn dy hr mn  sc      lat      lon      dep  mag  eh  ez  res  id
c sta  t-t  wght pha
c e.g.:
c# 1997  6 13 13 44 37.33  49.2424 -123.6192   3.42 3.43 0.0 0.0 0.0  1
c NAB      4.871   1    P
c BIB      5.043   0.5  P
c SHB      7.043   1    S
c WPB      8.934   1    P

c See hypoDD user guide for a description of the parameters.

	implicit none

	include 'ph2dt.inc'

	real		a_dist(MOBS)
	real		alat
	real		alon
	real		aoffs(MEV)
	real		a_time1(MOBS)
	real		a_time2(MOBS)
	doubleprecision	atoangle	! ASCII-to-angle function
	real		avoff
	real		avoff_str
	real		maxoff_str
	real		a_wtr(MOBS)
	real		az
	real		blat
	real		blon
	real		b_time1(MOBS)
	real		b_time2(MOBS)
	character	buf1*20		! Input buffer
	character	buf2*20		! Input buffer
	real		b_wtr(MOBS)
	integer		cuspid(MEV)
	integer		date(MEV)
	character	dattim*25	! Current time
	real		del
	real		depth(MEV)
	real		dist
	real		dlat
	real		dlon
	integer		dy
	logical		ex
	real		herr
	integer		hr(MEV)
	integer		i
	integer		iargc
	integer		icusp(MEV)
	integer		idata
	integer		iformat
	integer		ii
	integer		iimp
	integer		iindx(MOBS)
	integer		inb
	integer		indx(MEV)
	integer		iobs
	integer		iobsP
	integer		ipair
	integer		ipair_str
	integer		ista
	integer		itake
	integer		itmp
	integer		j
	integer		k
	integer		kk
        real            KMPERDEG
	integer		l
	real		lat(MEV)
	integer		limobs_pair
	integer		log
	real		lon(MEV)
	integer		m
	real		mag
	real		maxdist
	integer		maxobs_pair
	real		maxoffset
	integer		minute(MEV)
	integer		minobs_pair
	real		minwght
	integer		mnb
	integer		mo
	integer		n1
	integer		n2
	integer		n3
	integer		n4
	integer		n5
	integer		n6
	integer		n7
	integer		n8
	integer		narguments
	integer		ncusp
	integer		nerr
	integer		nev
	integer		nobs
	integer		nobs_ct(MEV)
	integer		npair
	integer		npha
	integer		nsta
	integer		ok
	real		PI
	real		p_time(MEV,MOBS)
	real		p_wghtr(MEV,MOBS)
	real		res
	real		rtime
	real		sec(MEV)
	real		s_lat(MSTA)
	real		s_lon(MSTA)
	integer		sscanf3		! String-reading function
	integer		trimlen
	real		vel
	real		verr
	real		wtr
	integer		yr
	character	a_lab(MOBS)*7
	character	a_pha(MOBS)*1
	character	b_lab(MOBS)*7
	character	b_pha(MOBS)*1
	character	fn0*80
	logical		fn0ex
	character	fn2*80
	logical		fn2ex
	character	fn9*80
	character	line*180
	character	p_pha(MEV,MOBS)*1
	character	p_sta(MEV,MSTA)*7
	character	s_lab(MSTA)*7
	character	str1*1
	character	str30*30
	character	str*40
	character	take(MEV,MEV)*1

	parameter	(PI=3.141593)
        parameter       (KMPERDEG=111.1949266)

c setpar:
c--Only standard format is now supported. Conversion from other formats occurs externally
      iformat= 0        ! 0=standard; 1=NCSN (non-y2k/y2k);
                        ! 2=pkf_hrsn (Bill's S-plus); 3= arcvel (stefano)
c file with cuspids to select for
      fn9= 'events.select'

      log= 20
      open(log,file='ph2dt.log',status='unknown')
      str= 'starting ph2dt (v1.3 - 08/2010)...'
      call datetime(dattim)
      write(6,'(a40,a)') str, dattim
      write(log,'(a40,a)') str, dattim

c--- get input parameter file name:
      narguments = iargc()
      if(narguments.lt.1) goto 2
      call getarg(1,str30)
      inquire(FILE= str30,exist=ex)
      if(.not. ex) stop' >>> ERROR OPENING INPUT PARAMETER FILE.'

c open/read input parameter file:
      open(1,status='unknown',file=str30(1:trimlen(str30)))
      idata= 2
      l=1
1     read (1,'(a)',end=111) line
      if(line(1:1).eq.'*' .or. line(2:2).eq.'*') goto 1
      if(l.eq.1) read (line,'(a)',err=112) fn2
      if(l.eq.2) read (line,'(a)',err=112) fn0

      if(l.eq.3) then
        read (line,*,err=112) minwght,maxdist,
     &         maxoffset,mnb,limobs_pair,minobs_pair,maxobs_pair
        close (1)
        goto 34
      endif
      l=l+1
      goto 1

c--Error statements
111   stop '** Premature end of command file'
112   write (*,*) '** Error reading data from input line:'
      write (*,'(a)') line
      stop

c--- Get modified (high-resolution) station file name:
2     write (6,'(a)') 'STATION FILE [<ret> station.dat]: '
      read (5,'(a)') fn2
      if (trimlen(fn2).le.1) then
         fn2= 'station.dat' !default input file name
      else
         fn2= fn2(1:trimlen(fn2))
      endif
      inquire (FILE= fn2,exist=fn2ex)
      if (.not. fn2ex) then
          stop' --> ERROR OPENING STATION FILE.'
      endif

c--- Get phase input file name:
      write (6,'(a)') 'PHASE INPUT FILE [<ret> phase.dat]: '
      read (5,'(a)') fn0
      if(trimlen(fn0).le.1) then
         fn0= 'phase.dat'       !default input file name
      else
         fn0= fn0(1:trimlen(fn0))
      endif
      inquire(FILE= fn0,exist=fn0ex)
      if(.not. fn0ex) then
          stop' --> ERROR OPENING PHASE FILE.'
      endif

c--- Set phase format: only standard format now supported
      iformat= 0

c--- get max pick quality
      write(6,'(a)') 'Min. pick weight/quality [<ret> 0]: '
      read(5,'(a)') str
      read(str,*,err=6,end=6)minwght
      goto 7
6     minwght= 0
7     continue

c--- Get max event-station distance
      write(6,'(a)') 'Max. distance between event pair and '//
     & 'station [<ret> 200 km]: '
      read(5,'(a)') str
      read(str,*,err=8,end=8)maxdist
      goto 9
8     maxdist= 200
9     continue

c--- Get max interevent offset (km) for which dtimes are calculated
      write(6,'(a)') 'Max. epicentral separation [<ret> 10 km]: '
      read(5,'(a)') str
      read(str,*,err=10,end=10)maxoffset
      goto 11
10    maxoffset= 10
11    continue

c--- Get number of nearest neighbours
      write(6,'(a)') 'Max. number of nearest neighbours per '//
     & 'event [<ret> 10]: '
      read(5,'(a)') str
      read(str,*,err=20,end=20)mnb
      goto 21
20    mnb= 10
21    continue

c--- Get number of dt-obs per pair so it counts as neighbour
      write(6,'(a)') 'Min. number of links necessary to '//
     & 'define a neighbor [<ret> 8]: '
      read(5,'(a)') str
      read(str,*,err=25,end=25)limobs_pair
      goto 26
25    limobs_pair= 8
26    continue

c--- Get min number of dt-obs per pair
      write(6,'(a)') 'Min. number of links saved per pair [<ret> 8]: '
      read(5,'(a)') str
      read(str,*,err=27,end=29)minobs_pair
      goto 28
27    minobs_pair=  8
28    continue

c--- Get max number of dt-obs per pair
      write(6,'(a)') 'Max. number of links saved per pair [<ret> 50]: '
      read(5,'(a)') str
      read(str,*,err=29,end=29)maxobs_pair
      goto 30
29    maxobs_pair= 50
30    continue

34    continue
      idata=2

c--- open files:
      open(12,file='dt.ct',status='unknown')
      open(14,file='event.dat',status='unknown')
      open(15,file='event.sel',status='unknown')

c--- read icusp's
      ncusp= 0
      inquire(FILE= fn9,exist=ex)
      if(ex) then
         open (1,file=fn9,status='unknown')
         i=1
35       read (1,'(a)',end=36,err=37) line
         read (line,*,err=37) icusp(i)
         i=i+1
         goto 35

c--Get out of read loop on error or end of file
37       write (6,*) '** Error reading id number:'
         write (6,*) line
         stop
36       ncusp= i-1
         close(1)
      endif

      write (*,'(/,"reading data ...")')
      write (log,'("reading data ...")')
c--- read stations
      open (2,file=fn2, status='unknown')
      i=1
40    read(2,'(a)',end=48)line

c       Split into fields separated by white space
	if (sscanf3(line, "%s%s%s", s_lab(i), buf1, buf2) .ne. 3) then
	  write (6,*) line
	  stop '** Bad station line'
	endif
	call rpad(s_lab(i))

c       Convert strings to numbers, interpreting colons, if any.
        s_lat(i) = atoangle(buf1)
        s_lon(i) = atoangle(buf2)

42      if (i.gt.MSTA) stop'>>> Increase MSTA in ph2dt.inc.'
        i= i+1
      goto 40
48    continue

      nsta= i-1
      write (*,*) '> stations = ', nsta
      write (log,*) '> stations = ', nsta
      close(2)

c--- check for double station:
      do i=1,nsta-1
        do j=i+1,nsta
           if (s_lab(i).eq.s_lab(j)) then
               write (*,*) s_lab(i)
               stop'>>> This station is listed twice in station file!'
           endif
        enddo
      enddo

      if(idata.eq.2) goto 666
666   continue	!no cross data

c--- read absolute network travel times:
      open (2,file=fn0, status='unknown')
      i= 1
      ii= 1
      npha= 0
100   continue
c--Inoperable code for reading other formats removed by FWK
      goto 131   !standard format

c--Fatal error on read messages, added by FWK
1290  write (6,*) line
      write (*,'(a)') line
      stop '** Bad earthquake line'
1292  write (6,*) line
      write (*,'(a)') line
      stop '** Bad phase line'

c--- start standard data format
c read header:
130   read (2,'(a)',end=159) line  		! read header line
      if (line(1:1).eq.'#') goto 160 		! store previous event
131   if (ii.eq.1) read(2,'(a)',end=200) line  	! read header line
      if (line(1:1).eq.'#') then
         read (line,*,err=1290) str1,yr,mo,dy,hr(i),minute(i),sec(i),
     &   lat(i),lon(i),depth(i),mag,herr,verr,res,cuspid(i)
         date(i)= yr*10000 + mo*100 + dy
         rtime= hr(i)*1000000 + minute(i)*10000 + sec(i)*100

         ii= ii+1
         k= 1  !phase counter
      else
c read phase data lines
         read (line,*,err=1292) p_sta(i,k),p_time(i,k),p_wghtr(i,k),
     &   p_pha(i,k)
         if (p_wghtr(i,k).ge.0.and.p_wghtr(i,k).lt.minwght) goto 130
         k= k+1
         if (k.gt.MOBS) stop'>>> Increase MOBS ph2dt.inc!'
      endif
      goto 130
c--- end standard format

c--- processing for all formats starts here:
159   if(k.eq.1) goto 200
160   nobs_ct(i)=k-1
      itake= 1
      if (ncusp.gt.0) then
         itake= 0
         do k=1,ncusp
            if (cuspid(i).eq.icusp(k)) itake= 1
         enddo
      endif
c--- event selection
c write header to total event list file:
      write(14,612)date(i),int(rtime),lat(i),
     &            lon(i),depth(i),mag,herr,verr,res,
     &            cuspid(i)

c--Keep an event only if it has more than minobs_pair observations, 
c  and is on ID list if one was given
c--Faulty selection code removed by FWK
      if (itake.eq.1 .and. nobs_ct(i).ge.minobs_pair) then

c write event to selected event list file:
         write (15,612) date(i),int(rtime),lat(i),
     &            lon(i),depth(i),mag,herr,verr,res,
     &            cuspid(i)
         npha= npha+nobs_ct(i)
         i= i+1
         if (i.gt.MEV) stop'>>> Increase MEV in ph2dt.inc!'
      endif

cfw612   format (i8,2x,i8,2x,f8.4,2x,f9.4,2x,
cfw     &       f9.3,2x,f3.1,2x,f6.2,2x,f6.2,2x,f5.2,1x,i10)
612   format (i8,2x,i8,2x,f8.4,2x,f9.4,2x,
     &       f9.3,2x,f4.1,2x,f6.2,2x,f6.2,2x,f5.2,1x,i10)

      if (iformat.eq.0.and.line(1:1).ne.'#') goto 200 	!no ne event
      goto 100

200   nev= i-1
      write(*,*)'> events total = ',ii-1
      write(*,*)'> events selected = ',nev
      write(*,*)'> phases = ',npha
      write(log,*)'> events total = ',ii-1
      write(log,*)'> events selected = ',nev
      write(log,*)'> phases = ',npha

c--- form dtimes:
      write(*,'("forming dtimes...")')
      write(log,'("forming dtimes...")')
      write(log,*)'Reporting missing stations (STA) and '
      write(log,*)'   outliers (STA,ID1,ID2,OFFSET (km),T1,T2,T1-T2):'
      n1= 0
      n2= 0
      n3= 0
      n4= 0
      n5= 0
      n6= 0
      n7= 0
      n8= 0
      nerr= 0

      do i=1,nev
         do j=1,nev
           take(i,j)= '1'
         enddo
      enddo

      ipair= 1
      ipair_str= 1
      avoff= 0
      avoff_str= 0
      maxoff_str= 0
      do i=1,nev

c find nearest neighb:
        do j=1,nev
          dlat= lat(i) - lat(j)
          dlon= lon(i) - lon(j)
          aoffs(j)= sqrt( (dlat*KMPERDEG)**2 +
     &                    (dlon*(cos(lat(i)*PI/180)*KMPERDEG))**2 +
     &                 (depth(i)-depth(j))**2)
        enddo
        aoffs(i)= 99999
        call INDEXX(nev,aoffs,INDX)

        inb= 0
        nobs= 0
        do m=1,nev-1   ! same event last
          if(inb.ge.mnb) goto 400    ! next event

          k= indx(m)	! get nearest first
          if(take(k,i).eq.'0') then	! already selec as strong neighb
             inb= inb+1
             goto 350	
          elseif(take(k,i).eq.'9') then	! already selec as weak neighb
             goto 350
          endif

          n1=n1+1

c check for max interevent offset:
          if(aoffs(indx(m)).gt.maxoffset) goto 400	! next event

c search for common stations/phases:
          iobs= 0
          iobsP= 0
          iimp= 0   !obs needs to be included regardless weight or dist
          do j=1,nobs_ct(i)
            do l=1,nobs_ct(k)
               if(p_sta(i,j).eq.p_sta(k,l).and.
     &            p_pha(i,j).eq.p_pha(k,l)) then

                  if(p_pha(i,j).eq.'P') n3= n3+1
                  if(p_pha(i,j).eq.'S') n6= n6+1
c check for station label in station file:
                  do ii= 1,nsta
                    ok= 0
                    if(iformat.eq.1) then   !moved station labels..
                      if( (p_sta(i,j)(6:6).eq.' '.and.
     &                  p_sta(i,j)(1:5).eq.s_lab(ii)(1:5)) .or.
     &                 (p_sta(i,j)(6:6).ne.' '.and.
     &                  p_sta(i,j)(1:6).eq.s_lab(ii)(1:6))) ok= 1
                    else
                      if(p_sta(i,j).eq.s_lab(ii)) ok= 1
                    endif
c select for station - pair centroid  distance
                    if(ok.eq.1) then
                         alat= s_lat(ii)
                         alon= s_lon(ii)
                         blat= (lat(i)+lat(k))/2
                         blon= (lon(i)+lon(k))/2
	                 call delaz(alat,alon,blat,blon,del,dist,az)

c delete far away stations 
                         if(dist.gt.maxdist) then
                             n5= n5+1
                             goto 300
                         endif
                         goto 250
                    endif
                  enddo
                  n4= n4+1
                  write(log,*)'Station not in station file: ',p_sta(i,j)
                  goto 300
250               continue
                  ista= ii

c get average weight: 8/16/00
                   wtr = (abs(p_wghtr(i,j)) + abs(p_wghtr(k,l)))/2

c remove outliers above the separation-delaytime line:
                  if(p_pha(i,j).eq.'P') vel= 4.
                  if(p_pha(i,j).eq.'S') vel= 2.3
                  if(abs(p_time(i,j)-p_time(k,l)).gt.
     &               aoffs(indx(m))/vel + 0.5) then  
                     write(log,'(a,a7,2i9,4f9.3)')'Outlier: ',
     & p_sta(i,j),cuspid(i),cuspid(k),aoffs(indx(m)),p_time(i,j),
     & p_time(k,l),p_time(i,j)-p_time(k,l)
                     nerr= nerr+1  
                     goto 300	
                  endif

                  iobs=iobs+1
                  if(p_pha(i,j).eq.'P') iobsP= iobsP+1
                  nobs=nobs+1
                  a_lab(iobs)= s_lab(ista)
                  a_time1(iobs)= p_time(i,j)
                  a_time2(iobs)= p_time(k,l)
                  a_wtr(iobs)= wtr
                  a_dist(iobs)= dist     !distance to station
                  a_pha(iobs)= p_pha(i,j)

                  if(p_wghtr(i,j).lt.0 .or. p_wghtr(k,l).lt.0) then
                     a_dist(iobs)= 0  	! set to 0 so it will be selected first
                     iimp= iimp+1
                  endif
                  goto 300
               endif
            enddo
300         continue   ! next station / observation
          enddo

          itmp= iobs
          if(iobs.gt.maxobs_pair) itmp= min(maxobs_pair+iimp,iobs)   !add important ones
          if(iobs.ge.minobs_pair) then  !pair selected

c sort obs by distance:
              if(iobs.gt.1) then
                call INDEXX(iobs,a_dist,iindx)
                do kk=1,iobs
                    b_lab(kk)=   a_lab(iindx(kk))
                    b_time1(kk)= a_time1(iindx(kk))
                    b_time2(kk)= a_time2(iindx(kk))
                    b_wtr(kk)=   a_wtr(iindx(kk))
                    b_pha(kk)=   a_pha(iindx(kk))
                enddo
              else
                b_lab(1)=   a_lab(1)
                b_time1(1)= a_time1(1)
                b_time2(1)= a_time2(1)
                b_wtr(1)=   a_wtr(1)
                b_pha(1)=   a_pha(1)
              endif

c write out delay times:
              write(12,'(a1,1x,i9,1x,i9)')'#',cuspid(i),cuspid(k)
              do kk= 1,itmp
                 write(12,'(a7,1x,f7.3,1x,f7.3,1x,f6.4,1x,a1)')
     &           b_lab(kk)(1:7),b_time1(kk),b_time2(kk), b_wtr(kk),
     &           b_pha(kk)
                 if(b_pha(kk).eq.'P') n7= n7+1
                 if(b_pha(kk).eq.'S') n8= n8+1
              enddo
              avoff= avoff + aoffs(k)
              ipair= ipair+1
          endif

          if(iobs.ge.limobs_pair) then	! select as strong neighbor
             take(i,k)= '0'
             inb= inb+1
             ipair_str= ipair_str+1
             avoff_str= avoff_str + aoffs(k)
             if(aoffs(k).gt.maxoff_str)maxoff_str= aoffs(k)
          else				! weak neighbor
             take(i,k)= '9'
          endif

350       continue  ! next neighbour
        enddo
400     continue  ! next event
        if(inb.lt.mnb) n2=n2+1
      enddo
      npair= ipair-1
      avoff= avoff/npair
      avoff_str= avoff_str/(ipair_str-1)

      write(*,*)'> P-phase pairs total = ',n3
      write(*,*)'> S-phase pairs total = ',n6
      write(*,*)'> outliers = ',nerr,' (',nerr*100/(n3+n6),'%)'
      write(*,*)'> phases at stations not in station list = ',n4
      write(*,*)'> phases at distances larger than MAXDIST = ',n5
      if(n3.gt.0)
     & write(*,*)'> P-phase pairs selected = ',n7,' (',n7*100/n3,'%)'
      if(n6.gt.0)
     & write(*,*)'> S-phase pairs selected = ',n8,' (',n8*100/n6,'%)'
      write(*,*)'> weakly linked events = ',n2,' (',n2*100/nev,'%)'
      write(*,*)'> linked event pairs = ',ipair
      write(*,*)'> average links per pair = ',(n7+n8)/ipair
      write(*,*)'> average offset (km) betw. linked events = ',
     & avoff
      write(*,*)'> average offset (km) betw. strongly linked events = ',
     & avoff_str
      write(*,*)'> maximum offset (km) betw. strongly linked events = ',
     & maxoff_str
      write(log,*)'> P-phase pairs total = ',n3
      write(log,*)'> S-phase pairs total = ',n6
      write(log,*)'> outliers = ',nerr,' (',nerr*100/(n3+n6),'%)'
      write(log,*)'> phases at stations not in station list = ',n4
      write(log,*)'> phases at distances larger than MAXDIST = ',n5
      if(n3.gt.0)
     & write(log,*)'> P-phase pairs selected = ',n7,' (',n7*100/n3,'%)'
      if(n6.gt.0)
     & write(log,*)'> S-phase pairs selected = ',n8,' (',n8*100/n6,'%)'
      write(log,*)'> weakly linked events = ',n2,' (',n2*100/nev,'%)'
      write(log,*)'> linked event pairs = ',ipair
      write(log,*)'> average links per pair = ',(n7+n8)/ipair
      write(log,*)'> average offset (km) betw. linked events = ',
     & avoff
      write(log,*)'> avg. offset (km) betw. strongly linked events = ',
     & avoff_str
      write(log,*)'> max. offset (km) betw. strongly linked events = ',
     & maxoff_str


      close(2)
      close(12)
      close(14)
      close(15)


	call datetime(dattim)

	write (log,'(/,"Done.  ",a)') dattim
	close (log)

	write (*,'(/,"Done.  ",a)') dattim

      write(*,'(/,a)')'Output files: dt.ct; event.dat; '//
     & 'event.sel; ph2dt.log'
      write(*,'(a)')'ph2dt parameters were: '
      write(*,*)'(minwght,maxdist,maxsep,maxngh,minlnk,'//
     & 'minobs,maxobs)'
      write(*,*)minwght,maxdist,maxoffset,mnb,limobs_pair,minobs_pair,
     & maxobs_pair

      end

      subroutine cluster(log, mev, mcl, npair, minobs,
     & ic1, ic2, in, icusp, acl, acli, apair_n,
     & clust, noclust, nclust)

	implicit none

c	Parameters:
	integer		log
	integer		mev
	integer		mcl
	integer		npair
	integer		minobs
	integer		ic1(*)		! (1..mpair)
	integer		ic2(*)		! (1..mpair)
	integer		in(*)		! (1..mpair)
	integer		icusp(*)	! (1..mev)
	integer		acl(*)		! (1..mev)
	integer		acli(*)		! (1..mev)
	character	apair_n((mev**2-mev)/2)*1
	integer		clust(mcl,*)	! (a..mcl, 1..mev)
	integer		noclust(*)	! (1..mev)
	integer		nclust

c	Local variables:
	integer		iev
	integer		nev
	integer		i
	integer		j
	integer		k
	integer		kk
	integer		n
	integer		ii
	integer		nn
	integer		ia
	integer		ifindi

      write(*,'("clustering ...  ")')


      iev= 0
      do i=1,npair
        do j=1,iev
          if(ic1(i).eq.icusp(j)) goto 10
        enddo
        iev= iev+1
        icusp(iev)= ic1(i)
        do j=1,iev
          if(ic2(i).eq.icusp(j)) goto 10
        enddo
        iev= iev+1
        icusp(iev)= ic2(i)
10      continue
      enddo
      nev= iev
      call sorti(nev,icusp)

      do i=1,npair
         j= ifindi(nev,icusp,ic1(i))
         k= ifindi(nev,icusp,ic2(i))
         if(k.gt.j) then   !map into lower triangle
            kk= k
            k= j
            j= kk
         endif
         if(in(i).ge.minobs) then
            apair_n(((j-1)**2-(j-1))/2+k)= '1'
         else
            apair_n(((j-1)**2-(j-1))/2+k)= '0'
         endif

      enddo

      do i=1,nev
         acl(i)= nev+1
      enddo
      k= 0
      n= 0
      do i=2,nev
         do j=1,i-1
            k=k+1
            if(apair_n(k).eq.'1')then
                if(acl(i).lt.acl(j)) then
                   if(acl(j).eq.nev+1) then
                      acl(j)= acl(i)
                   else
                      ia= acl(j)
                      do ii=1,i
                        if(acl(ii).eq.ia) acl(ii)= acl(i)
                      enddo
                   endif
                elseif(acl(j).lt.acl(i)) then
                   if(acl(i).eq.nev+1) then
                      acl(i)= acl(j)
                   else
                      ia= acl(i)
                      do ii=1,i
                        if(acl(ii).eq.ia) acl(ii)= acl(j)
                      enddo
                   endif
                elseif(acl(i).eq.nev+1) then
                   n= n+1
                   acl(i)= n
                   acl(j)= n
                endif
            endif
         enddo
      enddo

      call indexxi(nev,acl,acli)
      n= 1
      nn= 2
      if(acl(acli(1)).eq.nev+1) then
         i= 1
         goto 300
      else
         clust(n,nn)= icusp(acli(1))
      endif
      do i=2,nev
         if(acl(acli(i)).gt.acl(acli(i-1))) then
            clust(n,1)= nn-1
            n= n+1
            nn= 1
         endif
         if(acl(acli(i)).eq.nev+1) goto 300
         nn= nn+1
         clust(n,nn)= icusp(acli(i))
      enddo
      clust(n,1)= nn-1
      nclust= n
      goto 310
300   nclust= n-1
      do j=i,nev
        noclust(j-i+2)= icusp(acli(j))
      enddo
      noclust(1)= nev-i+1
310   continue
      if(nclust.ge.mcl) stop'>>> Increase MCL in ph2dt.inc.'

c--- sort - biggest cluster first
      if(nclust.gt.1) then
        do i=1,nclust-1
           do j= i+1,nclust
             if(clust(i,1).le.clust(j,1)) then
                do k=1,clust(i,1)+1
                     clust(mcl,k)= clust(i,k)
                enddo
                do k=1,clust(j,1)+1
                     clust(i,k)= clust(j,k)
                enddo
                do k=1,clust(mcl,1)+1
                     clust(j,k)= clust(mcl,k)
                enddo
             endif
           enddo
        enddo
      endif

      k= 0
      do i=1,nclust
        k= k + clust(i,1)
      enddo

      write(*,'(" > clustered events = ",i6)')k
      write(*,'(" > isolated events = ",i6)')
     & noclust(1)
      write(*,'(" > clusters = ",i6)')nclust
      write(*,'(" > largest cluster = ",i6," events")')clust(1,1)

      write(log,'("> Clustered events: ",i6)')k
      write(log,'("> Isolated events: ",i6,/,8(i8,2x))')
     & noclust(1),(noclust(j),j=2,noclust(1)+1)
      write(log,'(/,"> clusters:",i6,"; MINLNKS = ",i4)')nclust,
     & minobs
      k= 0
      do i=1,nclust
         write(log,'("> Cluster",i6,": ",i6," events",/,8(i8,2x))')
     & i,clust(i,1),(clust(i,j),j=2,clust(i,1)+1)
         k= k+clust(i,1)
      enddo
      write(log,*)

      end !of subroutine cluster1
c Current date and time, as a character string
c (This version merely returns a string of blanks)
c
c   conflict with c routiene of same name   jh feb 2020
	subroutine datetimex(dattim)

	implicit none

c	Parameters:
	character	dattim*20

      dattim = '                    '
      return
      end ! of subroutine datetime

c Compute distance and azimuth on a sphere

	subroutine delaz(alat, alon, blat, blon, del, dist, az)

c	computes distance and azimuth from a to b
c	a and b are in decimal degrees and n-e coordinates
c	del -- delta in degrees
c	dist -- distance in km
c	az -- azimuth from a to b clockwise from north in degrees

c	Original author:  Bruce Julian
c	new values for radius used (from Edi Kissling)

	implicit none

c	Parameters:
	real	alat, alon	! Coordinates of first point
	real	blat, blon	! Coordinates of second point
	real	del		! Central angle (degrees)
	real	dist		! Distance (km)
	real	az		! Azimuth from a to b (degrees)

	real	xtop
	real	xden

c	Local variables:
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

c	Built-in functions:  Declarations not needed
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

	xtop = dsin(diflon)
	xden=(dsin(acol)/dtan(bcol))-dcos(diflon)*dcos(acol)
	azr=atan2(xtop,xden)
c----- convert to degrees
	del=delr/rad
	az=azr/rad
	if(az.lt.0.0) az=360.+az
c-----compute distance in kilometers
	colat=pi2-(alatr+blatr)/2.d0
	radius=6378.163d0*
     &	(1.d0+3.35278d-3*((1.d0/3.d0)-(dcos(colat)**2)))
	dist=delr*radius
	return
c  ***** end of subroutine delaz *****
	end
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
	subroutine indexx(n, arrin, indx)

	implicit none

c	Parameters:
	integer	n
	real	arrin(n)
	integer	indx(n)

c	Local variables:
	integer	i
	integer	indxt
	integer	ir
	integer	j
	integer	l
	real	q

      if (n.lt.1) then
         return
      else if (n.eq.1) then
         indx(1) = 1
         return
      endif

      DO 11 J=1,N
        INDX(J)=J
11    CONTINUE
      L=N/2+1
      IR=N
10    CONTINUE
        IF(L.GT.1)THEN
          L=L-1
          INDXT=INDX(L)
          Q=ARRIN(INDXT)
        ELSE
          INDXT=INDX(IR)
          Q=ARRIN(INDXT)
          INDX(IR)=INDX(1)
          IR=IR-1
          IF(IR.EQ.1)THEN
            INDX(1)=INDXT
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
20      IF(J.LE.IR)THEN
          IF(J.LT.IR)THEN
            IF(ARRIN(INDX(J)).LT.ARRIN(INDX(J+1)))J=J+1
          ENDIF
          IF(Q.LT.ARRIN(INDX(J)))THEN
            INDX(I)=INDX(J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 20
        ENDIF
        INDX(I)=INDXT
      GO TO 10
      END
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
c Length of character string, excluding trailing blanks

	integer function trimlen(t)

	implicit none

c	Parameter:
	character t*(*)

      do 1 trimlen=LEN(t),1,-1
    1    if(t(trimlen:trimlen).ne.' ')RETURN
      trimlen=1
      end ! of integer function trimlen
