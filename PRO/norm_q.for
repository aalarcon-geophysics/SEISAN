c
c   norm_q calculates Q by codaq Q normalization method. The amplitde ratios
c   S to coda and P to coda are calculated by CODAQ proram and contained in 
c   codaq_norm_s.area and codaq_norm_p.area. One or the other file is read
c   for calculation of Qs or Qp. The input files contain the ratios for all
c   components and the user can choose if all or some should be used.
c   The chosen ratios are plotted as a function of either distance or 
c   travel time and the slope is used to calculate Q.
c   The velocity is hardwired to 3.5 and 7.0 km/sec for S and P-waves
c   respectively. The geometrical spreading is default 1.0 but can be 
c   chosen by the user.
c
c   The Q values are calculated for each frequency given in codaq1.par and the
c   standard Q relation Q=Q0**qalpha is calcualted.
c
c   Data can be seleced in a polygon created by epimap, file 
c   epimap.cor. The epimap.cor values can be accumulated in a file POLYGON.MAP
c   to be plotted with epimap.
c
c   jh mar 2019
c
c   changes
c 
c
      implicit none
      include 'seisan.inc'
      include 'seiplot.inc'
      character*120 txt          ! general text
      integer seiclen            ! function
      character*60 top_directory 
      character*1 dchar          ! directory separation character
      character*80 infile        ! input area file
      
      real ff(10)                ! frequencies to use
      real q(10)                 ! q-values for one frequency
      real sdq(10)               ! sd of q
      integer nnq(10)            ! number of q's for one frequency

      integer nfreq              ! number of frequencies
      real f,freq                ! frequecy to use
      integer n_used             ! q values used

      logical use_p_z            ! components to use
      logical use_p_n
      logical use_p_e
      logical use_s_z
      logical use_s_n
      logical use_s_e
      logical polygon            ! true if select in polygon


      real velocity, spreading
      real x(10000), y(10000)          ! work arrays
      real sigmay(10000)               ! sd of input y, not used
      real a,b                         ! for least squares
      real sigmaa, sigmab              ! standard deviation in a and b and q
      integer ifr                      ! frequency counter
      character*80 text       
      character*1 answer  

      character*1 append_map     ! y or blank if appending
      integer mode               ! how to weight     
      character*1 t_or_d         ! trave time of distance (t or d)

      real alalo(100,2)           ! poligon area
      integer narea
      logical inside
      real rat(10,10000)          ! amplitude ratioes for each  f
      integer nrat(10)            ! number amplitude ratios for each frequency
      real dist(10,10000)         ! distances
      real d                      ! one distance
      real ttime(10,10000)        ! travel times
      real traveltime             ! one travel time
      real q0                     ! q0
      real q10                    ! q at 10 hz 
      real sd100                  ! sd of q10

      real qalpha                ! qalpha in each grid point
      real lat,lon               ! lat  lon of observation point
      real ratio,xx,x0              ! help variables
      
      real cs                    ! constant ss (qalpha) value
      real crms                  ! rms when using a constant ss
      real sdcq                  ! standard deviation in qc
      real cq                    ! q0 whan using a constant ss
      real sdq0                  ! standard deviation in q0
      real ss         
      real sdss                  ! standard deviation in ss
      real corr                  ! correlation coefficient
      real rms                   ! rms error of fit

      real av,sd                 ! avrage and sd of ratios

      real cx(10),cy(10)                  ! can be used for colors, also return
      character*1 cha(10)                 ! dummy for xy_plot
      character*80 title                  ! title for plot      
      character*30 xtext,ytext            ! axis title
      integer draw                        ! how to draw
      real xm,ym                          ! help variables

      integer i,j,k,n,if,m,k1

c
c   get seisan defaults
c
      call get_seisan_def


c
c  set defaults for output on screen and one hardcopy file
c
          open(65,file='lsq.eps',status='unknown')  ! open postscript output file
          plotunit=65                               ! use unit 65 for output
          plotoption=1                              ! plot ps
          wsize=60                                  ! use 60 % of screen
          call get_window_size
cxx          if(size_lsq.gt.0) wsize=size_lsq ! from color.def
c
          draw=1    ! only plot symbols
c
c set some postscipt scalings
c
          write(65,*) ' 1.0 0.55 scale'
c

c                                                                              
c   get path to seismo                                                          
c                                                                               
      call topdir(top_directory)                                                
      call dir_char(dchar)   ! directory separation character
c   
      do i=1,10
        nrat(i)=0
        nnq(i)=1
      enddo

      n_used=0
c
c   get frequencies
c
      open(1,file='codaq1.out',status='old',err=25)
      goto 26
 25   continue
      write(6,*)'codaq1.out must be present'
      stop
 26   continue      

      do i=1,100
         read(1,'(a)') text
         if(text(1:5).eq.' Freq') then
           backspace 1
           goto 1
         endif
      enddo

 1    continue
      read(1,'(7x,10f12.1)') ff
      close(1)
      nfreq=0
      do i=1,10
        if(ff(i).ne.0.0) nfreq=nfreq+1
      enddo 
      write(6,'(a,10f8.2)') 'Frequencies', (ff(i),i=1,nfreq)

c
c   open midpoints file, either p or s, s is default
c  

      write(6,*)'Analyze P or S, S is default(enter), else p'
      read(5,'(a)') answer
      if(answer.eq.'p') then
         open(1,file='codaq_norm_p.area',status='old',err=40)
         write(6,*)'Use P-data'
      else
         answer='s'
         open(1,file='codaq_norm_s.area',status='old',err=40)
         write(6,*)'Use S-data'
      endif 
      write(6,*)
     *'Use travel time(t) or distance(d), travel time is default'
      read(5,'(a)')t_or_d
   
      write(6,*)'Geometrical spreading, enter for 1.0'
      read(5,'(a)') text
      if(text.eq.' ') then 
         spreading=1.
      else
         read(text,*) spreading
      endif

      write(6,*)
     *'Components to use, zne, enter for Z for P and N and E for S'
      read(5,'(a)') text
      if(text.eq.' ') then
        use_p_z=.true.
        use_p_n=.false.
        use_p_e=.false.
        use_s_z=.false.
        use_s_n=.true.
        use_s_e=.true.
      else
        use_p_z=.false.
        use_p_n=.false.
        use_p_e=.false.
        use_s_z=.false.
        use_s_n=.false.
        use_s_e=.false.

        do i=1,3
          if(text(i:i).eq.'z') then
            use_p_z=.true.
            use_s_z=.false.
          endif

          if(text(i:i).eq.'n') then
             use_p_n=.true.
             use_s_n=.true.
          endif

          if(text(i:i).eq.'e') then
             use_p_e=.true.
             use_s_e=.true.
          endif  
        enddo
      endif
 
      write(6,*)'Use polygon file (y/n), enter for y'
      read(5,'(a)') text
      if(text.eq.' ') then
         polygon=.true.
      else
         polygon=.false.
      endif

      if(.not.polygon) goto 38
c
c   open polygon file
c
      i=1

      open(3,file='epimap.cor',status='old',err=37)
 34   continue
      read(3,*,end=36) alalo(i,2),alalo(i,1)
      i=i+1      
      goto 34
 37   continue
      write(6,'(a)')'Polygon file epimap.cor missing'
      stop
 36   continue     
      narea=i-1
      close(3)
      write(6,'(a,2x,i4)')'Number of points defining polygon:',narea
 38   continue     
c
c   open midpoints file, either p or s, s is default
c  


c
c  start reading file
c
      read(1,'(a)') text         ! header line     

 10   continue
      read(1,'(a)',end=20) text
cxx
c      write(6,*) text
c
c   components to use
c
c    S
c
      if(answer.eq.'s') then
         if(text(25:25).eq.'Z'.and..not.use_p_z) goto 10
         if(text(25:25).eq.'N'.and..not.use_p_n) goto 10
         if(text(25:25).eq.'E'.and..not.use_p_e) goto 10
      endif
c
c    P
c

      if(answer.ne.'s') then
         if(text(25:25).eq.'Z'.and..not.use_s_z) goto 10
         if(text(25:25).eq.'N'.and..not.use_s_n) goto 10
         if(text(25:25).eq.'E'.and..not.use_s_e) goto 10
      endif

      read(text,'(25x,8f7.1)',end=20,err=10) 
     *lat,lon,f,ratio,traveltime,xx,xx,d


c
c   find which index for frequency
c
      do i=1,nfreq
        if(f.eq.ff(i)) if=i
      enddo


      inside=.false.
c
c   find if inside polygon if polygon is used
c
      if(polygon) then
        call polos(lat,lon,alalo,narea,inside)
      else
        inside=.true.
      endif

cxx
c      inside=.true.
      if(inside) then     ! save value if resonable
         if(ratio.gt.0.1.and.ratio.lt.100.0.and.traveltime.gt.1.0) then
            nrat(if)=nrat(if)+1
            rat(if,nrat(if))=ratio  ! store ratio
            ttime(if,nrat(if))=traveltime
            dist(if,nrat(if))=d
            n_used=n_used+1
c
c  save selcted area file
c
cxx
            write(3,'(a)') text
         endif
      endif
      goto 10

 40   continue
      write(6,*)'No codaq_norm_s.area file'
      stop

 20   continue
c
c-------------------------------------------------------------------------
c   all data read, calculate
c-------------------------------------------------------------------------
c
      if(polygon) then
         write(6,'(a,3x,i6)')
     *  'Number of ratio-midpoints within polygon:',
     *   n_used
      else
         write(6,'(a,3x,i6)')'Number of ratio-midpoints:',
     *   n_used
      endif



      if(answer.eq.'s') then
         velocity=3.5
      else
         velocity=7.0
      endif
c
c   frequency loop
c

      do ifr=1,10

c
c   remove ourliers
c
c       do i=1,nrat(ifr)
c          y(i)=rat(ifr,i)
c       enddo

c       call sdv(nrat(ifr),y,av,sd)
cxx
c       write(6,*)'av,sd', ifr,av,sd
c       n=0
c       do i=1,nrat(ifr)
c          if(abs(rat(ifr,i)-av).lt.1*sd) then
c             n=n+1
c             rat(ifr,n)=rat(ifr,i)
c             dist(ifr,n)=dist(ifr,i)
c             ttime(ifr,n)=ttime(ifr,i)
c          endif
c       enddo
cxx
c       write(6,*)'numbers',nrat(ifr),n
c       nrat(ifr)=n
   

        if(nrat(ifr).gt.5) then    ! only calcualate if at least 5 observations
c         if(n.gt.5) then    ! only calculate if at least 5 observations
           do i=1,nrat(ifr)
             y(i)=alog(rat(ifr,i)*dist(ifr,i)**spreading)           
             if(t_or_d.eq.'d') then
                 x(i)=dist(ifr,i)
              else
                 x(i)=ttime(ifr,i)
              endif
           enddo   

c
c   calulate slope 
c
         call linfit
     *   (x,y,sigmay,nrat(ifr),0,a,sigmaa,b,sigmab,corr)
cxx
         write(10,'(3f12.3)') (x(i),y(i),
     *rat(ifr,i)*dist(ifr,i)**spreading,i=1,nrat(ifr))
c
c   calculate q
c
         if(t_or_d.eq.'d') then
	    q(ifr)=-3.1416*ff(ifr)/(b*velocity)
         else
            q(ifr)=-3.1416*ff(ifr)/b
         endif

         sdq(ifr)=(q(ifr)*sigmab/(-b))



c
c   open plotter
c
             call open_display

             ytext='Amplitude ratio'
             if(t_or_d.eq.'d') then
                xtext='Distance'
             else
                xtext='Travel time'
             endif
             write(title,'(a,f5.1,a,f7.1,3x,a)')
     *       'Frequecy=', ff(ifr),'    Q=',q(ifr), answer
c
c   plot coordinate system and points with seisan routine
c
             call xy_plot
     *       (1,nrat(ifr),x,y,title,xtext,ytext,
     *       600.0,600.0,100.0,100.0,1,1,20.0,
     *       0,cha,i,cx,cy)
c
c   plot line with seisan routine given equation for line and size of coordinate system
c
             call xy_plot_line(b,a,100.0,100.0)
             text='Push any key to stop'
             call  tchars(text,20,750.0,700.0)   ! put command on plot

c
c   call up cursxor so plots remains and wait for input from keyboard
c	 
495       continue
          call xscursr(i,xm,ym)
          if (char(i).eq.'#') goto 495
c
c   close postscript output
c
          call close_post
c
c   close output plot file
c 
          close(65)
c
c   close display and back to alpha screen
c
          call clear_to_alpha
c


c



          write(6,*)nrat(ifr),ff(ifr),q(ifr),sdq(ifr)
       else
          q(ifr)=0.0
       endif
      enddo


c      call calc_normalization_q
c     *(ff, nrat,rat,dist,ttime,3.5,1.0,q,sdq) 

      do i=1,nfreq
          write(6,*)nrat(i),nnq(i),ff(i),q(i),sdq(i)
          if(q(i).ne.0.0) nnq(i)=1 
      enddo
c
c
c   calculate q0 and qalpha, require 3 frequencies to have data
c
           if(nfreq.ge.3) then
              mode=1
              call qzero
     *        (nfreq,ff,q,nnq,sdq,cs,cq,crms,sdcq,q0,sdq0,
     *        qalpha,sdss,corr,rms,mode)
           else
              write(6,*)' less than 3 frequencies for data, stop'
              stop
           endif
c
c   calculate q10
c
           call calq(10.0,q0,sdq0,qalpha,sdss,q10,sd100)


           write(6,*)

           write(6,'(a,f7.1,a,f5.1,a,f5.2,a,f5.2,a,f6.1,a,f5.1,a,
     *     f5.2)')
     *     'q0=',q0,'  sd',sdq0,'  qalpha=',qalpha,'  sd',sdss,
     *     '  q10=', q10,'  sd=',sd100 
           

c
c   save polygon values in file that can be plotted with epimap
c
cxx      write(6,*)
c      write(6,*) 'Append to POLYGON.MAP (n/y=default)'
c      read(5,'(a1)') append_map
c      if(append_map.eq.' '.or.append_map.eq.'y') then
c         open(1,file='POLYGON.MAP',status='unknown',access='append')
c         write(1,'(i4)') narea
c         write(1,'(10f8.3)') (alalo(i,2),alalo(i,1),i=1,narea)
c         close(1)
c      endif

      write(6,*)
      write(6,*)'Accumulated polygon coordinates in POLYGON.MAP'

      close(3)
      write(6,*)'Area file selected is codaq_polygon.area'
c
      
c


      stop
      end 


c
c
C  SUBROUTINE LINFIT
C  
C  PURPOSE
C   MAKE A LEAST-SQUARES FIT TO DATA WITH A STRAIGHT LINE
C       Y = A + B*X
C
C  USAGE
C    CALL LINFIT (X, Y, SIGMAY, NPTS, MODE, A, SIGMAA, B, SIGMAB ,R)
C
C  DESCRIPTIONMETERS
C      
C       X       - ARRAY OF DATA POINTS FOR INDEPENDENT VARIABLE 
C       Y       - ARRAY OF DATA POINTS FOR DEPENDENT VARIABLE
C       SIGMAY  - ARRAY OF STANDARD DEVIATIONS FOR Y DATA POINTS
C       NPTS    - NUMBER OF PAIRS OF DATA POINTS
C       MODE    - DETERMINES METHOD OF WEIGHTING LEAST-SQUARES FIT
C                 +1 (INSTRUMENTAL) WEIGHT(I)=1./SIGMAY(I)**2
C                  0 (NO WEIGHTING) WEIGHT(I)=1.
C                 -1 (STATISTICAL)  WEIGHT(I)=1./Y(I)
C       A       - Y INTERCEPT OF FITTED STRAIGHT LINE
C       SIGMAA  - STANDARD DEVIATION OF A
C       B       - SLOPE OF FITTED STRAIGHT LINE 
C       SIBMAB  - STANDARD DEVIATION OF B
C       R       - LINEAR CORRELATION COEFFICIENT
C
C  SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C    NONE
C  
C  MODIFICATIONS FOR FORTRAN II
C    OMIT DOUBLE PRECISION SPECIFICATIONS
C    CHANGE DSQRT TO SQRTF IN STATEMENTS 67,68, AND 71

      subroutine linfit(x,y,sigmay,npts,mode,a,sigmaa,b,sigmab,r)
      dimension x(*),y(*),sigmay(*)
c
c   accumulate weighed sums
c
 11   sum=0.0
      sumx=0.0
      sumy=0.0
      sumx2=0.0
      sumxy=0.0
      sumy2=0.0
 21   continue
      do 50 i=1,npts
	x1=x(i)
	y1=y(i)
	if(mode) 31,36,38
 31     if(y1) 34,36,32
 32     weight=1.0/y1
	go to 41
 34     weight=1.0/(-y1)
	go to 41
 36     weight=1.0
	go to 41
 38     weight=1.0/sigmay(i)**2
 41     sum=sum+weight
	sumx=sumx+weight*x1
	sumy=sumy+weight*y1
	sumx2=sumx2+weight*x1*x1
	sumxy=sumxy+weight*x1*y1
	sumy2=sumy2+weight*y1*y1
 50   continue
c
c   calculate coefficients and standard deviations
c
 51   delta=sum*sumx2-sumx*sumx
      a=(sumx2*sumy-sumx*sumxy)/delta
 53   b=(sumxy*sum-sumx*sumy)/delta
c
c  patched up to use same varnce at all times jh jun 94
c
c61   if(mode) 62,64,62 
c62   varnce=1.0
c     go to 67
 64   c=npts-2
c
c   modified to not crash with only 2 points
c
      if(c.gt.0) then
	 varnce=(sumy2+a*a*sum+b*b*sumx2
     *   -2.0*(a*sumy+b*sumxy-a*b*sumx))/c
      else
	 varnce=0.0
      endif
 67   sigmaa=sqrt(varnce*sumx2/delta)
 68   sigmab=sqrt(varnce*sum/delta)
 71   r=(sum*sumxy-sumx*sumy)/
     *sqrt(delta*(sum*sumy2-sumy*sumy))
      return
      end


	subroutine qzero
     *  (nfreq,fre,q,nq,sd,cs,cq,crms,sdcq,q0,sdq0,ss,sdss,corr,rms,
     *   mode)
c
c   may 2017 error calculation corrected
c
c   calculates the parameters in the relationship
c   q = q0*f**ss with ss variable or ss fixed
c 
c   input:
c   nfreq:      number of frequencies
c   fre:        the frequencies
c   q:          q - values
c   sd:         standard deviation
c   nq:         number of q values for that frequncy
c   cs:         constant ss value
c   mode:       how to weight
c
c   output:
c   crms        rms when using a constant ss
c   sdqc:       standard deviation in qc
c   cq          q0 whan using a constant ss
c   q0:         q0
c   sdq0        standard deviation in q0
c   ss:         ss
c   sdss:       standard deviation in ss
c   corr:       correlation coefficient
c   rms:        rms error of fit
c
	dimension q(10),sd(10),fre(10),nq(10),x(45000),y(45000),z(45000)
	dimension sigmay(45000)
	integer check,mode
   
	check=0
	nn=0
	ndif=0
	cq=0.0

c
c   weight by using each value nq times jun 94, now use weight by number, also wrong,
c   now use standard deviation/q-value
c

c   at least 2 values so sd is not zero unless mode is zero

	do 20 i=1,nfreq
	   number=nq(i)
	   if((q(i).gt.0.0.and.sd(i).gt.0.and.mode.eq.1).or.
     *        (q(i).gt.0.0.and.mode.eq.0)) then
	      ndif=ndif+1
		 nn=nn+1
		 if(check.eq.1) 
     *           write(3,*)' qzero: fre(i),q(i)',fre(i),q(i)

		 x(nn)=alog(fre(i))
		 y(nn)=alog(q(i))
c		 sigmay(nn)=1.0/sqrt(float(number))   ! wrong, used for many years

                 sigmay(nn)=sd(i)/q(i)

		 cq=cq+y(nn)-cs*x(nn)
	   endif
 20      continue
	 if(ndif.gt.1) then
            if(check.eq.1) write(3,*)' call linfit'
	    call linfit(x,y,sigmay,nn,mode,q0,sigmaq0,ss,sdss,corr)

            if(check.eq.1)write(3,*)' linfit called,q0,ss',q0,ss

            q0=exp(q0)
c
c   this from may 2017
c
            sdq0=sigmaq0*q0
c
c   this was also wrong
c	    sdq0=(10**sigmaq0-1.0)*q0  !same as 10**(sigma+q0) -10**q0 with q0 the log value

	 else
	    q0=0.0
	    sdq0=0.0
	    ss=0.0
 	    sdss=0.0
	    corr=0.0
	    rms=0.0
	 endif
	 if(nn.gt.0) then
	    q1=cq/float(nn)
c 	    cq=10.0**q1
            cq=exp(q1)
	    do 30 l=1,nn
	       crms=crms+(y(l)-q1-cs*x(l))*(y(l)-q1-cs*x(l))
	       z(l)= exp((y(l)-cs*x(l)))
 30         continue
	    if(check.eq.1)  write(6,*)' call sdv for cq'
	    call sdv(nn,z,av,sdcq)
	    crms=sqrt(crms/float(nn)) 
	 else
	    cq=0.0
	    sdcq=0.0
	    crms=0.0
	 endif
	 return
	 end

      subroutine calq(f,q0,sdq0,qalp,sdqalp,q,sdq)
c
c   calculate q in relation q=q0*f**qalpha and coreesponding sd error
c   j havskov with help from luis matias
c
      implicit none
      real f              ! frequency
      real q0,sdq0        ! q0 and sd
      real qalp,sdqalp    ! qalpha and and sd
      real q,sdq          ! q calcualted with sd
      real B,sdB          ! help variables
c
c   this a product of two terms A and B so final sd is
c   A*B*sqrt((sdA/A)**2+ (sdB/B)**2) where sdA and B are sd of A and 
c   B respectively. This assumes that the covariance between A and B
c   is zero. A=q0 and B=f**qalpha. So first the sd of B must be calculated
c   as sdB=(f**qalp)*alog(f)*sdqalp
c
c   see https://en.wikipedia.org/wiki/Propagation_of_uncertainty 
c
      B=f**qalp     
      sdB=(f**qalp)*alog(f)*sdqalp
      q=q0*f**qalp

      sdq=q*sqrt((sdB/B)**2+(sdq0/q0)**2)

      return
      end     

      subroutine calc_normalization_q
     *(fre, amp_number,amp_ratio,amp_dist,ttime,velocity,
     * spreading,q,sdq) 
c
c   calculate q by normalization method from input amplitude ratios and distances
c
c     fre       :  frequencies
c     amp_ratio :  ratio between P or S amplitude and codaq amplitude
c                  at each frequency
c     amp_dist  :  distances, at each frequency
c     ttime     :  travel time
c     amp_number:  number of values for each frequency
c     velocity  :  velocity
c     spreading :  geometrical spreading parameter
c     q         :  calculated q
c     sd        :  standard deviation


      implicit none
      
      real fre(10)                     ! frequencies
      real amp_ratio(10,*)             ! amplitude ratios for normlization method 
      real amp_dist(10,*)              ! distance for normlization method
      real ttime(10,*)                 ! travel time
      integer amp_number(10)           ! number of values for each frequnecy
      real velocity, spreading
      real x(10000), y(10000)          ! work arrays
      real sigmay(10000)               ! sd of input y, not used
      real q(10), sdq(10)              ! q and sd at each frequency
      real rms,corr,a,b
      real sigmaa, sigmab              ! standard deviation in a and b and q
      integer i,k,ifr


      do ifr=1,10
        if(amp_number(ifr).gt.5) then    ! only calcualate if at least 5 observations
           do i=1,amp_number(ifr)
             y(i)=alog(amp_ratio(ifr,i)*amp_dist(ifr,i)**spreading)
             x(i)=amp_dist(ifr,i)
           enddo      
c
c   calulate slope 
c
         call linfit
     *   (x,y,sigmay,amp_number(ifr),0,a,sigmaa,b,sigmab,corr)
c
c   calculate q
c
	 q(ifr)=3.1416*fre(ifr)/b

         sdq(ifr)=-(q(ifr)*sigmab/(-b))

          write(6,*)amp_number(ifr),fre(ifr),q(ifr),sdq(ifr)
       else
          q(ifr)=0.0
       endif
      enddo

      return
      end 
