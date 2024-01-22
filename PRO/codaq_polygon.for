c
c   reads codaq.area and average results in polygon created by epimap, file 
c   epimap.cor. The epimap.cor values are accumulated in a file POLYGON.MAP
c   to be plotted with epimap
c
c   jh oct 2018
c
c   changes
c  jan 2  2019 jh: also errors in q10, output of codaq_polygon.area
c  dec 17 2020 jh: output in a file
c  jan 22 2023 jh. fixt linfist so the same as in codaq, now sd larger
c
      implicit none
      include 'seisan.inc'
      character*120 txt          ! general text
      integer seiclen            ! function
      character*60 top_directory 
      character*1 dchar          ! directory separation character
      character*80 infile        ! input area file
      
      real ff(10)                ! frequencies to use
      real xq(10000)             ! q-values for one frequency
      real xqsd(10)              ! sd of xq
      integer nnq(10)            ! number of q's for one frequency

      integer nfreq              ! number of frequencies
      real f,freq                ! frequecy to use
      integer n_used             ! q values used
      character*80 text          
      integer nq(10)             ! number of q-values for each  f
      character*1 append_map     ! y or blank if appending
      integer mode               ! how to weight
      


      real av
      integer nav

      real alalo(100,2)           ! poligon area
      integer narea
      logical inside
      real q(10,10000)            ! q values for each  f
      real q0                     ! q0
      real q10                    ! q at 10 hz 
      real sd100                  ! sd of q10

      real qalpha                ! qalpha in each grid point
      real lat,lon               ! lat  lon of observation point
      real q1                    ! help variables
      
      real cs                    ! constant ss (qalpha) value
      real crms                  ! rms when using a constant ss
      real sdcq                  ! standard deviation in qc
      real cq                    ! q0 whan using a constant ss
      real sdq0                  ! standard deviation in q0
      real ss         
      real sdss                  ! standard deviation in ss
      real corr                  ! correlation coefficient
      real rms                   ! rms error of fit
      integer i,j,k,n,if,m,k1

c
c   get seisan defaults
c
      call get_seisan_def
c                                                                              
c   get path to seismo                                                          
c                                                                               
      call topdir(top_directory)                                                
      call dir_char(dchar)   ! directory separation character
c   
      do i=1,10
        nq(i)=0
      enddo
      open(7,file='codaq_polygon.out',status='unknown')
      open(3,file='codaq_polygon.area',status='unknown')

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
      write(7,'(a,10f8.2)') 'Frequencies', (ff(i),i=1,nfreq)

c
c   open polygon file
c
      i=1

      open(1,file='epimap.cor',status='old',err=37)
 34   continue
      read(1,*,end=36) alalo(i,2),alalo(i,1)
      i=i+1      
      goto 34
 37   continue
      write(6,'(a)')'Polygon file epimap.cor missing'
      stop
 36   continue     
      narea=i-1
      close(1)
      write(6,'(a,2x,i4)')'Number of points defining polygon:',narea
      write(7,'(a,2x,i4)')'Number of points defining polygon:',narea

c
c   open midpoints file
c  

 9    continue

      open(1,file='codaq.area',status='old',err=40) 

      read(1,'(a)') text    ! read header
c
c  start reading file
c
      

 10   continue
      read(1,'(a)',end=20) text
      read(text,'(25x,4f7.1)',end=20) lat,lon,f,q1
c
c   find which index for frequency
c
      do i=1,nfreq
        if(f.eq.ff(i)) if=i
      enddo

      inside=.false.
c
c   find if inside polygon
c

      call polos(lat,lon,alalo,narea,inside)

      if(inside) then     ! save value
         nq(if)=nq(if)+1
         q(if,nq(if))=1.0/q1  ! store inverse values for average
         n_used=n_used+1
c
c  save selcted area file
c
         write(3,'(a)') text
      endif
      goto 10
 40   continue
      write(6,*)'No codaq.area file'
      stop

 20   continue

      do i=1,nfreq
        write(6,*) 'f=',ff(i)
        write(7,*) 'f=',ff(i)
        write(6,'(10f7.0)')(1.0/q(i,k),k=1,nq(i))
        write(7,'(10f7.0)')(1.0/q(i,k),k=1,nq(i))
      enddo

      write(6,'(a,3x,i6)')'Number of Q-midpoints within polygon:',
     * n_used
      write(7,'(a,3x,i6)')'Number of Q-midpoints within polygon:',
     * n_used


c
c   average 1/q values 
c
      do if=1,nfreq
         if(nq(if).ge.2)then  ! must
             do m=1,nq(if)
               xq(m)=q(if,m)  ! still 1/q
             enddo

c
c   put av in index 1 and sd in index 2
c
              call sdv(nq(if),xq,q(if,1),
     *        q(if,2))

              q(if,2)=
     *        q(if,2)/(q(if,1)*q(if,1))  ! since 1/values
c
              q(if,1)=1.0/q(if,1)     ! back to q
c
c   write out
c
              write(6,'(a,f4.1,a,f6.1,a,f6.1,a,i5)') 
     *        'f=',ff(if),'  av=',q(if,1),'  sd=',q(if,2), 
     *        '  n=',nq(if)
              write(7,'(a,f4.1,a,f6.1,a,f6.1,a,i5)') 
     *        'f=',ff(if),'  av=',q(if,1),'  sd=',q(if,2), 
     *        '  n=',nq(if)
          else
              if(nq(if).eq.1) q(if,1)=1.0/q(if,1)
              if(nq(if).eq.0) q(if,1)=0.0
              q(if,2)=0.0
          endif
      enddo


c
c  transfer to variables for fit
c
           k=0 
           k1=0
           do if=1,nfreq
             xq(if)= q(if,1)
             xqsd(if)=q(if,2)
             nnq(if)= nq(if)
             if(nnq(if).gt.0) k1=k1+1  ! count how many frequencies have data, also if one data point
             if(nnq(if).gt.1) k=k+1    ! count how many frequencies have data, only 2 or more
           enddo
c
c   calculate q0 and qalpha, require 3 frequencies to have data
c
           if(k.ge.3) then
              mode=1
              call qzero
     *        (nfreq,ff,xq,nnq,xqsd,cs,cq,crms,sdcq,q0,sdq0,
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

           write(6,'(a,f7.1,a,f5.1,a,f5.2,a,f5.2,a,f6.1,a,f5.1,a,f5.2,a,
     *     f5.2,a,i2)') 
     *     'q0=',q0,'  sd',sdq0,'  qalpha=',qalpha,'  sd',sdss,
     *     '  q10=', q10,'  sd=',sd100, '  corr=',corr,
     *     '  rms=',rms,'  nf=',k

           write(7,'(a,f7.1,a,f5.1,a,f5.2,a,f5.2,a,f6.1,a,f5.1,a,f5.2,a,
     *     f5.2,a,i2)') 
     *     'q0=',q0,'  sd',sdq0,'  qalpha=',qalpha,'  sd',sdss,
     *     '  q10=', q10,'  sd=',sd100, '  corr=',corr,
     *     '  rms=',rms,'  nf=',k
           
c
c   all data, no weight
c
              mode=0

              call qzero
     *        (nfreq,ff,xq,nnq,xqsd,cs,cq,crms,sdcq,q0,sdq0,
     *        qalpha,sdss,corr,rms,mode)

c
c   calculate q10
c
           call calq(10.0,q0,sdq0,qalpha,sdss,q10,sd100)


           write(6,*)
           
           write(6,*)'Use all data, no weight'
           write(6,'(a,f7.1,a,f5.1,a,f5.2,a,f5.2,a,f6.1,a,f5.1,a,f5.2,a,
     *     f5.2,a,i2)') 
     *     'q0=',q0,'  sd',sdq0,'  qalpha=',qalpha,'  sd',sdss,
     *     '  q10=', q10,'  sd=',sd100, '  corr=',corr,
     *     '  rms=',rms,'  nf=',k

           write(7,*)'Use all data, no weight'
           write(7,'(a,f7.1,a,f5.1,a,f5.2,a,f5.2,a,f6.1,a,f5.1,a,f5.2,a,
     *     f5.2,a,i2)') 
     *     'q0=',q0,'  sd',sdq0,'  qalpha=',qalpha,'  sd',sdss,
     *     '  q10=', q10,'  sd=',sd100, '  corr=',corr,
     *     '  rms=',rms,'  nf=',k

c
c   save polygon values in file that can be plotted with epimap
c
      write(6,*)
      write(6,*) 'Append to POLYGON.MAP (n/y=default)'
      read(5,'(a1)') append_map
      if(append_map.eq.' '.or.append_map.eq.'y') then
         open(1,file='POLYGON.MAP',status='unknown',access='append')
         write(1,'(i4)') narea
         write(1,'(10f8.3)') (alalo(i,2),alalo(i,1),i=1,narea)
         close(1)
      endif

      write(6,*)
      write(6,*)'Accumulated polygon coordinates in POLYGON.MAP'

      close(3)
      write(6,*)'Area file selected is codaq_polygon.area'

      write(6,*)'Ouput file is codaq_polygon.out'
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
 61   if(mode) 62,64,62 
 62   varnce=1.0
      go to 67
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
c                 write(6,*)' qzero: fre(i),q(i)',nq(i),fre(i),q(i),sd(i)

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
