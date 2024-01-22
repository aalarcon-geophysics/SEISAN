c
c   average the models in h_models .out. The models averaged are in 
c   a given rms range
c
c   jh mar 2021
c

      implicit none
      
      character*200 text   ! one line
      character*80 infile  ! input file
      real rms,rms1,rms2   ! rms and range
      real rmsmin          ! minimum rms
      integer k,i,m        ! counters
      integer ngrid        ! number of tests
      real x(100000),y(100000) ! data 
      real xmax            ! max rms
      real z(25,100000)        ! maximum 8 layers
      real av(25),sd(25)
c
c   get range to use
c
    
 3    continue
      write(6,*) 'rms range to average(r) or '
      write(6,*) 
     *'range determined by % increase in minimum rms(p=default)'
      read(5,'(a)') text
      if(text(1:1).eq.'r') then
         write(6,*)'rms range'
         read(5,*) rms1,rms2
         goto 4
      elseif(text(1:1).eq.'p'.or.text.eq.' ') then
         write(6,*)'percentage'
         read(5,*) rms2
         rms1=-1.0
         goto 4
      endif
      goto 3
 4    continue


      write(6,*)'Input file, def is h_models.out'
      read(5,'(a)') infile
      if(infile.eq.' ') infile='h_models.out'
      open(1,file=infile,status='old')
c
c   first find min rms
c
 5    continue
      read(1,'(a)',end=6)text
      if(text(1:25).eq.'  Minimum and maximum rms') then
         read(text(26:70),*) rmsmin, xmax
         goto 6
      endif
      goto 5
 6    continue
      rewind(1)
      write(6,*)'Minimum rms is ',rmsmin
c
c   percentage
c
      if(rms1.lt.0.0) then
         rms2=rmsmin*(rms2/100.0)+rmsmin
         rms1=0.0
      endif
c
      
      i=0
      ngrid=0
 10   continue
      read(1,'(a)',end=20) text
      if(text.eq.' ') goto 20
      ngrid=ngrid+1
      read(text(1:7),*) rms
c
c   read whole line into variables
c
      if(rms.ge.rms1.and.rms.le.rms2) then
         i=i+1
         do m=1,25
           k=10+(m-1)*6
           read(text(k:k+5),'(f6.3)') z(m,i)
         enddo
      endif
      goto 10

 20   continue

      open(2,file='gridmin.out',status='unknown')
      
c
c   find avrage values
c
      do m=1,25
        do k=1,i
          x(k)=z(m,k)
        enddo

        call sdv(i,x,av(m),sd(m))
      enddo

      write(6,'(a,i8)')    'Number of iterations         ',ngrid
      write(6,'(a,i8)')    'Number of values in rms range',i
      write(6,'(a,2f8.3)') 'RMS range                    ',rms1,rms2
      write(6,'(a,2f7.2)') 'Vp/Vs ',av(1),sd(1)
      write(6,*)
      write(6,'(a)')
     *'      Vp      SD      Vs      SD       H      SD'     

      write(2,'(a,i8)')    'Number of iterations         ',ngrid
      write(2,'(a,i8)')    'Number of values in rms range',i
      write(2,'(a,2f8.3)') 'RMS range                    ',rms1,rms2
      write(2,'(a,2f7.2)') 'Vp/Vs ',av(1),sd(1)
      write(2,*)
      write(2,'(a)')
     *'      Vp      SD      Vs      SD       H      SD'                     

c



      m=-1
      do k=1,8
         m=m+3
         if(av(m+1).eq.0.0) goto 30
         write(6,'(6f8.2)') 
     *   av(m),sd(m),av(m+1),sd(m+1),av(m+2),sd(m+2)
         write(2,'(6f8.2)') av(m),sd(m),av(m+1),sd(m+1),av(m+2),sd(m+2)
      enddo
 30   continue

c
c   read 30 best models and average
c
      write(6,*)
      write(6,*)
      write(6,*)'Averages for 30 best models'
      write(6,*)

      write(2,*)
      write(2,*)
      write(2,*)'Averages for 30 best models'
      write(2,*)

      do i=1,5
         read(1,'(a)') text
      enddo

      do i=1,30
         read(1,'(a)') text
         do m=1,25
           k=10+(m-1)*6
           read(text(k:k+5),'(f6.3)') z(m,i)
         enddo
      enddo
      i=30

c
c   find avrage values
c

      do m=1,25
        do k=1,i
          x(k)=z(m,k)
        enddo
        call sdv(i,x,av(m),sd(m))
      enddo


      write(6,'(a,2f7.2)') 'Vp/Vs ',av(1),sd(1)
      write(6,*)
      write(6,'(a)')
     *'      Vp      SD      Vs      SD       H      SD'     


      write(2,'(a,2f7.2)') 'Vp/Vs ',av(1),sd(1)
      write(2,*)
      write(2,'(a)')
     *'      Vp      SD      Vs      SD       H      SD'                     
      m=-1
      do k=1,8
         m=m+3
         if(av(m+1).eq.0.0) goto 40
         write(6,'(6f8.2)') 
     *   av(m),sd(m),av(m+1),sd(m+1),av(m+2),sd(m+2)
         write(2,'(6f8.2)') av(m),sd(m),av(m+1),sd(m+1),av(m+2),sd(m+2)
      enddo
 40   continue

      write(6,*)
      write(6,*)'Output file is gridmin.out'

      stop
      end

