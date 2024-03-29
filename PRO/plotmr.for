c--------------------------------------------------------------------------
c  plot magnitude residual as a function of distance
c--------------------------------------------------------------------------c
c
c  For detail on parameters and variables naames, see rea.inc
c
c  program is a modification of plotml, jh feb 2023
c  
c  updates
c  
c
      implicit none                       ! force delcaration of all variables
      include 'seidim.inc'                ! dimensions for rea block
      include 'seiplot.inc'               ! seisan graphics
      include 'rea.inc'                   ! parameter common bliock

      character*80 data(5000)             ! s-file with data in text array
      character*80 err_text               ! error text

      character*1 cha(10)                 ! dummy for xy_plot
      character*80 text                   ! general text
      character*80 txt(5000)
      character*80 title                  ! title for plot      
      character*30 xtext,ytext            ! axis title
      real xc,yc
      real tor,slope,corr,rms             ! for lsq analysis 
      real xmin                           ! min time
      character*5 stat(5000)              ! station

      real av_mag                         ! av mag

      integer code                        ! error return code
      integer nevent                      ! number of events in file
      integer nmag			  ! number of stats with magnitude
      real dist(5000)                     ! distance
      real mag(5000)                      ! ml
      real res(5000)                      ! mag residual
      real amp(5000)                      ! amplitude
      real per(5000)                      ! period
      character*3 com(5000)               ! component
      character*8 phase(5000)             ! phase
      character*2 mag_type                ! magnitude type
      real xx,yy,x(10),y(10)              ! help variabels
      integer nars                        ! number of arguments
      integer i,l,k                       ! counter


      call get_seisan_def

      open(1,file='hypmag.out',status='old',err=10)
      goto 11
 10   continue
      write(6,*)' No such input file hypmag.out'
      stop
 11   continue
c
      av_mag=0.0
c
c   read all parameters from 1
c
      i=0
 20   continue
      i=i+1
      read(1,'(a5,1x,a3,1x,a8,2x,4f12.1)',end=25) 
     *stat(i),com(i),phase(i),amp(i),per(i),res(i),dist(i)
      if(res(i).ge.10.0) res(i)=9.9
      if(res(i).le.-10.0) res(i)=-9.9
      if(stat(i).ne.' ')
     *write(6,'(a,1x,f7.1,3x,f4.1)') stat(i),dist(i),res(i)
c      av_mag=av_mag+mag(i)
      goto 20
 25   continue
      nmag=i-1
      if(nmag.lt.2) then
         write(6,*)' Too few magnitudes'
         stop
      endif    

      mag_type='M'//phase(1)(4:4)
      if(phase(1)(1:4).eq.'SPEC') mag_type='MW'


c
c  set defaults for output on screen and one hardcopy file
c
          open(65,file='plotmr.eps',status='unknown')
          plotunit=65
          plotoption=1
          wsize=60
          call get_window_size

c
c   open plotter
c
c
c   set some postscipt scalings
c
          write(65,*) ' 1.0 0.55 scale'
c
c   open display
c
	call open_display 

c
c   make least squares analysis
c
         call lsqlin(nmag,dist,res,tor,slope,corr,rms)
c

c
c   plot points
c
c          av_mag=av_mag/nmag
          av_mag=1.0
          xtext='Dist(km)'
          ytext=mag_type
          title=mag_type//' residual vs hypocentral distance in km'
          if(mag_type.eq.'MW') 
     *    title=mag_type//' residual vs geo distance in km'

c          write(title(49:52),'(f4.1)') av_mag
          call xy_plot
     *    (1,nmag,dist,res,title,xtext,ytext,
     *    600.0,600.0,100.0,100.0,1,1,20.0,
     *    0,cha,i,x,y)	   

         
             
c  plot line
c
          call xy_plot_line(slope,tor,100.0,100.0)
          txt(1)='Select station by'
          txt(2)='clicking near symbol'
          txt(3)='q to quit'
          call xmessage(txt,3,25,720.0,700.0)

c
c   plot all stations
c
          i=1
          l=2   
          txt(1)='STAT   Dist Res  STAT   Dist Res'
 554      continue
          txt(l)=' '
          do k=1,2
             if(k.eq.1) then
                if(res(i).le.-1.0) then
                   write(txt(l)(1:17),'(a5,1x,i5,1x,i3)')
     *             stat(i),int(dist(i)),int(res(i))
                else
                   write(txt(l)(1:17),'(a5,1x,i5,1x,f3.1)')
     *             stat(i),int(dist(i)),res(i)
                endif

                i=i+1
                if(i.gt.nmag) goto 555
             endif
             if(k.eq.2) then
                if(res(i).le.-1.0) then
                   write(txt(l)(18:34),'(a5,1x,i5,1x,i3)')
     *             stat(i),int(dist(i)),int(res(i))
                else
                   write(txt(l)(18:34),'(a5,1x,i5,1x,f3.1)')
     *             stat(i),int(dist(i)),res(i)
                endif
                i=i+1
                l=l+1
                if(i.gt.nmag) goto 555
                goto 554
             endif
          enddo
 555      continue
          if(k.eq.2) l=l-1

          call xmessage(txt,l,36,710.0,10.0)

c
c  call up cursxor so plots remains
c
 
 30       continue

          call xy_plot_input(100.0,100.0,cha(1),xx,yy,xc,yc)
c
c   find corresponding station
c
          txt(1)=' '
          txt(1)='Too far from symbol,'
          txt(2)='try again'
          txt(3)=' '
          do i=1,nmag
              if(abs(xx-dist(i))*xc.lt.10.0.and.   ! distance in pixels
     *           abs(yy-res(i))*yc.lt.8.0) then
                 txt(1)(1:25)=stat(i)
                 txt(2)(1:25)=' '
                 txt(3)=' '
                 txt(4)=' '
              endif
          enddo
          call xmessage(txt,4,25,710.0,695.0)
          if(cha(1).ne.'q') goto 30   ! next input
c
      stop
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine xy_plot_input(x0,y0,c,x,y,xc,yc)
c
c  reads positions from the screen much like the main routine
c  all scaling values from routine xy_plot via common
c  x0,y0 are lower left hand corner of plot
c
c  x0,yo : same input as main routine
c  x,y   : output in scaled units
c  c     : character pressed
c  xc    : xscale
c
       implicit none
       character*1 c
       integer ichar
       reaL xc,yc   ! same as x and yscale
       real x1,x,y1,y   ! help variables
       real xfirst,yfirst,x0,y0,xscale,yscale,ymin,ymax,xmin,xmax   ! see xy_plot
       common/xy_plot_common/xfirst,yfirst,xscale,yscale,ymin,
     *                       ymax,xmin,xmax
c
c   call up cursor
c

1           continue
            call xscursr(ichar,x1,y1)
            if (char(ichar).eq.'#') goto 1
            c=char(ichar)
            x=(x1-x0)/xscale+xfirst
            y=(y1-y0)/yscale+yfirst
            xc=xscale
            yc=yscale
c
      return
      end

