c
c  program pspec to plot spectral fits and automatic amplitude  pics from automag
c
c  program asks no questions, will use automag.list with input of channels
c  to plot. Each channel is supposed to be stored in individual files as 
c  generated by automag and autosig.
c  mw amd ml  on plot only has a value if corresponding station has a distance in
c  s-file bfore running automag
c  if program has the s-file as argument, it is also possibel to delete spectral 
c  esitmates and IAML for the channel selected interactively by pressing d
c  in square of spectrum and/or wa trace 
c
c  j. havskov, oct 2013
c
c  updates
c  17-2-2014  jh: read seisan.def
c  14-3-2014  jh: fix component plotting, change ps file name, plot distance
c  12-2-2019  jh: wrong identification of spectrum, no longer SPECS.
c                 new format
c

      implicit none              
      include 'seiplot.inc'      ! seisan graphics
      real x(50000),y(50000)     ! plotting vector and signal
      real xpos(1000),ypos(1000) ! lower left hand corner of spec plot
      real xn(10000),yn(10000)   ! noise
      real xs(10000),ys(10000)   ! signal
      real xf(10000),yf(10000)   ! fit
      real x0,y0,x00,y00         ! start of plot
      real xsize, ysize          ! size of plot
      real xx,yy                 ! help variable
      character*80 text          ! general text
      character*80 infile        ! input file
      character*5  stat          ! station
      character*4  comp          ! component
      character*5  xstat(1000)   ! all stats
      character*4  xcomp(1000)   ! all comp
      character*1  type_of_spectrum ! P or S
      integer ncomp              ! number of stat-comp
      character*30 ampper        ! amplitude period
      character*30 xampper(1000) ! save----
      integer dist               ! distance
      character*105 header       ! header from s-file+ spectrum type
      character*15 mw            ! mw
      character*12 flimits       ! frequency limits used
      character*12 xflimits(1000)! save ---              
      integer seiclen
      integer iplot              ! plot number on all pages
      integer kplot              ! start number of plot on page
      integer idel               ! number of deles
      character*10 del(1000)     ! data to delete
      character*1 c_spec(100)    ! return chars from xy_plot, also input 
      character*30 xtext,ytext   ! text on x and y-axis
      real cx(100),cy(100)       ! position of chars output from cursor, also input of color
      integer nwa,i1,i2          ! number of wa points, first and last sample of pick
      real rate                  ! sample rate of wa data

      integer nnspec(100),nc,k,j   
      integer ich,i           
      integer nsignal,nnoise,nfit ! number of points in data sets              

      call get_seisan_def

c
c     open file with list of stations to use
c
      open(1,file='automag.list',status='unknown') 
c
c   set default size in % of sreen size of plot
c           
      wsize=80         ! common block variable
      call get_window_size
      if(size_sample_graphics.gt.0) wsize=size_sample_graphics ! from color.def
c
c   assign file unit for postscriptt file
c
      plotunit=65      ! common block variable

c
      plotoption=1     ! common block variable
c
c   open postscript output file
c
      open(65,file='plotspec.eps',status='unknown')

c
c   open plotter (display and initial output in postscript file)
c
      call open_display

      k=3       ! 3 data sets
      ncomp=0
      iplot=1
      kplot=1
   
c
c   no axis text
c

      xtext=''
      ytext=''
c
c   size of individual plots

      xsize=190.0
      ysize=200.0

      x00=10.0
      y00=780.0-ysize-25.0
      x0=x00
      y0=y00

      c_spec='3'   ! no number on axes
c
c  color of graphs
c
      cx(1)=6.0
      cx(2)=1.0
      cx(3)=3.0
c
c  plot spectra and wa traces
c
      do j=1,2000
c
c   read info on next spectrum
c
         read(1,'(a)',end=999) header
         read(1,'(a5,a4,i5)',end=999)stat,comp,dist
         read(1,'(a)',end=999) ampper
         read(1,'(a)',end=999) flimits
         read(1,'(a)',end=999) mw


         if(flimits.eq.' '.and.ampper.eq.' ') goto 300  ! no plot at all        
c
c   save channel name and more
c
         ncomp=ncomp+1
         xstat(ncomp)=stat
         xcomp(ncomp)=comp
         type_of_spectrum=header(103:103)  ! P or S
         xflimits(ncomp)=flimits
         xampper(ncomp)=ampper
c
c   save plot position
c
         xpos(ncomp)=x0
         ypos(ncomp)=y0
c
c   main title
c     
         call XCHARS(header(2:105),104,x00,765.0) 
c
c  plot stat comp
c
      
         text=stat//comp(1:2)//comp(4:4)
         call XCHARS(text,9,x0+xsize/2.0,Y0+ysize-15.0)
         write(text,'(a,i4)') 'Dist=',dist
         call XCHARS(text,9,x0+xsize/2.0,Y0+ysize-30.0)
c
c  plot mag and frequency limits, could be blank if no spectrum or no mag
c
         call xchars(ampper,6,x0+20.0,y0+60.0)
         call xchars(ampper(8:22),15,x0+20.0,y0+15.0)
         call XCHARS(mw,15,x0+20.0,Y0+45.0) 
         call XCHARS(flimits,12,x0+20.0,Y0+30.0)

         do i=1,4
            if(comp(i:i).eq.' ') comp(i:i)='_'
         enddo

         if(flimits.eq.' ') goto 100  ! no spectrum, try wa trace
c
c  read spec signal
c
         infile=' '
         infile=stat(1:seiclen(stat))//'.'//comp//'.obs'
         open(3,file=infile(1:seiclen(infile)),action='read',
     *   status='old')
         i=1
 1       continue
         read(3,*,end=2)x(i),y(i)
         x(i)=alog10(x(i))
         i=i+1
         goto 1
 2       continue
         nsignal=i-1
c         write(6,*)'nsignal',nsignal
         close(3)

c
c   read noise
c
         infile=' '
         infile=stat(1:seiclen(stat))//'.'//comp//'.noise'
         open(3,file=infile(1:seiclen(infile)),action='read',
     *   status='old')
         i=1
 3       continue
         read(3,*,end=4)xn(i),yn(i)
         i=i+1
         goto 3
 4       continue
         nnoise=i-1
         close(3)

c         write(6,*)'nnoise',nnoise

c
c   read fit
c
         infile=' '
         infile=stat(1:seiclen(stat))//'.'//comp//'.synth'
         open(3,file=infile(1:seiclen(infile)),action='read',
     *   status='old')  
         i=1
 5       continue
         read(3,*,end=6)xf(i),yf(i)
         i=i+1
         goto 5
 6       continue
         nfit=i-1
         close(3)

c
c  put signals into one vecor
c
         do i=1,nnoise
           x(i+nsignal)=alog10(xn(i))        
           y(i+nsignal)=yn(i)
         enddo

         do i=1,nfit
           x(i+nsignal+nnoise)=alog10(xf(i))        
           y(i+nsignal+nnoise)=yf(i)
         enddo

         nnspec(2)=nnoise
         nnspec(3)=nfit
         nnspec(1)=nsignal
c
c   plot spectrum
c
         text=' '      ! no title
        call xy_plot
     *   (k,nnspec,x,y,text,xtext,ytext,xsize,ysize,
     *   x0,y0,2,1,30.0
     *   ,0,c_spec,nc,cx,cy)
c
c-------------------------------------------------------------------
c   get here if no spectrum, possibly plot wa
c-------------------------------------------------------------------
c
 100     continue
c
c   read wa
c

         if(ampper.eq.' ') goto 200   ! no wa

         infile=stat(1:seiclen(stat))//'.'//comp//'.wa'
         open(3,file=infile,status='old')
         read(3,*) nwa,rate,i1,i2 
         i=1
 7       continue
         read(3,*,end=8)x(i)
         i=i+1
         goto 7
 8       continue
         nwa=i-1
         close(3)
c
c   plot wa
c
         call plot_wa(nwa,x,rate,i1,i2,xsize,40.0,x0,y0-40.0)
 20      continue

 200     continue
         
c--------------------------------------------------------------
c   prepare position of next plot
c--------------------------------------------------------------

         x0=x0+xsize+10.0
         if(x0.gt.1024.0-xsize) then
            x0=x00
            y0=y0-ysize-45.0
         endif
         iplot=iplot+1   ! plot number of next plot, could be on next page

c
c   check if next page
c
         text=
     *'q to quit, f is next, d inside frame to delete IAML and/or spec'

         if(y0.lt.0.0) then
 102       continue
           call  tchars(text,80,15.0,10.0)  ! this only comes on screen
           call xscursr(ich,xx,yy)

           if(char(ich).eq.'q') goto 1000

           if(char(ich).eq.'d') then
c
c   find which channel and if spec or wa 
c
              do i=kplot,iplot
                 if(xx.gt.xpos(i).and.xx.lt.xpos(i)+xsize.and.
     *           yy.gt.ypos(i).and.yy.lt.ypos(i)+ysize.and.
     *           xflimits(i).ne.' ') then
                    idel=idel+1
                    del(idel)(1:5)=xstat(i)
                    del(idel)(6:9)=xcomp(i)
                    del(idel)(10:10)='W'
                    call xmovabs(xpos(i),ypos(i))
                    call xdrwabs(xpos(i)+xsize,ypos(i)+ysize)
                 endif
              enddo

              do i=kplot,iplot
                 if(xx.gt.xpos(i).and.xx.lt.xpos(i)+xsize.and.
     *           yy.lt.ypos(i).and.yy.gt.ypos(i)-40.0.and.
     *           xampper(i).ne.' ') then
                    idel=idel+1
                    del(idel)(1:5)=xstat(i)
                    del(idel)(6:9)=xcomp(i)
                    del(idel)(10:10)='L'
                    call xmovabs(xpos(i),ypos(i))
                    call xdrwabs(xpos(i)+xsize,ypos(i)-40)
                 endif
              enddo
              goto 102
           endif   ! end delete section

          if(char(ich).eq.'f') then   ! new page
             x0=x00
             y0=y00
             call clear_to_alpha
             call close_post
             call open_display
             kplot=iplot
c
c  set some postscipt scalings
c 
             write(65,*) ' 1.0 0.55 scale'
           else       ! char not f
              goto 102 ! get next char
           endif
         endif            ! end of next page

 300    continue  ! no plot at all
      enddo       ! end of data loop 

      close(1)  
c
c   end of input  automag.list file, call up cursor
c

 999  continue
      iplot=iplot-1
 998  continue
      
      text=
     *'q or f to quit, d inside frame to delete IAML and/or spec'
      call  tchars(text,80,15.0,10.0)  ! this only comes on screen
      call xscursr(ich,xx,yy)

      if(char(ich).eq.'q') goto 1000
      if(char(ich).eq.'f') goto 1000
c
c   check if delete and save
c
      if(char(ich).eq.'d') then
c
c   find which channel and if spec or wa 
c


        do i=kplot,iplot
           if(xx.gt.xpos(i).and.xx.lt.xpos(i)+xsize.and.
     *        yy.gt.ypos(i).and.yy.lt.ypos(i)+ysize.and.
     *          xflimits(i).ne.' ') then
                idel=idel+1
                del(idel)(1:5)=xstat(i)
                del(idel)(6:9)=xcomp(i)
                del(idel)(10:10)='W'
                call xmovabs(xpos(i),ypos(i))
                call xdrwabs(xpos(i)+xsize,ypos(i)+ysize)
           endif
        enddo

        do i=kplot,iplot
           if(xx.gt.xpos(i).and.xx.lt.xpos(i)+xsize.and.
     *        yy.lt.ypos(i).and.yy.gt.ypos(i)-40.0.and.
     *        xampper(i).ne.' ') then
                idel=idel+1
                del(idel)(1:5)=xstat(i)
                del(idel)(6:9)=xcomp(i)
                del(idel)(10:10)='L'
                call xmovabs(xpos(i),ypos(i))
                call xdrwabs(xpos(i)+xsize,ypos(i)-40)
           endif
        enddo
      endif
      goto 998
  
c-----------------------------------------------------------------
c   finished plotting
c------------------------------------------------------------------
c
 1000 continue
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
c
      call clear_to_alpha
      close(1)           ! close output file

      if(idel.ge.1) then
         write(6,*) 
     *   'You have marked the following channels/data for delete'
         write(6,*)
         do i=1,idel
            write(6,'(a,2x,a)') del(i)(1:9),del(i)(10:10)
         enddo
         write(6,*)
         write(6,*)'Please confirm (y/n=enter)'
         read(5,'(a)') text
         if(text(1:1).eq.'y') then
            write(6,*)
            write(6,*)'******* Now delting *****************'
            write(6,*)
            call del_phase(idel,del,type_of_spectrum)
         endif
      endif

      write(6,*)'Output plot file is plotspec.eps'
 
      stop
      end
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine plot_wa(n,y,rate,i1,i2,xlength,ylength,x0,y0)
c
c   plot wa seismogram indicating where pics were made with different color
c
      implicit none
      integer n      ! number of points
      real y(*)      ! points to plot
      real x         ! x position
      real xstep     ! plot x-step
      real rate      ! sample rate
      integer i1,i2  ! sample number corresponding to wa pick
      real xlength   ! x length of plot
      real ylength   ! y length of plot
      real x0,y0     ! position of plot, lower left hand corner
      real max       ! max amplitude
      integer i,k1,k2,k,nsignal


c
c   plot frame
c
      call xmovabs(x0,y0)
      call xdrwabs(x0+xlength,y0)
      call xdrwabs(x0+xlength,y0+ylength)
      call xdrwabs(x0,y0+ylength)
      call xdrwabs(x0,y0)
c
c   plot signal, only use part of signal
c
      max=0.0
      k=0
      nsignal=10.0*rate   ! number of points for 10 sec
      k1=i1-4.0*rate      ! try 4 seconds before pick
      if(k1.le.0) k1=1
      k2=k1+nsignal       ! try fixed  10 s window
      if(k2.gt.n) k2=n
c
c  find max
c
      do i=k1,k2
        if(abs(y(i)).gt.max) max=abs(y(i))
      enddo
c
c   scale
c
      do i=k1,k2
        y(i)=(y(i)/max)*(ylength/2.0)+y0+ylength/2.0
      enddo

      xstep=xlength/nsignal  ! scale for fixed length
      
      x=x0
      call xmovabs(x0,ylength/2.0+y(k1))

      do i=k1+1,k2
         if(i.eq.i1) call xset_color(3)  ! make pick red
         if(i.eq.i2) call xset_color(6)
         x=x+xstep
c         write(17,*) x,y(i)
         call xdrwabs(x,y(i))
      enddo
        


      return
      end


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      subroutine del_phase(ndel,del,type_of_spectrum)
c
c  delete iaml and spec phases from a list. the s-file is an argument
c
c
      implicit none                       ! force delcaration of all variables
      integer ndel                        ! number of phases to delete
      character*10 del(*)                 ! station, comp and phase indicator to delete

      include 'seidim.inc'                ! dimensions for rea block
      include 'seisan.inc'                ! dimensions for rea block
      include 'rea.inc'                   ! parameter common bliock

      character*80 data(5000)             ! s-file with data in text array
      character*80 err_text               ! error text
      character*80 infile                 ! input s-file

      character*1 type_of_spectrum        ! P or S

      integer nars                        ! number of arguments
      character*80 arg(10)                ! arguments


      logical all                         ! true: read all data, false: headers
      integer code                        ! error return code
      integer nd                          ! number of deleted phases
      integer i,k                         ! counter

c
c   get input file name, check if exist
c
c
c   get arguments
c
      call get_arguments(nars,arg)
c
c   first argument is file name
c
      if(nars.gt.0) then
         infile=arg(1)
         open(1,file=infile,status='old',err=3)
         goto 5
 3       continue
         write(6,*)'No such file'
         stop
 5       continue
      else

 9       continue
         write(6,*) 'Give input file'
         read(5,'(a)') infile
         open(1,file=infile,status='old',err=10)
         goto 11
 10      continue
         write(6,*)' No such input file'
         goto 9
 11     continue
      endif
c
c   lines to delete
c
 12   continue


      all=.true.                  ! read all parameters

      nd=0                        ! deleted lines

c
c   read s-file
c
c
c   read all parameters for one event from file unit 1
c
      call rea_event_in(1,all,data,code)
c
c   check if end of file (code=1), if so jump out of loop
c
      if(code.eq.1) goto 1000
c
c   loop through all phasese  to delete
c
      do k=1,ndel
       do i=1,rea_nphase
c
c   delete spec-phases 
c----------------------------------------------------
c  
    
           if(rea_phase(i)(1:4).eq.'SPEC'.and.rea_stat(i).eq.
     *     del(k)(1:5).and.rea_comp(i).eq.del(k)(6:9).and.
     *     del(k)(10:10).eq.'W'.and.
     *     type_of_spectrum.eq.rea_spec_phase(i)) then
              rea_phase(i)(1:6)='DELETE'
              if(rea_new_out) then
                nd=nd+1
              else
                nd=nd+2
              endif
           endif
c
c   delete IAML-phases
c----------------------------------------------------
c

           if(rea_phase(i)(1:4).eq.'IAML'.and.rea_stat(i).eq.
     *     del(k)(1:5).and.rea_co(i)(1:1).eq.del(k)(6:6).and.
     *     rea_co(i)(2:2).eq.del(k)(9:9).and.del(k)(10:10).eq.'L')
     *     then
              rea_phase(i)(1:6)='DELETE'
              nd=nd+1
           endif
         enddo
      enddo
      


 100  continue         

c
c   write out modified event, the array data has also been modified
c
       rewind 1
       write(6,*)
       write(6,'(a,a)') 'Now writing to ',infile

       call rea_event_out(1,all,data,code)

c
c     end of file
c
 1000 continue
c
      write(6,*)            ! blank line
      write(6,*)'Number of lines deleted',nd
      write(6,*)

      return
      end
