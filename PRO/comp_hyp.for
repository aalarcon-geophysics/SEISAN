c
c simple program to compare two hyp output files, written on the Azores
c to compare result from hyp7.2 and hyp8.3
c
c lo June 2010
c
      implicit none                       ! force delcaration of all variables
      include 'seidim.inc'                ! dimensions for rea block
      include 'seisan.inc'                ! dimensions for rea block
      include 'rea.inc'                   ! parameter common bliock

      character*80 data1(5000),data2(5000)! s-file with data in text array
      character*80 err_text               ! error text
      character*80 infile1,infile2        ! input file
      real x(3),y(3)                      ! hypocenter locations

      logical all                         ! true: read all data, false: headers
      integer code                        ! error return code
      integer nevent                      ! number of events in file
      integer i                           ! counter
      real dist,distdeg,az,pi
      real distx,depthx,rmsx,rms1,rms2
      integer unit
      real epi,rms,dep                    ! statistics

      call get_seisan_def

      pi=4.*atan(1.)

c      infile1='hyp.out.1'
c      infile2='hyp.out.2'
      write(*,*) ' Program to compare hypocenters from two '//
     &   'Nordic files '
      write(*,*) ' Name of first Nordic file, reference data '
      read(5,'(a)') infile1
      write(*,*) ' Name of second Nordic file '
      read(5,'(a)') infile2
      write(*,*) ' Enter minimum numbers to select event with '//
     &           'significant change : distance '//
     &           '(km)/depth (km)/rms'
      read(*,*) distx,depthx,rmsx
      open(1,file=infile1,status='old',err=10)
      open(2,file=infile2,status='old',err=10)
      open(3,file='comp_hyp.out',status='unknown')
      open(4,file='comp_hyp_change.out',status='unknown')
      open(11,file='comp_hyp_events_changed.out',status='unknown')
      open(12,file='comp_hyp_events_unchanged.out',status='unknown')
      open(13,file='comp_hyp_report.out',status='unknown')
      goto 11
 10   continue
      write(6,*)' No such input file'
      stop
 11   continue
      all=.true.                  ! read all parameters
      nevent=0                    ! initialize counter

c
c-----------------------------------------------------------------
c  Loop to read events start here
c-----------------------------------------------------------------
c

  50  continue
c
c   read all parameters for one event form file unit 1
c
      call rea_event_in(1,all,data1,code)
c      write(*,'(a)') ' 1 ',data1(1)(1:79)
c      write(*,*) new_nordic_format,new_nordic_format_only
      if(code.eq.1) goto 1000
      rms1=hyp_rms(1)
      call rea_event_in(2,all,data2,code)
c      write(*,'(a)') ' 2 ',data2(1)(1:79)
c      write(*,*) new_nordic_format,new_nordic_format_only
      if(code.eq.1) goto 1000
      rms2=hyp_rms(1)

      nevent=nevent+1               ! count events
      write(3,'(a,i5)') ' event # ',nevent

c
c   write the whole first header line
c
      write(3,'(a)') data1(1)(1:79)
      write(3,'(a)') data2(1)(1:79)
c
c read hypocentre parematers
c
      read(data1(1)(24:43),'(f7.3,f8.3,f5.1)') x(2),x(1),x(3)
      read(data2(1)(24:43),'(f7.3,f8.3,f5.1)') y(2),y(1),y(3)
c
c check for size of change
c
      call delaz(x(2)*pi/180.,x(1)*pi/180.,dist,
     &  distdeg,az,y(2)*pi/180.,y(1)*pi/180.) 
c
c statitics
c
      epi=epi+dist*dist
      dep=dep+(x(3)-y(3))*(x(3)-y(3))
      rms=rms+(rms2-rms1)*(rms2-rms1)

      unit=12
      if ((dist.ge.distx.or.abs(x(3)-y(3)).ge.depthx.or. 
     &   rms2-rms1.ge.rmsx).and.
     &   rms1+.1.lt.rms2) then
        if (dist.ge.1000.) dist=999.
c write headers and change to screen

        write(6,'(a)') data1(1)(1:79)
        write(6,'(a)') data2(1)(1:79)
      write(*,'(a,f7.1,f5.1,1x,f5.1)') ' change epicentre,depth,rms: ',
     &     dist,abs(x(3)-y(3)),rms2-rms1

c add change to headers already written out

      write(3,'(a,f7.1,f5.1,1x,f5.1)') ' change epicentre,depth,rms: ',
     &     dist,abs(x(3)-y(3)),rms2-rms1

c write info to change file

        write(4,'(a)') data1(1)(1:79)
        write(4,'(a)') data2(1)(1:79)
      write(4,'(a,f7.1,f5.1,1x,f5.1)') ' change epicentre,depth,rms: ',
     &     dist,abs(x(3)-y(3)),rms2-rms1
     
        unit=11
      endif
      write(13,'(i5,1x,f7.1,1x,f5.1,1x,f5.1)') 
     &     nevent,dist,abs(x(3)-y(3)),rms2-rms1
c        write(3,'(a,f5.1)') ' change rms: ',
c     &     abs(rms1-rms2)

      write(3,*)
c
c write out event to file with changed ot unchanged events
c
      call rea_event_out(unit,all,data1,code)
c      write(unit,*)

      goto 50

c
c     end of file
c
 1000 continue
c
c statistics
c
      epi=sqrt(epi/float(nevent))
      dep=sqrt(dep/float(nevent))
      rms=sqrt(rms/float(nevent))


      write(*,'(a,a,f7.2,a,f7.2,a,f7.2)') 
     &  ' the standard deviation of the difference to the '//
     &           ' reference for the 3 parameters is: ',
     &           '   epicenter: ',epi,
     &           '   depth:     ',dep,
     &           '   rms:       ',rms 


c
c close files
c
      write(6,*)            ! blank line
      close(1)              ! close file
      close(2)              ! close file
      close(3)              ! close file
      close(4)              ! close file
      close(11)              ! close file
      close(12)              ! close file
      close(13)              ! close file
c
c info about output files
c
      write(6,*) 'Number of events in input file', nevent
      write(6,*) 'Output file names are: '
      write(6,*) '    comp_hyp.out: list with changes of all events'
      write(6,*) '    comp_hyp_change.out: list of events that changed'
      write(6,*) '    comp_hyp_events_changed.out: nordic format'
      write(6,*) '    comp_hyp_events_unchanged.out: nordic format'
      stop
      end
