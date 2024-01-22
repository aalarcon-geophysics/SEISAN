
c
c   select isc stations from isc station file corresponding
c   to isc events in user specified input file
c    
c   station list can be both isc or seisan format
c
c   j. havskov nov 93
c
c   changes:
c   jan 96 by jh: fixed to always convert to seisian format
c   apr 18      : bug in output format
c   jun 1999 jh   ------------   version 7.0 -----------------
C   jan 6 99    : change so that station is not selected more than once,
c                 idea thanks to Mohammad Raesi
c   apr 9 20  jh: new format for isc file, 5 char stations, high accuracy
c   mar 10 21 jh: do not stop if wrong line, just continue
c
      implicit none
      character*5 outsta(30000)    ! stations selected
      integer selected(30000)      ! indicator for stations found
      character*80 iscfile,norfile
      character*200 text
      integer nout,i,k,kk,j,nwrite,ilon,ilat,idepth
      real lat,lon,depth
      real latmin,lonmin           ! lat and lon minutes
      character*1 latx,lonx        ! NS EW
      character*1 depth_sign       ! - if depth smaller then 999
      integer nstat,nphase,nhead,nrecord,id
      character*1 type,exp
      logical high_accuracy        ! true if output in high accuracy
      character*80 data(25000)
c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

      high_accuracy=.false.
c
      write(6,*)' This program will read an S-file, find how many'
      write(6,*)' different stations there are and select those'
      write(6,*)' stations out of a station file, which can either'
      write(6,*)' be in SEISAN format or ISC format. The'
      write(6,*)' output is in SEISAN format. If no S-file is given'
      write(6,*)' the input station file is assumed to be in ISC'
      write(6,*)' format and the whole file will be converted to'
      write(6,*)' SEISAN format'
      write(6,*)
c

      write(6,*)' Give file with all stations(ISC or SEISAN format)'
      read(5,'(a)') iscfile
      write(6,*)
     *' Give file with readings, Nordic format, if none, return'
      read(5,'(a)') norfile
      write(6,*)' High accuracy output with ISC input (y/n=enter)'
      read(5,'(a)') text
      if(text(1:1).eq.'y') high_accuracy=.true.

      open(2,file=iscfile,status='old')
      open(3,file='iscsta.out',status='unknown')
c
c   go through events if available
c
      nout=0
      nwrite=0
      do i=1,25000
         selected(i)=0
      enddo
      if(norfile(1:3).ne.'   ') then 
         open(1,file=norfile,status='old')
         open(4,file='isc_miss.out',status='unknown')

 1       continue
         call indata(1,nstat,nphase,nhead,nrecord,type,exp,data,id)              
         if(nrecord.eq.0) goto 10
c
         do i=nhead+1,nrecord-1
            do k=1,nout
               if(data(i)(2:6).eq.outsta(k)) goto 2
            enddo
            nout=nout+1
            outsta(nout)=data(i)(2:6)
c
c   enter here if station found
c
 2          continue
         enddo
         goto 1
      endif
c
c   end of event list
c
 10   continue
c
c   find station in isc list, check both isc and seisan formats
c   convert to seisan format, if no input readings file, just
c   convert 
c
      read(2,'(a)',end=99) text
      if(text(1:5).eq.' ') goto 10
      if(text(1:10).eq.'RESET TEST') goto 10

      if(norfile(1:3).ne.'   ') then              ! nordic file input
        do k=1,nout     
          if(selected(k).ne.1) then               ! only select new stations
             if(text(1:5).eq.outsta(k)) then      ! only isc format if 5 first match
                selected(k)=1                     ! indicate that station found
                call find_lat(text,kk)            ! find where lat is in string
                if(kk.eq.0) then
                   write(6,*) 'no latitude found'
                  write(6,*) text
                  stop
                endif

                read(text(kk-3:kk+28),'(3f10.3)') 
     *          lat,lon,depth
                if(lat.gt.90.0.or.lat.lt.-90.0.or.
     *          lon.gt.180.0.or.lon.lt.-180.0.or.
     *          depth.gt.10000.0.or.depth.lt.-10000.0) then
                   write(6,*)'wrong numbers'
                   write(6,*) text
                   stop
                endif

                ilat=abs(lat)
                latmin=(abs(lat)-ilat)*60.0
                latx='N'
                if(lat.lt.0.0) latx='S'
                ilon=abs(lon)
                lonmin=(abs(lon)-ilon)*60.0
                lonx='E'
                if(lon.lt.0.0) lonx='W'
                idepth=depth
                depth_sign=' '
                if(depth.lt.-999.0) then
                   idepth=abs(depth)
                   depth_sign='-'
                endif

                if(high_accuracy) then

                   latmin=(latmin+0.0005)*1000
                   lonmin=(lonmin+0.0005)*1000
                   if(text(5:5).eq.' ') then   ! 4 char station
                      write(3,'(a1,1x,a4,i2,i5,a1,i3,i5,a,i4)')
     *                depth_sign,text(1:4),
     *                ilat,int(latmin),latx,ilon,int(lonmin),lonx,idepth
                   else                       ! 5 char station
                      write(3,'(a1,a5,i2,i5,a1,i3,i5,a,i4)')
     *                depth_sign,text(1:5),
     *                ilat,int(latmin),latx,ilon,int(lonmin),lonx,idepth
                   endif
c
c   normal accuracy
c           
                else

                   if(text(5:5).eq.' ') then   ! 4 char station
                      write(3,'(a1,1x,a4,i2,f5.2,a1,i3,f5.2,a,i4)')
     *                depth_sign,text(1:4),
     *                ilat,latmin,latx,ilon,lonmin,lonx,idepth
                   else                       ! 5 char station
                      write(3,'(a1,a5,i2,f5.2,a1,i3,f5.2,a,i4)')
     *                depth_sign,text(1:5),
     *                ilat,latmin,latx,ilon,lonmin,lonx,idepth
                   endif
                endif

           nwrite=nwrite+1

           endif   ! for isc input
c
c   maybe seisan format input, just copy if correct station
c
           if((text(1:1).eq.' '.or.text(1:1).eq.'-').and.   ! - is depth larger than 999
     *         (text(14:14).eq.'N'.or.text(14:14).eq.'S').and.
     *         (text(23:23).eq.'E'.or.text(23:23).eq.'W').and.
     *     (((text(2:2).eq.' '.and.outsta(k)(1:4).eq.text(3:6)).or.
     *     (  text(2:2).ne.' '.and.outsta(k)(1:5).eq.text(2:6))))) then
              selected(k)=1                     ! indicate that station found
              write(3,'(a)') text(1:27)
              nwrite=nwrite+1
            endif
         endif
        enddo    
c
c   no nordic file input, select all from isc
c
      else

         call find_lat(text,kk)
         if(kk.eq.0) then
            write(6,*) 'no latitude found'
            write(6,*) text
            goto 10
c            stop
         endif

         read(text(kk-3:kk+28),'(3f10.3)') 
     *   lat,lon,depth
         if(lat.gt.90.0.or.lat.lt.-90.0.or.
     *   lon.gt.180.0.or.lon.lt.-180.0.or.
     *   depth.gt.10000.0.or.depth.lt.-10000.0) then
            write(6,*)'wrong numbers'
            write(6,*) text
            stop
          endif

          ilat=abs(lat)
          latmin=(abs(lat)-ilat)*60.0
          latx='N'
          if(lat.lt.0.0) latx='S'
          ilon=abs(lon)
          lonmin=(abs(lon)-ilon)*60.0
          lonx='E'
          if(lon.lt.0.0) lonx='W'
          idepth=depth
          depth_sign=' '
          if(depth.lt.-999.0) then
             idepth=abs(depth)
             depth_sign='-'
          endif

          if(high_accuracy) then

             latmin=(latmin+0.0005)*1000
             lonmin=(lonmin+0.0005)*1000
             if(text(5:5).eq.' ') then   ! 4 char station
                write(3,'(a1,1x,a4,i2,i5,a1,i3,i5,a,i4)')
     *          depth_sign,text(1:4),
     *          ilat,int(latmin),latx,ilon,int(lonmin),lonx,idepth
             else                       ! 5 char station
                write(3,'(a1,a5,i2,i5,a1,i3,i5,a,i4)')
     *         depth_sign,text(1:5),
     *          ilat,int(latmin),latx,ilon,int(lonmin),lonx,idepth
              endif
           
          else

             if(text(5:5).eq.' ') then   ! 4 char station
                write(3,'(a1,1x,a4,i2,f5.2,a1,i3,f5.2,a,i4)')
     *          depth_sign,text(1:4),
     *          ilat,latmin,latx,ilon,lonmin,lonx,idepth
             else                       ! 5 char station
                write(3,'(a1,a5,i2,f5.2,a1,i3,f5.2,a,i4)')
     *          depth_sign,text(1:5),
     *          ilat,latmin,latx,ilon,lonmin,lonx,idepth
              endif
           endif
           nwrite=nwrite+1
      endif

      goto 10    ! next from station file
c
 99   continue

      if(norfile(1:3).ne.'   ') then
         write(6,*)' Number of stations found in readings file',nout
         k=0
         do i=1,nout
            if(selected(i).ne.1) then
               write(4,'(2x,a)') outsta(i)
               k=k+1
            endif
         enddo
         write(6,*)' Number of stations not found in station file',k
      endif

      write(6,*)' Number of stations selected in station file',nwrite
      write(6,*)' Output file is iscsta.out'

      if(norfile(1:3).ne.'   ') 
     *write(6,*)' Output file with missing stations is isc_miss.out'
      stop
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine find_lat(text,k)
c
c  find lat number in string, must have 5 decimals
c
      implicit none
      character*200 text
      character*1 t
      integer i,k,j
      
      k=0
      
      do i=1,150
        t=text(i:i)
        if(t.eq.'.') then
c
c   check if 5 following digits are numbers
c
        do j=i+1,i+5
          if(ichar(text(j:j)).lt.48.or.ichar(text(j:j)).gt.57) goto 1
        enddo
c
c   if here ok
c
        k=i
        return

  1     continue
        endif
      enddo

      return
      end

          