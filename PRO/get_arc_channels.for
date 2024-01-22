c
c   program to get all channels in archive for SEISDAN.DEF
c   output is formatted for SEISAN.DEF
c   an input file with all day files is needed, this can be made
c   with command ls -R in arc top directory 
c   skip LOG files
c   output can be in arc or arc2 lines   
c   all file names are read to start and end times of available data
c
      implicit none
      character*120 t
      character*5 stat(10000),sta     ! station code
      character*3 com(10000),co       ! component
      character*2 loc(10000),lo       ! location
      character*2 net(10000),nt       ! network
      character*3 com_select(100)     ! components to use
      character*5 stat_select(100)    ! stats
      character*2 net_select(100)     ! net
      integer year1(10000),year2(10000),yr
      integer doy1(10000),doy2(10000),dy
      character*80 infile
      integer nchan,k1,k2,k3,k4,k5,i,n,k,
     *n_stat_select,n_net_select,n_com_select,arc_format,
     *month,day
      integer seiclen

      n_stat_select=0
      n_net_select=0
      n_com_select=0

      write(6,*)'Input file name'
      read(5,'(a)') infile

      write(6,*)'Output format, arc_chan (def=enter) or arc_chan2(=2)'
      read(5,'(a)') t
      if(t.eq.' ') then
        arc_format=0
      else
        read(t,*)arc_format
      endif 
    

      open(1,file=infile,status='old')
      open(2,file='get_arc_channels.out',status='unknown')
c
c   channel selection
c
      write(6,*)
     *'Which channels like HHE or **Z, enter for all, else one per line'
      write(6,*)'* is wild card'
 28   continue
      read(5,'(a)')t
      if(t.ne.' ') then
         n_com_select=n_com_select+1
         com_select(n_com_select)=t(1:3)
         goto 28
      endif

      write(6,*)'Which networks, enter for all, else one per line'
 29   continue
      read(5,'(a)') t
      if(t.ne.' ')  then
         n_net_select=n_net_select+1
         net_select(n_net_select)=t(1:2)
         goto 29
      endif


      write(6,*)'Which stations, enter for all, else one per line'
 30   continue
      read(5,'(a)') t
      if(t.ne.' ') then
        n_stat_select=n_stat_select+1
        stat_select(n_stat_select)=t(1:5)
        goto 30
      endif
c
c  loop to read all
c
      nchan=0
      
 1    continue

      read(1,'(a)',end=100) t
c
c   find a line with file name
c
      if(t(3:3).eq.'.') then
c
c   find stat, comp, network and location code
c        
         
          do i=4,seiclen(t)
             if(t(i:i).eq.'.') then
                k1=i
                goto 5
             endif
          enddo
 5        continue

          do i=k1+1,seiclen(t)
             if(t(i:i).eq.'.') then
                k2=i
                goto 6
             endif
          enddo
 6        continue

          do i=k2+1,seiclen(t)
             if(t(i:i).eq.'.') then
                k3=i
                goto 7
             endif
          enddo
 7        continue

          do i=k3+1,seiclen(t)
             if(t(i:i).eq.'.') then
                k4=i
                goto 8
             endif
          enddo
 8        continue

          do i=k4+1,seiclen(t)
             if(t(i:i).eq.'.') then
                k5=i
                goto 9
             endif
          enddo
 9        continue
          
          nt=t(1:2)
          sta=t(4:k1-1)
          if(t(k1:k2).eq.'..') then
             lo=' '
          else
             lo=t(k1+1:k2-1)
          endif
          co=t(k2+1:k3-1)
          if(co.eq.'LOG') goto 1
c
c   time
c
          read(t(k4+1:k5-1),*,err=33,end=33) yr
       goto 34
 33    continue
       write(6,*) t
       stop
 34    continue
          read(t(k5+1:k5+3),*) dy

c
c   check component used
c
         if(n_com_select.ne.0) then
             do i=1,n_com_select
                n=0
                do k=1,3
                   if(co(k:k).eq.com_select(i)(k:k).
     *             or.com_select(i)(k:k).eq.'*') n=n+1
                enddo
                if(n.eq.3) goto 19
                goto 1
             enddo
          endif
 19       continue
c
c   check for network and station
c
          if(n_stat_select.ne.0) then
             do i=1,n_stat_select
                if (sta.eq.stat_select(i)) goto 20
             enddo
             goto 1
          endif
 20       continue

          if(n_net_select.ne.0) then
             do i=1,n_net_select
                if (nt.eq.net_select(i)) goto 21
             enddo
             goto 1
          endif
 21       continue
            
c
c   add to list if not there
c
          if(nchan.eq.0) then ! very first channel
            nchan=nchan+1
            net(nchan)=nt
            stat(nchan)=sta
            com(nchan)=co
            loc(nchan)=lo
c
c   first time of first channel
c
            year1(nchan)=yr
            doy1(nchan)=dy
            year2(nchan)=0
          else
c
c   check for time
c           
            do i=1,nchan
              if(stat(i).eq.sta.and.co.eq.com(i).
     *        and.nt.eq.net(i).and.lo.eq.loc(i)) then
c
c   could be last time
c
                 year2(i)=yr
                 doy2(i)=dy
                 goto 1      ! since already in list 
               endif 
            enddo

            nchan=nchan+1
            net(nchan)=nt
            stat(nchan)=sta
            com(nchan)=co
            loc(nchan)=lo
            year1(nchan)=yr
            doy1(nchan)=dy
 11         continue
         endif
         
       
      endif  ! endif for channel line


      goto 1

 100  continue

      do i=1,nchan
          write(6,*)stat(i),' ',com(i),' ',net(i),' ',loc(i),
     *    year1(i),doy1(i),year2(i),doy2(i)
      enddo
      do i=1,nchan
         t=' '
         t(1:9)='ARC_CHAN2'
         if(arc_format.eq.0) t(1:9)='ARC_CHAN ' 
         t(41:45)=stat(i)
         t(46:48)=com(i)
         t(49:50)=net(i)
         t(51:52)=loc(i)
         call dte(doy1(i),day,month,year1(i))
         if(arc_format.eq.0) then
            write(t(61:64),'(i4)') year1(i)           
            write(t(65:68),'(2i2)') month,day
         else
            write(t(58:61),'(i4)') year1(i)           
            write(t(62:65),'(2i2)') month,day
         endif
         if(year2(i).eq.0) then      ! only one day file
            year2(i)=year1(i)
            doy2(i)=doy1(i)+1
         endif
         call dte(doy2(i),day,month,year2(i))
         if(arc_format.eq.0) then
            write(t(71:74),'(i4)') year2(i)
            write(t(75:78),'(2i2)') month,day
         else
            write(t(67:70),'(i4)') year2(i)
            write(t(71:74),'(2i2)') month,day
            t(76:82)='arc_loc'
         endif
         write(2,'(a)') t
      enddo
      write(6,*)'Number of channels ',nchan
      write(6,*)'Output file is get_arc_channels.out'
 
      stop
      end


