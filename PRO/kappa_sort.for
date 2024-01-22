c   program does some kappa processing to prepare for plots etc
c
c   jh feb 2023
c
c   1: get all 3 components from same event, but only if all 3 are there
c      the input file is harwired to be spec_kappa_chan.all
c
c   2: make xy files of kappa vs distance and depth for a particular
c      channel
c
c   3: optinally make a station file for GMT when kappa has been added
c      to station code. input can be from an epimap.are where the
c      spec kappa nordic fles have been use for plotting, or one of the
c      three kappa nordic files


c
c
      implicit none
      character*200 z(10000)
      character*200 n(10000)
      character*200 e(10000)
      character*200 text
      real lat,lon                ! lat lon of station
      character*80  nordic_file   ! file name of file with kappa
      real edist,hdist            ! epiecentral and hypocentral distance
      real depth                  ! depth
      real av_edist,av_hdist,av_depth ! averages ----------------------
      integer n_edist,n_hdist,n_depth ! number of ---------------------
      character*5 stat,stat_name  ! station
      character*1 comp            ! orientation
      real kappa(1000)
      integer nkappa              ! number of kappa values
      integer seiclen             ! function
      real av,sd
      integer kz,kn,ke,m,k,i,k3

      n_edist=0
      n_hdist=0
      n_depth=0
      av_edist=0.0
      av_hdist=0.0
      av_depth=0.0

      write(6,*)
     *'Make station files with kappa as function of dist and depth'
      write(6,*)
     *'Station and component like (a5,a1) KTK1 Z, enter for no files'
      read(5,'(a5,a1)') stat,comp
   
      if(stat.eq.' '.or.comp.eq.' ') goto 2
      do i=1,5
         stat_name(i:i)=stat(i:i)
         if(stat(i:i).eq.' ') stat_name(i:i)='_'
      enddo

      open(10,file=stat_name//comp//'.edist',status='unknown')
      open(11,file=stat_name//comp//'.hdist',status='unknown')
      open(12,file=stat_name//comp//'.depth',status='unknown')

 2    continue

      open(1,file='kappa_chan.all',status='old',err=2000)
      write(6,*) 'File kappa_chan.all used'
      goto 2001
 2000 continue
      open(1,file='spec_kappa_chan.all',status='old',err=2002)
      write(6,*) 'File spec_kappa_chan is used'
      goto 2001
 2002 continue
      write(6,*)'No kappa_chan.all or spec_kappa_chan.all'
      stop
 2001 continue
      open(2,file='kappa_sort.out',status='unknown')
      kn=0
      kz=0
      ke=0
      k3=0

 5    continue
      read(1,'(a)',end=10) text
c
c  fish out for station distance etc
c
      if(text(1:5).eq.stat.and.text(10:10).eq.comp.and.stat.ne.' ') 
     *then
          read(text(16:20),*) kappa(1)
          read(text(24:30),*) depth
          read(text(35:40),*) hdist
          read(text(45:50),*) edist
          write(10,*) edist,kappa(1)
          write(11,*) hdist,kappa(1)
          write(12,*) depth,kappa(1)
          n_edist=n_edist+1 
          av_edist=av_edist+edist
          n_hdist=n_hdist+1 
          av_hdist=av_hdist+hdist
          n_depth=n_depth+1 
          av_depth=av_depth+depth
      endif
c
c   save data text string from respective components
c
      if(text(10:10).eq.'Z') then
         kz=kz+1
         z(kz)=text
      endif
      if(text(10:10).eq.'N') then
         kn=kn+1
         n(kn)=text
      endif
      if(text(10:10).eq.'E') then
         ke=ke+1
         e(ke)=text
      endif
      goto 5
 10   continue

      write(6,'(a,3i6)') 
     *' Number of kappa values, z,n,e', kz,kn,ke

      do i=1,kz
        do k=1,kn
           if(z(i)(86:104).eq.n(k)(86:104)) then
               do m=1,ke
                   if(z(i)(86:104).eq.e(m)(86:104)) then
                       write(2,'(a)') z(i)(1:104)
                       write(2,'(a)') n(k)(1:104)
                       write(2,'(a)') e(m)(1:104)
                       k3=k3+1
                       goto 20
                   endif
               enddo
           endif
         enddo
 20      continue
      enddo

c
c   averages from nordic files with kappa
c
      write(6,*)
      write(6,*)'Calculate avarage from Nordic file with kappa'
      write(6,*)
     *'Enter file name. e.g spec_kappa.z,kappa.z  or epimap.are'//
     *', enter for none'
      read(5,'(a)') nordic_file
      write(6,*)
      if(nordic_file.eq.' ') goto 50
c
      nkappa=0
      open(3,file=nordic_file,status='old')
      open(22,file='kappa_station.gmt',status='unknown')
      nkappa=0
  30  continue
      read(3,'(a)',end=40) text
      read(text(24:38),*) lat,lon
      read(3,'(a)',end=40) text
      nkappa=nkappa+1
      read(text(12:18),*) kappa(nkappa)
      k=abs(kappa(nkappa))*1000
      write(22,'(2f8.3,1x,a5,i3)')
     *lon,lat,text(2:6),k
      read(3,'(a)',end=40) text
      goto 30
 40   continue
      write(6,'(a,i5)')' Number of sites with kappa values in '
     *//nordic_file(1:seiclen(nordic_file)),nkappa
      call sdv(nkappa,kappa,av,sd)
      write(6,'(a,2f8.3)')' Average and sd for sites in '
     *//nordic_file(1:seiclen(nordic_file)), av,sd

 50   continue      
c
c   calculate averages of  distances etc for one station if option
c
      if(stat.ne.' ') then
         av_hdist=av_hdist/n_hdist
         av_edist=av_edist/n_edist
         av_depth=av_depth/n_depth
         write(6,'(a,3f6.1)')' Average, hdist,edist and depth',
     *   av_hdist,av_edist,av_depth
      endif

      write(6,*) 'Number of kappa values for all 3 componenets', k3
      write(6,*) 'Output file kappa_sort.out'
      if(stat.ne.' ') write(6,*) 'Output station files are: ', 
     *stat_name//comp//'.edist, ',
     *stat_name//comp//'.hdist, ',stat_name//comp//'.depth'
      if(nordic_file.ne.' ') 
     *write(6,'(a)') 
     *' File with GMT station locations and kappa is kappa_station.gmt'

      stop
      end
