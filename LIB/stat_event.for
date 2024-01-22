cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine stat_event(unit,phase,nphase,iphase,weight,nray,
     *format_ray,stat,sslat,sslon,nstat)
c
c   find coordinates for all station event pairs for a given iphase phases
c   using first nphase characters. write to unit in epimap format if
c   format_ray is 1, if 2 in gmt format.
c   output weight must be larger than weight
c   nray is number of rays found.
c   slat, slon and nstat initilized from calling program
c
c   unit:         unit to write to
c   phase:        phases to look for
c   npahse:       number fo phases
c   iphase:       number of chars of each phase that must match
c   weight:       min weight to use
c   nray:         number of rays found
c   format_ray:   1: epimap, 2: gmt
c
c   jh may 2021
c

      implicit none
      include 'seidim.inc'                ! dimensions for rea block
      include 'seisan.inc'                ! dimensions for rea block
      include 'rea.inc'                   ! parameter common block
      character*8 phase(50)               ! phases to look for
      integer nphase                      ! number of phases to look for
      integer iphase(50)                  ! lenght of each phase
      real slat,slon,elev                 ! station location and elevation
      real weight,x                       ! min weight to use
      integer nray                        ! number of rays found
      integer format_ray                  ! 1: epimap, 2 gmt
      real sslat(*),sslon(*)              ! coordinates of stations
      character*5 stat(*)                 ! stations found
      integer nstat                       ! number of stations found
      integer i,n,unit,k,m

      nray=0

      do i=1,rea_nphase
         read(rea_weight_out(i),'(f2.1)') x
         do k=1,nphase
            if(rea_phase(i)(1:iphase(k)).eq.phase(k)(1:iphase(k)).and.
     *      x.ge.weight) then           
c
c  get station coordiantes
c
               call stat_loc(rea_stat(i),'0',slat,slon,elev)
               if(slat.gt.0.0.and.slon.gt.0.0.and.
     *         hyp_lat(1).gt. -900.0) then
c
c   check if station is in list
c
               do m=1,nstat
                  if(rea_stat(i).eq.stat(m)) goto 10
               enddo

               nstat=nstat+1
               stat(nstat)=rea_stat(i)
               sslat(nstat)=slat
               sslon(nstat)=slon
 10            continue              
c
c   for epimap
c
                  if(format_ray.eq.1) then
                     write(unit,'(a)')'   2'
                     write(unit,'(4f8.3)')hyp_lat(1),hyp_lon(1),
     *               slat,slon
                  endif
c
c   for gmt
c
                  if(format_ray.eq.2) then
                     write(unit,'(a)')'>>'
                     write(unit,'(2f10.3)')hyp_lon(1),hyp_lat(1)
                     write(unit,'(2f10.3)')slon,slat
                  endif
 
                  nray=nray+1
               endif
            endif
         enddo
      enddo
      return
      end
      