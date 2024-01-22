c
c   modify RESP file names for missing location or network codes
c
c   make a dirf of RESP filwes that all require the same change
c   questions of changing/adding location and/or network codes, to see codes, plot with MULPLT
c   if file has one'.' for code, it will be replaced by '..', also when no change is indicated
c
c   jh June 2019
c
      implicit none
      character*80 filein,fileout,text
      character*2 location,network,net,loc
      character*5 stat
      character*3 com
      integer seiclen
c-- computer type
      logical pc,sun,linux 
      integer i,kstat,knext,k

c   get computertype
c    
      call computer_type(sun,pc,linux)

      write(6,*)'Location code, enter for no change'
      read(5,'(a2)') location

      if(location(2:2).eq.' '.and.location(1:1).ne.' ') then
         write(6,*)'Location must have 2 chars'
         stop
      endif

      write(6,*)'Network code, enter for no change'
      read(5,'(a2)') network

      if(network(2:2).eq.' '.and.network(1:1).ne.' ') then
         write(6,*)'Network must have 2 chars'
         stop
      endif

      open(1,file='filenr.lis',status='old',err=11)
c
  1   continue
      read(1,'(7x,a)',end=10) filein

      if(filein.eq.' ') goto 10  ! the end
      if(filein(1:4).ne.'RESP') then
          write(6,*) filein
          write(6,*)' Not a RESP file name, check filenr.lis'
          stop
      endif

      net=' '
      loc=' '
      stat=' '
      com=' '
      kstat=0

      do i=1,20
         if(filein(i:i).eq.'.'.and.net.eq.' ') then
            knext=i
            if(net.eq.' ') then   ! must be net
              if(filein(i+3:i+3).eq.'.') then
                 net=filein(i+1:i+2)
                 knext=i+3
              endif
              if(filein(i+1:i+1).eq.'.') then
                 net='..'
                 knext=i+1
              endif
              if(filein(i+1:i+1).ne.'.'.and.net.eq.' ') net='..'
            endif
         endif
      enddo
c
c   stat
c
 
      kstat=0
      do i=knext+1,20
        
         if(filein(i:i).ne.'.') then
            kstat=kstat+1
            stat(kstat:kstat)=filein(i:i)
         else
            goto 2
         endif
      enddo   
 2    continue

      knext=knext+kstat+1
c
c  loc

                  
              if(filein(knext+3:knext+3).eq.'.') then
                 loc=filein(knext+1:knext+2)
                 knext=knext+3
              endif
              if(filein(knext+1:knext+1).eq.'.'.and.loc.eq.' ') then
                 loc='..'
                 knext=knext+1
              endif
              if(filein(knext+1:knext+1).ne.'.'.and.loc.eq.' ') then
                 loc='..'
              endif

c
c  com     
c
              k=1
              do i=knext+1,knext+3
                 com(k:k)=filein(i:i)
                 k=k+1
              enddo             
    

c
c   posisbely change names
c
      if(network.ne.' ') net=network
      if(location.ne.' ') loc=location

      fileout=' '
      
      fileout(1:4)='RESP'
      if(net.eq.'..') then
         fileout(5:6)='..'
         k=7
      else
         fileout(5:5)='.'
         fileout(6:7)=net
         fileout(8:8)='.'
         k=9
      endif
      fileout(k:k+kstat-1)=stat(1:kstat)
      k=k+kstat
      if(loc.eq.'..') then
         fileout(k:k+1)='..'
         k=k+2
      else
         fileout(k:k)='.'
         fileout(k+1:k+2)=loc
         fileout(k+3:k+3)='.'
         k=k+4
      endif
      
      fileout(k:k+2)=com
      
      write(6,*) ' Input file: ', filein
      
      write(6,*) 'Output file: ',fileout

      if(pc) then
         text=
     *   'rename '//filein(1:seiclen(filein))//' '//
     *    fileout(1:seiclen(fileout))
      else
         text=
     *   'mv '//filein(1:seiclen(filein))//' '//
     *    fileout(1:seiclen(fileout))
      endif
      write(6,*) text
      call systemc(text,seiclen(text))

      goto 1


      



 11   continue
      write(6,*)'No filnr.lis file with RESP file names'
 10   continue

      stop
      end  