      subroutine nortype(unit,compact)
c
c   check if file is compact or not.
c
c   jh july 1993
c
c   updates
c
c   sep 16 98, jh : ---------   version 7.0 check-------------------
c                   no changes
c   mar 29 2020 jh: was checking 20 lines, increase to 50 as large
c                   events from isc might have many hypocenters
c   oct 28 2020 jh: one more condition
c
c
c   unit : file unit already opened
c   compact: true if compact
c
      implicit none
      character*80 text
      logical compact
      integer i,unit
c
         compact=.true.
c
c  assume that there are not more than 50 epicenter lines for one event
c
         do i=1,50                                                              
            read(unit,'(a80)',end=10) text                                     
            if(text(22:22).ne.'L'.and.text(22:22).ne.'R'.                 
     *      and.text(22:22).ne.'D'.and.text(22:22).ne.' ')                
     *      compact=.false.                                     
c
c   it could be that there are only epicenter lines and blank lines
c
            if(text(1:30).eq.'                              ') 
     *      compact=.false.
            if(text(80:80).eq.' '.and.text(1:79).ne.' ')
     *      compact=.false.
         enddo                      
c
 10      continue                                            
         rewind unit
         return
         end
