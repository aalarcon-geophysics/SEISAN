c 
c PROGRAM ILOC_LOOP
c
c SEISAN Program to locate many events in database with ILOC, automatic or 
c semi-automatic.
c Reads index.out file (output from SELECT).
c works only on linux
c 
c peter voss at GEUS, 2019-04-09 first version
c

      character*7 number
      character*1 answer
      character*58 sfile
      logical linux,pc

      linux=.TRUE.
      pc=.FALSE.
      answer='n'

      i=0
      j=0
      no=0
c
      open(1,file='index.out')
      do
      read(1,'(a7,a58,1x)',end=999)number,sfile
      write(6,'(a7,a58,1x)')number,sfile
      call system('iloc_seisan '//sfile)
c
      INQUIRE(file='iloc-out.isf',SIZE=iloc_out)
      if(iloc_out.GT.0) then
         write(6,*)' '
         write(6,*)' Add the iLoc solution just shown to'
         write(6,*)' the current ',
     *   'event in the data base (y=yes/n=no/A=yes-to-all)?'
         if(answer.NE.'A') then
           read(5,'(a)') answer
         endif
         if(answer.eq.'A'.or.answer.eq.'Y'.or.answer.eq.'y') then
            if(linux) call system('cp iloc_seisan.out '//sfile)
            if(pc) call system('copy iloc_seisan.out '//sfile)
         endif
      endif
c
      end do

  999 continue
      close(1)
      end


