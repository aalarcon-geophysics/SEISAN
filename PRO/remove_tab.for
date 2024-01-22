c     program to remove tabs and unprintble chars, jh
c
      implicit none
      integer i,n,k,m, line, tab
      character*80 text,infile

      n=0
      k=0
      m=0
      line=0
      tab=9

      write(6,*)' File name'
      read(5,'(a)') infile
      open(1,file=infile,status='old')
      open(2,file='remove_tab.out',status='unknown')
 10   continue

      read(1,'(a)',end=20) text
      line=line+1
      do i=1,80
        if(text(i:i).eq.char(9)) then
           write(6,'(a,3i7)')
     *     'Replaced tab character with blank, char,line and pos ',
     *     tab,line,i
          n=n+1
          text(i:i)=' '
        endif

        if(ichar(text(i:i)).gt.130) then 
           write(6,'(a,3i7)')
     *     'Replaced character with blank, char,line and pos     ', 
     *     ichar(text(i:i)),line,i
           k=k+1
           text(i:i)=' '
        endif

        if(ichar(text(i:i)).lt.32) then
           write(6,'(a,3i7)')
     *     'Replaced character with blank, char, line and pos    ', 
     *     ichar(text(i:i)),line,i
           m=m+1
           text(i:i)=' '
        endif

      enddo
      write(2,'(a)') text
      goto 10
 20   continue
      write(6,*)
      write(6,*)'Number of tabs found', n
      write(6,*)'Number of non standard chars above 130', k
      write(6,*)'Number of non standard chars below 32 - tabs', m
      write(6,*)'Output file is remove_tab.out'
      stop
      end


