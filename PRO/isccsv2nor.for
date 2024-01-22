c   
c   program converts csv ascii  format to nordic format
c   csv is used by isc for catalog data
c   only the first 3 magnitudes are used
c   if a magnitude type in input is b1, output is x
c
c   j. havskov, january 2014
c
c   changes
c
c 20220503 jh: format has been changed 


      implicit none
      character*200 t
      character*80 tt
      character*80 infile
      integer i,j,n,k


       write(6,*) 'Input file'
       read(5,'(a)') infile
       open(1,file=infile,status='old')
       open(2,file='isccsv2nor.out',status='unknown')
       
       n=0
c
c   data shiftet 5 columns compared to before
c
       k=5
c
c   find first line with data
c
       do i=1,200
          read(1,'(a)') t
          if(t(3:9).eq.'EVENTID') goto 1
       enddo

       

1      continue

c
c   read in a loop
c
       read(1,'(a)',end=99) t
c       write(6,'(a)') t
       if(t.eq.' ') goto 99   
       n=n+1
       tt=' '

       tt(2:5)=t(21+k:24+k)
       tt(7:8)=t(26+k:27+k)
       tt(9:10)=t(29+k:30+k)
       tt(12:13)=t(32+k:33+k)
       tt(14:15)=t(35+k:36+k)
       tt(17:20)=t(38+k:41+k)
       tt(22:22)='L'
       tt(24:30)=t(44+k:60+k)
       tt(31:38)=t(53+k:60+k)
c   depth
       tt(39:43)=t(63+k:67+k)
c   check if depth fixed
       if(t(69+k:72+k).eq.'TRUE') tt(44:44)='F'
       tt(46:48)=t(76+k:78+k)
c   first mag
       tt(57:59)=t(94+k:96+k)
       tt(60:60)=t(87+k:87+k)
       if(t(60+k:61+k).eq.'b1') tt(60:60)='x'
       tt(61:63)=t(98+k:101+k)
c   sec mag
       tt(65:67)=t(116+k:118+k)
       tt(68:68)=t(109+k:109+k)
       if(t(109+k:110+k).eq.'b1') tt(68:68)='x'
       tt(69:71)=t(120+k:122+k)
c   third mag
       tt(73:75)=t(138+k:140+k)
       tt(76:76)=t(131+k:131+k)
       if(t(131+k:132+k).eq.'b1') tt(76:76)='x'
       tt(77:79)=t(142+k:144+k)
       
       tt(80:80)='1'

       write(2,'(a)') tt 
       write(6,'(a)') tt      
       goto 1

 99    continue
       write(6,*)'Number of events',n
       write(6,*)'Output file name is isccsv2nor.out'
       stop
       end    

c    old format

c
c   read in a loop
c
c       read(1,'(a)',end=99) t
c       if(t.eq.' ') goto 99   
c       n=n+1
c       tt=' '
c       tt(2:5)=t(21:24)
c       tt(7:8)=t(26:27)
c       tt(9:10)=t(29:30)
c       tt(12:13)=t(32:33)
c       tt(14:15)=t(35:36)
cc       tt(17:20)=t(38:41)
c       tt(22:22)='L'
c       tt(24:30)=t(44:60)
c       tt(31:38)=t(53:60)
c   depth
c       tt(39:43)=t(63:67)
c   check if depth fixed
c       if(t(69:72).eq.'TRUE') tt(44:44)='F'
c       tt(46:48)=t(76:78)
c   first mag
c       tt(57:59)=t(94:96)
c       tt(60:60)=t(87:87)
c       if(t(60:61).eq.'b1') tt(60:60)='x'
c       tt(61:63)=t(98:101)
c   sec mag
c       tt(65:67)=t(116:118)
c       tt(68:68)=t(109:109)
c       if(t(109:110).eq.'b1') tt(68:68)='x'
c       tt(69:71)=t(120:122)
c   third mag
c       tt(73:75)=t(138:140)
c       tt(76:76)=t(131:131)
cc       if(t(131:132).eq.'b1') tt(76:76)='x'
c       tt(77:79)=t(142:144)
       
c       tt(80:80)='1'

c       write(2,'(a)') tt       
c       goto 1          
