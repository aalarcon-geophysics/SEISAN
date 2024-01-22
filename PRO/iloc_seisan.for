c
c   PROGRAM ILOC_SEISAN
c
c   SEISAN Program to run iloc
c   reads an sfile, convert sfile to isf format, run iloc and add
c   iloc hypocenter to sfile.
c   pv october 2018
c
c   2018-12-11 pv : added to SEISAN PRO
c   2020-11-23 pv : changed type from 3 to L
c   2020-11-23 pv : if agnecy isnt in env, look in station file.
c
      implicit none
      CHARACTER*80 DATA1(2500),data3(2500)
      CHARACTER*80 infile
      CHARACTER*1 TYPE,EXP
c-- arguments
      character*80 args(10)    
c---number of arguments 
      integer nars
c---time of making update
      character*12 p_time
      character*14 proc_time
      character*4 operator
      integer nhyp,norg       ! number of events located by hypoinverse
                              ! number of events in input cat file
      character*80 text
      integer nstat1,nstat2,nstat3,nphase1,nphase2,nhead1,
     *nhead3,nrecord1,nrecord2,nrecord3
      integer i,n,id1,id2,seiclen
      integer iloc_out_size
c
c    delete old input  output files
c
      open(111,file='iLoc.instruction',status='unknown') 
      close(111,status='delete')
      open(111,file='iloc-out.isf',status='unknown') 
      close(111,status='delete')
      open(111,file='isfnor.tmp',status='unknown') 
      close(111,status='delete')
      open(111,file='iloc_seisan.out',status='unknown') 
      close(111,status='delete')
c
      infile=' '
c
c   check if input from file given in argument
c
      call get_arguments(nars,args)
      if(nars.gt.0) then
         infile=args(1)
         operator=args(2)
      endif 

c
c   get input file name, check if exist
c

 9    continue
      if(infile.eq.' ') then
         write(6,*) 'Give input file'
         read(5,'(a)') infile
 22      continue
         write(6,*) 'Give operator'
         read(5,'(a)') operator
         if(operator.eq.' ') goto 22
      endif
c
      open(1,file=infile,status='old',err=10)
      goto 11
 10   continue
      write(6,*)' No such input file'
      infile=' '
      goto 9
 11   continue
      close(1)

      write(6,*)' input file: ',infile
c
c     in infile: remove lines without lat+lon
          write(6,'(a)') "remove ilines without lat+lon"
c
      i=1
      open(111,file='isfnor.tmp',status='new') 
      open(30,file=infile,status='old') 
  31  read(30,'(a)',end=32) text
      if(text(80:80).EQ."1")then
        if(i.EQ.1)then
          if(text(24:30).EQ."       ")then
            write(6,*)' Event must have epicenter!!!'
            goto 999
          else
            write(111,'(a)') text
            write(6,*) i
            write(6,'(a)') text
          endif
        endif
        if(i.NE.1.AND.text(24:30).EQ."       ")then
          i=i+0
        endif
      else
        write(111,'(a)') text
      endif
      i=i+1
      goto 31
  32  continue
      close(30)
      close(111)
          write(6,'(a)') "done remove ilines without lat+lon"
c
c     convert infile to iloc input
c
c  open output file
c
      open(111,file='isfnor.in',status='unknown') 
      write(111,'(a)')'1'
c     write(111,'(a)')infile
      write(111,'(a)')"isfnor.tmp"
      close(111)
      call systemc('cat isfnor.in | isfnor',22)
c     
      write(6,*)' tmp fix for isfnor.out ; cut PRIME line  '
      call systemc('sed -i "/#PRIME/d" norisf.out',29)
c     
      open(111,file='iLoc.instruction',status='unknown') 
      write(111,'(a)')
     +'ISFInputFile=norisf.out ISFOutputFile=iloc-out.isf'
      close(111)
c     
      call systemc('iloc isf < iLoc.instruction',27)
c
      INQUIRE(file='iloc-out.isf', SIZE=iloc_out_size)
c     write(6,*)'iloc_out_size=',iloc_out_size
c
      if(iloc_out_size.GT.0) then
        open(1,file=infile,status='old',err=999)
        CALL INDATA(1,NSTAT1,NPHASE1,NHEAD1,NRECORD1,TYPE,EXP,DATA1,ID1)
        close(1)
c
c  merge files if they correspond
c
        call merge_iloc
     *   (data1,data3,nhead1,nhead3,
     *    nrecord1,nrecord3)
c
        open(2,file='iloc_seisan.out',status='unknown') 
        do i=1,nrecord3
          write(2,'(a)') data3(i)
        enddo
      else
        write(6,*)''
        write(6,*)' Unable to use iLoc, see above'
        write(6,*)''
      endif
c
      goto 999
c
 999  continue
c     write(6,'(a,i5,a,i5)')'Original # ev ',norg,
c    *' Hypoinverse # ev',nhyp
c     write(6,*) 'Output file is hypinv_seisan.out'
      stop
      end      

      subroutine merge_iloc
     *(data1,data3,nhead1,nhead3,nrecord1,
     *nrecord3)
c
c
      implicit none
      CHARACTER*80 infile
      CHARACTER*80 DATA1(*),data3(*) 
      CHARACTER*144 text
c     logical err                    ! true if error line
      logical new_iloc               ! true is first iloc attempt
      logical new_iloc_com           ! true is first iloc attempt
      integer nhead1,
     *nhead3,nrecord1,nrecord3
      integer j,i,norg,nhyp

      character*5 agency
      call get_agency(agency)
      if(agency(1:5).EQ."     ")then
        call get_agency_hyp(data1(21:21),agency)
      endif
c
      new_iloc=.TRUE.
      new_iloc_com=.TRUE.
c
      open(42,file='iloc-out.isf',status='old')
  40  read(42,'(a)',end=41) text
      do while (text(119:122).NE.'ILOC')  
        read(42,'(a)',end=41) text
      enddo
  41  continue
c     write(6,*) '   ',text
      close(42)
c
c   main header, keep old magnitudes and agency
c
      j=0
      do i=1,nhead1-1
        j=j+1
        if(data1(i)(2:4).NE.'GAP'.AND.data1(i)(80:80).EQ.'L'.AND.
     +    data1(i)(61:65).EQ.agency(1:5).AND.
     +    data1(i)(67:73).EQ.'ILOC   ')then
          j=j-1
          new_iloc=.FALSE.
        elseif(data1(i)(2:4).EQ.'GAP'.AND.data1(i)(80:80).EQ.'L'.AND.
     +    data1(i)(61:65).EQ.agency(1:5).AND.
     +    data1(i)(67:73).EQ.'ILOC   ')then
          j=j-1
          new_iloc_com=.FALSE.
        else
          data3(j)=data1(i)
        endif
      enddo
c
c   Add iLoc solution
c
      j=j+1
      data3(j)(1:1)=' '
      data3(j)(2:5)=text(1:4)          ! year
      data3(j)(6:6)=' '
      data3(j)(7:8)=text(6:7)          ! mdr
      data3(j)(9:10)=text(9:10)        ! day
      data3(j)(11:11)=' '              ! 
      data3(j)(12:13)=text(12:13)      ! hour
      data3(j)(14:15)=text(15:16)      ! min
      data3(j)(16:16)='  '
      data3(j)(17:21)=text(18:22)      ! sec
      data3(j)(22:23)='  '
      data3(j)(24:31)=text(37:44)      ! lat
      data3(j)(32:33)='  '
      data3(j)(34:42)=text(46:54)      ! lon
      data3(j)(43:44)='  '
      data3(j)(45:49)=text(72:76)      ! depth
      data3(j)(50:52)='   '
      data3(j)(53:53)=text(77:77)      ! depth fix
      data3(j)(54:59)=text(31:35)      ! rms
      data3(j)(60:60)=' '
      data3(j)(61:65)=agency(1:5)      ! agency
      data3(j)(66:66)=' '
      data3(j)(67:73)='ILOC   '        ! method
      data3(j)(74:79)='       '
      data3(j)(80:80)='L'
c
c   Add iLoc solution parameters
c
      j=j+1
      data3(j)(1:20)='                    '
      data3(j)(21:40)='                    '
      data3(j)(41:60)='                    '
      data3(j)(61:80)='                    '
      data3(j)(1:4)=' GAP'
      data3(j)(5:7)=text(94:96)        ! azimuthal gap
      data3(j)(8:9)=' E'
      data3(j)(10:14)=text(25:29)      ! hypotime error
      data3(j)(15:16)=' j'
      data3(j)(17:21)=text(56:60)      ! Smajor
      data3(j)(22:23)=' i'
      data3(j)(24:28)=text(62:66)      ! Smin
      data3(j)(29:30)=' A'
      data3(j)(31:33)=text(68:70)      ! Az
      data3(j)(34:35)=' s'
      data3(j)(36:39)=text(89:92)      ! number of defining stations
      data3(j)(40:41)=' d'
      data3(j)(42:45)=text(84:87)      ! number of defining phases
c     
      data3(j)(46:46)='m'
      data3(j)(47:52)=text(98:103)     ! distance to closest station deg.
c     
      data3(j)(53:53)='M'
      data3(j)(54:59)=text(105:110)    ! distance to furthest station deg.
c     
      data3(j)(60:60)=' '
      data3(j)(61:65)=agency(1:5)      ! agency
      data3(j)(66:66)=' '
      data3(j)(67:73)='ILOC   '        ! method
      data3(j)(74:79)='       '
      data3(j)(80:80)='L'
c
      j=j+1
      data3(j)=data1(nhead1)
c
      do i=nhead1+1,nrecord1
        j=j+1
        data3(j)=data1(i)
      enddo
c
      if(new_iloc) then
        nrecord3=nrecord1+2
      else
        nrecord3=nrecord1
      endif
c
      return
      end

