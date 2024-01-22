c   Program imsnor
c   To exchange info with IMS1.0:SHORT.
c
c   Mario Villagran, March 2001
c   IDC-Vienna                  
c   International Data Centre
c
c   converts nordic format files into IMS1.0:SHORT format 
c   as specified in the IDC Formats and Protocols to submit information 
c   to the IDC and viceversa.
c
c   Input:   nordic or IMS1.0:SHORT format file
c
c            if input is nordic, the corresponding hyp.out
c            and print.out are necesary as well as a correlative
c            number to identify the event in IMS1.0:SHORT format.
c
c            if input is IMS1.0:SHORT no more info is needed.
c      
c   Oputput: IMS1.0:SHORT or nordic format file
c
c   comments: When CONVERTING from nordic to IMS1.0:SHORT
c
c             vector printout(1000*flines) contains space for 
c             700*flines print.out lines of an event
c             300*flines nordic file lines
c
c   comments: When CONVERTING from IMS1.0:SHORT to nordic
c
c             vector printout(1000*flines) contains space for 
c             nordic file lines
c
c             To extend the capacity you have to change
c             the parameter flines (factor to increase 
c             the size of the array 1, 2 ,3....).
c
c  Magnitudes, current sep 2018
c              only one magnitude of each type is transferred, the types are
c              b, B, s, S, l, L, d,D->C, w and blank
c              if more magnitudes of the same type are reported, only the last
c              in the list is used. if a priority agency is set (ISC is default)
c              it will be used indpendent of how many of the same type
c              are reported

c
c   Changes:
c   2001  july 5 jh : make distance real, help from mario
c   2007 04 26      : optionally put out a * for no location flag, 
c                     look for varibale 
c   2008 09 16   jh: make sure all lock flags are F, not f. also Mb changed to
c                    MB, limit number of stations to 999
c   2008 09 26   jh. soem d flag was appearing in fixf, removed
c   2008 10 07   jh  transfer first 6 chars of phase instead og 6 from ims, fix pdif
c   2010 mar 9   jh: i to il in read statement, was already done
c   2010 apr 27  jh: B to b and S to s in magnitudes
c   2012 feb 20  jh: problem with end of file
c   2012 sep 28  jh: another ----------------
c   2015 may 14  jh: sligt change in input format, skip events with comments in phases
c   2015 jul 22  jh: phases were skipped if no magnitude lines, take out the weighting
c                    cannot find documentation. program was using info under Def. if 'T'
c                    weight 0, if _, weight 4, seems like located event all have T
c   2016 feb 12  jh: change D to C for codaq magnitude, give output file name
c   2018 sep 27  jh: many changes related to magnitude
czj 2018 oct 15  zj: many change. All changes listed in the code as 'czj'
czj                  Added line is commented as !zj   
czj                  no change made in 'CONVERTING FROM NORDIC TO IMS1.0:SHORT'
c   2018 nov 26  jh: do not write residuals if not given in input
c   2019 apr  9  jh: new format
c   2019 nov 22  pv: removed a tmp change by zj




c
      implicit none

      include 'seisan.inc'
      include 'seidim.inc'
      include 'rea.inc'

c
c size of working array
      integer flines
      parameter        (flines=10)
c-- arguments, input file, location agency and mag agencies.
      character*136    printout(1000*flines),text,tttextt   !SEE comments BEFORE
      character*80     args(10),infile,printhyp,region,top_directory
      character*80     cjunk,nordorg(200)  ! changed from 50 to 200, sep 18
      character*30     ttext
      character        fecha*12,fechax*22,numero*8,sta*5,ph*4,junk*8
      character        lage*7,ph_out*6,fechaa*12,ev_type*1
      character*11     msta(150)
      character*3      prefage,age1,age2,age3,def,mage(9),mage0(40)
c--crustal model, type of event, id of event,kind of magnitudes.
      character*1      model,type,id,fixd,cd,uno,am,undsc,timf,locf
      character*2      mc,mc1,mc2,mc3,mc4,evex,pl
      character*2      cmg(9),cmg0(40)
      real             mg(9),mg0(40)
      character*3      magref   ! reference or prime magnitude
c--time variables, hypocentral parameters and magnitudes.
      integer          year,mo,dy,hr,mn,npha,nsta,number,gap,az,wg
      integer          nsm1,nsm2,nsm3,nsm4,n,dist,reg,kstop,phnumber
      real             xdist
      real             mag,mag1,mag2,mag3,mag4,hdist,smag,sbmag,snr
      real             sec,lat,lon,dep,rms,tres,velo,sec12
      real             ermag1,ermag2,ermag3,x1,x2,x3,mw,smw,azph,raz
      real             ermg(9),ermg0(40)
      integer          nm(9),nmags,nm0(40)
      real             timerr,x,smajor,sminor,erd,dmin,dmax,amp,per
c--ML,Mb
      real             a,b,c,d,smsta,q,equrad,polrad,pi,rad,radius
                       parameter (equrad = 6378.2064)
                       parameter (polrad = 6356.5838)
                       parameter (pi = 3.14159265)
                       parameter (rad = pi/180.)

      integer*4        stime,tarray(9),time,nmsta,inumid
      character*12     t1   ! for seisan system time
      character*14     t2   ! ---------------------
      character*1      lock ! to lock location
c---ellipse error
      real             ery,erx,erz,cvxy,cvxz,cvyz
      real             laterr,lonerr
c---faultplane parameters
      real             strike,dip,rake
      character        typf*3,authorf*3
c---moment tensor
      real             MRR,MTT,MPP,MRT,MPR,MTP,M0,M0G
      integer          sc
c---number of arguments and counters
      integer nars,ncards,i,jj,j,k,ncard,npcard,namp,work,karr,jarr,nmw
      integer nbamp,kk,kkk,jjj,mi
c---for indata
      character*80 data(5000)
      integer code

c--compact or not
      logical compact
c--computer type
      logical pc, sun,linux
c---
c


c
c print version
c
      include 'version.inc'
      out_version_date='July 23, 2001'
      if (version_new) out_version_date=version_date
      call print_ver

      call computer_type(sun,pc,linux)

      call get_seisan_def

c
c   check if input from file given in argument
c   display help if necesary
c
      call get_arguments(nars,args)

      if (nars.eq.0) then                            
        write(6,*) ' Choose option: '
        write(6,*) '     IMS1.0:SHORT -> nordic (1) or '
        write(6,*) '     nordic -> IMS1.0:SHORT (2)'
        read(*,'(a)') args(1)
        if (args(1).eq.'1') then
          write(*,*) ' IMS1.0:SHORT input file '
          read(*,'(a)') args(2)
        elseif (args(1).eq.'2') then
          write(*,*) ' Nordic input file '
          read(*,'(a)') args(2)
          write(*,*) ' Hypocenter output file (default=print.out)'
          read(*,'(a)') args(3)
          if (args(3).eq.' '.or.args(3).eq.'') args(3)='print.out'
          write(*,*) ' Number of first event'
          read(*,'(a)') args(4)
          write(*,*) ' Number of first phase'
          read(*,'(a)') args(5)
        endif
      elseif (nars.eq.1.and.args(0).eq.'-help') then
5       write(6,*)'          '
        write(6,*)' You must define the desired convertion type,'
        write(6,*)' input files and id number (this last only case 2)'
        write(6,*)' 1 for IMS1.0:SHORT------> nordic or'
        write(6,*)' 2 for nordic------> IMS1.0:SHORT'
        write(6,*)' examples:'
        write(6,*)'          '
        write(6,*)' norims 1 xx.inp'
        write(6,*)'          '
        write(6,*)' This converts file xx.inp from IMS1.0:SHORT to'
        write(6,*)' nordic output is file norims.nor'
        write(6,*)'          '
        write(6,*)' norims 2 hyp.out print.out 35 850'
        write(6,*)'          '
        write(6,*)' This creates a IMS1.0:SHORT file from the hyp.out'
        write(6,*)' and print.out files and gives to the first event '
        write(6,*)' in list the number 35, and 850 to the first phase'
        write(6,*)' if number is not defined 001 is the default'
        write(6,*)' for both, output is file norims.ims and compact.ims'
        write(6,*)'          '
        stop
      endif
c
c check arguments
c get input file names and decides what to do
c open files
c
      pl='+-'
      read(args(1),'(i1)',err=5)work
      if((work.eq.1.and.nars.gt.2).or.(nars.gt.5).or.
     *(work.gt.2).or.(work.lt.1))then
        write(6,*)' Wrong arguments !!!!!!!!!!!'
        write(6,*)'    '
        goto 5
      endif
      infile=args(2)
      open(1,file=infile,status='old')                                                             !zjkgfzsgdflt<wetr
c      stime=time()
      call systime(t1,t2)   ! seisan general routine
      read(t1,'(5i2)') (tarray(i),i=6,2,-1)
c      call gmtime(stime, tarray)      !GMT time
      write(ttext(1:30),'(a7,i4,3(a1,i2),i2,a8)')
     *'MSG_ID ',tarray(6)+2000,'/',tarray(5)+1,'/',tarray(4),'_',
     *tarray(3),tarray(2),' IDC_NDC'
      do i=13,22   !fill blanks
        if(ttext(i:i).eq.' ')ttext(i:i)='0'
      enddo
      if(work.eq.1)goto 50
c
c   HERE STARTS CONVERTING FROM NORDIC TO IMS1.0:SHORT
c
      printhyp=args(3)
      read(args(4),'(i5)',err=6)number
6     if(number.eq.0)number=1
      read(args(5),'(i5)',err=7)phnumber
7     if(phnumber.eq.0)phnumber=1
      open(7,file=printhyp,status='old')
      open(2,file='norims.ims',status='unknown')
      open(4,file='compact.ims',status='unknown')
c
c   check if it is a correct s-file
c
      call nortype(1,compact)
      if(compact) then
       write(6,*)' The file must be an F-file'
       stop
      endif
c
c Gets the time at which the file is done if sun GMT time
c note that on pc the time is local unless some extra-arrangement
c   WRITE IMS1.0:SHORT EXCHANGE DATA LINES
c
      write(2,'(a12)')'BEGIN IMS1.0'
      write(2,'(a13)')'MSG_TYPE DATA'
      write(2,'(a30)')ttext(1:30)
      write(2,'(a23)')'DATA_TYPE BULLETIN IMS1.0:SHORT'
      write(4,'(a12)')'BEGIN IMS1.0'
      write(4,'(a13)')'MSG_TYPE DATA'
      write(4,'(a30)')ttext(1:30)
      write(4,'(a23)')'DATA_TYPE BULLETIN IMS1.0:SHORT'
c
c   NORDIC ----> IMS1.0:SHORT    BIG LOOP STARTS HERE
c
      n=0        !  number of processed events
      a=1.00     ! default calculation of ML is
      b=1.11     ! Hutton & Boore, a,b,c,d
      c=0.00189
      d=-1.9680
c magnitude constants for ML (check if other values in STATION0.HYP)
      call topdir(top_directory)
      i=index(top_directory,' ')-1
      text(1:i+17)=top_directory(1:i)//'/DAT/STATION0.HYP'
      open(8,file=text(1:i+17),status='old')
8     read(8,'(a)')text(1:40)
      if(text(1:5).eq.'RESET')then
       if(text(12:13).eq.'75')read(text(16:),*)a
       if(text(12:13).eq.'76')read(text(16:),*)b
       if(text(12:13).eq.'77')read(text(16:),*)c
       if(text(12:13).eq.'78')read(text(16:),*)d
      endif
      close(8)
c
c  initialize variables
c
  10  continue
        mc1(1:2)='  '
        mc2(1:2)='  '
        mc3(1:2)='  '
        mc4(1:2)='  '
        mag1=0.0
        mag2=0.0
        mag3=0.0
        mag4=0.0
        ermag1=0.0
        ermag2=0.0
        ermag3=0.0
        region(1:80)='  '
        nsta=0
        npha=0
        dmax=0.0
        dmin=15000.00
        undsc='_'
        lage='       '
c
c   GET NECESARY INFORMATION FROM print.out AND THE F-file
c
        ncards=1
15      read(7,'(a80)',end=99)printout(1)(1:80)
           if(printout(1)(1:8).eq.' EVENT #')goto 20
        goto 15
20      ncards=ncards+1
         read(7,'(a80)')printout(ncards)(1:80)
         if(printout(ncards)(1:10).eq.'----------')goto 28
c
c displays possible errors and stop the program
c
         if(ncards.gt.650*flines)then
          write(6,*)printout(1)(1:20),'to many print.out lines!!'
          write(6,*)'to solve this, edit norims.f, read comments'
          goto 99
22        write(6,*)'print.out and S-file not matching !!!'
24        goto 99
         endif
        goto 20
28      continue
        do i=1,ncards
          if(printout(i)(1:19).eq.' Origin time error:')
     *    read(printout(i)(20:60),*)timerr
          if(printout(i)(1:18).eq.'   date hrmn   sec')
     *        read(printout(i+1)(1:80),'(73x,f5.1)')erd
          if(printout(i)(6:19).eq.'multiple-phase')
     *    read(printout(i)(1:5),'(i5)')npha
          if(printout(i)(1:14).eq.' Azimuthal Gap')
     *    read(printout(i)(36:38),'(i3)')gap
          if(printout(i)(1:17).eq.' stn   dist   azm')then
             do j=i+1,ncards
               if(printout(j)(1:3).eq.'   ')goto 30
                 read(printout(j)(1:11),'(7x,i4)')k
               if(dmax.lt.k)dmax=k
               if(dmin.gt.k)dmin=k
             enddo
          endif
30        continue
        enddo
c
c read info from current event and
c place the file ready to read next event
c
        ncard=700*flines  !reserve 701*flines to 1000*flines for nordic 
        namp=0
        nbamp=0
        nsm4=0
        npcard=0
        k=0
        smag=0.0
        sbmag=0.0
        smw=0.0
        nmw=0
        nmsta=0
        smsta=0
        mi=0
35      read(1,'(a80)',end=22)text(1:80)
        if(text(1:20).eq.'                    '.
     *      and.text(80:80).eq.' ')goto 40
        ncard=ncard+1
        if(k.eq.0)then
         if(text(80:80).eq.'1'.or.text(80:80).eq.' ')then
          if(text(22:22).eq.'L'.or.text(22:22).eq.'R'.or.
     *    text(22:22).eq.'D')then
            read(text(1:80),100)year,mo,dy,hr,
     *      mn,sec,model,type,id,lat,lon,dep,fixd,lage(5:7),nsta,
     *      rms,mag1,mc1(2:2),age1,mag2,mc2(2:2),age2,mag3,
     *      mc3(2:2),age3,uno
            if(mc1(2:2).eq.'C')mag1=0.0        !MC = MD
            if(mc1(2:2).eq.'C')mc1(2:2)=' '    !to avoid repetition
            if(mc2(2:2).eq.'C')mag2=0.0
            if(mc2(2:2).eq.'C')mc2(2:2)=' '
            if(mc3(2:2).eq.'C')mag3=0.0
            if(mc3(2:2).eq.'C')mc3(2:2)=' '
            k=1
          endif
         endif
        endif
        if(text(80:80).eq.'E')
     *    read(text(1:80),'(23X,F7.3,1X,F7.3,F5.1,3E12.4)')
     *    ery,erx,erz,cvxy,cvxz,cvyz
        if(text(2:15).eq.'Ellipse_as_IDC')
     *    read(text(1:80),'(16x,2(f6.1,1x),i3)')smajor,sminor,az
c statistics for MW
        if(text(80:80).eq.'3'.and.text(7:11).ne.'AVERA'.and.
     *  text(72:73).eq.'MW')then
          read(text(75:78),'(f4.1)')mw
          if(mc1(2:2).eq.'W')mag=mag1
          if(mc2(2:2).eq.'W')mag=mag2
          if(mc3(2:2).eq.'W')mag=mag3
          nmw=nmw+1
          smw=smw+(mw-mag)**2
        endif
c get stations with MD
        if(text(80:80).eq.'3'.and.text(2:2).eq.'$')then
          nmsta=nmsta+1
          msta(nmsta)=text(3:13)
          read(msta(nmsta)(9:11),'(f3.1)')x1
          smsta=smsta+x1
        endif
c
c NOTE:
c get standard deviation of IPRG magnitude and number of stations
c used to get it, this in case that s-file was obtained/converted
c from iprg format to nordic (using isrnor), IF NOT  ermag3 and nsm4 
c are obtained later when converting the arrivals.(see ARRIVAL LINES)
c
        if(text(1:5).eq.' O.T.')then
         do i=20,60
          if(text(i:i+3).eq.'ML =')
     *     read(text(i+5:i+19),'(f3.1,4x,f3.1,3x,i2)')mag4,ermag3,nsm4
         enddo
        endif
c sign where arrival lines start (appended on March 18,1997) m.v.
        if(text(1:5).eq.' STAT')npcard=ncard+1
c get number of amplitudes and stand dev of MLs
        if(mc1(2:2).eq.'L')mag=mag1
        if(mc2(2:2).eq.'L')mag=mag2
        if(mc3(2:2).eq.'L')mag=mag3
        if(npcard.gt.0.and.text(34:40).ne.'       '.and.
     *  text(34:40).ne.' AMPLIT')then
          read(text(1:80),'(33x,g8.1,f4.1,25x,f5.0)')amp,per,xdist
          xdist=dist
          hdist=sqrt(real(dist)*real(dist)+dep*dep)
          if(amp.gt.0.0.and.per.lt.5.0)then
            if(text(11:11).ne.'P')then
              namp=namp+1
              smag=smag+(a*alog10(amp)+b*alog10(hdist)+c*hdist+d-mag)**2
            else
              nbamp=nbamp+1
              hdist=real(dist)/111.2  ! hdist is distance in deg
              call mb_att(dep,hdist,q)
              sbmag=sbmag+alog10(amp/per)+q
              amp=0.0
            endif
          endif
        endif
c region
        if(text(1:2).eq.' *')then
          do i=3,80
           if(text(i:i).eq.'*')then
              region(1:)=text(4:i-2)
              call sei upc(region)
              goto 39
           endif
          enddo
        endif
c copy the f-file
39      printout(ncard)(1:80)=text(1:80)
        goto 35
c
c arrange magnitudes
c
40      continue
        if(mc1(2:2).ne.' ')mc1(1:1)='M'
        if(mc2(2:2).ne.' ')mc2(1:1)='M'
        if(mc3(2:2).ne.' ')mc3(1:1)='M'
        if(mag4.gt.0.0)mc4(1:2)='MD'
c
c  get ellipse dimensions
c  
        npha=npha+nsta
c
c writes date in "fecha" variable and fill blanks with zeros
c
        write(fechax(1:22),'(i4,2(a1,i2),1x,2(i2,a1),f5.2)')
     *   year,'/',mo,'/',dy,hr,':',mn,':',sec
        do i=1,22
          if(fechax(i:i).eq.' ')fechax(i:i)='0'
        enddo
        fechax(11:11)=' '
c
c   prepares the event id line in IMS1.0:SHORT format
c
        numero(1:8)=' '
        write(numero(1:8),'(i8)')number
        do i=1,8
          if(numero(i:i).eq.' ')numero(i:i)='0'
        enddo
c EVENT and REGION ID LINE (if)      !!start writing here
        write(2,'(a5,1x,a8,1x,a50)')'EVENT',numero(1:8),region(1:50)
        write(4,'(a5,1x,a8,1x,a50)')'EVENT',numero(1:8),region(1:50)
c HEADER EVENT LINES
        write(2,150)
        write(4,150)
c ORIGIN LINE
        if(nmsta.gt.0)then
        mc4='MD'
        nsm4=nmsta
        mag4=smsta/float(nmsta)
        smsta=0.0
           do k=1,nmsta
             read(msta(nmsta)(9:11),'(f3.1)')x1
             smsta=smsta+(x1-mag4)**2
           enddo
        ermag3=sqrt(smsta/nmsta)
        endif
        if(fixd.eq.'f')fixd='F'
        write(text(1:136),250)fechax(1:22),timerr,rms,lat,lon,smajor,
     *  sminor,az,dep,fixd,erd,npha,nsta,gap,lage(5:7),numero
        radius = cos(lat)**2/equrad**2
        radius = (radius +sin(lat)**2/polrad**2)**(-.5)
        dmin = real(dmin)/(radius*rad)
        dmax = real(dmax)/(radius*rad)
        write(text(97:110),'(2f7.2)')dmin,dmax
        evex='se'
        if(id.eq.' '.and.mag4.gt.2.5)evex='ke'
        if(id.eq.'E')evex='km'
        if(id.eq.'P')evex='sm'
        write(text(111:117),'(a4,1x,a2)')' m i',evex
        write(2,'(a)')text(1:136)
        write(4,'(a)')text(1:136)
c BLANK LINE
        write(2,*)'                              '
        write(4,*)'                              '
c MAGNITUDE HEADER LINE
        write(2,'(a)')'Magnitude  Err Nsta Author      OrigID'
        write(4,'(a)')'Magnitude  Err Nsta Author      OrigID'
c MAGNITUDE LINES (if)
        text(1:38)='                                      '
        if(mc1.ne.' ')then
          write(text(1:38),310)mc1,mag1,numero
          if(mc1(2:2).eq.'B')write(text(1:2),'(a2)')'mb'
          if(mc1(2:2).eq.'S')write(text(1:2),'(a2)')'Ms'
          if(mc1(2:2).eq.'W')write(text(1:2),'(a2)')'Mw'
          if(mc1(2:2).eq.'L')write(text(16:19),'(i4)')namp
          if(mc1(2:2).eq.'B')write(text(16:19),'(i4)')nbamp
          if(mc1(2:2).eq.'L'.and.namp.gt.1)
     *    write(text(12:14),'(f3.1)')sqrt(smag/namp)
          if(mc1(2:2).eq.'B'.and.nbamp.gt.1)
     *    write(text(12:14),'(f3.1)')sqrt(sbmag/nbamp)
          if(mc1(2:2).eq.'W'.and.nmw.gt.1)
     *    write(text(12:14),'(f3.1)')sqrt(smw/nmw)
          if(text(18:19).eq.' 0')text(18:19)='  '
          write(2,'(a38)')text(1:38)
          write(4,'(a38)')text(1:38)
        endif
        if(mc2.ne.' ')then
          write(text(1:38),310)mc2,mag2,numero
          if(mc2(2:2).eq.'B')write(text(1:2),'(a2)')'mb'
          if(mc2(2:2).eq.'S')write(text(1:2),'(a2)')'Ms'
          if(mc2(2:2).eq.'W')write(text(1:2),'(a2)')'Mw'
          if(mc2(2:2).eq.'L')write(text(16:19),'(i4)')namp
          if(mc2(2:2).eq.'B')write(text(16:19),'(i4)')nbamp
          if(mc2(2:2).eq.'L'.and.namp.gt.1)
     *    write(text(12:14),'(f3.1)')sqrt(smag/namp)
          if(mc2(2:2).eq.'B'.and.nbamp.gt.1)
     *    write(text(12:14),'(f3.1)')sqrt(sbmag/nbamp)
          if(mc2(2:2).eq.'W'.and.nmw.gt.1)
     *    write(text(12:14),'(f3.1)')sqrt(smw/nmw)
          if(text(18:19).eq.' 0')text(18:19)='  '
          write(2,'(a38)')text(1:38)
          write(4,'(a38)')text(1:38)
        endif
        if(mc3.ne.' ')then
          write(text(1:38),310)mc3,mag3,numero
          if(mc3(2:2).eq.'B')write(text(1:2),'(a2)')'mb'
          if(mc3(2:2).eq.'S')write(text(1:2),'(a2)')'Ms'
          if(mc3(2:2).eq.'W')write(text(1:2),'(a2)')'Mw'
          if(mc3(2:2).eq.'L')write(text(16:19),'(i4)')namp
          if(mc3(2:2).eq.'B')write(text(16:19),'(i4)')nbamp
          if(mc3(2:2).eq.'L'.and.namp.gt.1)
     *    write(text(12:14),'(f3.1)')sqrt(smag/namp)
          if(mc3(2:2).eq.'B'.and.nbamp.gt.1)
     *    write(text(12:14),'(f3.1)')sqrt(sbmag/nbamp)
          if(mc3(2:2).eq.'W'.and.nmw.gt.1)
     *    write(text(12:14),'(f3.1)')sqrt(smw/nmw)
          if(text(18:19).eq.' 0')text(18:19)='  '
          write(2,'(a38)')text(1:38)
          write(4,'(a38)')text(1:38)
        endif
        if(mag4.gt.0.0)then
          write(text(1:38),310)mc4,mag4,numero
          write(text(16:19),'(i4)')nsm4
          write(text(12:14),'(f3.1)')ermag3
          if(text(18:19).eq.' 0')text(18:19)='  '
          write(2,'(a38)')text(1:38)
          write(4,'(a38)')text(1:38)
        endif
c BLANK LINE
        write(2,*)'                              '
        write(4,*)'                              '
c HEADER ARRIVAL LINE
        if(ncard-npcard.gt.0)then
        write(2,220)
c ARRIVAL LINES
         do i=npcard,ncard
           amp=0.0
           mag=0.0
           def='___'
           mc(1:2)='  '
           read(printout(i)(2:80),1201)sta,ph,wg,cd,hr,mn,sec,amp,
     *     per,azph,velo,snr,k,tres,xdist,az
           raz=real(k)
           dist=xdist
           if(printout(i)(15:15).ne.'4'.and.printout(i)(11:11).ne.' '.
     *     and.printout(i)(11:11).ne.'T')def(1:1)='T'
           am='m'
           if(printout(i)(16:16).eq.'A')am='a'
           cd='_'
           if(cd.eq.'C')cd='c'
           if(cd.eq.'D')cd='d'
           hdist=sqrt(real(dist)*real(dist)+dep*dep)
           if(amp.gt.0.0.and.per.lt.5.0)then
            mag=a*alog10(amp)+b*alog10(hdist)+c*hdist+d
            mc='ML'
            if(ph(1:1).eq.'P')then
               hdist=real(dist)/(radius*rad) ! hdist is distance in deg
               call mb_att(dep,hdist,q)
               mag=alog10(amp/per)+q
               mc='Mb'
            endif
           endif
           write(fechax(11:22),'(2(i2,a1),f6.3)')hr,':',mn,':',sec
           do k=11,22
             if(fecha(k:k).eq.' ')fecha(k:k)='0'
           enddo
           mc4='  '
           if(nmsta.gt.0)then
             do k=1,nmsta
               if(sta(1:4).eq.msta(k)(1:4))then
                  read(msta(k)(9:11),'(f3.1)')mag4
                  mc4='MD'
               endif
             enddo
           endif
           write(numero(1:8),'(i8)')phnumber
           do k=1,8
             if(numero(k:k).eq.' ')numero(k:k)='0'
           enddo
c easy convertion from km to deg...not so accurate.
           x3 = real(dist)/(radius*rad)
           write(text(1:122),140)sta,x3,real(az),ph,fechax(11:22),tres,
     *     azph,raz,velo,def,snr,amp,per,am,cd,undsc,mc,mag,numero
 140    format(a5,1x,f6.2,f6.1,1x,a4,5x,a12,3f6.1,1x,f6.1,
     *  8x,a3,f6.1,f10.1,f6.2,1x,2a1,a1,1x,a2,4x,f4.1,1x,a8)
           if(text(104:105).eq.'  ')write(text(104:113),'(a2,4x,f4.1)')
     *     mc4,mag4
           if(text(20:20).eq.' ')text(42:46)='     '
           if(text(89:98).eq.' 0.0  0.00')text(89:98)='          '
           if(text(110:113).eq.' 0.0')text(110:113)='    '
           if(azph.eq.0.0)text(47:58)='     '
           if(azph.gt.0.0)text(75:75)='A'
           if(velo.eq.0.0)text(61:65)='     '
           if(velo.gt.0.0)text(76:76)='S'
           if(wg.eq.4)text(75:76)='__'
           if(snr.eq.0.0)text(78:82)='     '
           if(raz.eq.0.0)text(53:58)='      '
           write(2,'(a)')text(1:122)
           phnumber=phnumber+1
         enddo
        endif
c BLANK LINE
        write(2,*)'                              '
        n=n+1
        number=number+1
      goto 10
c   end of BIG LOOP

c
c---------------------------------------------------------------------------------------
c   HERE STARTS IF CONVERTING FROM IMS1.0:SHORT TO NORDIC
c---------------------------------------------------------------------------------------
c
50    open(2,file='norims.nor',status='unknown')
czj   Changed temporarily not to run program interactively.       
      prefage='ISC'     !zj
      magref='ISC'      !zj
      lock=' '          !zj

czj      write(*,*)' Which agency you prefer for nordic first header?'   
czj      write(*,*)' 3 chars...ISC is default or enter other'            
czj      read(*,'(a)') prefage
czj      if(prefage.eq.' ') prefage='ISC'
czj      write(*,*)' Which agency you prefer for magnitudes?'
czj      write(*,*)' 3 chars...ISC) is default or enter other'
czj      read(*,'(a)') magref
czj      if(magref.eq.' ') magref='ISC'
czj      write(6,*) ' Set no location flag (y/n=default)'
czj      read(5,'(a1)') lock
czj      if(lock.ne.'y'.and.lock.ne.' ') lock=' '
czj      if(lock.eq.'y') lock='*'

      write(*,*)' Which agency you prefer for nordic first header?'   
      write(*,*)' 3 chars...ISC is default or enter other'            
      read(*,'(a)') prefage
      if(prefage.eq.' ') prefage='ISC'
      write(*,*)' Which agency you prefer for magnitudes?'
      write(*,*)' 3 chars...ISC) is default or enter other'
      read(*,'(a)') magref
      if(magref.eq.' ') magref='ISC'
      write(6,*) ' Set no location flag (y/n=default)'
      read(5,'(a1)') lock
      if(lock.ne.'y'.and.lock.ne.' ') lock=' '
      if(lock.eq.'y') lock='*'

      n=0  !number of events
      ncards=i
      ncard=0
      kstop=0
c
c-----------------------------------------------------------------------------
c   IMS1.0:SHORT ----> NORDIC    BIG LOOP STARTS HERE
c-----------------------------------------------------------------------------
c
c     NEW EVENT
c
c the variables printout(1*flines)-->printout(350*flines)
c are reserved for lines above the nordic line type 7 (also included
c and printout(351*flines)-->printout(999*flines) for arrival lines
c
c
      write(printout(1000*flines)(1:80),'(a,a)')
     *' STAT SP IPHASW D HRMM SECON CODA AMPLIT PERI AZIMU ',
c     *'VELO SNR AR TRES W  DIS CAZ7'
     *'VELO AIN AR TRES W  DIS CAZ7'
60    continue
      karr=350*flines   !the arrival's counter
      j=0               !the header's counter
      jarr=1            !defines if input is short or long input file
      dmax=0.0
      dmin=15000.00
      cmg(1)='Mb'
      cmg(2)='Ms'
      cmg(3)='ML'
      cmg(4)='MD'
      cmg(5)='Ml'
      cmg(6)='  '
      cmg(7)='Mw'
      cmg(8)='MS'
      cmg(9)='MB'
      timf=' '
      locf=' '
czj default event type is 'D', based on distance we keep it or change it to 'R' or 'L'
      ev_type='D'
      do i=1,9
        mg(i)=0.0       
        ermg(i)=0.0
        nm(i)=0
      enddo
      mc1(1:2)='  '
      mc2(1:2)='  '
      mc3(1:2)='  '
      mag1=0.0
      mag2=0.0
      mag3=0.0
      nmags=0
      nsta=0
czj magnitude counter
      mi=0
czj parameters for calculating laterr and lon error from semi-major and azimuth
      laterr=0.0
      lonerr=0.0
65    continue
        read(1,'(a)',end=99)text
        if(text(1:4).eq.'STOP')then
          if(n.eq.0)then
            write(6,*)'No events were found in input file !!!'
68          write(6,*)'Error, Check event # ',n
          endif
          goto 99
        endif
c
c Identifies that there is an event, creates location line 
c of nordic format and ID line
c
        if(text(1:6).eq.'EVENT '.or.text(1:6).eq.'Event ')then
           n=n+1
cxx
           write(6,*) text(1:60)
           read(text(7:),*)inumid,cjunk(1:50)
           read(text(7:),'(a50)')region(1:50)
           reg=0
        endif
        if(text(4:18).eq.'Date       Time')then     !LONG IG starts here
c allows more than 1 blank line after header (german ims reports case)
c Aug 19, 1997....m.v.
         mi=0
69       read(1,'(a)',end=99)text     !reads blank lines
         if(text(5:5).eq.'/')then
           backspace(1)

c read first header line
czj   In the original version, there was problem reading seconds.
czj   Two lines are added to fix this problem.
czj   The problem: if second doesn't have decimal values, ims reports
czj   it as integer e.g.  55 not 55.00
czj   However in the original program the seconds format defined as F5.2. 
czj   Therefore seconds like 55 (without any point and decimals) in ims 
czj    was reported 0.55 in the nordic format. 
           read(1,'(a)',end=99,err=68)tttextt                !zj
           tttextt(20:20)='.'        !zj

           read(tttextt,255)year,mo,dy,hr,mn,sec,timf,timerr,
     *     rms,lat,lon,locf,smajor,sminor,az,dep,fixd,erd,npha,nsta,
     *     gap,x1,x2,text(1:3),evex,lage,numero

czj   In ims format name of agency can be more than 3, but in nordic format we
czj   are intrested in 3 first letters. In this case, agencies like ISC, ISC-EHB
czj   and ISCJB will all be reported as ISC. to avoid this, two lines are added.    
           if(lage.eq.'ISCJB') lage='JB_'      !zj Jeffry Bullen velocity Model
           if(lage.eq.'ISC-EHB') lage='EHB'    !zj

           do i=1,3
             if(lage(i:i).eq.' '.or.lage(i:i).eq.'_')lage(i:i)='X'
           enddo
           if(fixd.eq.'f')fixd='F'
c line type 1
           j=j+1
c
c   check if not too many stations
c
           if(nsta.gt.999) nsta=999
czj        In some examples rms is higher than 100, which doesn't fit to seisan format
czj        then I modified to have 99.9 and then replace it with blank.
           if(rms.gt.99) rms=99.9       !zj
c          
           
           write(printout(j),101)year,mo,dy,hr,mn,sec,' ','D',' ',lat,
     *     lon,dep,fixd,lage(1:3),nsta,rms,'1'
           
           if(nsta.eq.0) printout(j)(49:51)='   '      !zj
           if(rms.eq.99.9) printout(j)(52:55)='    '   !zj


czj   This information are added using ims format to improve the conversion
czj    and report all explosions
           if(evex.eq.'uk')printout(j)(23:23)=' '   !zj
           if(evex.eq.'de')printout(j)(23:23)='Q'   !zj
           if(evex.eq.'fe')printout(j)(23:23)='Q'   !zj
           if(evex.eq.'ke')printout(j)(23:23)='Q'   !zj
           if(evex.eq.'se')printout(j)(23:23)=' '   !zj
           if(evex.eq.'kr')printout(j)(23:23)=' '   !zj
           if(evex.eq.'sr')printout(j)(23:23)=' '   !zj
           if(evex.eq.'ki')printout(j)(23:23)=' '   !zj
           if(evex.eq.'si')printout(j)(23:23)=' '   !zj
           if(evex.eq.'km')printout(j)(23:23)='E'
           if(evex.eq.'sm')printout(j)(23:23)='P'
           if(evex.eq.'kh')printout(j)(23:23)='E'   !zj
           if(evex.eq.'sh')printout(j)(23:23)='P'   !zj
           if(evex.eq.'kx')printout(j)(23:23)='E'   !zj 
           if(evex.eq.'sx')printout(j)(23:23)='P'   !zj 
           if(evex.eq.'kn')printout(j)(23:23)='E'   !zj 
           if(evex.eq.'sn')printout(j)(23:23)='P'   !zj
           if(evex.eq.'ls')printout(j)(23:23)='L'   !zj
czj   Discreption from IMS  
czj        uk = unknown
czj	   de = damaging earthquake ( Not standard IMS )
czj	   fe = felt earthquake ( Not standard IMS )
czj	   ke = known earthquake
czj	   se = suspected earthquake
czj	   kr = known rockburst
czj	   sr = suspected rockburst
czj	   ki = known induced event
czj	   si = suspected induced event
czj	   km = known mine expl.
czj	   sm = suspected mine expl.
czj	   kh = known chemical expl. ( Not standard IMS )
czj	   sh = suspected chemical expl. ( Not standard IMS )
czj	   kx = known experimental expl.
czj	   sx = suspected experimental expl.
czj	   kn = known nuclear expl.
czj	   sn = suspected nuclear explosion
czj	   ls = landslide


           do i=7,18
            if(printout(j)(i:i).eq.' ')printout(j)(i:i)='0'
           enddo
c jh
           if(printout(j)(44:44).ne.' '.and.printout(j)(44:44).ne.'F') 
     *     printout(j)(44:44)=' '
           if(printout(j)(45:45).ne.' '.and.printout(j)(45:45).ne.'F') 
     *     printout(j)(45:45)=' '

           if(timf.eq.'f') timf='F'
           printout(j)(11:11)=timf(1:1)
           printout(j)(16:16)=' '
           if(locf(1:1).eq.'f') locf(1:1)='F'
           printout(j)(45:45)=locf(1:1)

czj  If there is information on error ellipse, we creat line type '3'
          if(smajor.gt.0.0)then                                        !zj
           j=j+1                                                       !zj
           write(printout(j),'(4x,a3,3x,a28,f5.1,1x,a12,f5.1,1x,a9,i3,    
     *     5x,a1)')lage(1:3),'Error ellipse is semi-major=',
     *     smajor,' semi-minor=',sminor,' Azimuth=',az,'3'             !zj
          endif                                                        !zj

czj  If there is time and location error, we creat line type 'E'
          if(timerr.ne.0.0.or.erd.ne.0.0) then                         !zj
czj Keep only 'E' line for prime solution, for rest save it as type '3' line.
           if(lage(1:3).eq.prefage)then                                !zj 
           	j=j+1                                                  !zj
           	write(printout(j),'(14x,F6.2,4x,F6.1,2x,F6.1,f5.1,
     *     	36x,a1)')timerr,laterr,lonerr,erd,'E'                            
           	if(laterr.eq.0.0)printout(j)(25:30)='      '                !zj 
           	if(lonerr.eq.0.0)printout(j)(33:38)='      '                !zj 
           	if(timerr.eq.0.0)printout(j)(15:20)='      '                !zj
           	if(erd.eq.0.0)printout(j)(39:43)='     '                    !zj
	   else
           	j=j+1                                                  !zj
           	write(printout(j),'(14x,F6.2,4x,F6.1,2x,F6.1,f5.1,
     *     	36x,a1)')timerr,laterr,lonerr,erd,'3'                            
           	if(laterr.eq.0.0)printout(j)(25:30)='      '                !zj 
           	if(lonerr.eq.0.0)printout(j)(33:38)='      '                !zj 
           	if(timerr.eq.0.0)printout(j)(15:20)='      '                !zj
           	if(erd.eq.0.0)printout(j)(39:43)='     '                    !zj 
           endif 
          endif

c
c   not date line, something with (
c
czj   In the original version all extra data was reported as line type '3'
czj   I commented out these lines to be able to creat line type 'F','M' and '3'

czj         elseif(text(2:2).eq.'(')then
czj700        j=j+1   !This case for info between brackets in the middle
czj           write(printout(j),'(1x,a2,a70,a3,3x,a1)')  !of origin lines
czj     *     '* ',text(1:70),'***','3'
czj           do i=2,136
czj             if(text(i:i).eq.')')goto 69
czj           enddo
czj           read(1,'(a)',end=70)text
czj           goto 700


czj   Here I start to read comments lines from IMS fromat and save type 'F','M' and '3'
czj   We don't need to keep all comments from IMS, only #Felt,#FaulthPlane,#MOMENTTensor
czj   #PRINAX,#PARAM

czj  Special parameters like depth recalculated from depth phases type '3' 
        
czj  Special parameters like depth recalculated from depth phases type '3'        
         elseif(text(2:8).eq.'(#PARAM')then                               !zj
           j=j+1                                                          !zj
           write(printout(j),'(3x,a70,6x,a1)')                            !zj
     *     text(1:70),'3'                                                 !zj
           do i=2,136                                                     !zj 
             if(text(i:i).eq.')')goto 69                                  !zj
           enddo                                                          !zj
czj  If the earthquake felt type '3'       
         elseif(text(2:6).eq.'(FELT')then                                 !zj
           j=j+1                                                          !zj
           write(printout(j),'(3x,a70,6x,a1)')                            !zj
     *     text(1:70),'3'                                                 !zj
           do i=2,136                                                     !zj
             if(text(i:i).eq.')')goto 69                                  !zj
           enddo                                                          !zj
czj  Principle axes type '3'    
         elseif(text(2:9).eq.'(#PRINAX')then                              !zj
           j=j+1                                                          !zj
           write(printout(j),'(3x,a73,3x,a1)')                            !zj 
     *     text(4:76),'3'                                                 !zj
           read(1,'(a)',end=70)text                                       !zj
           j=j+1                                                          !zj
           write(printout(j),'(3x,a73,3x,a1)')                            !zj  
     *     text(4:76),'3'                                                 !zj
           if(text(2:3).eq.'(+')then                                      !zj
             read(1,'(a)',end=70)text                                     !zj
             j=j+1                                                        !zj
             write(printout(j),'(3x,a73,3x,a1)')                          !zj 
     *       text(4:76),'3'                                               !zj
             read(1,'(a)',end=70)text                                     !zj
             j=j+1                                                        !zj
             write(printout(j),'(3x,a73,3x,a1)')                          !zj 
     *       text(4:76),'3'                                               !zj
	   endif                                                          !zj 
           do i=2,136                                                     !zj
             if(text(i:i).eq.')')goto 69                                  !zj
           enddo 
czj  Fault plane solution type 'F'       
         elseif(text(2:14).eq.'(#FAULT_PLANE')then                        !zj
           read(1,'(a)',end=70)text                                       !zj
           read(text(16:18),'(a3)')typf                                   !zj
           read(text(20:25),'(f6.2)')strike                               !zj
           read(text(27:31),'(f5.2)')dip                                  !zj
           read(text(33:39),'(f7.2)')rake                                 !zj
           read(text(55:57),'(a3)')authorf                                !zj
           j=j+1                                                          !zj
           write(printout(j),'(3(f10.1),36x,a3,1x,a6,2x,a2)')strike,               
     *     dip,rake,authorf,'IMSNOR','OF'                                 !zj
czj We don't need too keep both focal plane solution                         !zj 
czj           read(1,'(a)',end=70)text                                       !zj
czj           if(text(2:3).eq.'(+')then                                      !zj
czj             read(text(20:25),'(f6.2)')strike                             !zj
czj             read(text(27:31),'(f5.2)')dip                                !zj
czj             read(text(33:39),'(f7.2)')rake                               !zj
czj             j=j+1                                                        !zj
czj             write(printout(j),'(3(f10.1),48x,a2)')strike,dip,rake,'+F'   !zj
czj           else                                                           !zj
czj             backspace(1)                                                 !zj
czj           endif                                                          !zj
           do i=2,136                                                     !zj
             if(text(i:i).eq.')')goto 69                                  !zj                                       
           enddo                                                          !zj
czj  Moment tensor type 'M'        
         elseif(text(2:10).eq.'(#MOMTENS')then                            !zj
           read(1,'(a)',end=70)text                            !zj
           read(1,'(a)',end=70)text                                       !zj
           read(text(12:13),'(i2)')sc                                     !zj
           read(text(15:19),'(f5.3)')M0                                   !zj
           read(text(27:32),'(f6.3)')MRR                                  !zj
           read(text(34:39),'(f6.3)')MTT                                  !zj
           read(text(41:46),'(f6.3)')MPP                                  !zj
           read(text(48:53),'(f6.3)')MRT                                  !zj
           read(text(55:60),'(f6.3)')MTP                                  !zj
           read(text(62:67),'(f6.3)')MPR                                  !zj
           read(text(79:81),'(a3)')authorf                                !zj
           if(M0.ne.0.0)M0=M0/10                                          !zj
czj if the Moment tensor is empty don't write it and keep moment information
czj as type '3' line starting with: (M0 =
           if(MRR.ne.0.0.or.MTT.ne.0.0.or.MPP.ne.0.0.or.MRT.ne.0.0.or.
     *         MTP.ne.0.0.or.MPR.ne.0.0)then                              !zj
           	j=j+1                                                          !zj
           	write(printout(j),'(1x,i4,2(1x,2i2),1x,f4.1,3x,f7.3,                                    
     *     	f8.3,f5.1,2x,a3,22x,a7,2x,a1)')year,mo,dy,hr,mn,sec,lat,
     *     	lon,dep,lage(1:3),'IMSNOR ','M'               
           	j=j+1                                                          !zj
           	write(printout(j),'(1x,a2,6(f6.3,1x),a3,1x,i2,1x,
     *     	F5.3,a2,i2,9x,a6,3x,a1)')'MT',MRR,MTT,MPP,MRT,MPR,MTP,
     *     	authorf,sc,M0,'E+',sc+1,'IMSNOR','M'                           !zj

           	if(M0.eq.0.0.and.sc.eq.0.0)printout(j)(50:61)=' '              !zj 
           elseif(M0.ne.0.0)then
           	j=j+1                                                          !zj
           	write(printout(j),'(1x,a6,F5.3,a2,i2,a2,61x,a1)')'(M0 = ',
     *          M0,'E+',sc+1,' )','3'                       !zj
           endif

           read(1,'(a)',end=70)text                                       !zj
           do i=2,136                                                     !zj
             if(text(i:i).eq.')')goto 69                                  !zj
           enddo 
c
c   blank line
c

         elseif(text(1:5).eq.'     ')then
           if(prefage.ne.'   ')then
             kk=0     !sorts origin lines if a prefered agency exists

             do i=1,j
               if(printout(i)(46:48).eq.prefage)then
                  kk=kk+1                     
                  nordorg(kk)(1:80)=printout(i)(1:80)
                  printout(i)(1:5)='IMOUT'
czj In original verion of program, while sorting the header line of the prefered 
czj agency moved to top line, however the relevant comment lines didn't moved with h
czj with header which was confusing. 'do' loop here is added to fix this problem.

                  do jjj=i+1,j                                            !zj                                    
		  	if(printout(jjj)(80:80).eq.'3')then               !zj
				kk=kk+1                                   !zj
                       		nordorg(kk)(1:80)=printout(jjj)(1:80)     !zj
				printout(jjj)(1:5)='IMOUT'                !zj
                  	elseif(printout(jjj)(80:80).eq.'E')then           !zj
				kk=kk+1                                   !zj
                        	nordorg(kk)(1:80)=printout(jjj)(1:80)     !zj
				printout(jjj)(1:5)='IMOUT'                !zj
                  	elseif(printout(jjj)(80:80).eq.'F')then           !zj
				kk=kk+1                                   !zj
                        	nordorg(kk)(1:80)=printout(jjj)(1:80)     !zj
				printout(jjj)(1:5)='IMOUT'                !zj
                  	elseif(printout(jjj)(80:80).eq.'M')then           !zj
				kk=kk+1                                   !zj
                        	nordorg(kk)(1:80)=printout(jjj)(1:80)     !zj
				printout(jjj)(1:5)='IMOUT'                !zj
                  	else                                              !zj
                      		EXIT                                      !zj
                  	endif                                             !zj
		  enddo                                                   !zj
               endif
             enddo
             do i=1,j
               if(printout(i)(1:5).ne.'IMOUT')then
                  kk=kk+1
                  nordorg(kk)(1:80)=printout(i)(1:80)
               endif
             enddo
             do i=1,j
                  printout(i)(1:80)=nordorg(i)(1:80)   !done
             enddo
           endif
           goto 70
         endif
         goto 69

c read second header line, could be magnitude

70       continue
         read(1,'(a)',end=73)text     !reads blank lines
         if(text(1:5).eq.'Event'.or.text(1:5).eq.'EVENT')then
           backspace(1) !This in case that there are no magnitudes
           goto 73
         endif
c
c  also check for phases, there might not be magnitudes
c
         if(text(1:7).eq.'Sta    ') then
             backspace(1)
             goto 73
         endif

         if(text(1:14).ne.'Magnitude  Err')goto 70
c
c gets magnitude line by line, only done for Ms, mb and ML.....
c what about blank?
c
71       read(1,'(a)',end=99)text

         if(text.eq.' ')goto 72      ! only checked first 4 chars before so blank type would skip magnitudes
cblank        if(text(1:1).eq.'m'.or.text(1:1).eq.'M') then

czj Here is modified to keep all reported magnitudes, all types and all agencies
            mi=mi+1                                                         !zj
            read(text(1:23),300,err=68)cmg0(mi),mg0(mi),ermg0(mi),
     *      nm0(mi),mage0(mi)                                               !zj
czj Here we keep only 10 defined magnitude and remove the rest
czj We might need to decide on what to do with b/B or s/S. 
      if(cmg0(mi).eq.'b ')then
         cmg0(mi)='b'
      elseif(cmg0(mi).eq.'s ')then 
	 cmg0(mi)='s'
      elseif(cmg0(mi).eq.'L')then
	 cmg0(mi)='L'
      elseif(cmg0(mi).eq.'D ')then
	 cmg0(mi)='D'
      elseif(cmg0(mi).eq.'d ')then
	 cmg0(mi)='d'
      elseif(cmg0(mi).eq.'l ')then
	 cmg0(mi)='l'
czj read balnk magnitude type but don't save it
      elseif(cmg0(mi).eq.' ')then
	 mi=mi-1
      elseif(cmg0(mi).eq.'W'.or.cmg0(mi)(2:2).eq.'w')then
	 cmg0(mi)='W'
      elseif(cmg0(mi).eq.'S')then
	 cmg0(mi)='S'
      elseif(cmg0(mi).eq.'B')then
	 cmg0(mi)='B'
      else
         mi=mi-1
      endif


czj         if(text(2:4).eq.'b  ') then
czj            if(mg(1).ne.0.0.and.mage(1).eq.magref) goto 71    ! skip since already refrence mag
czj            read(text(1:23),300,err=68)mg(1),ermg(1),nm(1),mage(1)
czj         endif

czj         if(text(2:4).eq.'s  ') then
czj           if(mg(2).ne.0.0.and.mage(2).eq.magref) goto 71    ! skip since already refrence mag
czj           read(text(1:23),300,err=68)mg(2),ermg(2),nm(2),mage(2)
czj         endif

czj         if(text(2:4).eq.'L  ') then
czj           if(mg(3).ne.0.0.and.mage(3).eq.magref) goto 71    ! skip since already refrence mag
czj           read(text(1:23),300,err=68)mg(3),ermg(3),nm(3),mage(3)
czj         endif

czj         if(text(2:4).eq.'D  ') then
czj           if(mg(4).ne.0.0.and.mage(4).eq.magref) goto 71    ! skip since already refrence mag
czj           read(text(1:23),300,err=68)mg(4),ermg(4),nm(4),mage(4)
czj         endif

czj         if(text(2:4).eq.'d  ') then
czj           if(mg(4).ne.0.0.and.mage(4).eq.magref) goto 71    ! skip since already refrence mag
czj           read(text(1:23),300,err=68)mg(4),ermg(4),nm(4),mage(4)
czj         endif

czj         if(text(2:4).eq.'l  ') then
czj           if(mg(5).ne.0.0.and.mage(5).eq.magref) goto 71    ! skip since already refrence mag
czj           read(text(1:23),300,err=68)mg(5),ermg(5),nm(5),mage(5)
czj         endif

czj         if(text(2:4).eq.'   ') then
czj           if(mg(6).ne.0.0.and.mage(6).eq.magref) goto 71    ! skip since already refrence mag
czj           read(text(1:23),300,err=68)mg(6),ermg(6),nm(6),mage(6)
czj         endif

czj         if(text(2:4).eq.'w  ') then     
czj            if(mg(7).ne.0.0.and.mage(7).eq.magref) goto 71    ! skip since already refrence mag
czj            read(text(1:23),300,err=68)mg(7),ermg(7),nm(7),mage(7)
czj         endif

czj         if(text(2:4).eq.'S  ') then
czj           if(mg(8).ne.0.0.and.mage(8).eq.magref) goto 71    ! skip since already refrence mag
czj           read(text(1:23),300,err=68)mg(8),ermg(8),nm(8),mage(8)
czj         endif

czj         if(text(2:4).eq.'B  ') then
czj           if(mg(9).ne.0.0.and.mage(9).eq.magref) goto 71    ! skip since already refrence mag
czj           read(text(1:23),300,err=68)mg(9),ermg(9),nm(9),mage(9)
czjcblank         endif
czj        endif

        goto 71
c
c   end of magnitudes
c
72       continue
c  magnitudes insertion work  
czj The magnitude insertation modified in a way that each magnitudes goes to the relevant
czj  line type '1'. 
czj So Hypocentre reporting agency and magnitude repoting magnitude is the same.
czj As a result each agency can keep up to 3 magnitude, and we can have same magnitude type
czj  reported by different agency
         kk=56
         kkk=1
czj         do k=1,9    ! At the moment only 9 kinds of magnitude are parsed
	 do jj=1,j       !zj  counter on agency
             kk=56       !zj  back to kk=56 for each agency
          do k=1,mi      !zj  counter on all magnitudes
           if(mg0(k).gt.0.0.and.kk.le.72)then
c
c   change D to C for coda magnitude, jh feb 2016
              if(cmg0(k)(1:1).eq.'D') cmg0(k)(1:1)='C'

              if(mage0(k).eq.printout(jj)(46:48))then   !zj checks the agency
              if(printout(jj)(80:80).eq.'1')then      !zj type 'M' line also has agency name
czj              write(printout(kkk)(kk:kk+7),'(f4.1,a1,a3)')
czj     *        mg(k),cmg(k)(2:2),mage(k)
czj sometimes an agency reports the same time of magnitude more than once, here I skip it.
               if(cmg0(k)(1:1).ne.printout(jj)(60:60))then
               if(cmg0(k)(1:1).ne.printout(jj)(68:68))then
              	write(printout(jj)(kk:kk+7),'(f4.1,a1,a3)')     
     *        	mg0(k),cmg0(k)(1:1),mage0(k)             !zj
              	kk=kk+8
               endif
               endif
               endif                                   !zj
              endif                                    !zj
           endif
czj           if(kk.gt.72)then
czj             kk=56
c
c  shift all header lines down to make room for 2. magnitude line
c

czj              if(j.gt.1) then
czj                 do i=j-1,1,-1
czj                   printout(i+2)=printout(i+1)
czj                 enddo
czj                 j=j+1
czj              else
czj                 j=2
czj              endif              
czj              printout(2)=' '
czj              printout(2)(80:80)='1'
czj              printout(2)(1:55)=printout(1)(1:55)
czj              kkk=2

c             if(kkk.gt.j)goto 73
czj           endif
         enddo                                        !zj end of counter on magnitudes
       enddo                                          !zj ens of counter on agency for mag insertation
c
c line type I
c
73       continue
         write(printout(j+1)(1:80),'(a12,a11,1x,a2,a34,i4,5i2,a6)')
     *   ' ACTION:REG ',ttext(10:20),ttext(21:22),
     *   ' OP:nims STATUS:               ID:',year,mo,dy,hr,mn,
     *   int(sec),'     I'
         printout(j+1)(15:15)='-'
         printout(j+1)(18:18)='-'
         printout(j+1)(21:21)=' '
         printout(j+1)(24:24)=':'
         do i=61,74
           if(printout(j+1)(i:i).eq.' ')printout(j+1)(i:i)='0'
         enddo
c
c This line to keep some extra information from IDC that nordic
c format does not have specific fields...ellipse and qk ID

czj This information are added earier in the code when we read this information.
czj In original version, only information kept from last agency reported this information.
czj Now everything is kept. But maybe better to keep only from prime solution(?)
czj The reporting agency added and magnitude removed 

czj         write(printout(j+2),'(1x,a21,f5.1,1x,a8,f5.1,1x,a5,i3,
czj     *   1x,a3,3x,a9,i8,5x,a1)')'IDC: ellipse  smajor=',smajor,
czj     *   ' sminor=',sminor,' Azi=',az,printout(j)(46:48),
czj     *   'Event ID ',inumid,'3'
czj         j=j+2
          j=j+1                     !zj, only one line added

c in case region is specified adds one line type 3
         if(region(1:10).ne.'          '.and.reg.eq.0)then
           j=j+1
           reg=1
czj           write(printout(j),'(1x,a2,a50,a3,23x,a1)')
czj     *     '* ',region(1:50),'***','3'
czj The way the region is reported modified by adding 'LOCALITY:' 
           write(printout(j),'(1x,a9,1x,a50,18x,a1)')      !zj
     *     'LOCALITY:',region(1:50),'3'                    !zj
         endif

74       read(1,'(a)',end=82, err=82)text     !  reads blank lines

         if(text(1:7).eq.'Sta    ')then
           jarr=0
           goto 80   !go for arrivals
         elseif(text(1:1).eq.'(')then
75         j=j+1   !This case for info between brackets after
           write(printout(j),'(1x,a2,a70,a3,3x,a1)')  !origin lines
     *     '* ',text(1:70),'***','3'
           do i=2,136
             if(text(i:i).eq.')')goto 74
           enddo
           read(1,'(a)',end=99)text
           goto 75
         elseif(text(1:5).eq.'Event'.or.text(1:5).eq.'EVENT')then
           backspace(1)      !This case for no arrivals in ims file
           goto 82
         endif
         goto 74
c
c ARRIVALS
c

80       read(1,'(a)',end=99)text
c
c   skip comments, jh may 2015
c
         if(text(2:2).eq.'(') goto 80

         if(text.eq.'          ')then
81         read(1,'(a)',end=99)text

           if(text(1:4).ne.'    '.and.text(1:5).ne.'EVENT'.
     *     and.text(1:4).ne.'STOP'.and.text(1:5).ne.'Event')goto 85
           if(text(1:4).eq.'    ')goto 81
           backspace(1)
           if(text(1:4).eq.'STOP')kstop=1
82         continue
           if(locf(1:1).eq.' ') printout(1)(45:45)=lock
czj This line is added to write the correct event type in the first header line
           if(printout(1)(80:80).eq.'1')printout(1)(22:22)=ev_type   !zj
           write(2,'(1x,a79)')(printout(i)(2:80),i=1,j)
           write(2,'(a80)')printout(1000*flines)(1:80)
           if(jarr.eq.0)
     *     write(2,'(1x,a79)')(printout(i)(2:80),i=350*flines+1,karr)
           write(2,'(a80)')'    '
           read(1,'(a)',end=102,err=102)text
           backspace(1)
           goto 60
         endif
85       karr=karr+1
         read(text,1140)sta,x1,x2,ph_out,fecha,tres,azph,raz,velo,
     *   def,snr,amp,per,am,cd,undsc,mc,mag,junk

c
c  now read 6 chars of phase
c
 1140   format(a5,1x,f6.2,f6.1,1x,a6,3x,a12,3f6.1,1x,f6.1,      ! jh oct 08
     *  8x,a3,f6.1,f10.1,f6.2,1x,2a1,a1,1x,a2,4x,f4.1,1x,a8)
         if(x1.lt.dmin)dmin=x1
         if(x1.gt.dmax)dmax=x1
czj station magnitude is commented out
czj         if(mag.gt.0.0)then
czj          j=j+1
czj          write(printout(j)(1:80),'(1x,a1,1x,a5,1x,a2,1x,f4.2,63x,a1)')
czj     *    '$',sta,mc,mag,'3'
czj         endif
         if(cd.eq.'_')cd=' '
         if(cd.ne.' ')call sei upc(cd)
czj the same time problem occur here with second, it is now modified. 
         if(fecha(9:9).eq.' ')fecha(9:9)='.'          !zj

         read(fecha(1:12),'(2(i2,1x),f6.3)')hr,mn,sec
czj I dont understand thispart(!)
         write(ttext(1:6),'(f6.1)')raz  !approximate az residual
         read(ttext(6:6),'(i1)')k
         if(k.ge.5.and.raz.lt.0)raz=raz-1.0
         if(k.ge.5.and.raz.gt.0)raz=raz+1.0
         radius = cos(lat)**2/equrad**2
         radius = (radius +sin(lat)**2/polrad**2)**(-.5)
         radius = radius*x1*rad

czj  Here, the calculated distance is compared to dmin and dmax to decide if
czj   event is 'L','R','D'. The final decision made on max distance from all stations
         if(radius.gt.dmax)dmax=radius     !zj
         if(radius.lt.dmin)dmin=radius     !zj
         if(dmax.lt.3000.0.and.dmax.ge.1000.0)ev_type='R'   !zj
         if(dmax.lt.1000.0.and.dmax.gt.0.0)ev_type='L'      !zj

         wg=0
         if(velo.gt.999.9)velo=999.9
c
c   check for pdif, jh oct 08
c
         if(ph_out.eq.'P DIFF') ph_out='Pdif  '

         if(amp.lt.1000.)then
          write(printout(karr)(2:80),1120)sta,ph_out,cd,hr,mn,sec,
     *    amp,per,azph,velo,snr,int(raz),tres,int(radius),int(x2)
         else
          write(printout(karr)(2:80),1122)sta,ph_out,cd,hr,mn,sec,
     *    amp,per,azph,velo,snr,int(raz),tres,int(radius),int(x2)
         endif
c
c   now put weight   oct 08 jh take it out again july 15
c
         wg=0
c         if(def(1:1).eq.'_') wg=4
         if(ph_out(5:5).eq.' ') then
            write(printout(karr)(15:15),'(i1)') wg
            if(wg.eq.0) printout(karr)(15:15)=' '
         else
            write(printout(karr)(9:9),'(i1)') wg  ! phase longer than 4 chars
            if(wg.eq.0) printout(karr)(9:9)=' '
         endif

 1120   format(a5,4x,a6,a1,1x,2i2,1x,f5.2,5x,g7.2,1x,f4.1,f6.1,
     *  f5.1,f4.1,i3,f5.1,2x,i5,1x,i3,1x)
 1122   format(a5,4x,a6,a1,1x,2i2,1x,f5.2,5x,g7.1,1x,f4.1,f6.1,
     *  f5.1,f4.1,i3,f5.1,2x,i5,1x,i3,1x)
         if(text(14:18).eq.'     ')printout(karr)(77:79)='   '
         if(amp.eq.0.0)printout(karr)(34:45)='             '
         if(azph.eq.0.0)printout(karr)(46:51)='      '
         if(velo.eq.0.0)printout(karr)(52:56)='     '
         if(snr.gt.99.9)write(printout(karr)(57:60),'(f4.0)')snr
         if(snr.gt.999.9)printout(karr)(57:60)='999.'
         if(snr.eq.0.0)printout(karr)(57:60)='    '
         if(raz.eq.0.0)printout(karr)(61:63)='   '
         if(raz.lt.-99.9)printout(karr)(61:63)='-99'
         if(tres.lt.-99.9.or.tres.gt.999.9)
     *   write(printout(karr)(64:68),'(f5.0)')tres
c
cjh blank out if blank in input file
c
         if(text(43:46).eq.' ') printout(karr)(64:68)=' '

czj   The quality of pick is added: 'E---> emergen'
czj                                 'I---> impulse'
czj                                  blank for rest 
         if(undsc.eq.'e')printout(karr)(10:10)='E'   !zj
         if(undsc.eq.'i')printout(karr)(10:10)='I'   !zj


cc         if(def(1:1).eq.'_')write(printout(karr)(15:15),'(a1)')'4'

         if(tres.eq.0.0.and.printout(karr)(15:15).eq.'4')
     *   printout(karr)(64:68)='     '

cc         if(printout(karr)(15:15).eq.'0')printout(karr)(15:15)=' '

         if(am.eq.'a')write(printout(karr)(16:16),'(a1)')'A'
c        printout(karr)(7:8)='SZ'
         if(printout(karr)(19:19).eq.' ')printout(karr)(19:19)='0'
         if(printout(karr)(21:21).eq.' ')printout(karr)(21:21)='0'
         if(printout(karr)(24:24).eq.' ')printout(karr)(24:24)='0'
         goto 80
        endif      !LONG IF ends here
      goto 65
c
c   FORMATS   !!!!!
c
 100    format(1x,i4,2(1x,2i2),1x,f4.1,3a1,f7.3,f8.3,f5.1,a1,1x,a3,
     *  i3,f4.1,3(1x,f3.1,a1,a3),a1)
 101    format(1x,i4,2(1x,2i2),1x,f4.1,3a1,f7.3,f8.3,f5.1,a1,1x,a3,
     *  i3,f4.1,24x,a1)
 
 1201   format(a5,4x,a4,i1,1x,a1,1x,2i2,1x,f5.2,5x,g7.2,1x,f4.1,f6.1,
     *  f5.1,f4.1,i3,f5.1,2x,f5.0,1x,i3,1x)
 
 150    format(3x,'Date',7x,'Time',8x,'Err',3x,'RMS',1x,'Latitude',1x,
     *  'Longitude',2x,'Smaj',2x,'Smin',2x,'Az',1x,'Depth',3x,'Err',
     *  1x,'Ndef',1x,'Nsta',1x,'Gap',
     *  2x,'mdist  Mdist Qual   Author      OrigID')
 220    format('Sta',5x,'Dist  EvAz',1x,
     *  'Phase        Time      TRes  Azim AzRes   Slow   SRes',
     *  ' Def   SNR       Amp   Per Qual Magnitude    ArrID')
c write IMS1.0:SHORT format, nsta, no fixing flags for origin time and location.
 250    format(a22,1x,2f6.2,1x,f8.4,1x,f9.4,2f6.1,1x,
     *  i3,1x,f5.1,a1,f5.1,2i5,1x,i3,22x,a3,'_NDC',3x,a8)
 255    format(i4,1x,4(i2,1x),f5.2,a1,2f6.2,1x,f8.4,1x,f9.4,a1,f5.1,
     *  f6.1,1x,i3,1x,f5.1,a1,f5.1,2i5,1x,i3,2f7.2,1x,a3,1x,a2,1x,
     *  a7,3x,a8)
 300    format(1x,a2,3x,f4.1,1x,f3.1,1x,i4,1x,a3)
 310    format(a2,4x,f4.1,10x,'XXX_NDC',3x,a8)
c read the IMS1.0:SHORT format, except agency and number id
c
c Finish the program
close files
c
 99   continue
      if(work.eq.2)then
        write(2,'(a4)')'STOP'
        backspace(4)
        write(4,'(a4)')'STOP'
        close(4)
        close(7)
      else
        if(kstop.eq.0.and.n.gt.0)then
          kstop=1
          goto 82
        endif
      endif
102   close(1)
      
c
c   convert to new nordic
c
      if(new_nordic_format) then
        open(15,file='norims.new',status='unknown')

        rewind(2)
        
        do i=1,n
           call rea_event_in(2,.true.,data,code)   ! automatically read old
           call rea_event_out(15,.true.,data,code) ! select new for output
        enddo
      endif

      close(2)
 
      write(6,*)
      write(6,*)' The output file contains ',n,' events processed'
      if(work.eq.1)then
         if(new_nordic_format) then
            write(6,*)' The output file name is norims.new'
         else
             write(6,*)' The output file name is norims.nor'
         endif
      endif
      if(work.eq.2) write(6,*)' The output file name is norims.ims'
      stop
      end
