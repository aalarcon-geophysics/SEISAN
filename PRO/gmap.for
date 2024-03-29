c 
c PROGRAM GMAP
c
c program to convert Nordic format and SEISAN station and polygon files 
c to earth.google KML format.
c
c 2023-12-03 pv: added option to write GeoJSON file seisan.geojson
c 2021-12-07 pv: added flag for comverting DAT/place_names.eev to KML
c 2021-09-23 pv: added highaccuracy flag
c 2016-08-17 pv: fix bug setting MSIZE, fix bug sorting event_id=Q
c 2015-10-23 pv: moved event-id=Q from other events to eqs
c 2015-06-15 pv: cleanup
c 2013-06-05 pv: correct bug in error ellipse
c 2012-04-19 pv: added error ellipse
c peter voss at GEUS, 28 apr 2009 version 0.9.8 Beta : add -stat
c peter voss at GEUS, 28 apr 2009 version 0.9.7 Beta : add -poly
c peter voss at GEUS, 17 apr 2008 version 0.9.6 Beta : added parameters to SEISAN.DEF
c peter voss at GEUS, 26 feb 2008 version 0.9.5 Beta : add -TimeSpan
c peter voss at GEUS, 20 feb 2008 version 0.9.4 Beta : add -color ff0000ff
c                                                    : add -nodata
c peter voss at GEUS, 19 feb 2008 version 0.9.3 Beta : use first available mag in header
c peter voss at GEUS,  8 feb 2008 version 0.9.2 Beta : will handel compact files
c                                                      can handel <,>,&,",' in Title
c peter voss at GEUS, 11 dec 2007 version 0.9.1 Beta
c
c   arguments:
c              -help           print help list
c              -color ff0000ff define color of epicenters
c              -nodata         kml file will only contain header line
c              -errorellipse   kml file will include error ellipse
c              -highaccuracy   kml file will use lat and lon from first H line in s-file
c              -input_file     define name of input file (default is read from terminal)
c              -out_file       define name of output file (default is gmap.kml)
c              -verbose        be more verbose
c              -version        seisan version
c              -stat           convert seisan STATION?.HYP files into kml
c              -poly           convert seisan polygon files into kml 
c              -GeoJSON        Output in GeoJSON format
c

       implicit none

C
C    Seisan library inserts and routines...
C    ======================================
C
       include 'libsei.inc'                ! Open file definitions
       include 'seidim.inc'                ! dimentions
C
       external sei open,                  ! Open file routine.
     &          sei close,                 ! Close file routine.
     &          sei code                   ! Error encoder.
C
C    ============= end of list ==========
C
c-- INPUT FILE                                         
      character*80 file                   ! name of input file
      character*80 out_file               ! full name of output file
      character*80 out_file_geojson       ! full name of output file in GeoJSON format
      character*120 defdata
      character*80 data(max_data)
      character*80 title
      character*80 url,urle,urlpe,urlo
      character*8 color,colore,colorpe,coloro
      CHARACTER*1 TYPE,EXP

      character*80 ellipsecolor
      real ellipsewidth

      integer neq, nexp, npexp, nother, i
      integer nstat,nphase,nhead,nrecord

      real MSIZE,XSIZE,YSIZE

      integer narg                            ! number of arguments
      character*80 arg(40)                    ! arguments
      character*80 gmap_append_kml(100)       ! text that can be appended to gmap.kml
      integer n_gmap_append_kml               ! number of text lines

C-- ID LINE NUMBER
      INTEGER ID

      logical visible       ! show event in kml
      logical sdata
      logical compact       ! compact input file or not
      logical datafolder    ! kml file contain no folders with data
      logical errorellipse  ! kml file will include error ellipse
      logical highaccuracy  ! kml file will use lat and lon from first H line in s-file
      logical ttag          ! kml file contain timespan for scrolling in time domain
      logical geojson
      logical inputfile
      logical usetitle

c input file unit #
      integer read01,write01
c logical for existing file or not
      logical          b_old
c returned code
      integer          code
c-- unit for file
      integer def_unit
      logical  b_eof                         ! End of file?.

      logical  verbose
c
c print version
c
      include 'version.inc'
      out_version_date='APR 28, 2009'
      if (version_new) out_version_date=version_date
      call print_ver
c
c   get seisan defaults
c
      call get_seisan_def
c
      MSIZE=0.5
      XSIZE=0.2
      YSIZE=0.5

      url="http://maps.google.com/mapfiles/kml/pal2/icon26.png"
      urle="http://maps.google.com/mapfiles/kml/shapes/star.png"
      urlpe=
     +"http://maps.google.com/mapfiles/kml/shapes/open-diamond.png"
      urlo="http://maps.google.com/mapfiles/kml/shapes/square.png"

      out_file='gmap.kml'
      out_file_geojson='seisan.geojson'

      color="ff0000ff"
      colore="ff0000ff"
      colorpe="ff0000ff"
      coloro="ff0000ff"

      neq=0
      nexp=0
      npexp=0
      nother=0

      verbose=.FALSE.
      datafolder=.TRUE.
      geojson=.FALSE.
      inputfile=.FALSE.
      errorellipse=.FALSE.
      highaccuracy=.FALSE.
      ttag=.FALSE.
      usetitle=.FALSE.

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c   open and read default file
c   ---------------------------
      n_gmap_append_kml=0
c
      call sei get file( open$+ignore$,    ! Find and open without messages.
     &                   def_unit,         ! On file unit.
     &                   code,             ! Condition (n/a).
     &                   'DAT',            ! Alternative directory to search.
     &                   'SEISAN.DEF' )    ! For this file.
c
c   read file if there...
c   ---------------------
c
c     if(code.ne.e_ok$)  return

 333  continue
c
      read(def_unit,'(a)',iostat=code) defdata    ! Read from file.
      call sei code( fort$,                    ! Process fortran i/o condition.
     &               code,                     ! Condition.
     &               def_unit,                 ! On unit.
     &               b_eof )                   ! End of file?.
c
      if( .not.b_eof ) then                    ! Not end of file.
c
c   Look for GMAP parameters
c
       if (defdata(1:15).eq.'GMAP_ICON_QUAKE') then
	    read(defdata(41:80),'(a)') url(1:40)
       elseif (defdata(1:19).eq.'GMAP_ICON_EXPLOSION') then
	    read(defdata(41:80),'(a)') urle(1:40)
       elseif (defdata(1:19).eq.'GMAP_ICON_PROB_EXPL') then
	    read(defdata(41:80),'(a)') urlpe(1:40)
       elseif (defdata(1:22).eq.'GMAP_ICON_OTHER_EVENTS') then
	    read(defdata(41:80),'(a)') urlo(1:40)
       elseif (defdata(1:15).eq.'GMAP_ICON_MSIZE') then
            read(defdata(41:55),'(f15.5)') MSIZE
       elseif (defdata(1:15).eq.'GMAP_ICON_XSIZE') then
            read(defdata(41:55),'(f15.5)') XSIZE
       elseif (defdata(1:15).eq.'GMAP_ICON_YSIZE') then
            read(defdata(41:55),'(f15.5)') YSIZE
       elseif (defdata(1:15).eq.'GMAP_APPEND_KML') then
	    n_gmap_append_kml=n_gmap_append_kml+1
            gmap_append_kml(n_gmap_append_kml)=defdata(41:120)
       endif
c
c   go to next line
c
      goto 333
      endif
      call sei close( close$, def_unit, code ) ! Close (Default stop on error).
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      call get_arguments(narg,arg)
      if(narg.ge.1) then
      do i=1,narg
      if( arg(i)(1:8) .eq. '-verbose' ) verbose=.TRUE.
      if( arg(i)(1:12) .eq. '-place_names' ) then
        call gmap_place_names
        stop
      endif
      if( arg(i)(1:5) .eq. '-stat' ) then
        call gmapstat
        stop
      endif
      if( arg(i)(1:5) .eq. '-poly' ) then
        call gmappoly
        stop
      endif
      if( arg(i)(1:7) .eq. '-nodata' ) datafolder=.FALSE.
      if( arg(i)(1:8) .eq. '-GeoJSON' ) geojson=.TRUE.
      if( arg(i)(1:13) .eq. '-errorellipse' ) errorellipse=.TRUE.
      if( arg(i)(1:13) .eq. '-highaccuracy' ) highaccuracy=.TRUE.
      if( arg(i)(1:13) .eq. '-ellipsecolor' ) then
        ellipsecolor=arg(i+1)
      endif
      if( arg(i)(1:13) .eq. '-ellipsewidth' ) then
        call sei get values(1,arg(i+1), code )
        ellipsewidth=array$(1)
      endif
      if( arg(i)(1:9) .eq. '-timespan' ) ttag=.TRUE.
      if( arg(i)(1:6) .eq. '-title' ) then
	title=arg(i+1)
        usetitle=.TRUE.
      endif
      if( arg(i)(1:11) .eq. '-input_file' ) then
	file=arg(i+1)
        inputfile=.TRUE.
      endif
      if( arg(i)(1:9) .eq. '-out_file' ) then
	out_file=arg(i+1)
      endif
      if( arg(i)(1:6) .eq. '-color' ) then
	color=arg(i+1)
	colore=arg(i+1)
	colorpe=arg(i+1)
	coloro=arg(i+1)
        if( arg(i+1)(1:4) .eq. 'blue' ) then
          color="ffff0000"
          colore="ffff0000"
          colorpe="ffff0000"
          coloro="ffff0000"
        endif
        if( arg(i+1)(1:5) .eq. 'green' ) then
          color="ff00ff00"
          colore="ff00ff00"
          colorpe="ff00ff00"
          coloro="ff00ff00"
        endif
        if( arg(i+1)(1:6) .eq. 'yellow' ) then
          color="ffffff00"
          colore="ffffff00"
          colorpe="ffffff00"
          coloro="ffffff00"
        endif
        if( arg(i+1)(1:5) .eq. 'black' ) then
          color="ff000000"
          colore="ff000000"
          colorpe="ff000000"
          coloro="ff000000"
        endif
        if( arg(i+1)(1:5) .eq. 'white' ) then
          color="ffffffff"
          colore="ffffffff"
          colorpe="ffffffff"
          coloro="ffffffff"
        endif
      endif
      if( arg(i)(1:5).eq.'-help'.or.arg(i)(1:3).eq.'-h') then
        write(6,*)' '
        write(6,*)
     +' The GMAP program converts Nordic format to the KML format.'
        write(6,*)
     +' The output file gmap.kml can be opened with Google Earth.'
        write(6,*)
     +' The program prompts for a input file in nordic format, the'
        write(6,*)' input file can be compact.'
        write(6,*)
     +' The program also convert SEISAN station and polygon files.'
        write(6,*)' '
        write(6,*)' Usage: gmap [options]'
        write(6,*)'        input file    '
        write(6,*)'        Title used for kml folder'
        write(6,*)' '
        write(6,*)' ## Options ##'
        write(6,'(a17,a16)')'    -help        '
     +,' Print this list'
        write(6,'(a17,a14)')'    -h           '
     +,' Same as -help'
        write(6,'(a17,a49)')'    -input_file  '
     +,' Define name of input file name                  '
        write(6,'(a17,a49)')'    -title       '
     +,' Give title for KML container                    '
        write(6,'(a17,a47)')'    -color       '
     +,' Define color of epicenters [blue/green/yellow/'
        write(6,'(a17,a45)')'                 '
     +,' black/white]. Default color is red          '
        write(6,'(a17,a45)')'                 '
     +,' To uses other colors see describtion below  '
        write(6,'(a17,a45)')'    -timespan    '
     +,' Events gets timetag scroll in time domain   '
        write(6,'(a17,a45)')'    -nodata      '
     +,' kml file will only contain header infomation'
        write(6,'(a17,a44)')'    -errorellipse'
     +,' kml file will include error ellipse        '
        write(6,'(a17,a44)')'    -ellipsecolor'
     +,' Define color of ellipse in 8 digit hex code'
        write(6,'(a17,a44)')'    -ellipsewidth'
     +,' Define width of the ellipse line, in pixels'
        write(6,'(a17,a44)')'    -GeoJSON     '
     +,' Output file seisan.geojson is generated    '
        write(6,'(a17,a44)')'    -highaccuracy'
     +,' kml file will use lat and long from H line '
        write(6,'(a17,a46)')'    -place_names '
     +,' Locations given in DAT/place_names.eev file  '
        write(6,'(a17,a50)')'                 '
     +,' is converted to KML, output is gmap_place_names.kml'
        write(6,'(a17,a46)')'    -stat        '
     +,' Station locations given in STATION?.HYP files'
        write(6,'(a17,a46)')'                 '
     +,' is converted to KML, output is gmapstat.kml    '
        write(6,'(a17,a46)')'                 '
     +,' To change color/scale/icon edit gmapstat.kml   '
        write(6,'(a17,a46)')'                 '
     +,' and change the content of Style Id=stat        '
        write(6,'(a17,a46)')'    -poly        '
     +,' SEISAN polygon files like DAT/SALVADOR.MAP   '
        write(6,'(a17,a48)')'                 '
     +,' is converted to KML, output is gmappoly.kml    '
        write(6,'(a17,a48)')'                 '
     +,' To change color/width edit gmappoly.kml and    '
        write(6,'(a17,a48)')'                 '
     +,' change the content of Style Id=poly            '
        write(6,'(a17,a49)')'    -out_file    '
     +,' Define name of output file (default is gmap.kml)'
        write(6,'(a17,a16)')'    -verbose     '
     +,' Be more verbose'
        write(6,'(a17,a16)')'    -version     '
     +,' Seisan version '
        write(6,*)' '
        write(6,*)' Scale:'
        write(6,*)' The scale of the icons is set by the SEISAN.DEF '
        write(6,*)' parameters GMAP_ICON_MSIZE, GMAP_ICON_XSIZE and '
        write(6,*)' GMAP_ICON_YSIZE, see the manual for details.'
        write(6,*)' '
        write(6,*)' Color:'
        write(6,*)' Color and opacity (alpha) values are expressed in '
	write(6,*)' hexadecimal notation. The range of values for any '
	write(6,*)' one color is 0 to 255 (00 to ff). For alpha, 00 is'
	write(6,*)' fully transparent and ff is fully opaque. The order'
	write(6,*)' of expression is aabbggrr, ',
     &           'where aa=alpha (00 to ff);'
	write(6,*)' bb=blue (00 to ff); gg=green (00 to ff); '
        write(6,*)' rr=red (00 to ff).For example, if you want to apply'
	write(6,*)' a blue color with 50 percent opacity to an overlay,'
	write(6,*)' you would specify the following: '
	write(6,*)' <color>7fff0000</color>, ',
     &          'where alpha=0x7f, blue=0xff,'
	write(6,*)' green=0x00, and red=0x00. See also:'
        write(6,*)' http://code.google.com/apis/kml/'
	write(6,*)' documentation/kml_tags_21.html#color '
        write(6,*)' '
        write(6,*)' Examples:'
        write(6,*)'           gmap -input_file collect.out -title EQ'
        write(6,*)'           gmap -color blue -nodata -errorellipse'
        write(6,*)'           gmap -timespan -color 7eee00ee'
        write(6,*)'           gmap -out_file eq.kml -GeoJSON'
c       write(6,666)
c    +'            echo ','select.out\nDK events\n',
c    +' | gmap -out_file dk.kml'
        write(6,*)' '
      goto 999
      endif
      enddo
      endif

c 666 FORMAT(a17,'"',a23,'"',a24,1x)

      if(verbose) write(6,*)' GMAP: narg=',narg
      if(verbose) write(6,*)' GMAP: errorellipse',errorellipse
      if(verbose) write(6,*)' GMAP: errorellipse',errorellipse

 101  continue
      if(inputfile)then
        write(6,*)' INPUT FILE NAME: ',file
      else
        write(6,*)' INPUT FILE NAME'
        read(5,'(a)') file
      endif

c
c   open and check if file available
c
            call sei open(old$,                  ! Open an old file.
     &                    ' ',                   ! Prompt file name (n/a).
     &                    file,                  ! File name
     &                    read01,                ! Read unit #1
     &                    b_old,                 ! Already exists? (n/a).
     &                    code)                  ! Returned condition.
c
      if(.not.b_old) goto 101     ! try again
c
c   check type
c
      call nortype(read01,compact)
      if(verbose.and.compact) write(6,*)' GMAP: Input file is compact'
c if input file is compact copy it to gmap.tmp with extra empty line for subroutine INDATA
      if(compact) then
        call sei open(unknown$+warn$,        ! Open a unknown status file.
     &                    ' ',               ! Prompt file name (n/a).
     &                    'gmap.tmp',        ! File name
     &                    write01,            ! Read unit #1
     &                    b_old,             ! Already exists? (n/a).
     &                    code)              ! Returned condition.
 501    read(read01,'(a)',end=509) data(1)
        write(write01,'(a)') data(1)
        write(write01,'(a)')' '
	goto 501
 509    continue
	close(read01)
	rewind write01
	read01=write01
      endif
 
      if(verbose) 
     +write(6,*)' GMAP: The title must contain maximum 80 characters!'
      if(usetitle)then
        write(6,*)' Title: ',title
      else
        write(6,*)' Title:'
        read(5,'(a)') title
      endif

      if(verbose) 
     +write(6,*)' GMAP: open out_file :',out_file
      open(2,file=out_file,status='unknown')

      write(2,'(a38,1x)')'<?xml version="1.0" encoding="UTF-8"?>'
      write(2,'(a45,1x)')'<kml xmlns="http://earth.google.com/kml/2.1">'
      write(2,'(a10,1x)')'<Document>'
      write(2,'(a23,1x)')'<name><![CDATA['
      if(verbose) 
     +write(6,*)' GMAP: Change special characteres in Title to KML'
      do i=1,80
        if(title(i:i).eq.'"')then
          write(2,'(a,$)')"&#34;"
        elseif(title(i:i).eq."&")then
          write(2,'(a,$)')"&#38;"
        elseif(title(i:i).eq."'")then
          write(2,'(a,$)')"&#39;"
        elseif(title(i:i).eq."<")then
          write(2,'(a,$)')"&#60;"
        elseif(title(i:i).eq.">")then
          write(2,'(a,$)')"&#62;"
        else
          write(2,'(a,$)')title(i:i)
        endif
      enddo
      write(2,'(a21,1x)')']]></name>'
      write(2,'(a18,1x)')'<open>1</open>'
c
c Earthquakes :
c
      visible=.TRUE.
      sdata=.FALSE.
      write(2,'(1x)')
      write(2,'(a12,1x)')'<Folder>'
      write(2,'(a30,1x)')'<name>Earthquakes</name>'
      if(verbose)
     +write(6,*)' GMAP: construct folder with earthquakes'
      if(verbose) 
     +write(6,*)
     +' GMAP: check number of : eq, exp, pexp and other events'
  10  continue
      CALL INDATA(read01,NSTAT,NPHASE,NHEAD,NRECORD,TYPE,EXP,DATA,ID)

      if(nrecord.eq.0) goto 19
      if(EXP.eq.'Q')neq=neq+1
      if(EXP.eq.' ')neq=neq+1
      if(EXP.eq.'E')nexp=nexp+1
      if(EXP.eq.'P')npexp=npexp+1
      if(EXP.ne.'Q'.AND.EXP.ne.' '.AND.EXP.ne.'E'.AND.EXP.ne.'P')
     +  nother=nother+1

      if(EXP.eq.'Q'.OR.EXP.eq.' ') then
       CALL wkml(2,data,max_data,NRECORD,url,visible,color,sdata,ttag,
     +errorellipse,MSIZE,XSIZE,YSIZE,ellipsecolor,ellipsewidth,
     +highaccuracy)
      endif
      goto 10
  19  continue
      write(2,'(a13,1x)')'</Folder>'
      write(2,'(1x)')

c
c Earthquakes and data
c
      if(datafolder)then
      if(verbose)
     +write(6,*)' GMAP: construct folder with earthquakes and data'
      rewind read01
      visible=.FALSE.
      sdata=.TRUE.
      write(2,'(a12,1x)')'<Folder>'
      write(2,'(a39,1x)')
     +'<name>Earthquakes and data</name>'
   21 continue
      CALL INDATA(read01,NSTAT,NPHASE,NHEAD,NRECORD,TYPE,EXP,DATA,ID)
      if(nrecord.eq.0) goto 29
      if(EXP.eq.'Q'.OR.EXP.eq.' ') then
       CALL wkml(2,data,max_data,NRECORD,url,visible,color,sdata,ttag,
     +errorellipse,MSIZE,XSIZE,YSIZE,ellipsecolor,ellipsewidth,
     +highaccuracy)
      endif
      goto 21
  29  continue
      write(2,'(a13,1x)')'</Folder>'
      write(2,'(1x)')
      endif

c
c Explosions
c
      if(nexp.gt.0)then
      if(verbose)
     +write(6,*)' GMAP: construct folder with explosions'
      visible=.FALSE.
      sdata=.FALSE.
      rewind read01
      visible=.FALSE.
      write(2,'(a12,1x)')'<Folder>'
      write(2,'(a29,1x)')
     +'<name>Explosions</name>'
   31 continue
      CALL INDATA(read01,NSTAT,NPHASE,NHEAD,NRECORD,TYPE,EXP,DATA,ID)
      if(nrecord.eq.0) goto 39
      if(EXP.eq.'E') then
       CALL wkml(2,data,max_data,NRECORD,urle,visible,colore,sdata,ttag,
     +errorellipse,MSIZE,XSIZE,YSIZE,ellipsecolor,ellipsewidth,
     +highaccuracy)
      endif
      goto 31
  39  continue
      write(2,'(a13,1x)')'</Folder>'
      write(2,'(1x)')

c
c Explosions and data
c
      if(datafolder)then
      if(verbose)
     +write(6,*)' GMAP: construct folder with explosions and data'
      rewind read01
      visible=.FALSE.
      sdata=.TRUE.
      write(2,'(a12,1x)')'<Folder>'
      write(2,'(a38,1x)')
     +'<name>Explosions and data</name>'
   41 continue
      CALL INDATA(read01,NSTAT,NPHASE,NHEAD,NRECORD,TYPE,EXP,DATA,ID)
      if(nrecord.eq.0) goto 49
      if(EXP.eq.'E') then
       CALL wkml(2,data,max_data,NRECORD,urle,visible,colore,sdata,ttag,
     +errorellipse,MSIZE,XSIZE,YSIZE,ellipsecolor,ellipsewidth,
     +highaccuracy)
      endif
      goto 41
  49  continue
      write(2,'(a13,1x)')'</Folder>'
      write(2,'(1x)')
      endif
      endif

c
c Probable Explosions
c
      visible=.FALSE.
      sdata=.FALSE.
      if(npexp.gt.0)then
      if(verbose)
     +write(6,*)' GMAP: construct folder with prob. explosions'
        rewind read01
      write(2,'(a12,1x)')'<Folder>'
      write(2,'(a38,1x)')
     +'<name>Probable Explosions</name>'
   51 continue
      CALL INDATA(read01,NSTAT,NPHASE,NHEAD,NRECORD,TYPE,EXP,DATA,ID)
      if(nrecord.eq.0) goto 59
      if(EXP.eq.'P') then
        CALL wkml(2,data,max_data,
     +NRECORD,urlpe,visible,colorpe,sdata,ttag,
     +errorellipse,MSIZE,XSIZE,YSIZE,ellipsecolor,ellipsewidth,
     +highaccuracy)
      endif
      goto 51
  59  continue
      write(2,'(a13,1x)')'</Folder>'
      write(2,'(1x)')

c
c Probable Explosions and data
c
      if(datafolder)then
      if(verbose)
     +write(6,*)' GMAP: construct folder with prob. explosions and data'
      rewind read01
      visible=.FALSE.
      sdata=.TRUE.
      write(2,'(a12,1x)')'<Folder>'
      write(2,'(a47,1x)')
     +'<name>Probable Explosions and data</name>'
   61 continue
      CALL INDATA(read01,NSTAT,NPHASE,NHEAD,NRECORD,TYPE,EXP,DATA,ID)
      if(nrecord.eq.0) goto 69
      if(EXP.eq.'P') then
        CALL wkml(2,data,max_data,
     +NRECORD,urlpe,visible,colorpe,sdata,ttag,
     +errorellipse,MSIZE,XSIZE,YSIZE,ellipsecolor,ellipsewidth,
     +highaccuracy)
      endif
      goto 61
  69  continue
      write(2,'(a13,1x)')'</Folder>'
      write(2,'(1x)')
      endif
      endif

c
c Other events
c
      if(nother.gt.0)then
      if(verbose)
     +write(6,*)' GMAP: construct folder with other events'
      rewind read01 
      visible=.FALSE.
      sdata=.FALSE.
      write(2,'(a12,1x)')'<Folder>'
      write(2,'(a31,1x)')
     +'<name>Other events</name>'
   71 continue
      CALL INDATA(read01,NSTAT,NPHASE,NHEAD,NRECORD,TYPE,EXP,DATA,ID)
      if(nrecord.eq.0) goto 79
      if(EXP.ne.'Q'.and.EXP.ne.' '.and.EXP.ne.'E'.and.EXP.ne.'P') then
       CALL wkml(2,data,max_data,NRECORD,urlo,visible,coloro,sdata,ttag,
     +errorellipse,MSIZE,XSIZE,YSIZE,ellipsecolor,ellipsewidth,
     +highaccuracy)
      endif
      goto 71
   79 continue
      write(2,'(a13,1x)')'</Folder>'
      write(2,'(1x)')

c
c Other events and data
c
      if(datafolder)then
         rewind read01
      if(verbose)
     +write(6,*)' GMAP: construct folder with other events and data'
      visible=.FALSE.
      sdata=.TRUE.
      write(2,'(a16,1x)')'<Folder>'
      write(2,'(a40,1x)')
     +'<name>Other events and data</name>'
   81 continue
      CALL INDATA(read01,NSTAT,NPHASE,NHEAD,NRECORD,TYPE,EXP,DATA,ID)
      if(nrecord.eq.0) goto 89
      if(EXP.ne.'Q'.and.EXP.ne.' '.and.EXP.ne.'E'.and.EXP.ne.'P') then
       CALL wkml(2,data,max_data,NRECORD,urlo,visible,coloro,sdata,ttag,
     +errorellipse,MSIZE,XSIZE,YSIZE,ellipsecolor,ellipsewidth,
     +highaccuracy)
      endif
      goto 81
   89 continue
      write(2,'(a13,1x)')'</Folder>'
c     write(2,'(1x)')
      endif
      endif


c
c GeoJSON format all types
c
      if(geojson)then
      rewind read01 
      visible=.FALSE.
      sdata=.FALSE.
        open(3,file=out_file_geojson,status='unknown')
        write(3,'(a19,1x)')'var earthQuake = { '
        write(3,'(a32,1x)')'    "type": "FeatureCollection", '
        write(3,'(a18,1x)')'    "features": [ '
      i=0
      if(verbose) write(6,*)' GMAP: geojson file output'
   91 continue
      CALL INDATA(read01,NSTAT,NPHASE,NHEAD,NRECORD,TYPE,EXP,DATA,ID)
      if(nrecord.eq.0) goto 99
      i=i+1
      if(i.NE.1) write(3,'(a9,1x)')'},'
      write(3,'(a9,1x)')'{'
c     write(6,*)' GMAP: geojson file output',i,NRECORD
c     if(EXP.ne.'Q'.and.EXP.ne.' '.and.EXP.ne.'E'.and.EXP.ne.'P') then
       CALL wGeoJSON(3,data,max_data,NRECORD,
     +urlo,visible,coloro,sdata,ttag,
     +errorellipse,MSIZE,XSIZE,YSIZE,ellipsecolor,ellipsewidth,
     +highaccuracy)
c     endif
      goto 91
   99 continue
c     write(3,'(a9,1x)')'}'
c     write(3,'(a5,1x)')']'
c     write(3,'(a2,1x)')'};'
      write(3,'(8x,1a,/,4x,1a,/,a2)') '}', ']', '};'
      endif
c199  FORMAT(a10,',')
      close(3)  


      close(read01)  

c
c info and links
c

      if(n_gmap_append_kml.ge.1)then
	do i=1,n_gmap_append_kml
          write(2,'(a80,1x)')gmap_append_kml(i)
	enddo
      endif
      write(2,'(a13,1x)')'</Document>'
      write(2,'(a6,1x)')'</kml>'
      close(2)  

      write(6,*)"Number of Earthquakes         :",neq
      write(6,*)"          Explosions          :",nexp
      write(6,*)"          Probable Explosions :",npexp
      write(6,*)"          Other events        :",nother
      write(6,*)"Output file is ",out_file

 999  continue

      end


c
c---------------------------------------------------------------
c
      subroutine wGeoJSON(io,text,max_data,nr,url,
     +visible,color,sdata,ttag,
     +errorellipse,MSIZE,XSIZE,YSIZE,ellipsecolor,ellipsewidth,
     +highaccuracy)
c
      character*80 text(max_data)
      character*80 url
      character*120 dummy
      character*120 dummy1
      character*120 dummy2
      character*120 dummy3
      character*120 gmap_trim
      character*8 color
      character*7 magnitude
      character*9  eqlatc
      character*10 eqlonc
      character*10 eqdepc
      integer io,i,j
      real mag
      real eqlon,eqlat,eqdep,ery,erx,erz,cvxy,cvxz,cvyz
      real MSIZE,XSIZE,YSIZE
      logical visible
      logical sdata
      logical ttag                      ! timetag / timespan
      logical highaccuracy
      logical errorellipse  ! kml file will include error ellipse
      character*8 ellipsecolor
      real ellipsewidth

c
      magnitude="       "
c
c get lat long of eq from header line in s-file
      read(text(1)(24:30),'(f7.3)') eqlat
      read(text(1)(31:38),'(f8.3)') eqlon
      read(text(1)(24:30),'(a7)') eqlatc
      read(text(1)(31:38),'(a8)') eqlonc
      read(text(1)(39:43),'(a5)') eqdepc
c
c   check for high accuracy
      if(highaccuracy) then
        i=1
        do while ( i < nr )
          if(text(i)(80:80).EQ."H")then
            read(text(i)(24:32),'(f9.5)') eqlat
            read(text(i)(34:43),'(f10.5)') eqlon
            read(text(i)(24:32),'(a9)') eqlatc
            read(text(i)(34:43),'(a10)') eqlonc
            i=nr
          endif
          i=i+1
        enddo
      endif
c
c remove chars like backspace
      do i=1,nr
        do j=1,80
          if(ichar(text(i)(j:j)).lt.32) text(i)(j:j)=" "
        enddo
      enddo

c get magnitude
      if(text(1)(73:79).ne.'       ')magnitude=text(1)(73:79)
      if(text(1)(65:71).ne.'       ')magnitude=text(1)(65:71)
      if(text(1)(57:63).ne.'       ')magnitude=text(1)(57:63)

      if(text(1)(7:7).eq.' ')text(1)(7:7)='0'
      if(text(1)(9:9).eq.' ')text(1)(9:9)='0'
      if(text(1)(17:17).eq.' ')text(1)(17:17)='0'
      if(text(1)(28:28).eq.' ')text(1)(28:28)='0'
      if(text(1)(29:29).eq.' ')text(1)(29:29)='0'
      if(text(1)(30:30).eq.' ')text(1)(30:30)='0'
      if(text(1)(36:36).eq.' ')text(1)(36:36)='0'
      if(text(1)(37:37).eq.' ')text(1)(37:37)='0'
      if(text(1)(38:38).eq.' ')text(1)(38:38)='0'
c mag is size of circle
      if(magnitude.ne."       ")then
       mag=ichar(magnitude(1:1))-48.0
       mag=mag+0.1*(ichar(magnitude(3:3))-48.0)
      else
       mag=MSIZE
      endif
      if(mag.lt.MSIZE)mag=MSIZE
      mag=XSIZE*mag**YSIZE
      if(text(1)(23:23).ne."Q".AND.text(1)(23:23).ne." ")mag=mag*2.0
c
      write(io,'(a26,1x)')'   "geometry": { '
      write(io,'(a33,1x)')'    "type": "Point", '
      write(io,'(a33,1x)')'    "coordinates": [ '
      write(dummy,115)eqlonc
      write(io,*) "                   ",trim(gmap_trim(dummy))
      write(dummy,116)eqlatc
      write(io,*) "                   ",trim(gmap_trim(dummy))
      write(io,'(a12,1x)')']'
      write(dummy1,115)eqlatc
      write(dummy2,115)eqlonc
      write(dummy3,115)eqdepc
c     write(io,*)'    "coordinates": [ ',trim(gmap_trim(dummy1)),
c    +', ', trim(gmap_trim(dummy2)),', ',trim(gmap_trim(dummy3)),']'
      write(io,'(a11,1x)')'},'
      write(io,'(a28,1x)')'   "type": "Feature", '
      write(io,'(a25,1x)')'   "properties": { '
      write(io,
     +'(a27,a4,a1,a2,a1,a2,a1,a2,a1,a2,a1,a4,a3,1x)')
     +'"timeofevent": "',
     +text(1)(2:5),'-',text(1)(7:8),'-',text(1)(9:10),'T',
     +text(1)(12:13),':',text(1)(14:15),':',text(1)(17:20),
     +'Z",'
      write(io,117)magnitude(1:3)
      write(io,'(a11,1x)')'},'
      write(io,'(a15,1x)')' "id": 1'

 115  FORMAT(a10,',')
 116  FORMAT(a9)
 117  FORMAT('             "magnitude": "',a3,'"')

      return
      end

c
