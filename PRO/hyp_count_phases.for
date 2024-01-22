c
c   this program counts the phases used for locations as they appear in
c   the print.out file. The phases counted are local phases as identified 
c   and not the phases given as input. zero output weight phases are
c   not counted. 
c   in addtion the sum of the weights used for the phases are counted
c   only phases for up to 9 layers are counted
c
c   jh feb 2021

      implicit none
      character*80 text


c
c   counters for phases
c
      integer pg,pn2,pn3,pn4,pn5,pn6,pn7,pn8,pn9
      integer sg,sn2,sn3,sn4,sn5,sn6,sn7,sn8,sn9

      real xpg,xpn2,xpn3,xpn4,xpn5,xpn6,xpn7,xpn8,xpn9
      real xsg,xsn2,xsn3,xsn4,xsn5,xsn6,xsn7,xsn8,xsn9

      real w                 ! weight out
      character*3 pha        ! phase name

      open(2,file='hyp_count_phases.out',status='unknown')

      pg=0
      pn2=0
      pn3=0
      pn4=0
      pn5=0
      pn6=0
      pn7=0
      pn8=0
      pn9=0

      sg=0
      sn2=0
      sn3=0
      sn4=0
      sn5=0
      sn6=0
      sn7=0
      sn8=0
      sn9=0

      xpg=0
      xpn2=0
      xpn3=0
      xpn4=0
      xpn5=0
      xpn6=0
      xpn7=0
      xpn8=0
      xpn9=0

      xsg=0
      xsn2=0
      xsn3=0
      xsn4=0
      xsn5=0
      xsn6=0
      xsn7=0
      xsn8=0
      xsn9=0

      open(1,file='print.out',status='old')

 1    continue
      read(1,'(a)',end=2) text
c
c   weight out
c
      w=0.0
      read(text(73:76),*,err=1,end=1) w

      pha=text(34:36)
      if(pha.eq.'PG '.and.w.gt.0.0) pg=pg+1
      if(pha.eq.'PN2'.and.w.gt.0.0) pn2=pn2+1
      if(pha.eq.'PN3'.and.w.gt.0.0) pn3=pn3+1
      if(pha.eq.'PN4'.and.w.gt.0.0) pn4=pn4+1
      if(pha.eq.'PN5'.and.w.gt.0.0) pn5=pn5+1
      if(pha.eq.'PN6'.and.w.gt.0.0) pn6=pn6+1
      if(pha.eq.'PN7'.and.w.gt.0.0) pn7=pn7+1
      if(pha.eq.'PN8'.and.w.gt.0.0) pn8=pn8+1
      if(pha.eq.'PN9'.and.w.gt.0.0) pn9=pn9+1

      if(pha.eq.'SG '.and.w.gt.0.0) sg=sg+1
      if(pha.eq.'SN2'.and.w.gt.0.0) sn2=sn2+1
      if(pha.eq.'SN3'.and.w.gt.0.0) sn3=sn3+1
      if(pha.eq.'SN4'.and.w.gt.0.0) sn4=sn4+1
      if(pha.eq.'SN5'.and.w.gt.0.0) sn5=sn5+1
      if(pha.eq.'SN6'.and.w.gt.0.0) sn6=sn6+1
      if(pha.eq.'SN7'.and.w.gt.0.0) sn7=sn7+1
      if(pha.eq.'SN8'.and.w.gt.0.0) sn8=sn8+1
      if(pha.eq.'SN9'.and.w.gt.0.0) sn9=sn9+1


      if(pha.eq.'PG '.and.w.gt.0.0) xpg=xpg+w
      if(pha.eq.'PN2'.and.w.gt.0.0) xpn2=xpn2+w
      if(pha.eq.'PN3'.and.w.gt.0.0) xpn3=xpn3+w
      if(pha.eq.'PN4'.and.w.gt.0.0) xpn4=xpn4+w
      if(pha.eq.'PN5'.and.w.gt.0.0) xpn5=xpn5+w
      if(pha.eq.'PN6'.and.w.gt.0.0) xpn6=xpn6+w
      if(pha.eq.'PN7'.and.w.gt.0.0) xpn7=xpn7+w
      if(pha.eq.'PN8'.and.w.gt.0.0) xpn8=xpn8+w
      if(pha.eq.'PN9'.and.w.gt.0.0) xpn9=xpn9+w

      if(pha.eq.'SG '.and.w.gt.0.0) xsg=xsg+w
      if(pha.eq.'SN2'.and.w.gt.0.0) xsn2=xsn2+w
      if(pha.eq.'SN3'.and.w.gt.0.0) xsn3=xsn3+w
      if(pha.eq.'SN4'.and.w.gt.0.0) xsn4=xsn4+w
      if(pha.eq.'SN5'.and.w.gt.0.0) xsn5=xsn5+w
      if(pha.eq.'SN6'.and.w.gt.0.0) xsn6=xsn6+w
      if(pha.eq.'SN7'.and.w.gt.0.0) xsn7=xsn7+w
      if(pha.eq.'SN8'.and.w.gt.0.0) xsn8=xsn8+w
      if(pha.eq.'SN9'.and.w.gt.0.0) xsn9=sn9+w

  
      goto 1


 2    continue

      write(6,*)'Number of phases'
      write(6,'(a)')'    pg   pn2   pn3   pn4'//
     *'   pn5   pn6   pn7   pn8   pn9'
      write(6,'(12i6)')pg,pn2,pn3,pn4,pn5,pn6,pn7,pn8,pn9

      write(6,'(a)')'    sg   sn2   sn3   sn4'//
     *'   sn5   sn6   sn7   sn8   sn9'
      write(6,'(12i6)')sg,sn2,sn3,sn4,sn5,sn6,sn7,sn8,sn9
      write(6,*)
      write(6,*)'Sum of phase weights'
      write(6,'(a)')'    pg   pn2   pn3   pn4'//
     *'   pn5   pn6   pn7   pn8   pn9'   
      write(6,'(12f6.1)')
     *xpg,xpn2,xpn3,xpn4,xpn5,xpn6,xpn7,xpn8,xpn9
      write(6,'(a)')'    sg   sn2   sn3   sn4'//
     *'   sn5   sn6   sn7   sn8   sn9'
      write(6,'(12f6.1)')
     *xsg,xsn2,xsn3,xsn4,xsn5,xsn6,xsn7,xsn8,xsn9


      write(2,*)'Number of phases'

      write(2,'(a)')'    pg   pn2   pn3   pn4'//
     *'   pn5   pn6   pn7   pn8   pn9'
      write(2,'(12i6)')pg,pn2,pn3,pn4,pn5,pn6,pn7,pn8,pn9

      write(2,'(a)')'    sg   sn2   sn3   sn4'//
     *'   sn5   sn6   sn7   sn8   sn9'
      write(2,'(12i6)')sg,sn2,sn3,sn4,sn5,sn6,sn7,sn8,sn9
      write(2,*)
      write(2,*)'Sum of phase weights'
      write(2,'(a)')'    pg   pn2   pn3   pn4'//
     *'   pn5   pn6   pn7   pn8   pn9'   
      write(2,'(12f6.1)')
     *xpg,xpn2,xpn3,xpn4,xpn5,xpn6,xpn7,xpn8,xpn9
      write(2,'(a)')'    sg   sn2   sn3   sn4'//
     *'   sn5   sn6   sn7   sn8   sn9'
      write(2,'(12f6.1)')
     *xsg,xsn2,xsn3,xsn4,xsn5,xsn6,xsn7,xsn8,xsn9

      close(1) 
      close(2)

      write(6,*)'Output file is hyp_count_phases.out'
      stop
      end
      
