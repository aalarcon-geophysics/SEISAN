c
c  check and delete netdet jobs
c

      implicit none
    
      character*120 text
      character*80 arg(5)                ! arguments to  program
      character*120 cronjobs(20)         ! netdet cron jobs
      integer cron_number(20)            ! process number
      logical netdet_run                 ! true if netdet cor. par file
      integer seiclen
      integer i,k,m

c
c   updates
c

      text='ps ax | grep -v grep | grep netdet > netdet.run'

      call systemc(text,seiclen(text))
c
c   read file with netdet jobs
c
      open(1,file='netdet.run',status='old')
      write(6,*)
      write(6,'(a)') '**********netdet jobs running**********'
      write(6,*)
 
      i=1  
 1    continue
      read(1,'(a)',end=2) cronjobs(i)
      k=index(cronjobs(i),'netdet realtime')
      if(k.gt.0) then
         read(cronjobs(i)(1:7),*) cron_number(i)
         m=seiclen(cronjobs(i))
         cronjobs(i)=cronjobs(i)(k:m)
         cronjobs(i)(m+1:120)=' '
         write(6,'(i2,2x,a,i7)') i,
     *   cronjobs(i)(1:seiclen(cronjobs(i))),cron_number(i)
         i=i+1
      endif
      goto 1
 2    continue
      i=i-1
      k=i
 
      if(k.eq.0) then
         write(6,*)
         write(6,'(a)') '********** no netdet jobs running**********'
         write(6,*)
         stop
      endif  

 3    continue
      write(6,*)
      write(6,'(a)') 'Delete job, enter which one or enter to exit'
      read(5,'(a)') text
      if(text.ne.' ') then
         read(text,*) i
         if(i.le.0.or.i.gt.k) then
            write(6,*)'Wrong number'
            goto 3
         endif
         write(text,'(a,i7)')'kill ',cron_number(i)
         write(6,'(a)') text(1:16)
         call systemc(text,seiclen(text))
      endif


      stop
      end
