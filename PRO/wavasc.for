c
c  program to convert any seisan waveform file to a 2 column ASCII file.
c  one file will be made per channel as well as one file for all the 
c  converted data. The start of the file looks as follows:
c
c COCO  BH E 10 II 2007  928 1345  31.440   42953  40.000
c    31.440     1874.00
c    31.465     1787.00
c    31.490     2159.00
c    31.515     3002.00
c
c where the header has station, component, location, network, time, 
c number fo samples and sample rate. The following times are referred to
c the minute in the header.
c
c   the channel output filename is like
c 
c   2007-09-28-1345-31A.COCO_BH_E
c
c   where a stands for ASCII
c
c   The input can be an s-file where all waveform files in s-file
c   will be used or it can be a waveform file or a filenr.lis
c   of waveform files.
c



      implicit none
      include 'seidim.inc'                    ! for giving dimensions
      include 'libsei.inc'                    ! for all seisan subroutines
      include 'waveform.inc'                  ! waveform data
      include 'seisan.inc'                    ! seisan general parameters
      integer nstat,nphase,nhead,nrecord,id   ! for reading s-file
      character*1 type,exp                    ! -----------------
      character*80 sfilname                   ! s-file name
      character*80 data(max_data)             ! one s-file
      integer npresent                        ! number of wav files present
      character*80 text                       ! general text string
      character*80 question                   ! question
      character*80 outfile                    ! output file name
      real time                               ! time in sec after first sample
      integer k,i,in
c
c  use a waveform file name directly or the waveform files in an s-file
c

c
c  initialize 
c
      call wav_init
      call wav_mem_init
      call get_seisan_def
     
      open(2,file='wavasc.out',status='unknown')
c
c   get s-file name or wav name
c
      write(6,*)' S-file name, enter for wav-name directly'
      read(5,'(a)') sfilname
      if(sfilname.ne.' ') then
c
c   open s-file
c
         open(1,file=sfilname,status='old')
c
c  read s-file
c
         call indata
     *   (1,nstat,nphase,nhead,nrecord,type,exp,data,id)
         close(1)

c
c   get waveform file names, could be several
c
         call auto_tr(data,nhead,nrecord,wav_nfiles,wav_filename)

      else
c
c   get file name directly, can be one or many
c
        in=0
        question=' File name, # or filenr.lis for all'
        call filename(question,wav_filename(1))
        if(wav_filename(1)(1:10).eq.'filenr.lis'.or.
     *  wav_filename(1)(1:10).eq.'FILENR.LIS') then
           open(8,file='filenr.lis',status='old',err=20)
           goto 21
 20        continue
           write(6,*)' No filenr.lis'
           stop
 21        continue
           in=1
        endif
        wav_nfiles=1     ! there is only one file at the time in this case
      endif
c
c   file loop if many files
c
 1000 continue
      if(in.eq.1) then
         read(8,'(7x,a)')wav_filename(1) 
         if(wav_filename(1)(1:4).eq.'    ') goto 2000 ! the end
      endif
c
c   find how many waveform files are present,
c   replace the original names without path, with the present files
c   full path like
c
       npresent=0                      ! no files initially
       do i=1,wav_nfiles
          call  get_full_wav_name(wav_filename(i),text)            
          if(text.ne.' ') then
             npresent=npresent+1
             wav_filename(npresent)=text
          endif
      enddo
c
c   print how many files were found
c
      wav_nfiles=npresent
      write(6,*)' Number of wav-files present', wav_nfiles

c
c   terminate if no waveform files
c
      if(wav_nfiles.eq.0) stop

c
      wav_nchan=0
c
c   loop to read all headers of all files
c
      do i=1,wav_nfiles
         write(6,'(1x,a)') wav_filename(i)
c
c   read all headers of file i
c
         call read_wav_header(i)
c
c   output possible errors
c
         if(wav_error_message.ne.' ') write(6,'(1x,a)') 
     *   wav_error_message
c
c   write out the format
c
         write(6,'(1x,a,a)') 'Format ',wav_file_format(i)

      enddo
c
      write(6,*)' Total number of channels available:',wav_nchan
c
c   terminate if no channels
c
      if(wav_nchan.eq.0) stop
c
c   write some input info for each channnel on screen
c
      write(6,'(a,a)')
     *'CHA STAT  COMP LO NT  YEAR MO DA HR MI    SEC   NSAMP ',
     *'    RATE    DELAY DURATION'
      do i=1,wav_nchan
         write(6,'(i3,1x,a,1x,a,1x,a,1x,a,1x,i5,4i3,f7.3,i8,3f9.3)')
     *   i, wav_stat(i),wav_comp(i),wav_location(i),wav_network(i),
     *   wav_year(i),wav_month(i),wav_day(i),wav_hour(i),wav_min(i),
     *   wav_sec(i),wav_nsamp(i),wav_rate(i),wav_delay(i),
     *   wav_duration(i)
      enddo
c
c
c-----------------------------------------------------------
c  Data from all channels are now read and written out, both
c  in one file and to individual channel files
c-----------------------------------------------------------
c
      do i=1,wav_nchan
c
c   read  channel
c
         call wav_read_channel(i)
c
c   make filename
c
         write(outfile,'(i4,a,i2,a,i2,a,2i2,a,i2,a,a,a)') 
     *   wav_year(i),'-',wav_month(i),'-',wav_day(i),'-',wav_hour(i),
     *   wav_min(i),'-',
     *   int(wav_sec(i)),'A.',wav_stat(i),wav_comp(i)
         do k=21,29
           if(outfile(k:k).eq.' ') outfile(k:k)='_'
         enddo
         do k=1,20
           if(outfile(k:k).eq.' ') outfile(k:k)='0'
         enddo
         write(6,'(a,a)') ' Writing ASCII file: ', outfile

         open(3,file=outfile,status='unknown')

c   write in ASCII format header
c
         write(2,'(4(a,1x),i4,1x,2i2,1x,2i2,1x,f7.3,i8,f8.3)')
     *   wav_stat(i),wav_comp(i),wav_location(i),wav_network(i),
     *   wav_year(i),wav_month(i),wav_day(i),wav_hour(i),wav_min(i),
     *   wav_sec(i),wav_nsamp(i),wav_rate(i)

         write(3,'(4(a,1x),i4,1x,2i2,1x,2i2,1x,f7.3,i8,f8.3)')
     *   wav_stat(i),wav_comp(i),wav_location(i),wav_network(i),
     *   wav_year(i),wav_month(i),wav_day(i),wav_hour(i),wav_min(i),
     *   wav_sec(i),wav_nsamp(i),wav_rate(i)
c
c   write samples
c
         do k=1,wav_nsamp(i) 
            time=wav_sec(i)+(k-1)/wav_rate(i) 
            write(2,'(f10.3,g15.7)') time,signal1(k) 
            write(3,'(f10.3,g15.7)') time,signal1(k) 
         enddo
      enddo
c
c   if a list of files, go back to read next
c
      close(3)
      if(in.eq.1) goto 1000

c
c   the end, no more files
c
 2000 continue
  
c
      close(2)
      write(6,*)

      write(6,'(a)') 
     *' Output in file with all converted data is wavasc.out'



      stop
      end

