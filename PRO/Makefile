#################################################
#
# SEISAN 13.03
#

#   autosig has not been updated to new wa filter and will not work correctly
#   arcsei has not been upgraded so not linking
#
# Makefile for SEISAN PRO directory to compile SEISAN libraries  
# and programs. The SEISARCH environmental variable, which
# MUST BE: solaris, linux32, linux64, windows, macosx or macosxppc
# is used to produce 
# platform dependant executables. You may need to modify
# some of the variables used in the Makefile to adjust
# the Makefile to your system. To compile a specific 
# program use for example 'make solaris/mulplt' to compile
# mulplt for solaris. The output file created will be
# solaris/mulplt.
#
##################################################
#
# changes:
#
# ...
# jan 27 2011 jh: crisis99, hypinv, rsasei, evanoi now compiles
#                      remove bgisei, silmseed
# jan 29 2011 lo: cleared changes, cleaned file and prepared for
#                 windows gfortran 
# /c/ to c: , then it works within and without gw
# feb 03 2011 jh: add k2sei,arcsei,mag2
# feb 11 2011 jh: put slick back in
# feb 18 2011 jh: remove stalog, add magstat, remove arcsei from windows list
# feb 23 2011 jh hash_foc to hash_seisan do similar to fpfit_seisan
# feb 24 2011 lo changed gfortran64 to gfortran, and linux to g77
# jun 03 2011 jh added -fbounds-check
# jun 17 2011 jh added sample_read_seed
# aug 5  2011 jh add mscut
# nov 2  2011 jh include libmseed and dislin
# nov 28 2011 jh new program hinor
# dec 13 2011 jh change sample_read_seed to sample_read_write_seed
# dec 23 2011 jh add  tdmt_invc_seisan.c, fkrprog_seisan.for, wvint_seisan.for
# feb 22 2012 jh add selectc
# feb 28 2012 jh add codaq_area
# apr 08 2012 jh norhyp back in
# jun 5  2012 jh add seimeca.c, norcsv in list
# jun 15 2012 jh  arcsei was missing
# jan 2, 2013 jh add gcf2msd_many
# jan 28 2013 jh add afadsei.for
# mar 8  2013 jh add sample_instrument_correction
# apr 8  2013 jh add automag, asso
# may 2  2013 jh add plotml, plotmoment,dels
# jul 2 sep 1  2013 jh add -lgfortran to tdmc_inv_fortran, 
#                remove old risk programs
# oct 14 2013 jh: add plotspec
# jan 24 2014 jh: add cscnor
# feb 18 2014 jh: some clean up
# feb 21 2014 pv: added FreeBSD
# jun 11 2014 pv: cut g77, cut gfortran, add linux32, add linux64
# sep 4  2014 jh: add getstressdrop
# oct 22 2014 jh: add hypinv_seisan, remove hinor
# jul 27 2015 jh: add qstat, remove qsei
# oct 28 2015 jh: add get_arc
# dec 21 2015 tu: add autophase
# mar 16 2016 lo: removed programs that don't compile on mac from general list
# mar 31 2016 pv: removed msfix
# aug 10 2016 jh: add compare_hyp
# dec 4  2016 jh: rename hash_seisan to seisan, rename fpfit to fpfit_org
#                 rename fpt_seisan to fpfit
#                 add sample_udp_message
# jan 6  2017 jh  add plotfoc                
# nov 13 2017 jh  add sample_read_write_s_c.cpp
# nov 27 2017 jh  remove seimeca, add focmec_old,focmec_exe_old
# dec 22 2017 jh  add map
# jan 12 2018 lo  add magmbn
# jan 17 2018 jh  add mapg
# mar  8 2018 jh  ad get_web_locations.c
# may 10 2018 lo  cleanup
# jun  6 2018 lo  small fix on debug flags
# nov 16 2018 jh  add codaq_polygon
# dec 11 2018 pv  add iloc_seisan
# feb 27 2019 jh  add remove_tab
# 2019-04-09  pv  add iloc_loop
# 2019-04-09  pv  removed selectc
# 2019-04-22  jh  added nor2new
# 2019-04-23  jh  change fbounds-check to -fcheck=all, add norm_q,mag_order
# 2019-06-07  jh  add change_resp_name
# 2019-11-28  jh  remove checkre, add albnor, gsenor
# 2020-02-14  jh  add hypodd and ph2dat
# 2020-04-19  jh  add wavasc,imsnor
# 2020-06-01  jh  add MErge_s.for, sample_rea_to_rea.for
# 2020-08-27  jh  add velnor.for
# 2020-08-21  jh  hash to hash_seisan, hash is a bash command
# 2020-09-23  lo  change of flags
# 2020-11-03  jh  nor2new changed to nor2nor2
# 2020-11-25  jh  add gsrnor
# 2021-02-19  jh  add hyp_count_phases, eev_get_event
# 2021-02-24  pv  sample_read_write_s_c.cpp is not compiled by make all, see manual.
# 2021-03-24  jh  add magres.for,gridmin.for
# 2021-04-12  lo: add comp_hyp
# 2021-04-14  lo: removed qlg and database2mseed from default compiltion list
# 2021-06-09  lo: added sample_read_write_s_c, not tested on windows
# 2021-12-01  jh: add netnet, netdet_display, netdet_check real time condet
# 2022-04-04  jh: add wav_add.for
# 2022-07-11  jh: add get_arc_channels
# 2023-02-17  jh: add scpnor.for, kappa_sort
# 2023-03-10  jh: add plotmr.for, kappa.for
# 2023-06-30  jh: add backup_sfiles

##################################################
# THE ARCHITECTURE IS EXPECTED TO BE GIVEN BY
# THE ENVIRONMENTAL VARIABLE SEISARCH, WHICH
# MUST BE: solaris, linux32, linux64, windows, macosx or macosxppc
##################################################

##################################################
# PATH TO WHICH OUTPUT FILES ARE WRIITEN,
# THIS CAN BE SET TO $(SEISARCH) IF
# SEVERAL OPERATING SYSTEMS COMPILE
# SOURCE ON SHARED DISKS,
# OBVIOUSLY OUTPATH CAN BE USED TO WRITE
# OBJECTS TO ANY OTHER PLACE
#################################################

#
##  check if a windows platform, else assume linux
#
#
ifeq ($(OS),Windows_NT)
   SEISARCH = windows
   OUTPATH = .
else
   OUTPATH = $(SEISARCH)
endif
OUTPATH = .

debug_flags = -g -fcheck=all
#debug_flags = -g

# following defintions apply to distribution target
distribution: debug_flags =  
######distribution: OUTPATH = .


##################################################
# PATH TO WHICH EXECUTABLES ARE INSTALLED
INSTALL_PRO_PATH = /home/s2000/seismo/pro

##################################################
# LINK TO X11 LIBRARIEs
link_linux64   = -lX11 -L/usr/X11R6/lib
link_linux32   = -lX11 -L/usr/X11R6/lib
link_solaris   = -lX11 -lcurses -ltermcap
link_macosx    = -lX11 -L/usr/X11R6/lib
link_macosxppc = -lX11 -L/usr/X11R6/lib
link_windows   = -luser32 -lgdi32 -lopengl32 -lwsock32
###xlink_gfortran  = -lX11 -L/usr/X11R6/lib
links = $(link_$(SEISARCH))

##################################################
# SET PLATFORM SPECIFIC ROUTINES
COMP_linux64   = comp_linux.o
COMP_linux32   = comp_linux.o
COMP_solaris   = comp_sun.o
COMP_macosx    = comp_linux.o
COMP_macosxppc = comp_sun.o
COMP_windows   = comp_pc.o
###COMP_gfortran  = comp_linux.o
COMP_TYPE = COMP_$(SEISARCH)

##################################################
# COMPILERS
#
# fbounds-check gave too many problems, to be fixed
#
fc_linux64   = gfortran -I../INC -fdollar-ok  -fno-automatic $(debug_flags) -o $@
fc_linux32    = gfortran -I../INC -fdollar-ok  -fno-automatic $(debug_flags) -o $@
fc_solaris    = gfortran -I../INC -fdollar-ok  -fno-automatic $(debug_flags) -o $@
fc_macosx     = gfortran -m64 -I../INC -fdollar-ok  -fno-automatic $(debug_flags) -o $@
fc_macosxppc  = gfortran -I../INC -fdollar-ok  -fno-automatic $(debug_flags) -o $@
fc_windows   = gfortran -I../INC -fdollar-ok  -fno-automatic -lwsock32 -static $(debug_flags) -o $@
###fc_gfortran  = gfortran -I../INC -fdollar-ok  -fno-automatic $(debug_flags) -o $@

cc_linux64    = gcc -I../INC $(debug_flags) -o $@
cc_linux32    = gcc -I../INC $(debug_flags) -o $@
#cc_solaris   = cc $(debug_flags) -o $@
cc_solaris    = gcc -I../INC $(debug_flags) -o $@
cc_macosx     = gcc-11 -m64 -I../INC $(debug_flags) -o $@
cc_macosxppc  = gcc -I../INC $(debug_flags) -o $@
cc_windows    = gcc -I../INC $(debug_flags) -o $@
cc_g77        = gcc -I../INC $(debug_flags) -o $@
###cc_gfortran   = gcc -I../INC $(debug_flags) -o $@

ccc_windows   = g++ -I../INC $(debug_flags) -o $@ -static -lwsock32
ccc_linux64   = g++ -I../INC $(debug_flags) -o $@
ccc_linux32   = g++ -I../INC $(debug_flags) -o $@
ccc_solaris   = g++ -I../INC $(debug_flags) -o $@
ccc_macosx    = g++ -I../INC $(debug_flags) -o $@
ccc_macosxppc = g++ -I../INC $(debug_flags) -o $@

fc = $(fc_$(SEISARCH))
cc = $(cc_$(SEISARCH))
ccc= $(ccc_$(SEISARCH))

##################################################
# PATH TO INSTALL TOOL
INSTALL_linux64   = /usr/bin/install -m755
INSTALL_linux32   = /usr/bin/install -m755
INSTALL_solaris   = /usr/ucb/install -c -m755
INSTALL_macosx    = /usr/bin/install -m755
INSTALL_macosxppc = /usr/bin/install -m755
INSTALL_windows   = /usr/bin/install -m755
###INSTALL_gfortran  = /usr/bin/install  -m755
INSTALL = $(INSTALL_$(SEISARCH))

##################################################
# NAME OF SEISAN ARCHIVE
SEISAN_ARCHIVE = ../LIB/$(OUTPATH)/seisan.a

##################################################
# LIBRARIES
SL_linux64   = $(SEISAN_ARCHIVE)  
SL_linux32   = $(SEISAN_ARCHIVE) 
SL_solaris   = $(SEISAN_ARCHIVE) 
SL_macosx    = $(SEISAN_ARCHIVE)
SL_macosxppc = $(SEISAN_ARCHIVE)
SL_windows   = $(SEISAN_ARCHIVE) ../LIB/dismg.a c:/windows/system32/user32.dll 
###SL_gfortran  = $(SEISAN_ARCHIVE) 
SL = $(SL_$(SEISARCH))

##################################################
# PROGRAMS TO COMPILE ON SPECIFIC PLATFORM ONLY
ONLY_linux64 =  $(OUTPATH)/getstressdrop  $(OUTPATH)/slick
ONLY_linux32 =  $(OUTPATH)/getstressdrop  $(OUTPATH)/slick
ONLY_solaris =  $(OUTPATH)/getstressdrop  $(OUTPATH)/slick
# f95: ONLY_solaris = $(OUTPATH)/hypo71exe
ONLY_macosx =
ONLY_macosxppc =
ONLY_windows = \
  $(OUTPATH)/k2sei $(OUTPATH)/nansei \
  $(OUTPATH)/pcqsei  $(OUTPATH)/sudsei  $(OUTPATH)/getstressdrop  \
  $(OUTPATH)/slick
#  $(OUTPATH)/arcsei \
###ONLY_gfortran = 
PLATFORM_DEP_EXE = $(ONLY_$(SEISARCH))

##################################################
# PROGRAMS THAT ARE EXECUTABLE ONLY
EXEC_ONLY_linux64 = 
EXEC_ONLY_linux32 = 
EXEC_ONLY_solaris = y5dump rdseed seisan2mseed
EXEC_ONLY_macosx = 
EXEC_ONLY_macosxppc = 
EXEC_ONLY_windows = cnvssa.exe cnvssr.exe ismsei.exe kw2asc32.exe \
  lenpcs.exe  msrepack.exe rdseed.exe rt_seis.exe seisan.exe seisan2mseed.exe \
  sud2asc.exe y5dump.exe nmxb32n.dll nmxq32n.dll 
###EXEC_ONLY_g77 = msrepack rdseed seisan2mseed
###EXEC_ONLY_gfortran = 
EXEC_ONLY = $(EXEC_ONLY_$(SEISARCH))

##################################################
# INCLUDE FILES
INC_linux64 =
INC_linux32 =
INC_solaris =
INC_macosc =
INC_macosppc =
###INC_gfortran = 

INCS = ../INC/*.inc ../INC/*.f ../INC/*.h ../INC/*INC $(INC_$(SEISARCH))

##################################################
# SOME MORE DEFINITIONS
LIB = ../LIB
LIB_TO_PRO = ../PRO
CHECK_WITH = $(INCS) $(SL)

##################################################
# DEFINITION OF PROGRAMS TO COMPILE
PROGS = \
  $(OUTPATH)/afadsei \
  $(OUTPATH)/albnor \
  $(OUTPATH)/ahsei $(OUTPATH)/append $(OUTPATH)/associ \
  $(OUTPATH)/arc_del \
  $(OUTPATH)/avq \
  $(OUTPATH)/ascsei $(OUTPATH)/associ \
  $(OUTPATH)/asso $(OUTPATH)/automag \
  $(OUTPATH)/autoratio \
  $(OUTPATH)/autophase \
  $(OUTPATH)/auto $(OUTPATH)/autopic \
  $(OUTPATH)/autoreg $(OUTPATH)/autosig \
  $(OUTPATH)/backup_sfiles  \
  $(OUTPATH)/base $(OUTPATH)/bouch \
  $(OUTPATH)/bousei \
  $(OUTPATH)/bul $(OUTPATH)/bvalue\
  $(OUTPATH)/check_base $(OUTPATH)/cat_aga \
  $(OUTPATH)/change_resp_name \
  $(OUTPATH)/catstat $(OUTPATH)/change_mag\
  $(OUTPATH)/check  \
  $(OUTPATH)/cluster $(OUTPATH)/codaq $(OUTPATH)/codaq_area $(OUTPATH)/collect \
  $(OUTPATH)/codaq_polygon \
  $(OUTPATH)/compare_hyp $(OUTPATH)/comp_hyp \
  $(OUTPATH)/condet $(OUTPATH)/congap $(OUTPATH)/connoi\
  $(OUTPATH)/citsei $(OUTPATH)/corr \
  $(OUTPATH)/dimassei \
  $(OUTPATH)/delf $(OUTPATH)/dirf \
  $(OUTPATH)/dels \
  $(OUTPATH)/drsei $(OUTPATH)/eev $(OUTPATH)/edrnor \
  $(OUTPATH)/epimap \
  $(OUTPATH)/eev_get_event \
  $(OUTPATH)/exfilter $(OUTPATH)/evanoi \
  $(OUTPATH)/fk $(OUTPATH)/focmec \
  $(OUTPATH)/focmec_old \
  $(OUTPATH)/foc \
  $(OUTPATH)/focmec_exe_old \
  $(OUTPATH)/fpfit \
  $(OUTPATH)/fpfit_org \
  $(OUTPATH)/fk $(OUTPATH)/fkrprog_seisan \
  $(OUTPATH)/focmec_exe $(OUTPATH)/get_wav $(OUTPATH)/gmap \
  $(OUTPATH)/get_arc \
  $(OUTPATH)/get_arc_channels \
  $(OUTPATH)/get_stat \
  $(OUTPATH)/gridmin \
  $(OUTPATH)/giinor $(OUTPATH)/giisei $(OUTPATH)/gursei \
  $(OUTPATH)/gcf2msd_many  \
  $(OUTPATH)/get_web_locations  \
  $(OUTPATH)/gseresp $(OUTPATH)/gsrsei $(OUTPATH)/harnor \
  $(OUTPATH)/gsrnor \
  $(OUTPATH)/gseresp2seed \
  $(OUTPATH)/hash_seisan \
  $(OUTPATH)/herrman $(OUTPATH)/hersei $(OUTPATH)/hinnor \
  $(OUTPATH)/hspec8 \
  $(OUTPATH)/hypinv_seisan \
  $(OUTPATH)/hypodd \
  $(OUTPATH)/hsumnor $(OUTPATH)/hyp $(OUTPATH)/hypnor \
  $(OUTPATH)/hyp_isc  $(OUTPATH)/hypinv \
  $(OUTPATH)/hyp_count_phases \
  $(OUTPATH)/iasp $(OUTPATH)/invrad $(OUTPATH)/irisei \
  $(OUTPATH)/iloc_loop   \
  $(OUTPATH)/iloc_seisan \
  $(OUTPATH)/isfnor \
  $(OUTPATH)/imsnor \
  $(OUTPATH)/isccsv2nor \
  $(OUTPATH)/iscnor $(OUTPATH)/iscsta $(OUTPATH)/kinnor \
  $(OUTPATH)/leesei $(OUTPATH)/lensei \
  $(OUTPATH)/kacsei $(OUTPATH)/kinsei \
  $(OUTPATH)/kappa \
  $(OUTPATH)/kappa_sort \
  $(OUTPATH)/lsq $(OUTPATH)/m88sei $(OUTPATH)/magstat $(OUTPATH)/macromap \
  $(OUTPATH)/mag_order \
  $(OUTPATH)/magres \
  $(OUTPATH)/macroin $(OUTPATH)/mag $(OUTPATH)/makehin \
  $(OUTPATH)/map $(OUTPATH)/mapg \
  $(OUTPATH)/makerea  $(OUTPATH)/mag2 \
  $(OUTPATH)/mech \
  $(OUTPATH)/mulplt $(OUTPATH)/neisei \
  $(OUTPATH)/merge_s \
  $(OUTPATH)/mscut \
  $(OUTPATH)/neiccsv2nor \
  $(OUTPATH)/netdet \
  $(OUTPATH)/netdet_display \
  $(OUTPATH)/netdet_check \
  $(OUTPATH)/neweve $(OUTPATH)/norgse $(OUTPATH)/norims \
  $(OUTPATH)/nor2simulps $(OUTPATH)/nor2simulr \
  $(OUTPATH)/nor2nor2 \
  $(OUTPATH)/norm_q \
  $(OUTPATH)/norhin $(OUTPATH)/os9sei $(OUTPATH)/pdasei \
  $(OUTPATH)/norcsv $(OUTPATH)/norhead  $(OUTPATH)/norhyp \
  $(OUTPATH)/nor2dd $(OUTPATH)/nor2jhd_pujol $(OUTPATH)/nrwsei \
  $(OUTPATH)/p_align $(OUTPATH)/pinv \
  $(OUTPATH)/pdenor $(OUTPATH)/presp $(OUTPATH)/pr_resp \
  $(OUTPATH)/pfit $(OUTPATH)/psnsei \
  $(OUTPATH)/plotfoc \
  $(OUTPATH)/plotmoment \
  $(OUTPATH)/plotspec\
  $(OUTPATH)/plotml \
  $(OUTPATH)/plotmr \
  $(OUTPATH)/plotpolarity \
  $(OUTPATH)/plotphase \
  $(OUTPATH)/plotratio \
  $(OUTPATH)/ph2dt \
  $(OUTPATH)/qstat $(OUTPATH)/remodl \
  $(OUTPATH)/qnxsei $(OUTPATH)/remodl \
  $(OUTPATH)/rdseed_many \
  $(OUTPATH)/rdseed2seisan \
  $(OUTPATH)/remove_tab \
  $(OUTPATH)/report $(OUTPATH)/resamp \
  $(OUTPATH)/resp $(OUTPATH)/rhfoc10 $(OUTPATH)/rhwvinta \
  $(OUTPATH)/rmsdep $(OUTPATH)/rsanor $(OUTPATH)/rsasei \
  $(OUTPATH)/s89sei $(OUTPATH)/seedresp2gse \
  $(OUTPATH)/scpnor \
  $(OUTPATH)/sei2048 $(OUTPATH)/seiasc $(OUTPATH)/seidel \
  $(OUTPATH)/seisei $(OUTPATH)/select $(OUTPATH)/sample_read_cont \
  $(OUTPATH)/sample_resp_correction  \
  $(OUTPATH)/sample_rea_to_rea2  \
  $(OUTPATH)/sample_read_arc  \
  $(OUTPATH)/sample_read_write_seed  \
  $(OUTPATH)/sample_graphics  \
  $(OUTPATH)/sacsei $(OUTPATH)/sample_graphics \
  $(OUTPATH)/sample_read_wav $(OUTPATH)/sample_read_write_s \
  $(OUTPATH)/sample_write_wav \
  $(OUTPATH)/sample_udp_message  \
  $(OUTPATH)/sei2psxy \
  $(OUTPATH)/seigmt $(OUTPATH)/seim88a \
  $(OUTPATH)/seicut \
  $(OUTPATH)/seisaf \
  $(OUTPATH)/sgrsei \
  $(OUTPATH)/seipitsa $(OUTPATH)/selmap $(OUTPATH)/selsei \
  $(OUTPATH)/setbrn $(OUTPATH)/sissei $(OUTPATH)/spec \
  $(OUTPATH)/silsei \
  $(OUTPATH)/split $(OUTPATH)/statis \
  $(OUTPATH)/statstat $(OUTPATH)/stasei \
  $(OUTPATH)/swarm $(OUTPATH)/tersei $(OUTPATH)/tsig \
  $(OUTPATH)/tdmt_invc_seisan \
  $(OUTPATH)/trace_plot \
  $(OUTPATH)/ttlayer $(OUTPATH)/ttplot $(OUTPATH)/ttim \
  $(OUTPATH)/ttlocal \
  $(OUTPATH)/upd $(OUTPATH)/update $(OUTPATH)/usgsnor \
  $(OUTPATH)/velest $(OUTPATH)/velmenu \
  $(OUTPATH)/velnor \
  $(OUTPATH)/volcstat \
  $(OUTPATH)/wvint_seisan \
  $(OUTPATH)/wavasc \
  $(OUTPATH)/wav_add \
  $(OUTPATH)/wadati $(OUTPATH)/wad_plot $(OUTPATH)/wavfix \
  $(OUTPATH)/wavetool $(OUTPATH)/wavfullname \
  $(OUTPATH)/wgssei $(OUTPATH)/wkbj $(OUTPATH)/wkbj_or \
  $(OUTPATH)/xclust


info:
	@echo --------------------------------------------------
	@echo "SEISAN Makefile FOR PRO DIRETORY, OPTIONS ARE:"
	@echo " "
	@echo "   all - compiles all"
	@echo "   distribution - compiles all, no debug flag"
	@echo "   $(OUTPATH)/<program> - compile specfic program "
	@echo "   install - install executables"
	@echo "   clean - remove executables"
	@echo " "
	@echo "OUTPUT PATH IS SET TO $(OUTPATH) "
	@echo " "
	@echo "The platform is set through the environmental "
	@echo "variable SEISARCH. The following are supported:"
	@echo "solaris, linux32 (Linux 32bit), linux64 (64 bit), "
	@echo "macosx, macosxppc, windows"
	@echo "E.g. type:"
	@echo "setenv SEISARCH linux32"
	@echo "make all"
	@echo "to compile for a linux 32 bit computer."
	@echo --------------------------------------------------

##################################################
# ALL WILL COMPILE PROGRAM LIST AND PLATFORM DEP PROGRAMS
all: lib output $(OUTPATH) $(PLATFORM_DEP_EXE) $(PROGS)

distribution: all

lib:
	cd $(LIB); $(MAKE) all; cd $(LIB_TO_PRO)

output:
	@echo --------------------------------------------------
	@echo ---- COMPILING SEISAN PROGRAMS -------------------
	@echo --------------------------------------------------

linux64:
	mkdir linux64
linux32:
	mkdir linux32
solaris:
	mkdir solaris
macosx:
	mkdir macosx
macosxppc:
	mkdir macosxppc
windows:
	mkdir windows
g77:
	mkdir g77
gfortran:
	mkdir gfortran
freebsd:
	mkdir freebsd

# compile programs, NOTE below in front of @echo and $ ther MUST be a TAB

$(OUTPATH)/test: test.for $(CHECK_WITH)
	@echo ----- test -----
	$(fc) test.for $(SL) $(links)

$(OUTPATH)/test1: test1.for $(CHECK_WITH)
	@echo ----- test1 -----
	$(fc) test1.for $(SL) $(links)

$(OUTPATH)/test_prog: test_prog.for $(CHECK_WITH)
	@echo ----- test_prog -----
	$(fc) test_prog.for $(SL) $(links)

$(OUTPATH)/arc_test: arc_test.for $(CHECK_WITH)
	@echo ----- arc_test -----
	$(fc) arc_test.for $(SL) $(links)

$(OUTPATH)/arc_test1: arc_test1.for $(CHECK_WITH)
	@echo ----- arc_test1 -----
	$(fc) arc_test1.for $(SL) $(links)

$(OUTPATH)/afadsei: afadsei.for $(CHECK_WITH)
	@echo ----- afadsei -----
	$(fc) afadsei.for $(SL) $(links)

$(OUTPATH)/albnor: albnor.for $(CHECK_WITH)
	@echo ----- albnor -----
	$(fc) albnor.for $(SL) $(links)

$(OUTPATH)/ahsei: ahsei.for $(CHECK_WITH)
	@echo ----- ahsei -----
	$(fc) ahsei.for $(SL) $(links)

$(OUTPATH)/append: append.for $(CHECK_WITH)
	@echo ----- append -----
	$(fc) append.for $(SL) $(links)

$(OUTPATH)/arc_del: arc_del.for $(CHECK_WITH)
	@echo ----- arc_del -----
	$(fc) arc_del.for $(SL) $(links)

$(OUTPATH)/arcsei: arcsei.for $(CHECK_WITH)
	@echo ----- arcsei -----
	$(fc) arcsei.for $(SL) $(links)

$(OUTPATH)/ascsei: ascsei.for $(CHECK_WITH)
	@echo ----- ascsei -----
	$(fc) ascsei.for $(SL) $(links)

$(OUTPATH)/associ: associ.for $(CHECK_WITH)
	@echo ----- associ -----
	$(fc) associ.for $(SL) $(links)

$(OUTPATH)/asso: asso.for $(CHECK_WITH)
	@echo ----- asso -----
	$(fc) asso.for $(SL) $(links)

$(OUTPATH)/auto: auto.for $(CHECK_WITH)
	@echo ----- auto -----
	$(fc) auto.for $(SL) $(links)

$(OUTPATH)/automag: automag.for $(CHECK_WITH)
	@echo ----- automag -----
	$(fc) automag.for $(SL) $(links)

$(OUTPATH)/autoratio: autoratio.for $(CHECK_WITH)
	@echo ----- autoratio -----
	$(fc) autoratio.for $(SL) $(links)

$(OUTPATH)/autopic: autopic.for $(CHECK_WITH)
	@echo ----- autopic -----
	$(fc) autopic.for $(SL) $(links)

$(OUTPATH)/autoreg: autoreg.for $(CHECK_WITH)
	@echo ----- autoreg -----
	$(fc) autoreg.for $(SL) $(links)

$(OUTPATH)/autosig: autosig.for $(CHECK_WITH)
	@echo ----- autosig -----
	$(fc) autosig.for $(SL) $(links) -lm

$(OUTPATH)/avq:   avq.for $(CHECK_WITH)
	@echo ----- avq -----
	$(fc) avq.for $(SL) $(links)

$(OUTPATH)/base:   base.for $(CHECK_WITH)
	@echo ----- base -----
	$(fc) base.for $(SL) $(links)

$(OUTPATH)/backup_sfiles:   backup_sfiles.for $(CHECK_WITH)
	@echo ----- backup_sfiles -----
	$(fc) backup_sfiles.for $(SL) $(links)

$(OUTPATH)/bouch: bouch.for $(CHECK_WITH)
	@echo ----- bouch -----
	$(fc) bouch.for $(SL) $(links)

$(OUTPATH)/bousei: bousei.for $(CHECK_WITH)
	@echo ----- bousei -----
	$(fc) bousei.for $(SL) $(links)

$(OUTPATH)/bul: bul.for $(CHECK_WITH)
	@echo ----- bul -----
	$(fc) bul.for $(SL) $(links)

$(OUTPATH)/bvalue: bvalue.for $(CHECK_WITH)
	@echo ----- bvalue -----
	$(fc) bvalue.for $(SL) $(links)

$(OUTPATH)/cat_aga: cat_aga.for $(CHECK_WITH)
	@echo ----- cat_aga -----
	$(fc) cat_aga.for $(SL) $(links) 

$(OUTPATH)/catstat: catstat.for $(catstat_obj) $(CHECK_WITH)
	@echo ----- catstat -----
	$(fc) catstat.for $(SL) $(links)

$(OUTPATH)/change_mag: change_mag.for $(CHECK_WITH)
	@echo ----- change_mag -----
	$(fc) change_mag.for $(SL) $(links)

$(OUTPATH)/change_resp_name: change_resp_name.for $(CHECK_WITH)
	@echo ----- change_resp_name -----
	$(fc) change_resp_name.for $(SL) $(links)

$(OUTPATH)/check:   check.for $(CHECK_WITH)
	@echo ----- check -----
	$(fc) check.for $(SL) $(links)

$(OUTPATH)/check_base: check_base.for $(CHECK_WITH)
	@echo ----- check_base -----
	$(fc) check_base.for $(SL) $(links)

$(OUTPATH)/citsei: citsei.for $(CHECK_WITH)
	@echo ----- citsei -----
	$(fc) citsei.for $(SL) $(links)

$(OUTPATH)/cluster: cluster.for $(CHECK_WITH)
	@echo ----- cluster -----
	$(fc) cluster.for $(SL) $(links)

$(OUTPATH)/codaq: codaq.for $(CHECK_WITH)
	@echo ----- codaq -----
	$(fc) codaq.for $(SL) $(links)

$(OUTPATH)/codaq_area: codaq_area.for $(CHECK_WITH)
	@echo ----- codaq_area -----
	$(fc) codaq_area.for $(SL) $(links) 

$(OUTPATH)/codaq_polygon: codaq_polygon.for $(CHECK_WITH)
	@echo ----- codaq_polygon -----
	$(fc) codaq_polygon.for $(SL) $(links) 

$(OUTPATH)/corr: corr.for $(CHECK_WITH)
	@echo ----- corr -----
	$(fc) corr.for $(SL) $(links) 

$(OUTPATH)/collect: collect.for $(CHECK_WITH)
	@echo ----- collect -----
	$(fc) collect.for $(SL) $(links)

$(OUTPATH)/comp_hyp: comp_hyp.for $(CHECK_WITH)
	@echo ----- comp_hyp -----
	$(fc) comp_hyp.for $(SL) $(links)

$(OUTPATH)/compare_hyp: compare_hyp.for $(CHECK_WITH)
	@echo ----- compare_hyp ----- 
	$(fc) compare_hyp.for $(SL) $(links)

$(OUTPATH)/condet: condet.for $(CHECK_WITH)
	@echo ----- condet -----
	$(fc) condet.for $(SL) $(links)

$(OUTPATH)/congap: congap.for $(CHECK_WITH)
	@echo ----- congap -----
	$(fc) congap.for $(SL) $(links)

$(OUTPATH)/connoi: connoi.for $(CHECK_WITH)
	@echo ----- connoi -----
	$(fc) connoi.for $(SL) $(links)

$(OUTPATH)/decon: decon.for $(CHECK_WITH)
	@echo ----- decon -----
	$(fc) decon.for $(SL) $(links)

$(OUTPATH)/det: det.for $(CHECK_WITH)
	@echo ----- det -----
	$(fc) det.for $(SL) $(links)

$(OUTPATH)/dirf: dirf.c $(CHECK_WITH)
	@echo ----- dirf -----
	$(cc) dirf.c

$(OUTPATH)/database2mseed: database2mseed.for $(CHECK_WITH)
	@echo ----- database2mseed -----
	$(fc) database2mseed.for $(SL) $(links)

$(OUTPATH)/dimassei: dimassei.for $(CHECK_WITH)
	@echo ----- dimassei -----
	$(fc) dimassei.for $(SL) $(links)

$(OUTPATH)/delf: delf.for $(CHECK_WITH)
	@echo ----- delf -----
	$(fc) delf.for $(SL) $(links)

$(OUTPATH)/dels: dels.for $(CHECK_WITH)
	@echo ----- dels -----
	$(fc) dels.for $(SL) $(links)

$(OUTPATH)/drsei: drsei.for $(CHECK_WITH)
	@echo ----- drsei -----
	$(fc) drsei.for $(SL) $(links)

$(OUTPATH)/edrnor: edrnor.for $(CHECK_WITH)
	@echo ----- edrnor -----
	$(fc) edrnor.for $(SL) $(links)

$(OUTPATH)/eev: eev.for $(CHECK_WITH)
	@echo ----- eev -----
	$(fc) eev.for $(SL) $(links)

$(OUTPATH)/eev_get_event: eev_get_event.for $(CHECK_WITH)
	@echo ----- eev_get_event -----
	$(fc) eev_get_event.for $(SL) $(links)

$(OUTPATH)/epimap: epimap.for $(CHECK_WITH)
	@echo ----- epimap -----
	$(fc) epimap.for $(SL) $(links)

$(OUTPATH)/evanoi: evanoi.for $(CHECK_WITH)
	@echo ----- evanoi -----
	$(fc) evanoi.for $(SL) $(links)

$(OUTPATH)/exfilter: exfilter.for $(CHECK_WITH)
	@echo ----- exfilter -----
	$(fc) exfilter.for $(SL) $(links)

$(OUTPATH)/waveform_cache: waveform_cache.for $(CHECK_WITH)
	@echo ----- waveform_cache -----
	$(fc) waveform_cache.for $(SL) $(links)

$(OUTPATH)/wavetool: wavetool.for $(CHECK_WITH)
	@echo ----- wavetool -----
	$(fc) wavetool.for $(SL) $(links) 

$(OUTPATH)/wavfullname: wavfullname.for $(CHECK_WITH)
	@echo ----- wavfullname -----
	$(fc) wavfullname.for $(SL) $(links) 

$(OUTPATH)/fk: fk.for $(CHECK_WITH)
	@echo ----- fk -----
	$(fc) fk.for $(SL) $(links)

$(OUTPATH)/fkrprog_seisan: fkrprog_seisan.for $(CHECK_WITH)
	@echo ----- fkrprog_seisan -----
	$(fc) fkrprog_seisan.for 

$(OUTPATH)/focmec: focmec.for $(CHECK_WITH)
	@echo ----- focmec -----
	$(fc) focmec.for $(SL) $(links)

$(OUTPATH)/focmec_old: focmec_old.for $(CHECK_WITH)
	@echo ----- focmec_old -----
	$(fc) focmec_old.for $(SL) $(links)

$(OUTPATH)/foc: foc.for $(CHECK_WITH)
	@echo ----- foc -----
	$(fc) foc.for $(SL) $(links) 

$(OUTPATH)/focmec_exe: focmec_exe.for $(CHECK_WITH)
	@echo ----- focmec_exe -----
	$(fc) focmec_exe.for  $(SL) $(links)

$(OUTPATH)/focmec_exe_old: focmec_exe_old.for $(CHECK_WITH)
	@echo ----- focmec_exe_old -----
	$(fc) focmec_exe_old.for  $(SL) $(links)

$(OUTPATH)/fpfit: fpfit.for $(CHECK_WITH)
	@echo ----- fpfit -----
	$(fc) fpfit.for $(SL) $(links) 

$(OUTPATH)/fpfit_org: fpfit_org.for $(CHECK_WITH)
	@echo ----- fpfit_org -----
	$(fc) fpfit_org.for $(SL) $(links)

$(OUTPATH)/galoc: galoc.for $(CHECK_WITH)
	@echo ----- galoc -----
	$(fc) galoc.for $(SL) $(links)

$(OUTPATH)/get_arc: get_arc.for $(CHECK_WITH)
	@echo ----- get_arc -----
	$(fc) get_arc.for $(SL) $(links)

$(OUTPATH)/get_arc_channels: get_arc_channels.for $(CHECK_WITH)
	@echo ----- get_arc_channels-----
	$(fc) get_arc_channels.for $(SL) $(links)

$(OUTPATH)/get_wav: get_wav.for $(CHECK_WITH)
	@echo ----- get_wav -----
	$(fc) get_wav.for $(SL) $(links)

$(OUTPATH)/get_stat: get_stat.for $(CHECK_WITH)
	@echo ----- get_stat -----
	$(fc) get_stat.for $(SL) $(links)

$(OUTPATH)/gridmin: gridmin.for $(CHECK_WITH)
	@echo ----- gridmin -----
	$(fc) gridmin.for $(SL) $(links)

$(OUTPATH)/gmap: gmap.for $(CHECK_WITH)
	@echo ----- gmap -----
	$(fc) gmap.for $(SL) $(links)

$(OUTPATH)/giinor: giinor.for $(CHECK_WITH)
	@echo ----- giinor -----
	$(fc) giinor.for $(SL) $(links)

$(OUTPATH)/giisei: giisei.for $(CHECK_WITH)
	@echo ----- giisei -----
	$(fc) giisei.for $(SL) $(links)

$(OUTPATH)/gseresp: gseresp.for $(CHECK_WITH)
	@echo ----- gseresp -----
	$(fc) gseresp.for $(SL) $(links)

$(OUTPATH)/gseresp2seed: gseresp2seed.for $(CHECK_WITH)
	@echo ----- gseresp2seed -----
	$(fc) gseresp2seed.for $(SL) $(links)

$(OUTPATH)/gsrnor: gsrnor.for $(CHECK_WITH)
	@echo ----- gsrnor -----
	$(fc) gsrnor.for $(SL) $(links)

$(OUTPATH)/gsrsei: gsrsei.for $(CHECK_WITH)
	@echo ----- gsrsei -----
	$(fc) gsrsei.for $(SL) $(links)

$(OUTPATH)/gursei: gursei.for $(CHECK_WITH)
	@echo ----- gursei -----
	$(fc) gursei.for $(SL) $(links)

$(OUTPATH)/gcf2msd_many: gcf2msd_many.for $(CHECK_WITH)
	@echo ----- gcf2msd_many -----
	$(fc) gcf2msd_many.for $(SL) $(links)

$(OUTPATH)/harnor: harnor.for $(CHECK_WITH)
	@echo ----- harnor -----
	$(fc) harnor.for $(SL) $(links)

$(OUTPATH)/hash_seisan: hash_seisan.for $(CHECK_WITH)
	@echo ----- hash_seisan -----
	$(fc) hash_seisan.for $(SL) $(links)

$(OUTPATH)/herrman: herrman.for $(CHECK_WITH)
	@echo ----- herrman -----
	$(fc) herrman.for $(SL) $(links)
   
$(OUTPATH)/hersei: hersei.for $(CHECK_WITH)
	@echo ----- hersei -----
	$(fc) hersei.for $(SL) $(links)

$(OUTPATH)/hinnor: hinnor.for $(CHECK_WITH)
	@echo ----- hinnor -----
	$(fc) hinnor.for $(SL) $(links)
   
$(OUTPATH)/hypinv_seisan: hypinv_seisan.for $(CHECK_WITH)
	@echo ----- hypinv_seisan -----
	$(fc) hypinv_seisan.for $(SL) $(links)

$(OUTPATH)/hypodd: hypodd.for $(CHECK_WITH)
	@echo ----- hyypodd -----
	$(fc) hypodd.for $(SL) $(links) 

   
$(OUTPATH)/hspec8: hspec8.for $(CHECK_WITH)
	@echo ----- hspec8 -----
	$(fc) hspec8.for $(SL) $(links) 

$(OUTPATH)/hsumnor: hsumnor.for $(CHECK_WITH)
	@echo ----- hsumnor -----
	$(fc) hsumnor.for $(SL) $(links)

$(OUTPATH)/hyp: hyp.for $(CHECK_WITH)
	@echo ----- hyp -----
	$(fc) hyp.for $(SL) $(links) 

$(OUTPATH)/hyp_count_phases: hyp_count_phases.for $(CHECK_WITH)
	@echo ----- hyp_count_phases -----
	$(fc) hyp_count_phases.for $(SL) $(links) 

$(OUTPATH)/newhyp: newhyp.for $(CHECK_WITH)
	@echo ----- newhyp -----
	$(fc) newhyp.for $(SL) $(links)

$(OUTPATH)/hyp_isc: hyp_isc.for $(CHECK_WITH)
	@echo ----- hyp_isc -----
	$(fc) hyp_isc.for $(SL) $(links)

$(OUTPATH)/hypinv: hypinv.for $(CHECK_WITH)
	@echo ----- hypinv -----
	$(fc) hypinv.for  $(SL) $(links)

$(OUTPATH)/hypnor: hypnor.for $(CHECK_WITH)
	@echo ----- hypnor -----
	$(fc) hypnor.for $(SL) $(links)
   
$(OUTPATH)/hypo71: hypo71.for $(CHECK_WITH)
	@echo ----- hypo71 -----
	$(fc) hypo71.for $(SL) $(links)

$(OUTPATH)/hypo71exe: hypo71exe.for $(CHECK_WITH)
	@echo ----- hypo71exe -----
	$(fc) hypo71exe.for 

$(OUTPATH)/glodet: glodet.for $(CHECK_WITH)
	@echo ----- glodet -----
	$(fc) glodet.for $(SL) $(links)

$(OUTPATH)/iasp: iasp.for $(CHECK_WITH)
	@echo ----- iasp -----
	$(fc) iasp.for $(SL) $(links)

$(OUTPATH)/iloc_loop: iloc_loop.for $(CHECK_WITH)
	@echo ----- iloc_loop -----
	$(fc) iloc_loop.for $(SL) $(links)

$(OUTPATH)/iloc_seisan: iloc_seisan.for $(CHECK_WITH)
	@echo ----- iloc_seisan -----
	$(fc) iloc_seisan.for $(SL) $(links)

$(OUTPATH)/invrad:invrad.for  $(CHECK_WITH)
	@echo ----- invrad -----
	$(fc) invrad.for $(SL) $(links)

$(OUTPATH)/iscnor: iscnor.for $(CHECK_WITH)
	@echo ----- iscnor -----
	$(fc) iscnor.for $(SL) $(links)

$(OUTPATH)/isfnor: isfnor.for $(CHECK_WITH)
	@echo ----- isfnor -----
	$(fc) isfnor.for $(SL) $(links)

$(OUTPATH)/imsnor: imsnor.for $(CHECK_WITH)
	@echo ----- imsnor -----
	$(fc) imsnor.for $(SL) $(links)

$(OUTPATH)/iscsta: iscsta.for $(CHECK_WITH)
	@echo ----- iscsta -----
	$(fc) iscsta.for $(SL) $(links)

$(OUTPATH)/irisei: irisei.for $(CHECK_WITH)
	@echo ----- irisei -----
	$(fc) irisei.for $(SL) $(links)

$(OUTPATH)/kacsei: kacsei.for $(CHECK_WITH)
	@echo ----- kacsei -----
	$(fc) kacsei.for $(SL) $(links)

$(OUTPATH)/kappa: kappa.for $(CHECK_WITH)
	@echo ----- kappa_ -----
	$(fc) kappa.for $(SL) $(links)

$(OUTPATH)/kappa_sort: kappa_sort.for $(CHECK_WITH)
	@echo ----- kappa_sort -----
	$(fc) kappa_sort.for $(SL) $(links)

$(OUTPATH)/k2sei: k2sei.for $(CHECK_WITH)
	@echo ----- k2sei -----
	$(fc) k2sei.for $(SL) $(links)

$(OUTPATH)/kinnor: kinnor.for $(CHECK_WITH)
	@echo ----- kinnor -----
	$(fc) kinnor.for $(SL) $(links)

$(OUTPATH)/kinsei: kinsei.for $(CHECK_WITH)
	@echo ----- kinsei -----
	$(fc) kinsei.for $(SL) $(links)

$(OUTPATH)/leesei: leesei.for $(CHECK_WITH)
	@echo ----- leesei -----
	$(fc) leesei.for $(SL) $(links)

$(OUTPATH)/lensei: lensei.for $(CHECK_WITH)
	@echo ----- lensei -----
	$(fc) lensei.for $(SL) $(links)

$(OUTPATH)/lsq: lsq.for $(CHECK_WITH)
	@echo ----- lsq -----
	$(fc) lsq.for $(SL) $(links)

$(OUTPATH)/m88sei: m88sei.for $(CHECK_WITH)
	@echo ----- m88sei -----
	$(fc) m88sei.for $(SL) $(links)

$(OUTPATH)/macroin: macroin.for $(CHECK_WITH)
	@echo ----- macroin -----
	$(fc) macroin.for $(SL) $(links)

$(OUTPATH)/macromap: macromap.for $(CHECK_WITH)
	@echo ----- macromap -----
	$(fc) macromap.for $(SL) $(links)

$(OUTPATH)/mag: mag.for $(CHECK_WITH)
	@echo ----- mag -----
	$(fc) mag.for $(SL) $(links)

$(OUTPATH)/mag2: mag2.for $(CHECK_WITH)
	@echo ----- mag2 -----
	$(fc) mag2.for $(SL) $(links)

$(OUTPATH)/magmbn: magmbn.for $(CHECK_WITH)
	@echo ----- magmbn -----
	$(fc) magmbn.for $(SL) $(links)

$(OUTPATH)/mag_order: mag_order.for $(CHECK_WITH)
	@echo ----- mag_order -----
	$(fc) mag_order.for $(SL) $(links)

$(OUTPATH)/magres: magres.for $(CHECK_WITH)
	@echo ----- magres -----
	$(fc) magres.for $(SL) $(links)

$(OUTPATH)/magstat: magstat.for $(CHECK_WITH)
	@echo ----- magstat -----
	$(fc) magstat.for $(SL) $(links)

$(OUTPATH)/makehin: makehin.for $(CHECK_WITH)
	@echo ----- makehin -----
	$(fc) makehin.for $(SL) $(links)

$(OUTPATH)/makerea: makerea.for $(CHECK_WITH)
	@echo ----- makerea -----
	$(fc) makerea.for $(SL) $(links)

$(OUTPATH)/mech:   mech.for $(CHECK_WITH)
	@echo ----- mech -----
	$(fc) mech.for $(SL) $(links)

$(OUTPATH)/merge_s:   merge_s.for $(CHECK_WITH)
	@echo ----- merge_s -----
	$(fc) merge_s.for $(SL) $(links)

$(OUTPATH)/map:   map.for $(CHECK_WITH)
	@echo ----- map -----
	$(fc) map.for $(SL) $(links)

$(OUTPATH)/mapg:   mapg.for $(CHECK_WITH)
	@echo ----- mapg -----
	$(fc) mapg.for $(SL) $(links)

$(OUTPATH)/mscut: mscut.c $(CHECK_WITH)
	@echo ----- mscut-----
	$(cc) mscut.c $(SL) $(links)

$(OUTPATH)/mulplt: mulplt.for $(CHECK_WITH)
	@echo ----- mulplt -----
	$(fc) mulplt.for $(SL) $(links)

$(OUTPATH)/nansei: nansei.for $(CHECK_WITH)
	@echo ----- nansei -----
	$(fc) nansei.for $(SL) $(links)

$(OUTPATH)/neiccsv2nor: neiccsv2nor.for $(CHECK_WITH)
	@echo ----- neiccsv2nor -----
	$(fc) neiccsv2nor.for $(SL) $(links)

$(OUTPATH)/isccsv2nor: isccsv2nor.for $(CHECK_WITH)
	@echo ----- isccsv2nor -----
	$(fc) isccsv2nor.for $(SL) $(links)

$(OUTPATH)/neisei: neisei.for $(CHECK_WITH)
	@echo ----- neisei -----
	$(fc) neisei.for $(SL) $(links)

$(OUTPATH)/netdet: netdet.for $(CHECK_WITH)
	@echo ----- netdet -----
	$(fc) netdet.for $(SL) $(links)

$(OUTPATH)/netdet_display: netdet_display.for $(CHECK_WITH)
	@echo ----- netdet_display -----
	$(fc) netdet_display.for $(SL) $(links)

$(OUTPATH)/netdet_check: netdet_check.for $(CHECK_WITH)
	@echo ----- netdet_check -----
	$(fc) netdet_check.for $(SL) $(links)

$(OUTPATH)/neweve: neweve.for $(CHECK_WITH)
	@echo ----- neweve -----
	$(fc) neweve.for $(SL) $(links)

$(OUTPATH)/new_version: new_version.for $(CHECK_WITH)
	@echo ----- new_version -----
	$(fc) new_version.for $(SL) $(links)

$(OUTPATH)/norgse: norgse.for $(CHECK_WITH)
	@echo ----- norgse -----
	$(fc) norgse.for $(SL) $(links)

$(OUTPATH)/nor2nor2: nor2nor2.for $(CHECK_WITH)
	@echo ----- nor2nor2 -----
	$(fc) nor2nor2.for $(SL) $(links)

$(OUTPATH)/norm_q: norm_q.for $(CHECK_WITH)
	@echo ----- norm_q -----
	$(fc) norm_q.for $(SL) $(links)

$(OUTPATH)/norhin: norhin.for $(CHECK_WITH)
	@echo ----- norhin -----
	$(fc) norhin.for $(SL) $(links)

$(OUTPATH)/norhead: norhead.for $(CHECK_WITH)
	@echo ----- norhead -----
	$(fc) norhead.for $(SL) $(links)

$(OUTPATH)/nor2dd: nor2dd.for $(CHECK_WITH)
	@echo ----- nor2dd -----
	$(fc) nor2dd.for $(SL) $(links)

$(OUTPATH)/nor2simulps: nor2simulps.for $(CHECK_WITH)
	@echo ----- nor2simulps -----
	$(fc) nor2simulps.for $(SL) $(links)

$(OUTPATH)/nor2simulr: nor2simulr.for $(CHECK_WITH)
	@echo ----- nor2simulr -----
	$(fc) nor2simulr.for $(SL) $(links)

$(OUTPATH)/nor2jhd_pujol: nor2jhd_pujol.for $(CHECK_WITH)
	@echo ----- nor2jhd_pujol -----
	$(fc) nor2jhd_pujol.for $(SL) $(links)

$(OUTPATH)/norhyp: norhyp.for $(CHECK_WITH)
	@echo ----- norhyp -----
	$(fc) norhyp.for $(SL) $(links)

$(OUTPATH)/norims: norims.for $(CHECK_WITH)
	@echo ----- norims -----
	$(fc) norims.for $(SL) $(links)

$(OUTPATH)/norrayinvr: norrayinvr.for $(CHECK_WITH)
	@echo ----- norrayinvr -----
	$(fc) norrayinvr.for $(SL) $(links)

$(OUTPATH)/nornoise: nornoise.for $(CHECK_WITH)
	@echo ----- nornoise -----
	$(fc) nornoise.for $(SL) $(links)

$(OUTPATH)/nrwsei: nrwsei.for $(CHECK_WITH)
	@echo ----- nrwsei -----
	$(fc) nrwsei.for $(SL) $(links)

$(OUTPATH)/os9sei: os9sei.for $(CHECK_WITH)
	@echo ----- os9sei -----
	$(fc) os9sei.for $(SL) $(links)

$(OUTPATH)/p_align: p_align.for $(CHECK_WITH)
	@echo ----- p_align -----
	$(fc) p_align.for $(SL) $(links)

$(OUTPATH)/pinv: pinv.for $(CHECK_WITH)
	@echo ----- pinv -----
	$(fc) pinv.for $(SL) $(links)

$(OUTPATH)/pcqsei: pcqsei.for $(CHECK_WITH)
	@echo ----- pcqsei-----
	$(fc) pcqsei.for $(SL) $(links)

$(OUTPATH)/pdasei: pdasei.for $(CHECK_WITH)
	@echo ----- pdasei-----
	$(fc) pdasei.for $(SL) $(links)

$(OUTPATH)/pdenor: pdenor.for $(CHECK_WITH)
	@echo ----- pdenor -----
	$(fc) pdenor.for $(SL) $(links)

$(OUTPATH)/pfit: pfit.for $(CHECK_WITH)
	@echo ----- pfit -----
	$(fc) pfit.for $(SL) $(links)

$(OUTPATH)/ph2dt: ph2dt.for $(CHECK_WITH)
	@echo ----- ph2dt -----
	$(fc) ph2dt.for $(SL) $(links) 

$(OUTPATH)/pr_resp: pr_resp.for $(CHECK_WITH)
	@echo ----- pr_resp -----
	$(fc) pr_resp.for $(SL) $(links) 

$(OUTPATH)/presp:  presp.for $(CHECK_WITH)
	@echo ----- presp -----
	$(fc) presp.for $(SL) $(links)

$(OUTPATH)/plotml:  plotml.for $(CHECK_WITH)
	@echo ----- plotml -----
	$(fc) plotml.for $(SL) $(links)

$(OUTPATH)/plotmr:  plotmr.for $(CHECK_WITH)
	@echo ----- plotmr -----
	$(fc) plotmr.for $(SL) $(links)

$(OUTPATH)/plotfoc:  plotfoc.for $(CHECK_WITH)
	@echo ----- plotfoc -----
	$(fc) plotfoc.for $(SL) $(links)

$(OUTPATH)/plotmoment:  plotmoment.for $(CHECK_WITH)
	@echo ----- plotmoment -----
	$(fc) plotmoment.for $(SL) $(links)

$(OUTPATH)/plotspec:  plotspec.for $(CHECK_WITH)
	@echo ----- plotspec -----
	$(fc) plotspec.for $(SL) $(links)

$(OUTPATH)/plotratio:  plotratio.for $(CHECK_WITH)
	@echo ----- plotratio -----
	$(fc) plotratio.for $(SL) $(links)

$(OUTPATH)/plotpolarity:  plotpolarity.for $(CHECK_WITH)
	@echo ----- plotpolarity -----
	$(fc) plotpolarity.for $(SL) $(links)

$(OUTPATH)/plotphase:  plotphase.for $(CHECK_WITH)
	@echo ----- plotphase -----
	$(fc) plotphase.for $(SL) $(links)

$(OUTPATH)/psnsei: psnsei.for $(CHECK_WITH)
	@echo ----- psnsei -----
	$(fc) psnsei.for $(SL) $(links)

$(OUTPATH)/qnxsei: qnxsei.for $(CHECK_WITH)
	@echo ----- qnxsei -----
	$(fc) qnxsei.for $(SL) $(links)

$(OUTPATH)/qlg:    qlg.for $(CHECK_WITH)
	@echo ----- qlg -----
	$(fc) qlg.for $(SL) $(links)

$(OUTPATH)/qstat:   qstat.for $(CHECK_WITH)
	@echo ----- qstat -----
	$(fc) qstat.for $(SL) $(links)

$(OUTPATH)/remodl: remodl.for $(CHECK_WITH)
	@echo ----- remodl -----
	$(fc) remodl.for $(SL) $(links)

$(OUTPATH)/report: report.for $(CHECK_WITH)
	@echo ----- report -----
	$(fc) report.for $(SL) $(links)

$(OUTPATH)/resamp: resamp.for $(CHECK_WITH)
	@echo ----- resamp -----
	$(fc) resamp.for $(SL) $(links)

$(OUTPATH)/rdseed_many: rdseed_many.for $(CHECK_WITH)
	@echo ----- rdseed_many -----
	$(fc) rdseed_many.for $(SL) $(links)

$(OUTPATH)/rdseed2seisan: rdseed2seisan.for $(CHECK_WITH)
	@echo ----- rdseed2seisan -----
	$(fc) rdseed2seisan.for $(SL) $(links)

$(OUTPATH)/remove_tab: remove_tab.for $(CHECK_WITH)
	@echo ----- remove_tab -----
	$(fc) remove_tab.for $(SL) $(links)

$(OUTPATH)/resp: resp.for $(CHECK_WITH)
	@echo ----- resp -----
	$(fc) resp.for $(SL) $(links)

$(OUTPATH)/rhfoc10: rhfoc10.for $(CHECK_WITH)
	@echo ----- rhfoc10 -----
	$(fc) rhfoc10.for $(SL) $(links)

$(OUTPATH)/rhwvinta: rhwvinta.for $(CHECK_WITH)
	@echo ----- rhwvinta -----
	$(fc) rhwvinta.for $(SL) $(links)

$(OUTPATH)/rmsdep: rmsdep.for $(CHECK_WITH)
	@echo ----- rmsdep -----
	$(fc) rmsdep.for $(SL) $(links)

$(OUTPATH)/rsanor: rsanor.for $(CHECK_WITH)
	@echo ----- rsanor -----
	$(fc) rsanor.for $(SL) $(links) 

$(OUTPATH)/rsasei: rsasei.for $(CHECK_WITH)
	@echo ----- rsasei -----
	$(fc) rsasei.for $(SL) $(links) 

$(OUTPATH)/scpnor: scpnor.for $(CHECK_WITH)
	@echo ----- sscpnor -----
	$(fc) scpnor.for $(SL) $(links) 

$(OUTPATH)/sudsei: sudsei.for $(CHECK_WITH)
	@echo ----- sudsei -----
	$(fc) sudsei.for $(SL) $(links) 

$(OUTPATH)/s89sei: s89sei.for $(CHECK_WITH)
	@echo ----- s89sei -----
	$(fc) s89sei.for $(SL) $(links) 

$(OUTPATH)/sample_resp_correction: sample_resp_correction.for $(CHECK_WITH)
	@echo ----- sample_resp_correction -----
	$(fc) sample_resp_correction.for $(SL) $(links)  

$(OUTPATH)/sample_graphics: sample_graphics.for $(CHECK_WITH)
	@echo ----- sample_graphics -----
	$(fc) sample_graphics.for $(SL) $(links) 

$(OUTPATH)/sample_read_arc: sample_read_arc.for $(CHECK_WITH)   
	@echo ----- sample_read_arc -----
	$(fc) sample_read_arc.for $(SL) $(links)

$(OUTPATH)/sample_rea_to_rea2: sample_rea_to_rea2.for $(CHECK_WITH)   
	@echo ----- sample_rea_to_rea2 -----
	$(fc) sample_rea_to_rea2.for $(SL) $(links)

$(OUTPATH)/sample_read_write_seed: sample_read_write_seed.for $(CHECK_WITH)   
	@echo ----- sample_read_write_seed -----
	$(fc) sample_read_write_seed.for $(SL) $(links)

$(OUTPATH)/sample_read_cont: sample_read_cont.for $(CHECK_WITH)   
	@echo ----- sample_read_cont -----
	$(fc) sample_read_cont.for $(SL) $(links)  

$(OUTPATH)/sample_read_write_s: sample_read_write_s.for $(CHECK_WITH)
	@echo ----- sample_read_write_s -----
	$(fc) sample_read_write_s.for $(SL) $(links) 

$(OUTPATH)/sample_udp_message: sample_udp_message.for $(CHECK_WITH)
	@echo ----- sample_udp_message -----
	$(fc) sample_udp_message.for $(SL) $(links) 

$(OUTPATH)/nor2gis: nor2gis.for $(CHECK_WITH)
	@echo ----- nor2gis -----
	$(fc) nor2gis.for $(SL) $(links)

$(OUTPATH)/norcsv: norcsv.for $(CHECK_WITH)
	@echo ----- norcsv -----
	$(fc) norcsv.for $(SL) $(links)

$(OUTPATH)/fixff: fixff.for $(CHECK_WITH)
	@echo ----- fixff -----
	$(fc) fixff.for $(SL) $(links)

$(OUTPATH)/sample_read_wav: sample_read_wav.for $(CHECK_WITH)
	@echo ----- sample_read_wav -----
	$(fc) sample_read_wav.for $(SL) $(links) 

$(OUTPATH)/sample_write_wav: sample_write_wav.for $(CHECK_WITH)
	@echo ----- sample_write_wav -----
	$(fc) sample_write_wav.for $(SL) $(links) 

$(OUTPATH)/sacsei: sacsei.for $(CHECK_WITH)
	@echo ----- sacsei -----
	$(fc) sacsei.for $(SL) $(links)

$(OUTPATH)/seedresp2gse: seedresp2gse.for $(CHECK_WITH)
	@echo ----- seedresp2gse -----
	$(fc) seedresp2gse.for $(SL) $(links)

$(OUTPATH)/sei2048: sei2048.c $(CHECK_WITH)
	@echo ----- sei2048 -----
	$(cc) sei2048.c

$(OUTPATH)/seiasc: seiasc.for $(CHECK_WITH)
	@echo ----- seiasc -----
	$(fc) seiasc.for $(SL) $(links)

$(OUTPATH)/seicut: seicut.for $(CHECK_WITH)
	@echo ----- seicut -----
	$(fc) seicut.for $(SL) $(links)

$(OUTPATH)/seidel: seidel.for $(CHECK_WITH)
	@echo ----- seidel -----
	$(fc) seidel.for $(SL) $(links)

$(OUTPATH)/sei2psxy: sei2psxy.for $(CHECK_WITH)
	@echo ----- sei2psxy -----
	$(fc) sei2psxy.for $(SL) $(links)

$(OUTPATH)/seigmt: seigmt.for $(CHECK_WITH)
	@echo ----- seigmt -----
	$(fc) seigmt.for $(SL) $(links)

$(OUTPATH)/seisaf: seisaf.for $(CHECK_WITH)
	@echo ----- seisaf -----
	$(fc) seisaf.for $(SL) $(links)

$(OUTPATH)/sgrsei: sgrsei.for $(CHECK_WITH)
	@echo ----- sgrsei -----
	$(fc) sgrsei.for $(SL) $(links)

$(OUTPATH)/seim88a: seim88a.for $(CHECK_WITH)
	@echo ----- seim88a -----
	$(fc) seim88a.for $(SL) $(links)

$(OUTPATH)/seipitsa: seipitsa.for $(CHECK_WITH)
	@echo ----- seipitsa -----
	$(fc) seipitsa.for $(SL) $(links)

$(OUTPATH)/seisei: seisei.for $(CHECK_WITH)
	@echo ----- seisei -----
	$(fc) seisei.for  $(SL) $(links)

$(OUTPATH)/select: select.for $(CHECK_WITH)
	@echo ----- select -----
	$(fc) select.for $(SL) $(links)

$(OUTPATH)/selmap: selmap.for $(CHECK_WITH)
	@echo ----- selmap -----
	$(fc) selmap.for $(SL) $(links)

$(OUTPATH)/selsei: selsei.for $(CHECK_WITH)
	@echo ----- selsei -----
	$(fc) selsei.for $(SL) $(links)

$(OUTPATH)/silsei: silsei.for $(CHECK_WITH)
	@echo ----- silsei -----
	$(fc) silsei.for $(SL) $(links)

$(OUTPATH)/sissei: sissei.for $(CHECK_WITH)
	@echo ----- sissei -----
	$(fc) sissei.for $(SL) $(links)

$(OUTPATH)/setbrn: setbrn.for $(CHECK_WITH)
	@echo ----- setbrn -----
	$(fc) setbrn.for $(SL) $(links)

$(OUTPATH)/spec: spec.for $(CHECK_WITH)
	@echo ----- spec -----
	$(fc) spec.for $(SL) $(links)

$(OUTPATH)/split: split.for $(CHECK_WITH)
	@echo ----- split -----
	$(fc) split.for $(SL) $(links)

$(OUTPATH)/stathist: stathist.for $(CHECK_WITH)
	@echo ----- stathist-----
	$(fc) stathist.for $(SL) $(links)

$(OUTPATH)/statis: statis.for $(CHECK_WITH)
	@echo ----- statis -----
	$(fc) statis.for $(SL) $(links)

$(OUTPATH)/statstat:  statstat.for $(CHECK_WITH)
	@echo ----- statstat  -----
	$(fc) statstat.for $(SL) $(links)

$(OUTPATH)/stasei:  stasei.for $(CHECK_WITH)
	@echo ----- stasei  -----
	$(fc) stasei.for $(SL) $(links)

$(OUTPATH)/swarm:  swarm.for $(CHECK_WITH)
	@echo ----- swarm  -----
	$(fc) swarm.for $(SL) $(links)

$(OUTPATH)/tersei: tersei.for $(CHECK_WITH)
	@echo ----- tersei -----
	$(fc) tersei.for $(SL) $(links)

$(OUTPATH)/trace_plot: trace_plot.for $(CHECK_WITH)
	@echo ----- trace_plot -----
	$(fc) trace_plot.for $(SL) $(links)

$(OUTPATH)/trace_plot2: trace_plot2.for $(CHECK_WITH)
	@echo ----- trace_plot2 -----
	$(fc) trace_plot2.for $(SL) $(links)

$(OUTPATH)/tsig:   tsig.for $(CHECK_WITH)
	@echo ----- tsig -----
	$(fc) tsig.for $(SL) $(links)

$(OUTPATH)/ttlayer: ttlayer.for $(CHECK_WITH)
	@echo ----- ttlayer -----
	$(fc) ttlayer.for $(SL) $(links)

$(OUTPATH)/ttim:   ttim.for $(CHECK_WITH)
	@echo ----- ttim -----
	$(fc) ttim.for $(SL) $(links)

$(OUTPATH)/ttplot:  ttplot.for $(CHECK_WITH)
	@echo ----- ttplot -----
	$(fc) ttplot.for $(SL) $(links)

$(OUTPATH)/ttlocal:  ttlocal.for $(CHECK_WITH)
	@echo ----- ttlocal -----
	$(fc) ttlocal.for $(SL) $(links)

$(OUTPATH)/upd:    upd.for $(CHECK_WITH)
	@echo ----- upd -----
	$(fc) upd.for $(SL) $(links)

$(OUTPATH)/update: update.for $(CHECK_WITH)
	@echo ----- update -----
	$(fc) update.for $(SL) $(links)

$(OUTPATH)/usgsnor: usgsnor.for $(CHECK_WITH)
	@echo ----- usgsnor -----
	$(fc) usgsnor.for $(SL) $(links)

$(OUTPATH)/velmenu: velmenu.for $(CHECK_WITH)
	$(fc) velmenu.for $(SL) $(links)

$(OUTPATH)/velest: velest.for $(CHECK_WITH)
	$(fc) velest.for $(SL) $(links)

$(OUTPATH)/velnor: velnor.for $(CHECK_WITH)
	$(fc) velnor.for $(SL) $(links)

$(OUTPATH)/volcstat: volcstat.for $(CHECK_WITH)
	@echo ----- volcstat -----
	$(fc) volcstat.for $(SL) $(links)
   
$(OUTPATH)/wadati: wadati.for $(CHECK_WITH)
	@echo ----- wadati -----
	$(fc) wadati.for $(SL) $(links)
	
$(OUTPATH)/wad_plot: wad_plot.for $(CHECK_WITH)
	@echo ----- wad_plot-----
	$(fc) wad_plot.for $(SL) $(links)

$(OUTPATH)/wavfix: wavfix.for $(CHECK_WITH)
	@echo ----- wavfix -----
	$(fc) wavfix.for $(SL) $(links)

$(OUTPATH)/wgssei: wgssei.for $(CHECK_WITH)
	@echo ----- wgssei -----
	$(fc) wgssei.for $(SL) $(links)

$(OUTPATH)/wkbj:   wkbj.for $(CHECK_WITH)
	@echo ----- wkbj -----
	$(fc) wkbj.for $(SL) $(links)

$(OUTPATH)/wkbj_or:   wkbj_or.for $(CHECK_WITH)
	@echo ----- wkbj_or -----
	$(fc) wkbj_or.for $(SL) $(links)

$(OUTPATH)/wavasc:   wavasc.for $(CHECK_WITH)
	@echo ----- wavasc -----
	$(fc) wavasc.for $(SL) $(links) 

$(OUTPATH)/wav_add:   wav_add.for $(CHECK_WITH)
	@echo ----- wav_add -----
	$(fc) wav_add.for $(SL) $(links) 

$(OUTPATH)/wvint_seisan:   wvint_seisan.for $(CHECK_WITH)
	@echo ----- wvint_seisan -----
	$(fc) wvint_seisan.for $(SL) $(links) 


$(OUTPATH)/xclust:   xclust.for $(CHECK_WITH)
	@echo ----- xclust -----
	$(fc) xclust.for $(SL) $(links)

$(OUTPATH)/xtest: xtest.for $(CHECK_WITH)
	@echo ----- xtest -----
	$(fc) xtest.for $(SL) $(links)
$(OUTPATH)/boxes: boxes.for $(CHECK_WITH)
	@echo ----- boxes -----
	$(fc) boxes.for $(SL) $(links)


#
# c files
#
$(OUTPATH)/autophase: autophase.c $(CHECK_WITH)
	@echo ----- autophase -----
	$(cc) autophase.c $(SL) $(links) -lm -O 

$(OUTPATH)/getstressdrop: getstressdrop.c $(CHECK_WITH)
	@echo ----- getstressdrop -----
	$(cc) getstressdrop.c $(SL) -lm

$(OUTPATH)/get_web_locations:   get_web_locations.c $(CHECK_WITH)
	@echo ----- get_web_locations -----
	$(cc) get_web_locations.c $(SL) -lm

$(OUTPATH)/msedit:   msedit.c $(CHECK_WITH)
	@echo ----- msedit -----
	$(cc) msedit.c $(SL) -lm

$(OUTPATH)/msrouter1:   msrouter1.c $(CHECK_WITH)
	@echo ----- msrouter1 -----
	$(cc) msrouter1.c $(SL) -lm

$(OUTPATH)/sample_read_write_s_c:  sample_read_write_s_c.cpp  
	@echo ----- sample_read_write_s_c-----
	$(ccc) sample_read_write_s_c.cpp   $(SL) -lm -lgfortran -lquadmath -lwsock32
# -luser32 -lgdi32 -lopengl32 -lwsock32

$(OUTPATH)/slick:   slick.c $(CHECK_WITH)
	@echo ----- slick -----
	$(cc) slick.c $(SL) -lm

$(OUTPATH)/selectc: selectc.c $(CHECK_WITH)
	@echo ----- selectc -----
	$(cc) selectc.c $(SL)

$(OUTPATH)/tdmt_invc_seisan:   tdmt_invc_seisan.c $(CHECK_WITH)
	@echo ----- tdmt_invc_seisan -----
	$(cc) tdmt_invc_seisan.c $(SL) -lm -lgfortran -lquadmath

#
# remove all programs
#
clean:
	rm -f $(PROGS) $(EXEC_ONLY) $(ONLY)
	cd $(LIB); $(MAKE); cd $(LIB_TO_PRO)

# 
# install all files defined by $(PROG) and $(ONLY), 
# if newer than files in $(INSTALL_PRO_PATH)
#
install: install-progs install-only install-exec

install-progs: 
	@for i in $(PROGS) ; do \
	  echo "Install: $(INSTALL) $$i $(INSTALL_PRO_PATH)"; \
	  $(INSTALL) $$i $(INSTALL_PRO_PATH); \
	done

install-only:
	@for i in $(PLATFORM_DEP_EXE) ; do \
	  echo "Install: $(INSTALL) $$i $(INSTALL_PRO_PATH)"; \
	  $(INSTALL) $$i $(INSTALL_PRO_PATH); \
	done

install-exec:
	@for i in $(EXEC_ONLY) ; do \
	  echo "Install: $(INSTALL) $$i $(INSTALL_PRO_PATH)"; \
	  $(INSTALL) $$i $(INSTALL_PRO_PATH); \
	done


