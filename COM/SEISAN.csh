#    
# SEISAN definitions to be sourced from users .cshrc file
#

#
# set platform, used for compilation, can be solaris , gfortran or g77
# MUST BE: solaris, linux32, linux64, windows, macosx or macosxppc
#
setenv SEISARCH linux64
#setenv SEISARCH linux32
#setenv SEISARCH solaris

#
#    set SEISAN top directory
#
setenv SEISAN_TOP /home/s2000/seismo

#
# set default data base
#
setenv DEF_BASE TEST_

#
# set editor
#
setenv SEISAN_EDITOR vi

#
# set printer
#
setenv PRINTER sps

#
# set X and Y scaling for Postscript output 
#   some examples:  A4     x=0.55,y=1.0
#                   Letter x=0.55,y=0.9
#
setenv SEISAN_PSSCALE_X 0.55 
setenv SEISAN_PSSCALE_Y 1.0 

#
# aliases for SEISAN
#
alias pr            'cd $SEISAN_TOP/PRO'     
alias li            'cd $SEISAN_TOP/LIB'      
alias ic            'cd $SEISAN_TOP/INC'        
alias re            'cd $SEISAN_TOP/REA'
alias da            'cd $SEISAN_TOP/DAT'
alias wo            'cd $SEISAN_TOP/WOR'
alias wa            'cd $SEISAN_TOP/WAV'
alias ca            'cd $SEISAN_TOP/CAL'
alias co            'cd $SEISAN_TOP/COM'
alias in            'cd $SEISAN_TOP/INF'
alias is            'cd $SEISAN_TOP/ISO'

#
# include seismo/LIB in LD_LIBRARY_PATH, needed by NANSEI
#
if ($?LD_LIBRARY_PATH) then
  setenv LD_LIBRARY_PATH $SEISAN_TOP/LIB:$LD_LIBRARY_PATH
else
  setenv LD_LIBRARY_PATH $SEISAN_TOP/LIB
endif


#
# aliases for Java tools
#
alias sformat 'java -DSEISAN_TOP=$SEISAN_TOP -classpath $SEISAN_TOP/PRO/sformat.jar Sformat' 
alias smap 'java -jar $SEISAN_TOP/PRO/seis2viewer.jar $1'
alias nor2qml 'java -jar  $SEISAN_TOP/PRO/nor2qml.jar'
#
#   alias python tools
#
alias mopad 'python $SEISAN_TOP/PRO/mopad.py $1 $2'
alias spectrogram 'python  $SEISAN_TOP/PRO/spectrogram.py $1 $2 $3 $4'
#
# path
#
set pro_path = ($SEISAN_TOP/PRO)
if( `echo $path | grep -c pro_path` == 0 ) then 
  set path=($pro_path $path)
endif
set com_path = ($SEISAN_TOP/COM)
if( `echo $path | grep -c com_path` == 0 ) then 
  set path=($com_path $path)
endif

#
# set SEISAN extension code, this variable can be used to implement specific 
# code into SEISAN, at the moment used are:
#                BGS
#
setenv SEISAN_EXTENSION none

