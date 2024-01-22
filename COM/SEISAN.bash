#
# SEISAN definitions to be sourced from users .bashrc file 
#

#
# set platform, used for compilation, 
# MUST BE:  linux32, linux64, windows, macosx or macosxppc
#
export SEISARCH="linux64"
#export SEISARCH="linux32"
#export SEISARCH="solaris"

#
#    set SEISAN top directory
#
export SEISAN_TOP="/home/s2000/seismo"   

#
# set editor
#
export SEISAN_EDITOR="vi"

#
# set def base 
#
export DEF_BASE="TEST_"

#
# set printer
#
export PRINTER="hpbw2"

#
# set X and Y scaling for Postscript output
#   some examples:  A4     x=0.55,y=1.0
#                   Letter x=0.55,y=0.9
#
export SEISAN_PSSCALE_X="0.55"
export SEISAN_PSSCALE_Y="1.0"

#
# set up search PATH
#
export PATH="$SEISAN_TOP/PRO:$PATH"
export PATH="$SEISAN_TOP/COM:$PATH"

#
# aliases for SEISAN
#
alias pr='cd $SEISAN_TOP/PRO'
alias li='cd $SEISAN_TOP/LIB'
alias ic='cd $SEISAN_TOP/INC'
alias re='cd $SEISAN_TOP/REA'
alias da='cd $SEISAN_TOP/DAT'
alias wo='cd $SEISAN_TOP/WOR'
alias wa='cd $SEISAN_TOP/WAV'
alias ca='cd $SEISAN_TOP/CAL'
alias co='cd $SEISAN_TOP/COM'
alias in='cd $SEISAN_TOP/INF'



#
# aliases for Java tools
#
alias sformat='java -DSEISAN_TOP=$SEISAN_TOP -classpath $SEISAN_TOP/PRO/sformat.jar Sformat'

alias smap='java -jar $SEISAN_TOP/PRO/seis2viewer.jar $1'
alias nor2qml='java -jar  $SEISAN_TOP/PRO/nor2qml.jar'

#
#   alias python tools
#
alias  mopad='python $SEISAN_TOP/PRO/mopad.py $1 $2'
alias  spectrogram='python $SEISAN_TOP/PRO/spectrogram.py $1 $2i $3 $4'


#
# set SEISAN extension code, this variable can be used to implement specific 
# code into SEISAN, at the moment used are:
#                BGS
#
export SEISAN_EXTENSION="none"

