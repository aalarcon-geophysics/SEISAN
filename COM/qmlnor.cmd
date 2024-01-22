@echo off
rem  Script to convert from qml to Nordic with filename as only option
set infile=%1
IF "%infile%"=="" (goto nofile)
@echo on
java -jar  %SEISAN_TOP%/PRO/nor2qml.jar --convert="s" --version="nordic" --input=%1
@echo off
echo Output Nordic file is sfile.out
goto end
:nofile
echo You must give an input qml xml  file
:end 
